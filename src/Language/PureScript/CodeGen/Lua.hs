-- Mostly copied from JS version.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.PureScript.CodeGen.Lua (
                                         moduleToLua
                                       , module Common
                                       ) where

import Language.PureScript.Crash
import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.Options
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C

import Language.PureScript.CodeGen.Lua.Common as Common
import Language.PureScript.CodeGen.Lua.Optimizer

import qualified Language.Lua.Syntax as L
import qualified Language.Lua.PrettyPrinter as L

import Control.Arrow ((&&&))

import Data.Maybe
import Data.List ((\\), delete, intersect)

import qualified Data.Map.Strict as M

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.Supply.Class

import System.FilePath.Posix ((</>))


-- | Creates a Lua Block returning the exports of a module in a table.
moduleToLua :: (MonadReader Options m, MonadSupply m, MonadError MultipleErrors m) =>
               Module Ann -> Maybe L.Exp -> m L.Block
moduleToLua (Module coms mn imps exps foreigns decls) foreign_ =
  rethrow (addHint (ErrorInModule mn)) $ do
    let usedNames = getNames =<< decls
        mnLookup = renameImports usedNames imps
    luaImports <- traverse (importToLua mnLookup) .
                  delete (ModuleName [ProperName C.prim]) .
                  (\\ [mn]) $ imps
    let decls' = renameModules mnLookup decls
    luaDecls <- traverse (bindToLua mn) decls'
    let foreign' = L.LocalAssign ["_foreign"] (mkForeign <$> foreign_)
        mkForeign exp = [exp]
        moduleBody = foreign' : luaImports ++ concat luaDecls
        foreignExps = intersect exps (map fst foreigns)
        standardExps = exps \\ foreignExps
        expPairs = map (string . runIdent &&& var . identToLua) standardExps
                   ++ map (string . runIdent &&& foreignIdent) foreignExps
        exps' = L.TableConst $ map (uncurry L.ExpField) expPairs
    return . optimize $ L.Block moduleBody (Just [exps'])


-- | Extracts all declaration names from a binding group.
getNames :: Bind Ann -> [Ident]
getNames (NonRec ident _) = [ident]
getNames (Rec vals) = map fst vals


-- | Creates alternative names for each module so they don't collide with
-- declarations.
renameImports :: [Ident] -> [ModuleName] -> M.Map ModuleName ModuleName
renameImports ids mns = go M.empty ids mns
  where
    go :: M.Map ModuleName ModuleName -> [Ident] -> [ModuleName] -> M.Map ModuleName ModuleName
    go acc used (mn':mns') =
      let mni = Ident $ runModuleName mn'
      in if mni `elem` used
         then let newName = freshModuleName 1 mn' used
              in go (M.insert mn' newName acc) (Ident (runModuleName newName) : used) mns'
         else go (M.insert mn' mn' acc) (mni : used) mns'
    go acc _ [] = acc

    freshModuleName :: Integer -> ModuleName -> [Ident] -> ModuleName
    freshModuleName i mn'@(ModuleName pns) used =
      let newName = ModuleName $ init pns ++ [ProperName $ runProperName (last pns) ++ "_" ++ show i]
      in if Ident (runModuleName newName) `elem` used
         then freshModuleName (i + 1) mn' used
         else newName


-- | Generates Lua code for a module import, binding the required module to the
-- alternative.
importToLua :: MonadReader Options m => M.Map ModuleName ModuleName -> ModuleName
               -> m L.Stat
importToLua mnLookup mn' = do
  path <- asks optionsRequirePath
  let mnSafe = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
      moduleBody = funcall (var "require") [string pathString]
      pathString = maybe id (</>) path $ runModuleName mn'
  return $
    L.LocalAssign [moduleNameToLua mnSafe] $ Just [moduleBody]


-- | Replaces the `ModuleName`s in the AST so that the generated code refers to
-- the collision-avoiding renamed module imports.
renameModules :: M.Map ModuleName ModuleName -> [Bind Ann] -> [Bind Ann]
renameModules mnLookup binds =
  let (f, _, _) = everywhereOnValues id goExpr goBinder
  in map f binds
  where
    goExpr :: Expr a -> Expr a
    goExpr (Var ann q) = Var ann (renameQual q)
    goExpr e = e
    goBinder :: Binder a -> Binder a
    goBinder (ConstructorBinder ann q1 q2 bs) = ConstructorBinder ann (renameQual q1) (renameQual q2) bs
    goBinder b = b
    renameQual :: Qualified a -> Qualified a
    renameQual (Qualified (Just mn') a) =
      let mnSafe = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
      in Qualified (Just mnSafe) a
    renameQual q = q


-- | Generate code in the Lua AST for a declaration
bindToLua :: (MonadError MultipleErrors m, MonadSupply m, MonadReader Options m) =>
             ModuleName -> Bind Ann -> m [L.Stat]
bindToLua mn (NonRec ident val@Constructor{}) = do
  lua <- valueToLua mn val
  let luaIdent = identToLua ident
  return $ [L.LocalAssign [luaIdent] Nothing, L.Assign [L.VarName luaIdent] [lua]]
bindToLua mn (NonRec ident val) = do
  lua <- valueToLua mn val
  return $ [L.LocalAssign [identToLua ident] (Just [lua])]
bindToLua mn (Rec vals) = (emptyDecs:) <$> traverse makeAss vals
  where names = map fst vals
        emptyDecs = L.LocalAssign (map identToLua names) Nothing
        makeAss (ident, val) = do
          lua <- valueToLua mn val
          return $ L.Assign [L.VarName $ identToLua ident] [lua]
  

-- | Generate code in the Lua AST for a value or expression
valueToLua :: (MonadSupply m, MonadError MultipleErrors m, MonadReader Options m) =>
              ModuleName -> Expr Ann -> m L.Exp
valueToLua mn (Literal (pos, _, _, _) l) =
  maybe id rethrowWithPosition pos $ literalToValueLua mn l
valueToLua mn (Var (_, _, _, Just (IsConstructor _ [])) name) =
  return $ selectS (qualifiedToLua mn id name) "value"
valueToLua mn (Var (_, _, _, Just (IsConstructor _ _)) name) =
  return $ selectS (qualifiedToLua mn id name) "create"
valueToLua mn (Accessor _ prop val) =
  flip selectS prop <$> valueToLua mn val
valueToLua mn (ObjectUpdate _ o ps) = do
  obj <- valueToLua mn o
  sts <- traverse (sndM (valueToLua mn)) ps
  extendObj obj sts
valueToLua mn e@(Abs _ arg val) = do
  ret <- valueToLua mn val
  return . L.EFunDef $ L.FunBody [identToLua arg] False (L.Block [] (Just [ret]))
valueToLua mn e@App{} = do
  let (f, args) = unApp e []
  args' <- traverse (valueToLua mn) args
  case f of
   Var (_, _, _, Just IsNewtype) _ -> return (head args')
   Var (_, _, _, Just (IsConstructor _ fields)) name
     | length args == length fields ->
         return . L.PrefixExp . L.PEFunCall $
         L.MethodCall (expToPexp $ qualifiedToLua mn id name) "new" (L.Args args')
   _ -> flip (foldl (\fn a -> funcall fn [a])) args' <$> valueToLua mn f
  where
    unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
    unApp (App _ val arg) args = unApp val (arg : args)
    unApp other args = (other, args)
valueToLua mn (Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
  return $ if mn == mn'
           then foreignIdent ident
           else varToLua mn qi
valueToLua mn (Var (_, _, _, Just IsForeign) ident) =
  error $ "Encountered an unqualified reference to foreign ident " ++ showQualified showIdent ident
valueToLua mn (Var _ ident) =
  return $ varToLua mn ident
valueToLua mn (Case (maybeSpan, _, _, _) values binders) = do
  vals <- traverse (valueToLua mn) values
  bindersToLua mn maybeSpan binders vals
valueToLua mn (Let _ ds val) = do
  ds' <- concat <$> traverse (bindToLua mn) ds
  ret <- valueToLua mn val
  let function =
        expToPexp . L.EFunDef $ L.FunBody [] False (L.Block ds' (Just [ret]))
  return . L.PrefixExp . L.PEFunCall $ L.NormalFunCall function (L.Args [])
valueToLua _ (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) =
  let create = L.EFunDef $ L.FunBody ["value"] False funBody
      funBody = L.Block [] (Just [var "value"])
  in return $ L.TableConst [L.NamedField "create" create]
valueToLua _ (Constructor _ _ (ProperName ctor) []) =
  let value = L.TableConst [L.NamedField "ctor" (string ctor)]
      new = L.EFunDef $ L.FunBody ["_"] False (L.Block [] (Just [value]))
  in return $ L.TableConst [ L.NamedField "value" value
                           , L.NamedField "new" new
                           ]
valueToLua _ (Constructor _ _ (ProperName ctor) fields) =
  let args = map identToLua fields
      constructor =
        let body =
              L.Block []
              (Just [L.TableConst $ L.NamedField "ctor" (string ctor) : map (L.Field . var) args])
        in L.EFunDef $ L.FunBody ("_" : args) False body
      create =
        let created = L.PrefixExp .
                      L.PEFunCall $
                      L.MethodCall (expToPexp . var . identToLua . Ident $  ctor)
                      "new"
                      (L.Args $ map var args)
        in foldr (\f inner -> L.EFunDef $ L.FunBody [f] False (L.Block [] (Just [inner])))
           created
           args
  in return $ L.TableConst [ L.NamedField "new" constructor
                           , L.NamedField "create" create
                           ]


literalToValueLua :: (MonadSupply m, MonadError MultipleErrors m, MonadReader Options m) =>
                     ModuleName -> Literal (Expr Ann) -> m L.Exp
literalToValueLua _ (NumericLiteral ei) = pure $ L.Number . either show show $ ei
literalToValueLua _ (StringLiteral s) = pure $ string s
literalToValueLua _ (CharLiteral c) = pure $ string [c]
literalToValueLua _ (BooleanLiteral b) = pure $ L.Bool b
literalToValueLua mn (ArrayLiteral xs) =
  let mkField val = L.Field <$> valueToLua mn val
  in L.TableConst <$> traverse mkField xs
literalToValueLua mn (ObjectLiteral ps) =
  let mkField k v = L.ExpField (string k) <$> valueToLua mn v
  in L.TableConst <$> traverse (uncurry mkField) ps


-- | Shallow copy a table
extendObj :: MonadSupply m => L.Exp -> [(String, L.Exp)] -> m L.Exp
extendObj obj extensions = do
  newObjName <- freshName
  let newObj = L.LocalAssign [newObjName] (Just [L.TableConst []])
      copyObj = L.ForIn ["k", "v"] [funcall (var "pairs") [obj]] copyBody
      copyBody = L.Block [L.Assign [lhs] [rhs]] Nothing
      lhs = L.Select (expToPexp $ var newObjName) (var "k")
      rhs = var "v"
      setNewFields = map (uncurry setField) extensions
      setField k v = L.Assign [L.Select (expToPexp $ var newObjName) (string k)] [v]
      fun = L.EFunDef . L.FunBody [newObjName] False $ funBody
      funBody = L.Block (newObj : copyObj : setNewFields) (Just [var newObjName])
  return $ L.PrefixExp . L.PEFunCall $ L.NormalFunCall (L.Paren fun) (L.Args [obj])


varToLua :: ModuleName -> Qualified Ident -> L.Exp
varToLua _ (Qualified Nothing ident) = var . identToLua $ ident
varToLua mn qual = qualifiedToLua mn id qual


-- | Generate code in the Lua AST for a reference to a variable that may have a
-- qualified name
qualifiedToLua :: ModuleName -> (a -> Ident) -> Qualified a -> L.Exp
qualifiedToLua mn f (Qualified (Just (ModuleName [ProperName mn'])) a)
  | mn' == C.prim = var . runIdent $ f a
qualifiedToLua mn f (Qualified (Just mn') a)
  | mn /= mn' =
      select (var $ moduleNameToLua mn')
      (string . runIdent $ f a)
qualifiedToLua mn f (Qualified _ a) =
  var . identToLua $ f a


foreignIdent :: Ident -> L.Exp
foreignIdent ident = select (var "_foreign") (string $ runIdent ident)


-- | Generate code in the Lua AST for pattern match binders and guards
bindersToLua :: forall m. (MonadSupply m, MonadError MultipleErrors m, MonadReader Options m) =>
                ModuleName -> Maybe SourceSpan -> [CaseAlternative Ann] -> [L.Exp] -> m L.Exp
bindersToLua mn maybeSpan binders vals = do
  valNames <- replicateM (length vals) freshName
  let assignments = L.LocalAssign valNames (Just vals)
  luas <- forM binders $ \(CaseAlternative bs result) -> do
    ret <- guardsToLua result
    go valNames ret bs
  let funBody =
        L.Block (assignments : concat luas ++ [funcallS "error" [failedPatternError valNames]])
        Nothing
  return $
    funcall (L.EFunDef $ L.FunBody [] False funBody) []
  where
    go :: [String] -> [L.Stat] -> [Binder Ann] -> m [L.Stat]
    go _ done [] = return done
    go (v:vs) done' (b:bs) = do
      done'' <- go vs done' bs
      binderToLua mn v done'' b
    go _ _ _ = internalError "Invalid arguments to bindersToLua"

    failedPatternError :: [String] -> L.Exp
    failedPatternError _ = string $ "Pattern match failure at " ++
                           maybe "" displayStartEndPos maybeSpan

    guardsToLua :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [L.Stat]
    guardsToLua (Left gs) = forM gs $ \(cond, val) -> do
      cond' <- valueToLua mn cond
      done <- valueToLua mn val
      return $ L.If [(cond', L.Block [] (Just [done]))] Nothing
    guardsToLua (Right v) = (:[]) . L.Do . L.Block [] . Just . (:[]) <$> valueToLua mn v


binderToLua :: forall m. MonadSupply m =>
               ModuleName -> String -> [L.Stat] -> Binder Ann -> m [L.Stat]
binderToLua _ _ done (NullBinder{}) = return done
binderToLua mn varName done (LiteralBinder _ l) = literalToBinderLua mn varName done l
binderToLua _ varName done (VarBinder _ ident) =
  return $ L.LocalAssign [identToLua ident] (Just [var varName]) : done
binderToLua mn varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
  binderToLua mn varName done b
binderToLua mn varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
  lua <- go (zip3 [1..] fields bs) done
  let Qualified _ ctor' = ctor
  return $ case ctorType of
    ProductType -> lua
    SumType ->
      let argCtor = selectS (var varName) "ctor"
      in [L.If
          [(L.Binop L.EQ argCtor (string . runProperName $ ctor'),
            L.Block lua Nothing)] Nothing]
  where
    go :: [(Int, Ident, Binder Ann)] -> [L.Stat] -> m [L.Stat]
    go [] done' = return done'
    go ((i, field, binder) : remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      lua <- binderToLua mn argVar done'' binder
      return $
        L.LocalAssign [identToLua . Ident $ argVar]
        (Just [select (var varName) (L.Number $ show i)]) : lua
binderToLua _ _ _ ConstructorBinder{} =
  internalError "binderToLua: Invalid ConstructorBinder"
binderToLua mn varName done (NamedBinder _ ident binder) = do
  lua <- binderToLua mn varName done binder
  return $ L.LocalAssign [identToLua ident] (Just [var varName]) : lua


varIsEq :: String -> L.Exp -> L.Exp
varIsEq varName other =
  L.Binop L.EQ (var varName) other



literalToBinderLua :: forall m. MonadSupply m =>
                      ModuleName -> String -> [L.Stat] -> Literal (Binder Ann) -> m [L.Stat]
literalToBinderLua _ varName done (NumericLiteral num) =
  return $ [L.If
            [(varIsEq varName (L.Number $ either show show num), L.Block done Nothing)]
           Nothing ]
literalToBinderLua _ varName done (CharLiteral c) =
  return $ [L.If
            [(varIsEq varName (string [c]), L.Block done Nothing)]
            Nothing ]
literalToBinderLua _ varName done (StringLiteral s) =
  return $ [L.If
            [(varIsEq varName (string s), L.Block done Nothing)]
            Nothing ]
literalToBinderLua _ varName done (BooleanLiteral True) =
  return $ [L.If
            [(var varName, L.Block done Nothing)] Nothing]
literalToBinderLua _ varName done (BooleanLiteral False) =
  return $ [L.If
            [(L.Unop L.Not (var varName), L.Block done Nothing)] Nothing]
literalToBinderLua mn varName done (ObjectLiteral bs) = go done bs
  where
    go :: [L.Stat] -> [(String, Binder Ann)] -> m [L.Stat]
    go done' [] = return done'
    go done' ((prop, binder) : bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      lua <- binderToLua mn propVar done'' binder
      return $
        L.LocalAssign [propVar]
        (Just [select (var varName) (string prop)])
        : lua
literalToBinderLua mn varName done (ArrayLiteral bs) = do
  lua <- go done 1 bs
  return $
    [L.If
     [(L.Binop L.EQ (L.Unop L.Len $ var varName) (L.Number . show $ length bs),
       L.Block lua Nothing)] Nothing]
  where
    go :: [L.Stat] -> Integer -> [Binder Ann] -> m [L.Stat]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName
      done'' <- go done' (index + 1) bs'
      lua <- binderToLua mn elVar done'' binder
      return $
        L.LocalAssign [elVar]
        (Just [select (var varName) (L.Number . show $ index)])
        : lua
