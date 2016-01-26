module Language.PureScript.CodeGen.Lua.Optimizer (optimize) where

import Data.Generics
import Data.List(foldl')
import qualified Data.Set as S

import Language.PureScript.CodeGen.Lua.Common
import Language.PureScript.CodeGen.Lua.Subst

import qualified Language.Lua.Syntax as L

applyAll :: [a -> a] -> a -> a
applyAll = foldl' (.) id


optimize :: L.Block -> L.Block
optimize = applyAll [applyVars, applyVars', inlineEffOps, removeReturnApps,
                     removeVarAsgns, removeDupAsgns, inlineCommonOperators,
                     removeIfTrues, removeDollars]


applyVars :: Data a => a -> a
applyVars = everywhere (mkT applyVar)
  where
    applyVar
      (L.Block stats (Just
        [ L.PrefixExp (L.PEFunCall
           (L.NormalFunCall
            (L.Paren (L.EFunDef (L.FunBody args _ block)))
            (L.Args vars))) ]))
      | all isVar vars && length args == length vars =
          let vars' = map getVar vars
              L.Block stats' ret = substBlock' (zip args vars') block
          in L.Block (stats ++ stats') ret
    applyVar b = b

    substBlock' :: [(L.Name, L.Name)] -> L.Block -> L.Block
    substBlock' [] b = b
    substBlock' ((r, l) : ss) b = substBlock' ss (runSubst 0 (substBlock r l b))


applyVars' :: Data a => a -> a
applyVars' = everywhere (mkT applyVar)
  where
    applyVar :: L.PrefixExp -> L.PrefixExp
    applyVar
      (L.PEFunCall (L.NormalFunCall
        (L.Paren (L.EFunDef (L.FunBody args _ (L.Block [] (Just [exp])))))
        (L.Args vars)))
      | all isVar vars && length args == length vars =
          let vars' = map getVar vars
          in L.Paren (substExp' (zip args vars') exp)
    applyVar e = e
    
    substExp' :: [(L.Name, L.Name)] -> L.Exp -> L.Exp
    substExp' [] e = e
    substExp' ((r, l) : ss) e = substExp' ss (runSubst 0 (substExp r l e))


isVar :: L.Exp -> Bool
isVar (L.PrefixExp L.PEVar{}) = True
isVar (L.PrefixExp (L.Paren p)) = isVar p
isVar _ = False

getVar :: L.Exp -> L.Name
getVar (L.PrefixExp (L.PEVar (L.VarName var))) = var
getVar (L.PrefixExp (L.Paren e)) = getVar e
getVar notAVar = error $ "getVar on a non-var: " ++ show notAVar


-- | Replace `Prelude.$` calls with direct function applications.
-- This assumes using standard Prelude.
removeDollars :: Data a => a -> a
removeDollars = everywhere (mkT removeDollarExp)
  where
    removeDollarExp
      (L.PrefixExp (L.PEFunCall (L.NormalFunCall
        (L.PEFunCall (L.NormalFunCall (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String "\"$\"")))
                                      (L.Args [arg1])))
        (L.Args [arg2])))) =
        L.PrefixExp $ L.PEFunCall $ L.NormalFunCall (expToPexp arg1) $ L.Args [arg2]
    removeDollarExp exp = exp


-- | Replace `if true then stats end` with `stats`. If `stats` has a return
-- statement, then remove all following statements.
removeIfTrues :: Data a => a -> a
removeIfTrues = everywhere (mkT removeIfTrue)
  where
    removeIfTrue
      b@(L.Block stats ret) =
        case searchIfTrue stats of
         Nothing -> b
         Just (prevStats, ifBody, Left restStats) ->
           -- `if true then stats end` found, without return statement in `stats`
           L.Block (prevStats ++ ifBody ++ restStats) ret
         Just (prevStats, ifBody, Right ret') ->
           -- `if true then stats end` found, `stats` have a return statement
           L.Block (prevStats ++ ifBody) (Just ret')

    searchIfTrue [] = Nothing
    searchIfTrue (L.If [(L.Bool True, L.Block ifBody Nothing)] Nothing : rest) =
      return ([], ifBody, Left rest)
    searchIfTrue (L.If [(L.Bool True, L.Block ifBody (Just ret))] Nothing : _) =
      return ([], ifBody, Right ret)
    searchIfTrue (stat : stats) = do
      (prevs, ifBody, rest) <- searchIfTrue stats
      return (stat : prevs, ifBody, rest)


-- | In the codegen we never mutate a variable, so remove any reassignments
-- of varibles in blocks.
-- (multiple assignments of variables are introduced in codegen for pattern matching)
removeDupAsgns :: Data a => a -> a
removeDupAsgns = everywhere (mkT rmDupAsgns)
  where
    rmDupAsgns :: L.Block -> L.Block
    rmDupAsgns (L.Block stats ret) = L.Block (iterStats S.empty stats) ret
    
    iterStats :: S.Set L.Name -> [L.Stat] -> [L.Stat]
    iterStats names (stat@(L.LocalAssign [name] (Just [_])) : rest)
      | name `S.member` names = iterStats names rest
      | otherwise = stat : iterStats (S.insert name names) rest
    iterStats names (stats : rest) = stats : iterStats names rest
    iterStats _ [] = []


-- | Replace variables assigned to another variables with their right-hand
    -- sides.
removeVarAsgns :: Data a => a -> a
removeVarAsgns = everywhere (mkT rmVarAsgns)
  where
    rmVarAsgns :: L.Block -> L.Block
    rmVarAsgns (L.Block stats ret) =
      uncurry L.Block $ iterStats stats ((fmap . fmap $ removeVarAsgns) ret)

    iterStats :: [L.Stat] -> Maybe [L.Exp] -> ([L.Stat], Maybe [L.Exp])
    iterStats (stat@(L.LocalAssign [name] (Just [rhs])) : rest) rets
      | isVar rhs = iterStats (map (subst name rhs) rest) ((fmap . fmap $ subst name rhs) rets)
      | otherwise =
          let (stats', rets') = iterStats rest rets
          in (stat : stats', rets')
    iterStats (stat : rest) rets =
      let (stats', rets') = iterStats rest rets
      in (stat : stats', rets')
    iterStats [] rets = ([], rets)

    subst :: Data a => L.Name -> L.Exp -> a -> a
    subst var rhs = everywhere (mkT subst')
      where
        subst' pexp@(L.PEVar ((L.VarName var')))
          | var == var' = expToPexp rhs
          | otherwise   = pexp
        subst' pexp = pexp


-- | Remove redundant function application in `return (function () .. stats .. end)()`.
removeReturnApps :: Data a => a -> a
removeReturnApps = everywhere (mkT rmRet)
  where
    rmRet (L.Block stats (Just [L.PrefixExp
            (L.PEFunCall (L.NormalFunCall (L.Paren (L.EFunDef (L.FunBody [] _ (L.Block stats' (Just rets)))))
                                          (L.Args [])))])) =
      L.Block (stats ++ stats') (Just rets)
    rmRet b = b


-- | Inline Eff monad operations.
inlineEffOps :: Data a => a -> a
inlineEffOps = everywhere (mkT iter)
  where
    iter :: L.Exp -> L.Exp

    -- inline whileE
    iter (L.PrefixExp (L.PEFunCall (L.NormalFunCall
           (L.PEFunCall (L.NormalFunCall
             (L.PEVar (L.Select (L.PEVar (L.VarName "Control_Monad_Eff")) (L.String "\"whileE\"")))
             (L.Args [arg1])))
           (L.Args [arg2])))) =
      let body = L.While (funcall arg1 []) (L.Block [funcallStat arg2 []] Nothing)
      in L.EFunDef $ L.FunBody [] False (L.Block [body] $ Just [])

    -- inline untilE
    iter (L.PrefixExp (L.PEFunCall (L.NormalFunCall
           (L.PEVar (L.Select (L.PEVar (L.VarName "Control_Monad_Eff")) (L.String "\"untilE\"")))
           (L.Args [arg])))) =
      let body = L.While (L.Unop L.Not (funcall arg [])) (L.Block [] Nothing)
      in L.EFunDef $ L.FunBody [] False (L.Block [body] $ Just [])

    -- inline return and pure
    iter (L.PrefixExp (L.PEFunCall (L.NormalFunCall
             (L.PEFunCall (L.NormalFunCall
               (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String method)))
               (L.Args [L.PrefixExp
                 (L.PEVar (L.Select (L.PEVar (L.VarName "Control_Monad_Eff")) (L.String "\"applicativeEff\"")))
               ])))
             (L.Args [arg1]))))
      | method `elem` ["\"return\"", "\"pure\""] =
          L.EFunDef (L.FunBody [] False (L.Block [] (Just [arg1])))
             

    -- inline (>>=) and (>>)
    iter e@(L.PrefixExp (L.PEFunCall (L.NormalFunCall
             (L.PEFunCall (L.NormalFunCall
               (L.PEFunCall (L.NormalFunCall
                 (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String method)))
                 (L.Args [L.PrefixExp 
                   (L.PEVar (L.Select (L.PEVar (L.VarName "Control_Monad_Eff")) (L.String "\"bindEff\"")))
                 ])))
               (L.Args [arg1])))
             (L.Args [arg2]))))
      | method `elem` ["\"bind\"", "\">>=\""] =
      case arg2 of
        L.EFunDef (L.FunBody ["_"] False (L.Block [] (Just [ret]))) ->
          let body = [funcallStat arg1 []]
          in L.EFunDef $ L.FunBody [] False (L.Block body (Just [funcall ret []]))
        L.EFunDef (L.FunBody [argName] False (L.Block [] (Just [ret]))) ->
          let body = [L.LocalAssign [argName] $ Just [funcall arg1 []]]
          in L.EFunDef $ L.FunBody [] False (L.Block body (Just [funcall ret []]))
        _ -> e

    iter e = e


inlineCommonOperators :: Data a => a -> a
inlineCommonOperators = everywhere (mkT inlineCommonOperators')

inlineCommonOperators' :: L.Exp -> L.Exp
inlineCommonOperators' = applyAll
    [ binary "semiringNumber" "+" L.Add
    , binary "semiringNumber" "*" L.Mul
    , binary "semiringNumber" "add" L.Add
    , binary "semiringNumber" "mul" L.Mul
    , binary "ringNumber" "-" L.Sub
    , binary "ringNumber" "sub" L.Sub
    , binary "moduloSemiringNumber" "/" L.Div
    , binary "moduloSemiringNumber" "div" L.Div
      
    , binary "semiringInt" "+" L.Add
    , binary "semiringInt" "*" L.Mul
    , binary "semiringInt" "add" L.Add
    , binary "semiringInt" "mul" L.Mul
    , binary "ringInt" "-" L.Sub
    , binary "ringInt" "sub" L.Sub
    , binary "moduloSemiringInt" "mod" L.Mod
    , binary "moduloSemiringInt" "%" L.Mod

    , binary "ordNumber" "<" L.LT
    , binary "ordNumber" ">" L.GT
    , binary "ordNumber" ">=" L.GTE
    , binary "ordNumber" "<=" L.LTE

    , binary "ordInt" "<" L.LT
    , binary "ordInt" ">" L.GT
    , binary "ordInt" ">=" L.GTE
    , binary "ordInt" "<=" L.LTE

    , binary "ordBoolean" "<" L.LT
    , binary "ordBoolean" ">" L.GT
    , binary "ordBoolean" ">=" L.GTE
    , binary "ordBoolean" "<=" L.LTE

    , binary "ordString" "<" L.LT
    , binary "ordString" ">" L.GT
    , binary "ordString" ">=" L.GTE
    , binary "ordString" "<=" L.LTE

    , binary "ordChar" "<" L.LT
    , binary "ordChar" ">" L.GT
    , binary "ordChar" ">=" L.GTE
    , binary "ordChar" "<=" L.LTE

    , binary "eqNumber" "==" L.EQ
    , binary "eqNumber" "/=" L.NEQ
    , binary "eqInt" "==" L.EQ
    , binary "eqInt" "/=" L.EQ
    , binary "eqString" "==" L.EQ
    , binary "eqString" "/=" L.NEQ
    , binary "eqBoolean" "==" L.EQ
    , binary "eqBoolean" "/=" L.NEQ
    , binary "eqChar" "==" L.EQ
    , binary "eqChar" "/=" L.EQ

    , binary "semigroupString" "++" L.Concat
    , binary "semigroupString" "<>" L.Concat
    , binary "semigroupString" "append" L.Concat

    , binary "booleanAlgebraBoolean" "&&" L.And
    , binary "booleanAlgebraBoolean" "||" L.Or
    , binary "booleanAlgebraBoolean" "conj" L.And
    , binary "booleanAlgebraBoolean" "disj" L.Or

    , unary "ringNumber" "negate" L.Neg
    , unary "booleanAlgebraBoolean" "not" L.Not
    ]
  where
    binary :: String -> String -> L.Binop -> L.Exp -> L.Exp
    binary typecls method binop exp =
      case exp of
       (L.PrefixExp (L.PEFunCall (L.NormalFunCall
         (L.PEFunCall (L.NormalFunCall
           (L.PEFunCall (L.NormalFunCall
             (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String method')))
             (L.Args [
                 (L.PrefixExp (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String typecls'))))
                 ])))
           (L.Args [arg1])))
         (L.Args [arg2]))))
         | read method' == method && read typecls' == typecls -> L.Binop binop arg1 arg2
         | otherwise -> exp
       _ -> exp

    unary :: String -> String -> L.Unop -> L.Exp -> L.Exp
    unary typecls method unop exp =
      case exp of
        (L.PrefixExp (L.PEFunCall (L.NormalFunCall
          (L.PEFunCall (L.NormalFunCall
            (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String method')))
            (L.Args [L.PrefixExp
                     (L.PEVar (L.Select (L.PEVar (L.VarName "Prelude")) (L.String typecls')))])))
          (L.Args [arg]))))
          | read method' == method && read typecls' == typecls -> L.Unop unop arg
          | otherwise -> exp
        _ -> exp
