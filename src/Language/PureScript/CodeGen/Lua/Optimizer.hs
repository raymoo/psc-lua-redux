module Language.PureScript.CodeGen.Lua.Optimizer (optimize) where

import Data.Generics
import Data.List(foldl')

import Language.PureScript.CodeGen.Lua.Common
import qualified Language.Lua.Syntax as L

applyAll :: [a -> a] -> a -> a
applyAll = foldl' (.) id


optimize :: L.Block -> L.Block
optimize = applyAll [removeReturnApps, inlineCommonOperators, removeDollars, inlineEffOps]

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
