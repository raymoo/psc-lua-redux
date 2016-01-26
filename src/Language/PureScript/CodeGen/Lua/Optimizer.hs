module Language.PureScript.CodeGen.Lua.Optimizer (optimize) where

import Data.Generics
import Data.List(foldl')

import Language.PureScript.CodeGen.Lua.Common
import qualified Language.Lua.Syntax as L

applyAll :: [a -> a] -> a -> a
applyAll = foldl' (.) id


optimize :: L.Block -> L.Block
optimize = applyAll [inlineCommonOperators]


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
