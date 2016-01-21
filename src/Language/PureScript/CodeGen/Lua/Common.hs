{- Has some code essentially copied from osa1/psc-lua -}

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Lua.Common where

import Data.Char
import Data.List (intercalate, foldl')

import Language.PureScript.Names

import qualified Language.Lua.Syntax as L
import qualified Language.Lua.PrettyPrinter as L

-- | Convert an Ident into a valid Lua identifier:
--
-- * Wraps reserved identifiers with _
--
-- * Symbols are wrapped with _ between a symbol name or their ordinal value.
--
identToLua :: Ident -> String
identToLua (Ident name) | nameIsLuaReserved name = '_' : name ++ "_"
identToLua (Ident name) = name >>= identCharToString
identToLua (Op op) = op >>= identCharToString


identCharToString :: Char -> String
identCharToString c
  | isAlphaNum c = [c]
  | otherwise =
      case c of
       '_' -> "_"
       '.' -> "_dot_"
       '$' -> "_dollar_"
       '~' -> "_tilde_"
       '=' -> "_eq_"
       '<' -> "_less_"
       '>' -> "_greater_"
       '!' -> "_bang_"
       '#' -> "_hash_"
       '%' -> "_percent_"
       '^' -> "_up_"
       '&' -> "_amp_"
       '|' -> "_bar_"
       '*' -> "_times_"
       '/' -> "_div_"
       '+' -> "_plus_"
       '-' -> "_minus_"
       ':' -> "_colon_"
       '\\' -> "_bslash_"
       '?' -> "_qmark_"
       '@' -> "_at_"
       '\'' -> "_prime_"
       _ -> '_' : show (ord c) ++ "_"


nameIsLuaReserved :: String -> Bool
nameIsLuaReserved s =
  s `elem` [ "and"
           , "end"
           , "in"
           , "repeat"
           , "break"
           , "false"
           , "local"
           , "return"
           , "do"
           , "for"
           , "nil"
           , "then"
           , "else"
           , "function"
           , "not"
           , "true"
           , "elseif"
           , "if"
           , "or"
           , "until"
           , "while"
           ]


moduleNameToLua :: ModuleName -> String
moduleNameToLua (ModuleName pns) = intercalate "_" (map runProperName pns)


var :: String -> L.Exp
var = L.PrefixExp . L.PEVar . L.VarName


funcall :: L.Exp -> [L.Exp] -> L.Exp
funcall f args =
      L.PrefixExp $ L.PEFunCall $ L.NormalFunCall (expToPexp f) (L.Args args)

funcallS :: String -> [L.Exp] -> L.Stat
funcallS f args = L.FunCall $ L.NormalFunCall (L.PEVar $ L.VarName f) (L.Args args)


funcallStat :: L.Exp -> [L.Exp] -> L.Stat
funcallStat f args =
      L.FunCall $ L.NormalFunCall (expToPexp f) (L.Args args)

pprint :: L.LPretty l => [l] -> String
pprint ls =
  foldl' (\str ss -> "\n" ++ ss str) "" $ map (L.displayS . L.renderPretty 0.8 100 . L.pprint) ls


expToPexp :: L.Exp -> L.PrefixExp
expToPexp (L.PrefixExp pexp) = pexp
expToPexp e = L.Paren e


replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace lhs rhs (x : xs) =
      (if x == lhs then rhs else x) : replace lhs rhs xs


select :: L.Exp -> L.Exp -> L.Exp
select tab index = L.PrefixExp . L.PEVar $ L.Select (expToPexp tab) index


selectS :: L.Exp -> String -> L.Exp
selectS tab index = select tab (L.String index)


-- | Hack to make string literals work properly
string :: String -> L.Exp
string = L.String . show
