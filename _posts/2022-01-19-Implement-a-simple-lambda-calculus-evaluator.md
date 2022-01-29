---
title: Haskell 中实现一个简易的lambda 演算解释器
layout: post
math: true
comments: true
---

实际上这是Codewars的一个题，但没能完全通过。

先放代码：

```hs
module LambdaTermReduction (lambda,free,bound,reduce) where

import Data.List (nub)
-- | For example:
-- | Lam "λ"
-- | App "@"
-- | (λx.x) @ y ==> x x λ y @
-- | (λx.λx.x @ x) @ y ==> x x x x @ λ λ y @
-- | ((λx.λy.λz.((x @ z) @ (y @ z))) @ (λx.λ_.x)) @ (λz.z)
-- | = (λy.λz.((λx.λ_.x @ z) @ (y @ z))) @ (λz.z)
-- | = (λy.λz.((λ_.z) @ (y @ z))) @ (λz.z)
-- | = (λy.λz.z) @ (λz.z)
-- | = λz.z
data Lambda = App Lambda Lambda
            | Lam String Lambda
            | Body Lambda
            | T String
            | F String
            deriving (Eq)

instance Show (Lambda) where
  show (App a b) = show a ++ " " ++ show b ++ " @"
  show (Lam a b) = a ++ " " ++ show b ++ " λ"
  show (Body a)  = show a
  show (T a)     = a
  show (F a)     = a

prefixPrint :: Lambda -> String
prefixPrint (App a b) = "App (" ++ prefixPrint a ++ ") (" ++ prefixPrint b ++ ")"
prefixPrint (Lam a b) = "Lam " ++ a ++ " (" ++ prefixPrint b ++ ")"
prefixPrint (Body a)  = "Body (" ++ prefixPrint a ++ ")"
prefixPrint (T a)     = "T " ++ a
prefixPrint (F a)     = "F " ++ a

untypedLambdaExpress :: Lambda -> String
untypedLambdaExpress (App a b) = "(" ++ untypedLambdaExpress a ++ " " ++ untypedLambdaExpress b ++ ")"
untypedLambdaExpress (Lam a (Body b)) = "Lam " ++ a ++ "." ++ untypedLambdaExpress b
untypedLambdaExpress (T a)     = a
untypedLambdaExpress (F a)     = a

isBound :: Lambda -> Bool
isBound (T _) = True
isBound _     = False

isFree :: Lambda -> Bool
isFree (F _) = True
isFree _     = False

variable :: Lambda -> String -> Lambda
variable (App a b) s = App (variable a s) (variable b s)
variable (Lam a b) s = Lam a (variable b s)
variable (Body a)  s = Body (variable a s)
variable (F a)     s = if s == a then T a else F a
variable a         s = a

parser :: [Lambda] -> String -> String -> Lambda
parser [a] _ [] = a
parser _   _ [] = error "Syntax error have occurred."
parser a b (c:s)
  | c == ' ' = parser a "" s
  | c == 'λ' = parser ((Lam (show nxt) (Body (variable top (show nxt)))):pop2 a) "" s
  | c == '@' = parser ((App nxt top):pop2 a) "" s
  | otherwise = if s /= [] && head s /= ' ' then parser a (b++[c]) s else parser (F (b++[c]):a) "" s
  where top = head a
        nxt = head $ tail a
        pop2 = tail . tail

lambda :: String -> Lambda
lambda = parser [] ""

free :: Lambda -> [String]
free (App a b) = nub $ free a ++ free b
free (Lam a b) = nub $ free b
free (Body a)  = nub $ free a
free (F a)     = [a]
free _ = []

bound :: Lambda -> [String]
bound (App a b) = nub $ bound a ++ bound b
bound (Lam a b) = nub a: bound b
bound (Body a)  = nub $ bound a
bound (T a)     = [a]
bound _ = []

app :: String -> Lambda -> Lambda -> Lambda
app var (Lam a (Body b)) lam = if a == var then Lam a $ Body (app "" b lam)
                               else Lam a $ Body (app var b lam)
app var exp@(App a b)    lam = if exp == apped then exp else reduce $ apped
  where apped = App (app var a lam) (app var b lam)
app var (T a)            lam | a == var  = lam
                             | otherwise = (T a)
app var a                _   = a

reduce :: Lambda -> Lambda
reduce (App (Lam a (Body b)) lam) = app a b lam
reduce exp@(App (App a b) lam)    = if exp == apped then exp else reduce $ apped
  where apped = App (reduce (App a b)) lam
reduce (Lam a (Body b))           = Lam a $ reduce b
reduce a = a
```
