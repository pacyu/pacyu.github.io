---
title: Haskell 中实现一个简易的lambda 演算解释器
comments: true
layout: post
---

实际上这是Codewars的一个题，没能完全通过，因为超时，以及某些测试不能过。

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

parser :: [Lambda] -> String -> Lambda
parser [a] [] = a
parser _   [] = error "Syntax error have occurred."
parser a (c:s)
  | c == ' ' = parser a s
  | c == 'λ' = parser ((Lam (show nxt) (Body (variable top (show nxt)))): pop2 a) s
  | c == '@' = parser ((App nxt top): pop2 a) s
  | otherwise = parser (F (c:[]): a) s
  where top = head a
        nxt = head $ tail a
        pop2 = tail . tail

lambda :: String -> Lambda
lambda = parser []

free :: Lambda -> [String]
free (App a b) = nub $ free a ++ free b
free (Lam a b) = nub $ free b
free (Body a)  = nub $ free a
free (F a)     = [a]
free _ = []

bound :: Lambda -> [String]
bound (App a b) = nub $ bound a ++ bound b
bound (Lam a b) = nub $ [a] ++ bound b
bound (Body a)  = nub $ bound a
bound (T a)     = [a]
bound _ = []

app :: String -> Lambda -> Lambda -> Lambda
app var (Lam a (Body b)) lam = Lam a $ Body (app var b lam)
app var (App a b)        lam = reduce $ App (app var a lam) (app var b lam)
app var (T a)            lam | a == var  = lam
                             | otherwise = (T a)
app var a                _   = a

reduce :: Lambda -> Lambda
reduce (App (Lam a (Body b)) lam) = app a b lam
reduce (App (App a b) lam)        = reduce $ App (reduce (App a b)) lam
reduce (Lam a (Body b))           = Lam a $ reduce b
reduce a = a
```

已知 `reduce` 不能通过的测试是：
```
x x x x @ λ λ y @ ==> (λx.λx.x x) y => λx.x x
```
其实我总觉得这个例子有点不是那么很合适，因为我个人觉得最好、最简单的办法就是用不同的 identifiers 来命名 bound variables，虽然它这里可能是想考虑作用域的问题。

至于为什么超时了我也不太清楚是哪些测试样例没想到的。有可能是`reduce` 导致的，我想应该不会出现类似 Y 组合子这样的表达式吧，如果不是的话，那就只可能是我还有其他会导致无限循环的情况没考虑到。。。

我测试了不动点组合子，看起来没有问题，会无限执行 `reduce` 函数，也测试了 Church number 以及一些其它示例，都能如期的输出正确结果。
