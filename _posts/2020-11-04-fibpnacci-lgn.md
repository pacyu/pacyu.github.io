---
title: 斐波那契数O(lgn)算法及证明
layout: post
math: true
comments: true
toc: true
---

介绍一种对数时间复杂度的斐波那契数列算法。

## 指数时间
我们都知道 Fibonacci 数平凡的树形递归算法时间复杂度是 $$O(1.618^{n})$$，其证明很简单（参考[wikipeida-Fibonacci](https://en.wikipedia.org/wiki/Fibonacci_number#Matrix_form)：
利用矩阵形式：

$$
\begin{pmatrix}
F_{i+2}\\ 
F_{i+1}
\end{pmatrix} = \begin{pmatrix}
1 & 1\\ 
1 & 0
\end{pmatrix}\begin{pmatrix}
F_{i+1}\\ 
F_{i}
\end{pmatrix}
$$

这里简写为 $$\vec{F_{i+1}} = \mathbf{A}\vec{F_{i}}$$，将其递归累乘得到 $$\vec{F_{n}} = \mathbf{A}^n\vec{F_{0}}$$。

其中矩阵 $$\mathbf{A}$$ 的特征方程为 $$\lambda^2 - \lambda - 1 = 0$$，特征值 $$\phi = \frac{1 + \sqrt{5}}{2}$$ 和 $$-\phi^{-1} = \frac{1 - \sqrt{5}}{2}$$ 对应的特征向量分别为 $$\vec{\mu} = \begin{pmatrix}
\phi\\ 
1
\end{pmatrix}$$ 和 $$\vec{\nu} = \begin{pmatrix}
-\phi^{-1}\\ 
1
\end{pmatrix}$$，

初始值 $$\vec{F_{0}} = \begin{pmatrix}
1\\ 
0
\end{pmatrix} = \frac{1}{\sqrt{5}}(\vec{\mu} - \vec{\nu})$$，

所以

$$\begin{aligned}
\vec{F_{n}} &= \frac{1}{\sqrt{5}}(\mathbf{A}^n\vec{\mu} - \mathbf{A}^n\vec{\nu}) \\
&= \frac{1}{\sqrt{5}}(\phi^n\vec{\mu} - (-\phi)^{-n}\vec{\nu}) \\
&= \frac{1}{\sqrt{5}}(\frac{1 + \sqrt{5}}{2})^n\begin{pmatrix}
\phi\\ 
1
\end{pmatrix} - \frac{1}{\sqrt{5}}(\frac{1 - \sqrt{5}}{2})^n\begin{pmatrix}
-\phi^{-1}\\ 
1
\end{pmatrix}
\end{aligned}$$

即得到 Fibonacci 的通项公式为：
$$\begin{aligned}
F_{n} &= \frac{1}{\sqrt{5}}(\frac{1 + \sqrt{5}}{2})^n - \frac{1}{\sqrt{5}}(\frac{1 - \sqrt{5}}{2})^n
\end{aligned}$$

现在可以解释之所以该算法时间复杂度是 $$O(1.618^{n})$$，是因为 $$\vec{F_{n}} = \mathbf{A}^n\vec{F_{0}} = O(\mathbf{A}^n) = O(-\phi^{-n}) = O(1.618^{n})$$。

算法 Lisp 实现：

```scheme
(define (fib n)
	(cond ((= n 0) 1))
		((= n 1) 1))
		(else (+ (fib (- n 2)) (fib (- n 1))))
```

## 线性时间
该算法考虑如下变换规则$$T$$： $$a\leftarrow a + b, b \leftarrow a$$，初始时 $$a=1，b=0$$。

这是一种带记忆搜索的迭代算法，它的证明很显然，所以仅给出算法实现。

算法 Lisp 实现：

```scheme
(define (fib n)
	(define (fib-iter a b n)
		(if (= n 0)
			b
			(fib-iter (+ a b) a (- n 1))))
	(fib-iter 1 0 n))
```

## 对数时间
该算法特别巧妙的地方是它在线性迭代的变换规则基础上做了一些改动，考虑变换规则 $$T_{pq}$$ ：

$$\begin{aligned}
a &\leftarrow bq + aq + ap \\
b &\leftarrow bp + aq
\end{aligned}$$，

初始时 $$a=1, b=0, p=0, q=1$$，如果我们能证明应用两次 $$T_{pq}$$ 变换等效于使用一次同样形式的变换 $$T_{p'q'}$$，其中 $$p', q'$$ 可以通过 $$p, q$$ 计算出来，那么我们就找到了一种求出这种变换的平方的方法，所以下面我将证明，应用两次变换 $$T_{pq}$$ 我们可以得到一个固定模式。

首先写出应用一次 $$T_{pq}$$ 变换后的形式：

$$\begin{aligned}
a &\leftarrow bq + aq + ap \\
b &\leftarrow bp + aq
\end{aligned} \overset{First}{\Rightarrow} \begin{aligned}
a &\leftarrow (bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p \\
b &\leftarrow (bp + aq)p+ (bq+aq+ap)q
\end{aligned}$$

我们展开上面的所有括号，并按初始形式重新写出式子以观察

$$\begin{aligned}
a &\leftarrow bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2 \\
b &\leftarrow bp^2 + apq + bq^2 + apq + bq^2 + aq^2+apq
\end{aligned} \rightarrow \begin{aligned}
a &\leftarrow b(2pq + q^2) + a(2pq + q^2) + a(p^2+q^2) \\
b &\leftarrow b(p^2+q^2) + a(2pq + q^2)
\end{aligned}$$

令 $$p = p^2+q^2, q = 2pq + q^2$$，有：

$$\begin{aligned}
a &\leftarrow bq + aq + ap \\
b &\leftarrow bp + aq
\end{aligned}$$

然后我们在此基础上应用第二次变换会发现依然还是可以写出同样的形式：

$$\begin{aligned}
a &\leftarrow b(2pq + q^2) + a(2pq + q^2) + a(p^2+q^2) \\
b &\leftarrow b(p^2+q^2) + a(2pq + q^2)
\end{aligned} \overset{Second}{\Rightarrow} \begin{aligned}
a &\leftarrow \{b(p^2+q^2) + a(2pq + q^2)\}(2pq + q^2) + \{b(2pq + q^2) + a(2pq + q^2) + a(p^2+q^2)\}(2pq + q^2) + \{b(2pq + q^2) + a(2pq + q^2) + a(p^2+q^2)\}(p^2+q^2) \\
b &\leftarrow \{b(p^2+q^2) + a(2pq + q^2)\}(p^2+q^2) + \{b(2pq + q^2) + a(2pq + q^2) + a(p^2+q^2)\}(2pq + q^2)
\end{aligned} \\
\overset{p=p^2+q^2, q=2pq+q^2}{\rightarrow} \begin{aligned}
a &\leftarrow bq + aq + ap \\
b &\leftarrow bp + aq
\end{aligned} \overset{Second}{\Rightarrow} \begin{aligned}
a &\leftarrow (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p \\
b &\leftarrow (bp + aq)p + (bq + aq + ap)q
\end{aligned} = \begin{aligned}
a &\leftarrow b(2pq + q^2) + a(2pq + q^2) + a(p^2+q^2) \\
b &\leftarrow b(p^2+q^2) + a(2pq + q^2)
\end{aligned}$$

于是我们得到算法：

$$\begin{cases}
\left\{\begin{matrix}
a &\leftarrow a \\
b &\leftarrow b \\
p &\leftarrow p^2+q^2 \\ 
q &\leftarrow 2pq + q^2
\end{matrix}\right. & \text{ when x is even }  \\ \\ 
\left\{\begin{matrix}
a &\leftarrow bq + aq + ap \\ 
b &\leftarrow bp + aq \\
p &\leftarrow p \\
q &\leftarrow q
\end{matrix}\right. & \text{ when x is not even }
\end{cases}$$

算法 Lisp 实现：

```scheme
(define (fib n)
	(define (fib-iter a b p q n)
		(cond ((= n 0) b)
			((even? n) (fib-iter a
					b
					(+ (square p) (square q))
					(+ (square q) (* 2 p q))
					(/ n 2)))
			(else (fib-iter (+ (* b q) (* a q) (* a p))
					(+ (* b p) (* a q))
					p
					q
					(- n 1)))))
	(fib-iter 1 0 0 1 n))
```

其实这是 `SICP` 第一章的一道练习题，证明不难，就是变量多可能有点绕，一开始心算有点打脑壳，然后草稿纸上写出来就很简单了。

有關更多題解見我的github。
