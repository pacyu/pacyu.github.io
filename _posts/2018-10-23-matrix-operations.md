---
title: 矩阵运算
layout: post
date: '2018-09-08 01:10:44 +0800'
tags:
- Math
- C/C++
categories:
- algorithms
- mathematical
cover_index: "/assets/95669376.jpg"
math: true
toc: true
comments: true
---

这篇主要介绍一些矩阵运算相关算法。

## 矩阵乘法

矩阵相乘：$$ C = AB $$

### 普通算法

* 时间复杂度 $$ O(N^3) $$

```c
memset(c, 0, sizeof(c));
for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
        for (int k = 0; k < N; k++)
            c[i][j] += a[i][k] * b[k][j];
```

### Strassen算法

* 时间复杂度 $$ O(N^{lg7}) $$

```c
void Strassen_algo(vector<vector<float>> & c, vector<vector<float>>  a, vector<vector<float>>  b)
{
}
```

## 线性方程组

$$ n $$ 个未知数 $$ m $$ 个方程的线性方程组

$$
\left\{
\begin{matrix}
a_{00}x_0 & a_{01}x_1 & \cdots & a_{0n}x_n &= b_0 \\
a_{10}x_0 & a_{11}x_1 & \cdots & a_{1n}x_n &= b_1 \\
\vdots \\
a_{m0}x_0 & a_{m1}x_1 & \cdots & a_{mn}x_n &= b_m 
\end{matrix}\right.
$$

将上式写为以向量 $$ x $$ 为未知元的向量方程

$$ Ax = b \quad (1) $$

### 方程是否有解
设 $$ B = (A, b) $$ 为增广矩阵，$$ R(A) $$ 是矩阵 $$ A $$ 的秩，$$ R(A, b) $$ 是增广矩阵 $$ B $$ 的秩。

* 无解的充分必要条件是 $$ R(A) < R(A, b) $$;
* $$ n $$ 元线性方程组 $ Ax = b \quad $ 有唯一解的充分必要条件是 $$ R(A) = R(A, b) = n $$;
* 有无限多解的充分必要条件是 $$ R(A) = R(A, b) \leq n $$.

### LUP分解原理
$$ LUP $$ 分解的基本思想就是找出三个 $ n \times n $ 矩阵 $$ L $$、$$ U $$、$$ P $$，满足

$$ PA = LU \quad (2) $$

其中，$$ L $$ 是一个单位下三角矩阵，$$ U $$ 是一个上三角矩阵，$$ P $$ 是一个置换矩阵。我们称满足式 $$ (2) $$ 的矩阵 $$ L $$、$$ U $$、$$ P $$ 为矩阵 $$ A $$ 的 **LUP 分解**。

可以证明，如果矩阵 $$ A $$ 是一个非奇异矩阵，那么我们一定能找到 $$ L $$、$$ U $$、$$ P $$ 分解满足上式。

这有什么用呢？

我们先对 (1) 进行置换，在等式 $$ Ax = b $$ 两边同时乘以 $$ P $$ 置换，得到：$$ PAx = Pb $$，利用 $$ (2) $$ 式有

$$ LUx = Pb $$

设 $$ y = Ux $$，其中 $$ x $$ 就是要求解的向量解。首先，通过一种称为“正向替换”的方法求解单位下三角系统

$$ Ly = Pb \quad (3) $$

得到未知向量 $$ y $$。然后，通过一种称为“反向替换”的方法求解上三角系统

$$ Ux = y \quad (4) $$

得到向量解 $$ x $$。由于置换矩阵 $$ P $$ 是可逆的，在等式 $$ (2) $$ 两边同时乘以 $$ P^{-1} $$，于是

$$ A = P^{-1}LU $$

因此，向量 $$ x $$ 就是 $$ Ax = b $$ 的解

$$
\begin{aligned}
Ax
&= P^{-1}LUx \\
&= P^{-1}Ly \\
&= P^{-1}Pb \\
&= b
\end{aligned}
$$

### 正向替换与反向替换
进一步看看正向替换与反向替换是如何进行的。

* 正向替换

已知 $$ L $$、$$ P $$ 和 $$ b $$，正向替换可在 $$ O(n^2) $$ 的时间内求解单位下三角系统 $$ (3) $$。

$$
Pb = \begin{bmatrix}
p_{0,0} & p_{0,1} & \cdots & p_{0,n} \\
p_{1,0} & p_{1,1} & \cdots & p_{1,n} \\
\vdots & \vdots & \cdots & \vdots \\
p_{n,0} & p_{n,1} & \cdots & p_{n,n}
\end{bmatrix}
\begin{bmatrix}
b_0 \\
b_1 \\
\vdots \\
b_n
\end{bmatrix} = \begin{bmatrix}
\sum_{j = 0}^{n}p_{0,j} \cdot b_j \\
\sum_{j = 0}^{n}p_{1,j} \cdot b_j \\
\vdots \\
\sum_{j = 0}^{n}p_{n,j} \cdot b_j
\end{bmatrix}
$$

为了方便起见，这里用一个数组 $$ \pi[0...n] $$ 简洁地表示置换 $$ P $$。对 $$ i = 0, 1, 2, \cdots, n $$，元素 $$ \pi[i] $$ 表示 $$ P_{i, \pi[i]} = 1 $$，并且对 $$ j \neq \pi[i] $$ 有 $$ P_{ij} = 0 $$。因此，$$ PA $$ 第 $$ i $$ 行第 $$ j $$ 列的元素为 $$ a_{\pi[i],j} $$，$$ Pb $$ 的第 $$ i $$ 个元素为 $$ b_{\pi[i]} $$。因为 $$ L $$ 是单位下三角矩阵，我们可以重写等式 $$ (3) $$ 为：

$$
\begin{bmatrix}
y_0 & & & & & & & & &= b_{\pi[0]} \\
l_{10}y_0 & + & y_1 & & & & & & &= b_{\pi[1]} \\
l_{20}y_0 & + & l_{21}y_1 & + & y_2 & & & & &= b_{\pi[2]} \\
\vdots \\
l_{n0}y_0 & + & l_{n1}y_1 & + & l_{n2}y_2 & + & \cdots & + & y_n &= b_{\pi[n]}
\end{bmatrix}
$$

第一个等式可以求出 $$ y_0 $$，我们把它代入第二个等式，求出 

$$ y_1 = b_{\pi[1]} - l_{10}y_0 $$

在将 $$ y_1, y_2 $$ 代入第三个等式，得到

$$ y_2 = b_{\pi[2]} - (l_{20}y_0 + l{21}y_1) $$

一般的，我们把 $$ y_0, y_1, \cdots, y_{i - 1} $$ “正向替换”到第 $$ i $$ 个等式中，就可以求解 $$ y_i $$：

$$ y_i = b_{\pi[i]} - \sum_{j = 0}^{i - 1} l_{ij}y_{j} $$


* 反向替换

与正向替换类似，求解上三角系统等式 (4)。

$$
\begin{bmatrix}
u_{0,0}x_0 & + & u_{0,1}x_1 & + & \cdots & + & u_{0,n-2}x_{n-2} & + & u_{0,n-1}x_{n-1} & + & u_{0,n}x_n &= y_{0} \\
& & u_{1,1}x_1 & + &  \cdots & + & u_{1,n-2}x_{n-2} & + & u_{1,n-1}x_{n-1} & + & u_{1,n}x_n &= y_{1} \\
& & & & & & & & & & \vdots \\
& & & & & & u_{n-2,n-2}x_{n-2} & + & u_{n-2,n-1}x_{n-1} & + & u_{n-2,n}x_n &= y_{n - 2} \\
& & & & & & & & u_{n-1,n-1}x_{n-1} & + & u_{n-1,n}x_n &= y_{n - 1} \\
& & & & & & & & & & u_{n,n}x_n &= y_{n}
\end{bmatrix}
$$

这里我们先求解第 $$ n $$ 个等式，然后往前一个等式代，因此可以如下相继求出 $$ x_n, x_{n - 1}, \cdots, x_1 $$ 的解：

$$
\begin{aligned}
x_n &= \frac{y_{n}}{u_{n,n}} \\
x_{n - 1} &= \frac{y_{n - 1} - u_{n - 1, n}x_n}{u_{n - 1,n - 1}} \\
\vdots
\end{aligned}
$$

一般的，有

$$ x_i = \frac{y_i - \sum_{j = i + 1}^{n} u_{ij}x_{j}}{u_{ij}} $$

### 计算LUP分解
我们该如何找到这样的 $$ L $$、 $$ U $$、 $$ P $$ 矩阵呢？

通常在 $$ LUP $$ 分解中包含一个置换矩阵 $$ P $$ 的原因是为了避免矩阵 $$ A $$ 中的主元 $$ a_{i,i} = 0 $$ ，即矩阵对角线上的数。

但如果矩阵 $$ A $$ 的对角线上的数都不为 $$ 0 $$，我们有一个 $$ LU $$ 分解算法就可以计算出矩阵 $$ L $$ 、$$ U $$。

下面先给出 $$ LU $$ 分解算法的实现，之后再分析算法原理。

### LU分解算法实现
示例(Example):

$$
\begin{bmatrix}
2 & 3 & 1 & 5 \\
6 & 13 & 5 & 19 \\
2 & 19 & 10 & 23 \\
4 & 10 & 11 & 31 \\
\end{bmatrix}
$$

```cpp
#include <iostream>
#include <vector>
const int N = 4;
using namespace std;
void lu(vector<vector<float>> &l, vector<vector<float>> &u, float a[][N])
{
    for (int i = 0; i < N; i++)
	{
		l[i][i] = 1;
		for (int j = i + 1; j < N; j++)
			l[i][j] = 0;
	}
	for (int i = 1; i < N; i++)
		for (int j = 0; j < i; j++)
			u[i][j] = 0;
	for (size_t i = 0; i < N; i++)
	{
		u[i][i] = a[i][i];
		for (size_t j = i + 1; j < N; j++)
		{
			l[j][i] = a[j][i] / u[i][i];
			u[i][j] = a[i][j];
		}
		for (size_t j = i + 1; j < N; j++)
			for (size_t k = i + 1; k < N; k++)
				a[j][k] -= l[j][i] * u[i][k];
	}
}
int main()
{
	vector<vector<float>>  l(N, vector<float>(N));
	vector<vector<float>>  u(N, vector<float>(N));
	float a[N][N] = {
		2, 3, 1, 5,
		6, 13, 5, 19,
		2, 19, 10, 23,
		4, 10, 11, 31
	};

	lu(l, u, a);

	cout << "L 矩阵:" << endl;
	for (int i = 0; i < N; i++)
	{
		for (int j = 0; j < N; j++)
			cout << l[i][j] << '\t';
		cout << endl;
	}
	cout << "U 矩阵:" << endl;
	for (int i = 0; i < N; i++)
	{
		for (int j = 0; j < N; j++)
			cout << u[i][j] << '\t';
		cout << endl;
	}

	memset(a, 0, sizeof(a));

	for (int i = 0; i < N; i++)
		for (int j = 0; j < N; j++)
			for (int k = 0; k < N; k++)
				a[i][j] += l[i][k] * u[k][j];

	cout << "A 矩阵:" << endl;
	for (int i = 0; i < N; i++)
	{
		for (int j = 0; j < N; j++)
			cout << a[i][j] << '\t';
		cout << endl;
	}
	return 0;
}
```

### LU分解算法分析
前面说到对于一个非奇异矩阵 $$ A $$，即 $$ \det{A} \neq 0 $$，就能找到其 $$ LU $$ 分解，那么运用正向替换与反向替换就可以求出线性方程组 $$ Ax = b $$ 的解。

> 原理：算法利用高斯消元法来创建 $$ LU $$ 分解。首先从其他方程中减去第一个方程的倍数，以把那些方程中的第一个变量消去。然后，从第三个及以后的方程中减去第二个方程的倍数，把这些方程的第一个和第二个变量消去。继续上述过程，直到系统变为一个上三角矩阵形式，实际上此矩阵就是 $$ U $$。矩阵 $ L $ 是由消去变量所用的行的乘数组成。

采用递归算法实现这个策略。我们希望构造出一个 $$ n \times n $$ 的非奇异矩阵 $$ A $$ 的一个 $$ LU $$ 分解。
如果 $$ n = 1 $$，则构造完成，因为可以算则 $$ L = I_1 $$ (注：$$ I_n $$ 是单位阵)， $$ U = A $$。对于 $$ n > 1 $$，我们把 $$ A $$ 拆成 $$ 4 $$ 部分

$$
\begin{bmatrix}
a_{11} & | & a_{12} & \cdots & a_{1n} \\
-&-&-&-&- \\
a_{21} & | & a_{22} & \cdots & a_{2n} \\
\vdots & | \\
a_{n1} & | & a_{n2} & \cdots & a_{nn}
\end{bmatrix} = \begin{bmatrix}
a_{11} & \omega^T \\
\upsilon & A'
\end{bmatrix}
$$

其中 $$ \upsilon $$ 是一个 $$ n - 1 $$ 维列向量，$$ \omega^T $$ 是一个 $$ n - 1 $$ 维行向量，$$ A' $ 是一个 $ (n - 1) \times (n - 1) $$ 矩阵。然后，利用矩阵代数 (通过简单地从头到尾使用乘法来验证方程式)，可以把 $$ A $$ 分解为

$$
\begin{bmatrix}
1 & 0 \\
\frac{\upsilon}{a_{11}} & I_{n - 1}
\end{bmatrix}
\begin{bmatrix}
a_{11} & \omega^T \\
0 & A' - \frac{\upsilon\omega^T}{a_{11}}
\end{bmatrix}
$$

项 $$ \frac{\upsilon\omega^T}{a_{11}} $$ 是一个 $ (n - 1) \times (n - 1) $$ 矩阵，它与矩阵 $$ A' $$ 大小一致。所得矩阵

$$ A' - \frac{\upsilon\omega^T}{a_{11}} $$

称为矩阵 $$ A $$ 对于 $$ a_{11} $$ 的[舒尔补](https://en.wikipedia.org/wiki/Schur_complement)。

如果矩阵 $$ A $$ 是非奇异的，那么舒尔补矩阵也是非奇异的。

因为舒尔补是非奇异的，现在我们可以递归地找出它的一个 $$ LU $$ 分解。我们说

$$ A' - \frac{\upsilon\omega^T}{a_{11}} = L'U' $$

其中 $$ L' $$ 是单位下三角矩阵，$$ U' $$ 是上三角矩阵。然后，利用矩阵代数可得

$$
\begin{aligned}
A &= 
\begin{bmatrix}
1 & 0 \\
\frac{\upsilon}{a_{00}} & I_{n - 1}
\end{bmatrix}
\begin{bmatrix}
a_{00} & \omega^T \\
0 & A' - \frac{\upsilon\omega^T}{a_{00}}
\end{bmatrix} = \begin{bmatrix}
1 & 0 \\
\frac{\upsilon}{a_{00}} & I_{n - 1}
\end{bmatrix}
\begin{bmatrix}
a_{k0} & \omega^T \\
0 & L'U'
\end{bmatrix} \\
&=
\begin{bmatrix}
1 & 0 \\
\frac{\upsilon}{a_{00}} & L'
\end{bmatrix}
\begin{bmatrix}
a_{00} & \omega^T \\
0 & U'
\end{bmatrix} =
LU
\end{aligned}
$$

* 时间复杂度 $ O(N^3) $

### LUP分解算法实现
示例(Example):

$$
\begin{bmatrix}
2 & 0 & 2 & 0.6 \\
3 & 3 & 4 & -2 \\
5 & 5 & 4 & 2 \\
-1 & -2 & 3.4 & -1
\end{bmatrix}
$$

```cpp
#include <iostream>
#include <vector>
const int N = 4;
using namespace std;
void lup(vector<float> &P, float a[][N])
{
	for (size_t i = 0; i < N; i++)
	{
		float p = 0; size_t i_;
		for (size_t j = i; j < N; j++)
			if (fabsf(a[j][i]) > p)
				p = fabsf(a[j][i]), i_ = j;
		if (p == 0) return;
		swap(P[i], P[i_]);
		for (size_t j = 0; j < N; j++)
			swap(a[i][j], a[i_][j]);
		for (size_t j = i + 1; j < N; j++)
		{
			a[j][i] /= a[i][i];
			for (size_t k = i + 1; k < N; k++)
				a[j][k] -= a[j][i] * a[i][k];
		}
	}
}
int main()
{
	vector<float> P(N);
	float b[][N] = {
		2, 0, 2, 0.6,
		3, 3, 4, -2,
		5, 5, 4, 2,
		-1, -2, 3.4, -1
	};

    lup(P, b);

	cout << "B 矩阵:" << endl;
	for (int i = 0; i < N; i++)
	{
		for (int j = 0; j < N; j++)
			cout << b[i][j] << '\t';
		cout << endl;
	}
    cout << "P 矩阵:" << endl;
	for (size_t i = 0; i < P.size(); i++)
		cout << i << P[i] << endl;
	return 0;
}
```

**version 2.`lup()`**

```cpp
void lup2(vector<vector<float>>&l, vector<vector<float>>&u, vector<float> &P, float a[][N])
{
	for (int i = 0; i < N; i++)
	{
		l[i][i] = 1;
		for (int j = i + 1; j < N; j++)
			l[i][j] = 0;
	}
	for (int i = 1; i < N; i++)
		for (int j = 0; j < i; j++)
			u[i][j] = 0;
	for (int i = 0; i < N; i++)
	{
		float p = 0; int i_;
		for (int j = i; j < N; j++)
			if (fabsf(a[j][i]) > p)
				p = fabsf(a[j][i]), i_ = j;
		if (p == 0) return;
		swap(P[i], P[i_]);
		for (int j = 0; j < N; j++)
			swap(a[i][j], a[i_][j]);
		u[i][i] = a[i][i];
		for (int j = i + 1; j < N; j++)
		{
			l[j][i] = a[j][i] / u[i][i];
			u[i][j] = a[i][j];
		}
		for (int j = i + 1; j < N; j++)
			for (int k = i + 1; k < N; k++)
				a[j][k] -= l[j][i] * u[i][k];
	}
}
```

### LUP分解算法分析
> 原理：算法考虑每次计算过程，若主元为 $$ 0 $$ 或主元的值大于除数时，我们将找到合适的行与当前主元所在的行进行交换。比如，第 $$ 1 $$ 行，第 $$ 1 $$ 列为 $$ 0 $$，我们把第 $$ 1 $$ 行与第 $$ k $$ 行互换，这等价于用一个置换矩阵 $$ Q $$ 左乘矩阵 $$ A $$。因此可以把 $$ QA $$ 写成 $$ QA = \begin{bmatrix}
a_{k1} & \omega^T \\
\upsilon & A'
\end{bmatrix} $$，其中 $$ \begin{aligned}
\upsilon &= (a_{21}, a_{31}, \cdots, a_{n1})^T \\
\omega^T &= (a_{k2}, a_{k3}, \cdots, a_{kn})
\end{aligned} $$

$$ A' $$ 是一个 $$ (n - 1) \times (n - 1) $$ 矩阵。因为 $$ a_{k1} \neq 0 $$，现在可以执行与 $$ LU $$ 分解基本相同的线性代数运算，但现在能保证不会除以 $$ 0 $$

$$
QA =
\begin{bmatrix}
a_{k1} & \omega^T \\
\upsilon & A'
\end{bmatrix} = \begin{bmatrix}
1 & 0 \\
\frac{\upsilon}{a_{k1}} & I_{n - 1}
\end{bmatrix}
\begin{bmatrix}
a_{k1} & \omega^T \\
0 & A' - \frac{\upsilon\omega^T}{a_{k1}}
\end{bmatrix}
$$

如果 $$ A $$ 是非奇异的，那么舒尔补 $$ A' - \frac{\upsilon\omega^T}{a_{k1}} $$ 也是非奇异的。因此，可以递归地找出它的一个 $$ LUP $$ 分解，包括单位下三角矩阵 $$ L' $$、上三角矩阵 $$ U' $$ 和置换矩阵 $$ P' $$，满足

$$ P'(A' - \frac{\upsilon\omega^T}{a_{k1}}) = L'U' $$

定义 $$
P = \begin{bmatrix}
1 & 0 \\
0 & P'
\end{bmatrix}
Q
$$

它是一个置换矩阵，因为它是两个置换矩阵的乘积。有

$$
\begin{aligned}
PA &= 
\begin{bmatrix}
1 & 0 \\
0 & P'
\end{bmatrix}
QA = 
\begin{bmatrix}
1 & 0 \\
0 & P'
\end{bmatrix}
\begin{bmatrix}
1 & 0 \\
\frac{\upsilon}{a_{k1}} & I_{n - 1}
\end{bmatrix}
\begin{bmatrix}
a_{k1} & \omega^T \\
0 & A' - \frac{\upsilon\omega^T}{a_{k1}}
\end{bmatrix} \\
&= 
\begin{bmatrix}
1 & 0 \\
P'\frac{\upsilon}{a_{k1}} & P'
\end{bmatrix}
\begin{bmatrix}
a_{k1} & \omega^T \\
0 & A' - \frac{\upsilon\omega^T}{a_{k1}}
\end{bmatrix} = \begin{bmatrix}
1 & 0 \\
P'\frac{\upsilon}{a_{k1}} & I_{n - 1}
\end{bmatrix}
\begin{bmatrix}
a_{k1} & \omega^T \\
0 & P'(A' - \frac{\upsilon\omega^T}{a_{k1}})
\end{bmatrix} \\
&= 
\begin{bmatrix}
1 & 0 \\
P'\frac{\upsilon}{a_{k1}} & I_{n - 1}
\end{bmatrix}
\begin{bmatrix}
a_{k1} & \omega^T \\
0 & L'U'
\end{bmatrix} = \begin{bmatrix}
1 & 0 \\
P'\frac{\upsilon}{a_{k1}} & L'
\end{bmatrix}
\begin{bmatrix}
a_{k1} & \omega^T \\
0 & U'
\end{bmatrix} =
LU
\end{aligned}
$$

这样就推出了 $$ LUP $$ 分解。因为 $$ L' $$ 是单位下三角矩阵，所以 $$ L $$ 也是单位下三角矩阵；又因为 $$ U' $$ 是上三角矩阵，于是 $$ U $$ 也是上三角矩阵。

* 时间复杂度 $$ O(N^3) $$

## 求解线性方程组算法
有了以上基础知识以后，我们才能拿到 $$ L $$、$$ U $$、$$ P $$ ，步入最后一步求解出线性方程组 $$ Ax = b $$ 的向量解。

### 完整的算法实现
```cpp
#include <iostream>
#include <vector>
const int N = 3;
using namespace std;
void lu(vector<vector<float>> &l, vector<vector<float>> &u, float a[][N])
{
	for (int i = 0; i < N; i++)
	{
		l[i][i] = 1;
		for (int j = i + 1; j < N; j++)
			l[i][j] = 0;
	}
	for (int i = 1; i < N; i++)
		for (int j = 0; j < i; j++)
			u[i][j] = 0;
	for (int i = 0; i < N; i++)
	{
		u[i][i] = a[i][i];
		for (int j = i + 1; j < N; j++)
		{
			l[j][i] = a[j][i] / u[i][i];
			u[i][j] = a[i][j];
		}
		for (int j = i + 1; j < N; j++)
			for (int k = i + 1; k < N; k++)
				a[j][k] -= l[j][i] * u[i][k];
	}
}
void lup(vector<float> &P, float a[][N])
{
	for (int i = 0; i < N; i++)
	{
		float p = 0; int i_;
		for (int j = i; j < N; j++)
			if (fabsf(a[j][i]) > p)
				p = fabsf(a[j][i]), i_ = j;
		if (p == 0) return;
		swap(P[i], P[i_]);
		for (int j = 0; j < N; j++)
			swap(a[i][j], a[i_][j]);
		for (int j = i + 1; j < N; j++)
		{
			a[j][i] /= a[i][i];
			for (int k = i + 1; k < N; k++)
				a[j][k] -= a[j][i] * a[i][k];
		}
	}
}
void lup2(vector<vector<float>>&l, vector<vector<float>>&u, vector<float> &P, float a[][N])
{
	for (int i = 0; i < N; i++)
	{
		l[i][i] = 1;
		for (int j = i + 1; j < N; j++)
			l[i][j] = 0;
	}
	for (int i = 1; i < N; i++)
		for (int j = 0; j < i; j++)
			u[i][j] = 0;
	for (int i = 0; i < N; i++)
	{
		float p = 0; int i_;
		for (int j = i; j < N; j++)
			if (fabsf(a[j][i]) > p)
				p = fabsf(a[j][i]), i_ = j;
		if (p == 0) return;
		swap(P[i], P[i_]);
		for (int j = 0; j < N; j++)
			swap(a[i][j], a[i_][j]);
		u[i][i] = a[i][i];
		for (int j = i + 1; j < N; j++)
		{
			l[j][i] = a[j][i] / u[i][i];
			u[i][j] = a[i][j];
		}
		for (int j = i + 1; j < N; j++)
			for (int k = i + 1; k < N; k++)
				a[j][k] -= l[j][i] * u[i][k];
	}
}
void lup_solve(vector<vector<float>>&l, vector<vector<float>>&u, vector<float>&x, vector<float>&y, vector<float> b, vector<float> P, float a[][N])
{
	for (int i = 0; i < N; i++)
	{
		float sum = 0;
		for (int j = 0; j < i; j++)
			sum += l[i][j] * y[j];
		y[i] = b[P[i]] - sum;
	}
	for (int i = N - 1; i >= 0; i--)
	{
		float sum = 0;
		for (int j = i + 1; j < N; j++)
			sum += u[i][j] * x[j];
		x[i] = (y[i] - sum) / u[i][i];
	}
}
int main()
{
	vector<vector<float>>  l(N, vector<float>(N));
	vector<vector<float>>  u(N, vector<float>(N));
	/*float a[][N] = {
		2, 3, 1, 5,
		6, 13, 5, 19,
		2, 19, 10, 23,
		4, 10, 11, 31
	};
	float b[][N] = {
		2, 0, 2, 0.6,
		3, 3, 4, -2,
		5, 5, 4, 2,
		-1, -2, 3.4, -1
	};*/
	float c[][N] = {
		1, 2, 0,
		3, 4, 4,
		5, 6, 3
	};
	vector<float> P(N);
	vector<float> x(N);
	vector<float> y(N);
	vector<float> b { 3, 7, 8 };

	for (int i = 0; i < N; i++)
		P[i] = (float)i;

	lup2(l, u, P, c);
	
	lup_solve(l, u, x, y, b, P, c);

	cout << "解向量 x:" << endl;
	for (int i = 0; i < N; i++)
		cout << x[i] << endl;
	return 0;
}
```

**示例：**

$$
\begin{bmatrix}
1 & 2 & 0 \\
3 & 4 & 4 \\
5 & 6 & 3
\end{bmatrix}
$$

**output:**

使用 `lup2()` 函数

```
解向量 x:
-10.08
7.94
1.92
```

### 算法分析
显然，代码很直观的实现了对算法的描述过程，即利用正向替换，代换出 $$ Ly = Pb $$ 的 $$ y $$，然后再利用反向替换计算出 $$ Ux = y $$ 中的解向量 $$ x $$。

* `lup_solve()`的时间复杂度为 $$ O(N^2) $$

## 矩阵求逆
$$ A' $$ 是 $$ A $$ 的逆矩阵，则有

$$ AA' = E $$

其中 $ E $ 是单位阵。

设 $$ X = A' $$， $$ X_i $$ 表示 $$ X $$ 的第 $$ i $$ 列， $$ e_i $$ 是 $$ E $$ 的第 $$ i $$ 列。于是可以利用 $$ A $$ 的 $$ LUP $$ 分解求解方程中的 $$ X $$，需分别求解每一个方程

$$ AX_i = e_i $$

中的 $$ X_i $$。一旦得到 $$ LUP $$ 分解，就可以在 $$ O(N^2) $$ 时间内计算 $$ n $$ 个 $$ X_i $$ 列中的每一个，因此可以在 $$ O(N^3) $$ 时间内从 $$ A $$ 的 $$ LUP $$ 分解计算 $$ X $$。既然可以在 $$ O(N^3) $$ 内确定出 $$ A $$ 的 $$ LUP $$ 分解，我们就可以在 $$ O(N^3) $$ 的时间内求矩阵 $$ A $$ 的逆矩阵 $$ A' $$。

### 算法演示
**示例：**

$$
\begin{bmatrix}
1 & 2 & 0 \\
3 & 4 & 4 \\
5 & 6 & 3
\end{bmatrix}
$$

```cpp
int main()
{
	vector<vector<float>>  l(N, vector<float>(N));
	vector<vector<float>>  u(N, vector<float>(N));
	vector<float> x(N);
	vector<float> y(N);
	vector<float> P(N);
	vector<vector<float>> E {
		vector<float>{ 1, 0, 0 }, 
		vector<float>{ 0, 1, 0 },
		vector<float>{ 0, 0, 1}
	};
    float a[][N] = {
		1, 2, 0,
		3, 4, 4,
		5, 6, 3
	};

	lup2(l, u, P, a);
	
	for (int j = 0; j < E.size(); j++)
	{
		lup_solve(l, u, x, y, E[j], P, a);
		cout << "解向量 X[" << j << "]:" << endl;
		for (int i = 0; i < N; i++)
			cout << x[i] << endl;
	}
	return 0;
}
```

**output:**

```
解向量 X[0]:
-1.2
1.1
-0.2
解向量 X[1]:
-0.6
0.3
0.4
解向量 X[2]:
0.8
-0.4
-0.2
```

即 $$
A'=\begin{bmatrix}
-1.2 & -0.6 & 0.8 \\
1.1 & 0.3 & -0.4 \\
-0.2 & 0.4 & -0.2
\end{bmatrix}
$$

## 参考文献

* [1] 「算法导论・第三版」
* [2] [matrix multiplicaton - wikipedia](https://en.wikipedia.org/wiki/Matrix_multiplication)
