---
title: 高中数学-向量篇（持续更新）
layout: post
date: '2018-09-02 18:54:58 +0800'
tags:
- Math
categories:
- mathematical
cover_index: "/assets/66981073.jpg"
math: true
comments: true
---

这里是关于向量运算的题目题解，题目质量大概参差不齐。

向量在现代数学和物理中有着很重要的作用，指的是一种具有大小和方向的量。又叫做矢量([Vector-wikipedia](https://en.wikipedia.org/wiki/Vector))

## 题目
1.已知向量 $$ \vec{OA}, \vec{OB} $$ 满足 $$ \|OA\| = \|OB\| = 1, \vec{OA} \cdot \vec{OB} = 0, \vec{OC} = \lambda \ \vec{OA} + \mu \ \vec{OB} (\lambda ,\mu \in \mathbb{R}) $$. 若 $$ M $$ 为 $$ AB $$ 的中点，并且 $$ \|\vec{MC}\| = 1 $$，则点 $$ (\lambda , \mu) $$ 在 (  )

A. 以 $$ (-\frac{1}{2}, \frac{1}{2}) $$ 为圆心，半径为 $$ 1 $$ 的圆上.

B. 以 $$ (\frac{1}{2}, -\frac{1}{2}) $$ 为圆心，半径为 $$ 1 $$ 的圆上.

C. 以 $$ (-\frac{1}{2}, -\frac{1}{2}) $$ 为圆心，半径为 $$ 1 $$ 的圆上.

D. 以 $$ (\frac{1}{2}, \frac{1}{2}) $$ 为圆心，半径为 $$ 1 $$ 的圆上.

题解：

假设 $$ A $$ 点的坐标为 $$ (x_0, y_0) $$, $$ B $$ 点的坐标为 $$ (x_1, y_1) $$.

由题目前2个条件知：

$$ x_0 ^2 + y_0 ^2 = 1, x_1 ^2 + y_1 ^2 = 1, x_0 \ x_1 + y_0 \ y_1 = 0 $$

将 $$ A,B $$ 点坐标带入第3个条件：$$ \vec{OC} = \lambda \ \vec{OA} + \mu \ \vec{OB} (\lambda ,\mu \in \mathbb{R}) $$

于是求出 $$ C $$ 的坐标：$$ (\lambda \ x_0 + \mu \ x_1, \lambda \ y_0 + \mu \ y_1) $$

$$ M $$ 是 $ AB $ 的中点，所以得到 $$ M $$ 的坐标为：$$ (\frac{x_1 - x_0}{2}, \frac{y_1 - y_0}{2}) $$

又因为 $$ \|\vec{MC}\| = 1 $$, 把上面 $$ M, C $$ 的坐标代入进行计算并化简，最终可得：$$ (\lambda + \frac{1}{2})^2 + (\mu - \frac{1}{2})^2 = 1 $$

所以，选 B.

2.
