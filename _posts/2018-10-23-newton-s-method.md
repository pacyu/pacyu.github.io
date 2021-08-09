---
title: 牛顿法(Newton's method)
layout: post
date: '2018-08-04 16:21:39 +0800'
tags:
- C/C++
- Math
categories:
- algorithms
- mathematical
cover_index: "/assets/222044074.jpg"
math: true
comments: true
toc: true
---

[牛顿法](https://en.wikipedia.org/wiki/Newton%27s_method)也是数值分析中很常见的算法了。嘛，网上对它的各种介绍也很多，但还是稍微说一下这个算法。

对于方程 $$ f(x) = 0 $$，初中的时候大部分的方程我们都可以使用求根公式解决，但实际上有许多方程我们很难求出它的根，于是只能用数值分析方法来逼近根值。基本原理:设 $$ y=f(x) $$ 在 $$ x \in [a,b] $$  上连续，如果有 $$ f(a)f(b) < 0 $$ ，则 $$ f(x), x \in (a, b) $$ 一定过零点（记不得叫啥定理了，好像叫费马引理。至于为什么，画个图就很容易明白了。），那么 $$ f(x) $$ 在点 $$ x0, (a < x_0 < b)$$ 处的切线方程也过零点，根据 $ f(x) $ 曲线的类型：递增、递减、凸或凹，我们会发现起点的选择对于切线随着点向根方向移动时的效率也是不同的。

比如：若 $$ x \in [a, b], f(a) < 0, f(b) > 0, f''(x) < 0 $$ 时，起点应从 $$a$$ 点开始迭代效率会更高，即考察 $$ f(a) $$ 与 $$ f''(x) $$ 是否同号，如果同号，则 $$a$$ 点为起点，否则 $$b$$ 点为起点。

总之，根据 $$ y = f(x) $$ 在 $$ x_0 $$ 处求出切线方程有：

$$ f(x) - f(x_0) = f'(x_0)(x - x_0) $$

由于 $$ f(x) = 0 $$，所以有：

$$ x_1 = x_0 - \frac{f(x_0)}{f'(x_0)} $$

再在点 $$ (x_1, f(x_1)) $$ 作切线，可得根得近似值 $$ x_2 $$。如此反复进行，一般的，在点 $$(x_{n-1}, f(x_{n-1}))$$ 作切线，最终可得到迭代方程：

$$ x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)} $$

## 算法实现
C++:
```cpp
#include <iostream>
#include <vector>
#include <cassert>
#include <functional>

const double h = 1e-5;

auto square(double x) -> double const { return x * x; }
auto cube(double x) -> double const { return x * x * x; }

template<class Fn_Ty>
auto partial_derivative(Fn_Ty g) {
    auto a = [g](const auto& vec, size_t i)
    {
        auto components = vec;
        components[i] += h;
        return (g(components) - g(vec)) / h;
    };
    return a;
}

template<class Fn_Ty, class Arg>
auto fixed_point(Fn_Ty g, Arg first_guess)
{
    auto close_enough = [](auto v1, auto v2)
    {
        return (std::abs(v1 - v2) < 1e-5);
    };
    auto try_again = [g, close_enough](auto guess, auto& try_ref)
    {
        auto next = g(guess);
        if (close_enough(guess, next)) return next;
        else return try_ref(next, try_ref);
    };
    return try_again(first_guess, try_again);
}

template<class Fn_Ty>
auto deriv(Fn_Ty g)
{
    auto a = [g](auto x)
    {
        return (g(x + h) - g(x)) / h;
    };
    return a;
}

template<class Fn_Ty>
auto newton_transform(Fn_Ty g)
{
    auto a = [g](auto x)
    {
        return x - (g(x) / deriv(g)(x));
    };
    return a;
}

template<class Fn_Ty, class Arg>
auto newton_method(Fn_Ty g, Arg guess)
{
    return fixed_point(newton_transform(g), guess);
}
```

## 示例
求 $$f(x) = \cos(x) - x^3 = 0$$ 的零点。
```cpp
int main()
{
	std::cout << newton_method([](double x) { return std::cos(x) - cube(x); }, 1.0) << std::endl;
	return 0;
}
```

输出：`0.865474`

![图1](/img/cosx-x^3.png)

## 相关内容
### 牛顿法开方
关于牛顿法开方（或任意次方）算法，强烈推荐看看这个回答：https://www.guokr.com/question/461510/

简单提一下需要注意得地方：求 $$ \sqrt{2} $$ 时，考虑有 $$ f(x) = x^2 - a = 0 $$，其中 $$a = 2$$。

开方代码实现：
```cpp
double m_sqrt(double x)
{
    auto a = [x](auto y)
    {
        return square(y) - x;
    };
    return newton_method(a, 1.0);
}

int main()
{
    std::cout << m_sqrt(2.0) << std::endl;
    return 0;
}
```

### 极值
$$ x_{i+1} = x_i - \frac{f'(x_i)}{f''(x_i)} $$

代码实现：
```cpp
template<class Fn_Ty>
auto minimization(Fn_Ty g)
{
    return newton_method(deriv(g), 1.0);
}

int main()
{
    std::cout << minimization([](auto x) { return 2. * square(1. - 4. * x) + square(1. - 2. * x); }) << std::endl;
    return 0;
}
```

### 微分
$$ f(x + \Delta x) \approx f(x) + f'(x) \cdot \Delta x $$

例如：

1. 求 $$ 2^{1.4} $$

由 $$ f(x) = a^x $$，$$ f(x + \Delta x) = a^{x + \Delta x} \approx a^{x} + a^{x} ln(a) \cdot \Delta x $$
有 $$ 2^{1 + 0.4} \approx 2 + 2ln(2) \cdot 0.4 \approx 2.554518... $$

而原表达式 $$ 2^{1.4} = 2.63902... $$

2. 求 $$ 2^{9} $$

利用公式有 $$ 2^{10 - 1} = (2^{10})^{(1 - \frac{1}{10})} \approx 2^{10} + 2^{10} ln(2^{10}) \cdot 0.1 \approx 709.783... $$

而 $$2^9$$ 等于 $$512$$

## 参考
1. [Newton's method - wikipedia](https://en.wikipedia.org/wiki/Newton%27s_method)
2. [果壳网](https://www.guokr.com/question/461510/)
3. 高等数学
