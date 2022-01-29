---
title: 树状数组（Fenwick tree）
layout: post
date: '2018-08-01 21:46:03 +0800'
tags:
- C/C++
categories:
- algorithms
cover_index: "/assets/61082747.jpg"
comments: true
---

树状数组是一种是常用于区间操作的算法。

## 算法实现

```cpp
#include <iostream>
#define LSB(i) (i & -i)
const int M = 10001;
int bit_tree[M];
int freq[M];
int getsum(int index)
{
    int sum = 0;
    for (index += 1; index > 0; index -= LSB(index))
        sum += bit_tree[index];
    return sum;
}
void update(int n, int index, int val)
{
    for (index += 1; index <= n; index += LSB(index))
        bit_tree[index] += val;
}
void init(int freq[], int n)
{
    for (int i = 1; i <= n; i++)
        bit_tree[i] = 0;
    for (int i = 0; i < n; i++)
        update(n, i, freq[i]);
}
int main()
{
    int n;
    std::cin >> n;
    for (int i = 0; i < n; i++)
        std::cin >> freq[i];
    init(freq, n);
    for (int i = 0; i < n; i++)
    std::cout << getsum(i) << std::endl;
    for (int i = 1; i <= n; i++)
        std::cout << bit_tree[i] << ' ';
    return 0;
}
```

## 模板

```cpp
#include <bits/stdc++.h>
#define MP make_pair
#define PB push_back
#define st first
#define nd second
#define rd third
#define rg register
#define FOR(i, a, b) for(int i =(a); i <=(b); ++i)
#define RE(i, n) FOR(i, 1, n)
#define FORD(i, a, b) for(int i = (a); i >= (b); --i)
#define REP(i, n) for(int i = 0;i <(n); ++i)
#define VAR(v, i) __typeof(i) v=(i)
#define FORE(i, c) for(VAR(i, (c).begin()); i != (c).end(); ++i)
#define ALL(x) (x).begin(), (x).end()
#define SZ(x) ((int)(x).size())
using namespace std;

#define lowbit(x) ((x) & (-x))
const int N = 500010;
int id[N];
void upd(int n, int k, int x)
{
    while (k <= n) id[k] += x, k += lowbit(k);
}
void dif(int n, int l, int r, int x)
{
    upd(n, l, x);
    upd(n, r + 1, -x);
}
int sum(int k)
{
    int ans = 0;
    while (k > 0) ans += id[k], k -= lowbit(k);
    return ans;
}
int org(int k)
{
    return sum(k) - sum(k - 1);
}
int ask(int l, int r)
{
    return sum(r) - sum(l - 1);
}
int main()
{
    int n, m, k, x, opera, l, r, pre;
    pre = 0;
    cin >> n >> m;
    FOR (i, 1, n)
    {
        cin >> x;
        upd(n, i, x - pre); // 差分后更新到树状数组
        pre = x;
    }
    while(m--)
    {
        cin >> opera;
        switch(opera)
        {
            case 1: cin >> l >> r >> x; dif(n, l, r, x); break;
            case 2: cin >> k; cout << sum(k) << endl; break;
        }
    }
    return 0;
}
```

## 参考

[fenwick tree](https://en.wikipedia.org/wiki/Fenwick_tree)
