---
title: 线段树（Segment tree）
layout: post
date: '2018-08-01 22:04:32 +0800'
tags:
- C/C++
categories:
- algorithms
cover_index: "/assets/15429725.jpg"
comments: true
---

一种区间操作算法。

## 算法实现

```cpp
#include <iostream>
#define LC(i) ((i) << 1)
#define RC(i) ((i) << 1 | 1)
#define M 10010
#define min(a, b) ((a) < (b) ? (a) : (b))
struct { int l, r, val; } SegTree[M];
int num[] = { 18, 17, 19, 13, 15, 11, 20 };
void build(int l, int r, int k)
{
    if (l == r)
        SegTree[k].val = num[l];
    else
    {
        int m = (l + r) >> 1;
        build(l, m, LC(k));
        build(m + 1, r, RC(k));
        SegTree[k].val = min(SegTree[LC(k)].val, SegTree[RC(k)].val);
    }
}
void update(int l, int r, int k, int pos, int v)
{
    if (l > pos || r < pos || r < l)
        return;
    if (l == r)
    {
        SegTree[k].val = v;
        return;
    }
    int m = (l + r) >> 1;
    if (pos <= m)
        update(l, m, LC(k), pos, v);
    else
        update(m + 1, r, RC(k), pos, v);
    SegTree[k].val = min(SegTree[LC(k)].val, SegTree[RC(k)].val);
}
int query(int l, int r, int x, int y, int k)
{
    if (l > r || l > y || r < x)
        return M;
    if (x <= l && r <= y)
        return SegTree[k].val;
    int m = (l + r) >> 1;
    return min(query(l, m, x, y, LC(k)), query(m + 1, r, x, y, RC(k)));
}
int main()
{
    int len, x, y, k, v;
 
    len = (sizeof(num) / sizeof(int)) - 1;
    build(0, len, 1);
 
    for (int i = 1; i <= 2 * len; i++)
        std::cout << SegTree[i].val << ' ';
    std::cout << std::endl;
 
    std::cout << "查询[x,y]区间的最小值：" << std::endl;
    std::cin >> x >> y;
    std::cout << query(0, len, x, y, 1) << std::endl;
 
    std::cout << "更新下标k位置上的值：" << std::endl;
    std::cin >> k >> v;
    update(0, len, 1, k, v);
 
    for (int i = 1; i <= 2 * len; i++)
        std::cout << SegTree[i].val << ' ';
    std::cout << std::endl;
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
#define l(i) ((i) << 1)
#define r(i) ((i) << 1 | 1)
const int N = 500010;
struct { int l, r, val, tag; } segment[N];
inline int read()
{
    char c; int ret = 0, sgn = 1;
    do{c = getchar();}while((c < '0' || c > '9') && c != '-');
    if(c == '-') sgn = -1; else ret = c - '0';
    while((c = getchar()) >= '0' && c <= '9') ret = ret * 10 + (c - '0');
    return sgn * ret;
}
void build(int num[], int s, int e, int p)
{
    if (s == e) segment[p].val = num[s];
    else
    {
        segment[p].tag = 0;
        int m = (s + e) >> 1;
        build(num, s, m, l(p));
        build(num, m + 1, e, r(p));
        segment[p].val = segment[l(p)].val + segment[r(p)].val;
    }
}
void update(int s, int e, int b, int f, int p, int v)
{
    if (b <= s && e <= f)
    {
        segment[p].val += v*(e - s + 1);
        segment[p].tag += v;
        return;
    }
    int m = (s + e) >> 1;
    segment[l(p)].tag += segment[p].tag;
    segment[l(p)].val += segment[p].tag*(m - s + 1);
    segment[r(p)].tag += segment[p].tag;
    segment[r(p)].val += segment[p].tag*(e - m);
    segment[p].tag = 0;
    if (b <= m) update(s, m, b, f, l(p), v);
    if (f > m) update(m + 1, e, b, f, r(p), v);
    segment[p].val = segment[l(p)].val + segment[r(p)].val;
}
int query(int s, int e, int b, int f, int p)
{
    if (b <= s && e <= f) return segment[p].val;
    int m = (s + e) >> 1, ans = 0;
    segment[l(p)].tag += segment[p].tag;
    segment[l(p)].val += segment[p].tag*(m - s + 1);
    segment[r(p)].tag += segment[p].tag;
    segment[r(p)].val += segment[p].tag*(e - m);
    segment[p].tag = 0;
    if (b <= m) ans += query(s, m, b, f, l(p));
    if (f > m) ans += query(m + 1, e, b, f, r(p));
    return ans;
}
int main()
{
    int n, m, num[N], x, y, v, opera;
    cin >> n >> m;
    for (int i = 1; i <= n; i++)
        cin >> num[i];
    build(num, 1, n, 1);
    while (m--)
    {
        cin >> opera;
        switch(opera)
        {
            case 1: cin >> x >> y >> v; update(1, n, x, y, 1, v); break;
            case 2: cin >> x >> y; cout << query(1, n, x, y, 1) << endl; break;
        }
    }
    return 0;
}
```

## 参考

[github-consmos](https://github.com/darkchii/cosmos/blob/master/code/data_structures/src/tree/segment_tree/segment_Tree_rmq.adb)

[博客园-TenosDoIt](http://www.cnblogs.com/TenosDoIt/p/3453089.html)
