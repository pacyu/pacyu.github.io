---
title: 计数排序
layout: post
date: '2018-09-09 20:38:06 +0800'
tags:
- C/C++
- Sorting
categories:
- algorithms
cover_index: "/assets/52183994.jpg"
comments: true
---

### 简介

计数排序是一种空间换时间的排序算法。通常用作[基数排序](https://en.wikipedia.org/wiki/Radix_sort)的子过程。

### 参考文献

1. [Counting sort - wikipeida](http://en.wikipedia.org/wiki/Counting_sort)
2. [计数排序 - 百度百科](https://baike.baidu.com/item/%E8%AE%A1%E6%95%B0%E6%8E%92%E5%BA%8F)
3. [Counting sort - geeksforgeeks](https://www.cdn.geeksforgeeks.org/counting-sort/)

### 算法实现

```cpp
#include <iostream>
using namespace std;
const int MAXN = 100000;
const int k = 1000;
int a[MAXN], c[MAXN], ranked[MAXN];
int main()
{
    int n;
    cin >> n;
    for (int i = 0; i < n; ++i)
    {
        cin >> a[i];
        ++c[a[i]];
    }
    for (int i = 1; i < k; ++i)
        c[i] += c[i - 1];
    for (int i = n - 1; i >= 0; --i)
        ranked[--c[a[i]]] = a[i];
    for (int i = 0; i < n; ++i)
        cout << ranked[i] << ' ';
    return 0;
}
```

### 算法分析

* 时间复杂度 O(n + k)

* 空间复杂度 O(n + k)
