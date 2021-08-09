---
title: Kruskal's algorithm
layout: post
date: '2018-08-29 18:18:43 +0800'
tags:
- C/C++
categories:
- algorithms
cover_index: "/assets/4201472.jpg"
comments: true
toc: true
---

Kruskal算法可用来求解最小生成树(minimum-spanning-tree, MST)问题，还可以用来生成迷宫。

## 算法分析

算法先要将 $$ G(V, E) $$ 的集合 $$ E $$ 按权重 $$ \Omega $$ 由小到大排序，然后还利用了不相交集中的`find()`(这里使用的是带路径压缩功能的) 和`union()`(这里函数名使用`marge()`) 函数，`find()`用于判断是否连通，如果连通则不能构成MST，反之则加入到MST的集合中，并调用`union()`函数将顶点连通。

##### 时间复杂度

$$ O(E lg V) $$

##### 空间复杂度

$$ O(V + E) $$

## 算法实现

```C++
#include "stdafx.h"
#include <iostream>
#include <algorithm>
#include <vector>
#include <set>
using namespace std;
const int N = 10010;
int p[N];
vector<pair<int, pair<int, int>>> graph;
void init(int V, int E)
{
	for (int i = 1; i <= V; i++)
		p[i] = i;
	for (int i = 0; i < E; i++)
	{
		int w, s, e;
		cin >> w >> s >> e;
		graph.push_back(pair<int, pair<int ,int>>(w, pair<int, int>(s, e)));
	}
	sort(graph.begin(), graph.end());
	for (auto e : graph)
		cout << e.first << e.second.first << e.second.second << endl;
}
int find(int x)
{
	if (x != p[x])	p[x] = find(p[x]);
	return p[x];
}
void marge(int x, int y)
{
	int r = find(x), t = find(y);
	if (r != t)	p[r] = t;
}
vector<pair<int, int>> kruskal(int V, int E)
{
	vector<pair<int, int>> msts;
	init(V, E);
	for (vector<pair<int, pair<int, int>>>::iterator i = graph.begin(); i != graph.end(); i++)
	{
		if (find(i->second.first) != find(i->second.second))
		{
			msts.push_back(i->second);
			marge(i->second.first, i->second.second);
		}
	}
	return msts;
}
int main(int argc, char **argv)
{
	int V, E;
	cin >> V >> E;
	vector<pair<int, int>> es = kruskal(V, E);
	for (auto e : es)
		cout << e.first << "  " << e.second << endl;
	return 0;
}
```

## 参考

1.[Kruskal's algorithm - wikipedia](https://en.wikipedia.org/wiki/Kruskal%27s_algorithm)
2.[Maze generation algorithm - wikipedia](https://en.wikipedia.org/wiki/Maze_generation_algorithm)
3.CLRS $$ P_{366} $$ 伪代码
