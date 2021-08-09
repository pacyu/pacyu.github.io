---
title: 并查集
layout: post
date: '2018-07-28 13:47:58 +0800'
tags:
- C/C++
categories:
- algorithms
cover_index: "/assets/15429725.jpg"
comments: true
---

## About algorithm

[Union-Find algorithm](https://en.wikipedia.org/wiki/Disjoint-set_data_structure)(also called a Disjoint-set or Merge find algorithm) is a very useful data structure.

## References

Recommend to see this document:[Union-Find.pdf](https://www.cs.princeton.edu/~rs/AlgsDS07/01UnionFind.pdf)


## Algorithm implementation

The C code as follows:

```cpp
#include <iostream>
const int N = 10010;
int id[N];
void init()
{
	for (int i = 0; i < N; i++)
		id[i] = i;
}
int find(int r)
{
	while (id[r] != r)	r = id[r];
	return r;
}
void transplant(int i) // path compression
{
	int r = find(i);
	while (i != r)
	{
		int p = id[i];
		id[i] = r;
		i = p;
	}
}
bool issmooth(int p, int q)
{
	return find(p) == find(q);
}
void join(int p, int q)
{
	int r1 = find(p), r2 = find(q);
	if (r1 != r2)
		id[r1] = r2;
}
int main()
{
	init();
	for (int i = 0; i < 8; i++)
		cout << i << "->" << id[i] << ' ';

	join(1, 2);
	join(1, 3);
	join(2, 4);
	join(5, 6);
	join(6, 7);
	join(6, 8);

	if (issmooth(2, 7))
		cout << "true" << endl;
	else
		cout << "false" << endl;

	transplant(1);
	for (int i = 0; i < 8; i++)
		cout << i << "->" << id[i] << ' ';
	getchar();
	return 0;
}

/*
	1--2--4
	| /
	|/
	3
	5--6--7
	   |
	   8
*/
```
