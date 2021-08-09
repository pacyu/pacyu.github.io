---
title: 后缀树
layout: post
date: '2018-09-25 22:47:46 +0800'
tags:
- C/C++
categories:
- algorithms
cover_index: "/assets/5307140.jpg"
comments: true
---

详见:[Suffix tree](https://www.cnblogs.com/darkchii/p/9116558.html)

# 路径压缩版后缀树

```cpp
#include <iostream>
using namespace std;
#define rep(i, a, b) for(int i = a; i < b; i++)
#define trans(c) (c - 'a')
#define SIZE 26
#define MAX (100010 << 2)
struct BaseNode {
	int len;
	const char*s;
	int pos[MAX];
	BaseNode*next[SIZE];
	BaseNode()
	{
		len = 0;
		rep(i, 0, MAX) pos[i] = 0;
		rep(i, 0, SIZE) next[i] = nullptr;
	}
	BaseNode(const char*s, int p)
	{
		this->s = s, this->len = p;
		rep(i, 0, MAX) pos[i] = 0;
		rep(i, 0, SIZE) next[i] = nullptr;
	}
};
class SuffixTree {
private:
	BaseNode*root;
	/**/
	void add(const char*s, int p);
	void print(BaseNode*r);
	void destory(BaseNode*&r);
public:
	SuffixTree()
	{
		root = nullptr;
	}
	void insert(const char*s);
	void insert(string s)
	{
		insert(s.c_str());
	}
	void remove(const char*s)
	{

	}
	void visual()
	{
		print(root);
	}
	bool match(const char*s);
	bool match(string s)
	{
		match(s.c_str());
	}
	~SuffixTree()
	{
		destory(root);
	}
};
void SuffixTree::add(const char*s, int p)
{
	int i = 0; while (s[i]) i++;
	if (!root->next[p]) root->next[p] = new BaseNode(s, i);
	root->next[p]->pos[i] = i;
}
void SuffixTree::insert(const char*s)
{
	root = new BaseNode();
	while (*s)
	{
		add(s, trans(*s));
		s++;
	}
}
bool SuffixTree::match(const char*s)
{
	const char* ps = root->next[trans(*s)]->s;
	while (*s) if (*ps++ != *s++) return false;
	return true;
}
void SuffixTree::print(BaseNode*r)
{
	if (r)
	{
		rep(i, 0, SIZE)
			if (r->next[i])
			{
				cout << i << ':' << endl;
				rep(j, 0, r->next[i]->len + 1)
					if (r->next[i]->pos[j])
					{
						rep(k, 0, r->next[i]->pos[j])
							cout << r->next[i]->s[k];
						cout << '$' << endl;
					}
			}
	}
}
void SuffixTree::destory(BaseNode*&r)
{
	if (r)
	{
		rep(i, 0, SIZE) destory(r->next[i]);
		delete r;
	}
}
int main()
{
	SuffixTree st;
	st.insert("banana");
	st.visual();
	if (st.match("na")) cout << "Yes" << endl;
	else cout << "No" << endl;
	return 0;
}
```

output:

```
0:
a$
ana$
anana$
1:
banana$
13:
na$
nana$
Yes
```
