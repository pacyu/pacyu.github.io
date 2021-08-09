---
title: Coq 证明几何不等式
math: true
comments: true
layout: post
---

一开始是想形式化几何不等式（均值不等式），然后想用来证明一些简单的不等式问题玩下，但发现直接证明 $$a + b \ge 2\sqrt{a b}$$ 太难，没想到好办法，应该是我太菜。。。于是选择证明 $$\sqrt{(a + b)^2} \ge \sqrt{4ab} \longrightarrow a + b \ge 2\sqrt{a b}$$ 的情形。

```coq
From Coq.Reals Require Import Reals RIneq R_sqrt.


Lemma rdiv_r:
 forall r, (r <> 0 -> r / r = r * / r)%R.
Proof.
  intros.
  auto.
Qed.

Lemma two_pow2_eq_4: (2 ^ 2 = 4)%R.
Proof.
  simpl.
  rewrite <- Rmult_comm.
  rewrite Rmult_assoc.
  rewrite Rmult_1_l.
  auto.
Qed.

Lemma two_eq_sqrt_4 : (2 = sqrt 4)%R.
Proof.
  rewrite <- two_pow2_eq_4.
  rewrite sqrt_pow2.
  - reflexivity.
  - intuition.
Qed. 

Theorem ineq_arith_means:
 forall a b, (0 <= a -> 0 <= b -> sqrt (4 * a * b) <= sqrt ((a + b)^2)  -> 2 * sqrt(a * b) <= a + b)%R.
Proof.
  intros a b H1 H2 H3.
  rewrite <- sqrt_pow2 with (a + b)%R.
  rewrite <- sqrt_pow2 with (2 * sqrt (a * b))%R.
  rewrite Rpow_mult_distr.
  rewrite pow2_sqrt.
  rewrite two_pow2_eq_4.
  *
    rewrite <- Rmult_assoc.
    assumption.
  *
    apply Rmult_le_pos.
    auto.
    auto.
  *
    rewrite two_eq_sqrt_4.
    rewrite <- sqrt_mult.
    apply sqrt_positivity.
    + apply Rmult_le_pos.
      - intuition.
      - apply Rmult_le_pos. auto. auto.
    + intuition.
    + apply Rmult_le_pos. auto. auto.
  *
    apply Rplus_le_le_0_compat.
    auto.
    auto.
Qed.

```
