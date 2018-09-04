

import Polynomial

a = Poly [1, 0, 2]
b = Poly [3, 0, 4]
c = b * a

{-
Okay, what's the plan?

I want a function that says
1. a is a zero of polynomial p
2. b is a zero of polynomial q
3. Here is a polynomial whose zero is a+b.

For example I want to add (x^2 - 2)
and (x^2 - 3)
and get (x^4 - 10x + 1).

How?

I need to consider the free commutative ring over a and b
where a^2 = 2 and b^2 = 3.

I want to calculate powers of (a + b) in this ring.
The results will be polynomials over a and b.

Once I find n such that (a + b)^n is a linear combination of
(a + b)^i for i < n, I have my answer.

I need some code for dealing with polynomials over more than one variable.
Techniques:
1. Use Poly (Poly Int).  Drawback: canonicalizing might be a pain in the ass
2. 

-}

