#|
Empirically, par2 produces more accurate results than par1. See sicpex2.14.scm

Let's evaluate the Eva Lu Ator's logic. She says that "a formula to compute 
with intervals [...] will produce tighter error bounds if it can be written 
in such a form that no variable [...] is repeated".

It seems logical, because by repeating variables their errors are repeated too, 
and their impact is much bigger when those variables are multiplied or divided. 
By using formulas that avoid redundant variables, the errors of the variables 
themselves are not repeated, and the error of arithmetic operations on modern 
computers are usually a lot smaller than the error of common intervals.

|#

