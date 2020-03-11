# My Compiler

For my Computer Languages and Translators course I developed my own compiler in Python 3 using the *ply* library. 
The scope of the language is to perform simple mathematical equations, consequently, the language only accepts:
-Global variables 
-Variables type integer or real
-One or two dimensional variables
-Subroutines
-Arithmetic operations following natural order of execution ((), *, /, +, -)
-Logical operations following order of execution (not, and, or)
-Comparison operations with equal order of execution (<, >, <=, >=, ==, !=) 
-Statutes:
	- read
	- print
	- call
	- if (elsif, else) end
	- do end
	- exit

The sintax of the program follows the structure shown in the images below.
![Sintax Diagram 1](/images/SintaxDiagram1.png)
![Sintax Diagram 2](/images/SintaxDiagram2.png)
![Sintax Diagram 3](/images/SintaxDiagram3.png)

The compiler follows the following grammar:
0. PROGRAMA : program id VAR RUT end
1. VAR : V VAR
2. VAR : V
3. RUT : R RUT
4. RUT : R
5. V : integer id
6. V : integer id dim
7. V : real id
8. V : real id dim
9. R : subroutine id EST end
10. R : subroutine id end
11. EST : S EST
12. EST : S
13. dim : ( cte , cte )
14. dim : ( cte )
15. S : id dim = EA
16. S : id = EA
17. S : read -> id
18. S : print -> ‘ text ’
19. S : print -> EA
20. S : call id ()
21. S : if C2 else EST end
22. S : if C2 end
23. S : do id = C3 , C3 , C3 EST end
24. S : do id = C3 , C3 EST end
25. S : do EST end
26. S : exit
27. C2 : EL then EST elsif C2
28. C2 : EL then EST
29. C3 : cte
30. C3 : id
31. EA : MD + EA
32. EA : MD - EA
33. EA : MD
34. MD : EXP * MD
35. MD : EXP / MD
36. MD : EXP
37. EXP : ( EA )
38. EXP : id dim
39. EXP : id
40. EXP : cte 
41. EL : EAND or EL
42. EL : EAND
43. EAND : ENOT and EAND
44. EAND : ENOT
45. ENOT : EC
46. ENOT : not id
47. ER : C4 < C5
48. ER : C4 <= C5
49. ER : C4 > C5
50. ER : C4 >= C5
51. ER : C4 == C5
52. ER : C4 != C5
53. ER : ( EL )
54. C4 : id
55. C4 : id dim

The repository includes 5 example program files that execute the compiler.
1. Sum and multiplication of two-dimensional matrices
2. Sorting of a one-dimensional vector
3. Mathematical operations: factorial, exponential and e^x by Taylor equation.
4. Printing the name of a given number between 0 and 7.
5. Multiplication of 2 numbers using recursion. 