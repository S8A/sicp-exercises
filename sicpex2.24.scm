#|

1 ]=> (list 1 (list 2 (list 3 4)))
;Value 14: (1 (2 (3 4)))


Its a list of two elements: its first element is 1 and its second element is 
a list of two elements, 2 and a list of 3 and 4.


Box-and-pointer structure:
(1 (2 (3 4))) --> |·|·|--> |·|/|
                   ↓        ↓
                   1       |·|·|--> |·|/|
                            ↓        ↓
                            2       |·|·|--> |·|/|
                                     ↓        ↓
                                     3        4

Tree:
(1 (2 (3 4)))
     /\
    /  \
   1   (2 (3 4))
        /\
       /  \
      2  (3 4)
           /\
          /  \
         3    4


|#