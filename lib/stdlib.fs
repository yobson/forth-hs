
: IF ( cond -- *FB )
  POSTPONE ?BRANCH
  HERE 0 ,
; IMMEDIATE

: THEN ( *FB -- )
  HERE OVER -
  SWAP !
; IMMEDIATE

: ELSE ( *FB -- )
  POSTPONE BRANCH
  HERE 0 , SWAP
  HERE OVER -
  SWAP !
; IMMEDIATE


: fib
  DUP 0 = IF ELSE
  DUP 1 = IF ELSE
  DUP 1 - fib SWAP 2 - fib +
  THEN THEN
;

0 fib
1 fib
2 fib
3 fib
4 fib
5 fib
6 fib
7 fib
8 fib
9 fib

test

