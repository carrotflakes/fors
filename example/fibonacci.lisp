(ql:quickload :fors)

(fors:with-fors ()
  ; calculate fibonacci
  (fors:run '(
    [ fib
      dup 0 swap <=
      [ drop 0 ]
      [
        dup 1 != 
        [ dup 1 swap - fib swap 2 swap - fib + ] if
      ] if-else
    ] def 30 fib))
  (print (fors:dump fors:*stack*)))
