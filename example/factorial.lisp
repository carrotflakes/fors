(ql:quickload :fors)

(fors:with-fors ()
  (fors:run '([ factorial dup 1 = [ drop 1 ] [ dup 1 swap - factorial * ] if-else ] def
              1 factorial p 5 factorial p 10 factorial p)))
