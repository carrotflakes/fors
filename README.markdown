# fors
fors is a stack-based model of computation. 

## Usage
``` lisp
(ql:quickload :fors)

(fors:with-fors (:text-size 100000)
  ; calculate fibonacci
  (run '(
    [ fib
      dup 0 swap <=
      [ drop 0 ]
      [
        dup 1 != 
        [ dup 1 swap - fib swap 2 swap - fib + ] if
      ] if-else
    ] def 30 fib)))
```

## Installation

## Author

* carrotflakes (carrotflakes@gmail.com)

## Copyright

Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)

## License

Licensed under the LLGPL License.
