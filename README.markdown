# CL-PMATCH - Symbolic Pattern Matching For Common Lisp

## Usage

```common-lisp

(pmatch '(a b c) '(a b c))
;-> T

(pmatch '(a b c) '(a b d))
;-> NIL

(pmatch '(foo (or bar baz) qux) '(foo baz qux))
;-> T

(pmatch '(I (:verb (or love hate)) rock-n-roll) '(I love rock-n-roll))
;-> ((:VERB LOVE))

(pmatch '(a b c (:middle (? d) (? e) (? f)) g (:end h)) '(a b c d f g h))
;-> ((:MIDDLE D F) (:END H))

(pmatch '((any) 1000) '(b 1000))
;-> T

(pmatch '((any integer) 1000) '(b 1000))
;-> NIL

(pmatch '((any integer) 1000) '(10 1000))
;-> T
```

### Rules

The rules are very similar to regular expressions.

1. Normal symbols match each other: `(pmatch '(a) '(a))`  
2. Sublists starting with 'OR match on one of the elements of the sublist.  
3. Sublists starting with '? match the subgroup 0 or 1 times.  
4. Sublists starting with '+ match the subgroup 1 or more times.  
5. Sublists starting with '* match the subgroup 0 or more times.  
6. Sublists starting with keywords denote named capture groups.  
7. Sublists with 'ANY and an optional type (defaulting to T) match any one
element that matches that type (T matches all types).  

Where the regular expression `/fo+ba(rs)?/` would match "fooooba", if characters
were represented as a list of symbols, the same match would look like this:
```common-lisp
(pmatch '(f (+ o) b a (? r s)) '(f o o o o b a))
;-> T
```

## Installation


### ASDF-Install

```common-lisp
(asdf-install:install "http://github.com/deliciousrobots/cl-pmatch/tarball/master")
```

## Author

* Stephen A. Goss (steveth45@gmail.com)

## Copyright

Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)

# License

Licensed under the Modified BSD License.

