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

