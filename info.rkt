#lang info
(define collection "loudhum")
(define deps '("base" "html-parsing" "html-writing" "sxml"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/loudhum.scrbl" ())))
(define pkg-desc "Racket code for the FunDHum class/textbook")
(define version "0.6.0")
(define pkg-authors '(rebelsky))
