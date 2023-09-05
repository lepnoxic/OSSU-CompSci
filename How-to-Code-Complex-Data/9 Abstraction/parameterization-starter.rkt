;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname parameterization-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; parameterization-starter.rkt

(define (area r)
  (* pi (sqr r)))

(area 4) ;area of circle radius 4
(area 6) ;area of circle radius 6


;; ====================

;; ListOfString -> Boolean
;; produce true if los includes "UBC"


;(define (contains-ubc? los) false) ;stub

;<template from ListOfString>

(define (contains-ubc? los) (contains? "UBC" los))

;; ListOfString -> Boolean
;; produce true if los includes "McGill"


;(define (contains-mcgill? los) false) ;stub

;<template from ListOfString>

(define (contains-mcgill? los) (contains? "McGill" los))

;; String (listof String) -> Boolean
;; produce true if los includes s
(check-expect (contains? "UBC" empty) false)
(check-expect (contains? "UBC" (cons "McGill" empty)) false)
(check-expect (contains? "UBC" (cons "UBC" empty)) true)
(check-expect (contains? "UBC" (cons "McGill" (cons "UBC" empty))) true)
(check-expect (contains? "McGill" (cons "UBC" empty)) false)
(check-expect (contains? "McGill" (cons "McGill" empty)) true)
(check-expect (contains? "McGill" (cons "UBC" (cons "McGill" empty))) true)
(check-expect (contains? "Toronto" (cons "UBC" (cons "McGill" empty))) false)

(define (contains? s los)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) s)
             true
             (contains? s (rest los)))]))


;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list of sqr of every number in lon
(check-expect (squares empty) empty)
(check-expect (squares (list 3 4)) (list 9 16))

;(define (squares lon) empty) ;stub

;<template from ListOfNumber>

(define (squares lon) (map2 sqr lon))

;; ListOfNumber -> ListOfNumber
;; produce list of sqrt of every number in lon
(check-expect (square-roots empty) empty)
(check-expect (square-roots (list 9 16)) (list 3 4))

;(define (square-roots lon) empty) ;stub

;<template from ListOfNumber>

(define (square-roots lon) (map2 sqrt lon))

;; (X -> Y) (listof X) -> (listof Y)
;; produce given fn and (list n0 n1 ...) produce (list (fn n0) (fn n1) ...) 
(check-expect (map2 sqr empty) empty)
(check-expect (map2 sqr (list 4 3)) (list 16 9))
(check-expect (map2 sqrt (list 16 9)) (list 4 3))

(define (map2 fn lon)
  (cond [(empty? lon) empty]
        [else
         (cons (fn (first lon))
               (map2 fn (rest lon)))]))


;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list with only positive? elements of lon
(check-expect (positive-only empty) empty)
(check-expect (positive-only (list 1 -2 3 -4)) (list 1 3))

;(define (positive-only lon) empty) ;stub

;<template from ListOfNumber>

(define (positive-only lon) (filter2 positive? lon))

;; ListOfNumber -> ListOfNumber
;; produce list with only negative? elements of lon
(check-expect (negative-only empty) empty)
(check-expect (negative-only (list 1 -2 3 -4)) (list -2 -4))

;(define (negative-only lon) empty) ;stub

;<template from ListOfNumber>

(define (negative-only lon) (filter2 negative? lon))

;; (X -> Boolean) (listof X) -> (listof X)
;; filter list to contain only those elements for which bo produces true 
(check-expect (filter2 zero? empty) empty)
(check-expect (filter2 positive? (list 0 1 2 -3)) (list 1 2))
(check-expect (filter2 negative? (list 0 -1 -2 3)) (list -1 -2))

(define (filter2 bo lon)
  (cond [(empty? lon) empty]
        [else
         (if (bo (first lon))
             (cons (first lon)
                   (filter2 bo (rest lon)))
             (filter2 bo (rest lon)))]))