;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname maze-2w-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Solve simple square mazes

;; maze-v1.rkt


;; Constants:

;; Data definitions:

;; Maze is (listof Boolean)
;; interp. a square maze
;;         each side length is (sqrt (length <maze>))
;;         true  (aka #t) means open, can move through this
;;         false (aka #f) means a wall, cannot move into or through a wall
;;

(define O #t) ;Open
(define W #f) ;Wall

(define M0
  (list O W W W
        W W W W
        W W W W
        W W W W))

(define M1
  (list O W W W W
        O O W O O
        W O W W W 
        O O W W W
        O O O O O))

(define M2
  (list O O O O O
        O W W W O
        O W W W O
        O W W W O
        O W W W O))

(define M3            
  (list O O O O O
        O W W W W
        O W W W W
        O W W W W 
        O O O O O))

(define M4
  (list O O O O O
        O W W W O
        O W O O O
        O W O W W
        W W O O O))

(define-struct pos (x y))
;; Pos is (make-pos Integer Integer)
;; interp. an x, y position in the maze.
;;         0, 0 is upper left.
;;         a position is only valid for a given maze if:
;;            - (<= 0 x (sub1 <size>))
;;            - (<= 0 y (sub1 <size>))
;;            - there is a true in the given cell
;;
(define P0 (make-pos 0 0)) ;upper left  in 4x4 maze
(define P1 (make-pos 3 0)) ;upper right  "  "   "
(define P2 (make-pos 0 3)) ;lower left   "  "   "
(define P3 (make-pos 3 3)) ;lower left   "  "   "


;; Functions:

;; Maze Pos -> Boolean
;; produce contents of given square in given maze
(check-expect (mref (list #t #f #f #f) (make-pos 0 0)) #t)
(check-expect (mref (list #t #t #f #f) (make-pos 0 1)) #f)

(define (mref m p)
  (local [(define s (sqrt (length m))) ;each side length
          (define x (pos-x p))
          (define y (pos-y p))]
    
    (list-ref m (+ x (* y s)))))

;; Maze -> Boolean
;; produce true if maze is solvable, else false
(check-expect (solvable? M0) false)
(check-expect (solvable? M1) true)
(check-expect (solvable? M2) true)
(check-expect (solvable? M3) true)
(check-expect (solvable? M4) false)

; (define (solvable? m) false) ; stub

(define (solvable? m)
  (local [(define (solve/one p)
            (cond [(solved? m p) true]
                  [else (solve/lop (next-pos m p))]))

          (define (solve/lop lop)
            (cond [(empty? lop) false]
                  [else
                   (or (solve/one (first lop))
                       (solve/lop (rest lop)))]))] 
    (solve/one (make-pos 0 0))))

;; Maze Pos -> Boolean
;; if pos is lower-right corner of maze then produce true
(check-expect (solved? (list O O W O) (make-pos 1 1)) true)
(check-expect (solved? M2 (make-pos 0 4)) false)
(check-expect (solved? M2 (make-pos 4 4)) true)

(define (solved? m p)
  (local [(define s (sqrt (length m)))] ;each side length
    (and (= (add1 (pos-x p)) s)
         (= (add1 (pos-y p)) s))))

;; Maze Pos -> (listof Pos)
;; generate possible and valid next positions in the maze
(check-expect (next-pos M1 (make-pos 0 0)) (list (make-pos 0 1)))
(check-expect (next-pos M2 (make-pos 0 0)) (list (make-pos 1 0) (make-pos 0 1)))
(check-expect (next-pos M3 (make-pos 4 0)) empty)
(check-expect (next-pos M3 (make-pos 0 4)) (list (make-pos 1 4)))

(define (next-pos m p)
  (local [(define (generate-pos p)       ;; Pos -> (listof Pos)
            (local [(define x (pos-x p)) ;; generate next two pos
                    (define y (pos-y p))]
              (list (make-pos (add1 x) y) (make-pos x (add1 y)))))       

          (define (valid-only lop)
            (filter valid? lop))
          
          (define (valid? p)
            (and (outside? p) (wall? p)))

          (define (wall? p) (mref m p))

          (define (outside? p)
            (local [(define s (sqrt (length m)))]
              (not (or (>= (pos-x p) s)
                       (>= (pos-y p) s)))))
          ]
    (valid-only (generate-pos p))))