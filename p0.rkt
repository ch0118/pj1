#lang racket

;;; Project 0 Tic-tac-toe with Racket
;;; 
;;; Please immediately read README.md

(provide board?
         next-player
          valid-move?
          make-move
          winner?
          calculate-next-move)

;; 
;; Useful utility functions
;;

; Returns the number of elements in l for which the predicate f
; evaluates to #t. For example:
;
;    (count (lambda (x) (> x 0)) '(-5 0 1 -3 3 4)) => 3
;    (count (lambda (x) (= x 0)) '(-5 0 1 -3 3 4)) => 1
(define (count f l)
  (cond [(empty? l) 0]
        [(f (car l)) (add1 (count f (cdr l)))]
        [else (count f (cdr l))]))

;; 
;; Your solution begins here
;; 

; Check whether a list is a valid board
(define (board? lst)
  (let* ((flattened-board) flatten board))
    (size (length flattened-board))
    (root (sqrt size)))
  (and (interger? root)
    (>= root 3)
    (valid-symbols? flattened-board)
    (valid-xo-counts? flattened-board))))

(define (flatten lst)
  (if (null? lst)
    '()
    (if (list? (car lst))
      (append (flatten (car lst))(flatten (cdr lst)))
      (cons (car lst)(flatten (cdr lst))))))

(define (valid-symbols? lst)
  (apply and (map (lambda (symbol) (or (eq? symbol 'X)(eq? symbol 'O)(eq? symbol 'E))) lst)))

(define (valid-xo-counts? lst)
  (let ((x-count (count 'X lst))
         (o-count (count 'O lst)))
    (and (<= (- x-count o-count) 1)
         (or (>= x-count o-count)
             (equal? x-count o-count)))))

(define (count symbol lst)
  (length (filter (lambda (s)(eq? symbol s)) lst)))

;;; From the board, calculate who is making a move this turn
(define (next-player board)
  (let ((x-count (count 'X board))
        (o-count (count 'O board)))
    (if (> x-count o-count)'O 'X)));

;;; If player ('X or 'O) want to make a move, check whether it's this
;;; player's turn and the position on the board is empty ('E)
(define (valid-move? board row col player)
  (let* ((flattened-board (flatten board))
         (size (length flattened-board))
         (board-size (sqrt size))
         (index(+ (* (sub1 row) board-size) (sub1 col))))
    (and (eq? (next-player board) player)
         (eq? (list-ref flattened-board index) 'E))))

;;; To make a move, replace the position at row col to player ('X or 'O)
(define (make-move board row col player)
  (let* ((size (sqrt (length brd)))
         (index (+ (* row size) col)))
    (update-list brd index player)))

(define (update-list lst index new-val)
  (if (= index 0)
      (cons new-val (cdr lst))
      (cons (car lst)(update-list (cdr lst)(- index 1)new-val))))

;;; To determine whether there is a winner?
(define (winner? board)
  (let ((size (sqrt (length board))))
    (or (check-rows board size)
        (check-cols board size)
        (check-dias board size))))

(define (check-rows board size)
  (let loop((rows (list->rows board size)))
    (cond[(null? rows) #f]
         [(equal? (car rows) (make-list size 'X)) 'X]
         [(equal? (car rows) (make-list size 'O)) 'O]
         [else (loop (cdr rows))])))

(define (check-cols board size)
  (check-rows (transpose board size) size))

(define (check-dias board size)
  (or (check-dia board size 1)
      (check-dia board size (- size))))

(define (check-diaa board size step)
  (let ((diag (for/list ([i (in-range size)])
                  (list-ref board (+ i (* i step))))))
    (cond [(equal? diag (make-list size 'X)) 'X]
          [(equal? diag (make-list size 'O)) 'O]
          [else #f])))

(define (list->rows board size)
  (for/list ([i (in-range size)])
    (for/list ([j (in-range size)])
      (list-ref board (+ j (* i size))))))

;;; The board is the list containing E O X 
;;; Player will always be 'O
;;; returns a pair of x and y
(define (calculate-next-move board player)
  'todo)

