;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname change) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; (fewest-coins amt currency) produces the smallest list of coins that 
;; are denominatons in currency and sum to amt
;; Requires: amt must be greater than or equal to 0
;;           currency must contain at least one element and the element 1
;; Examples:
(check-expect (fewest-coins 45 '(1 5 10 25)) '(25 10 10))
(check-expect (fewest-coins 115 '(1 5 10 25 100 200)) '(100 10 5))
(check-expect (fewest-coins 27 '(1 9 19)) '(9 9 9))

;; fewest-coins: Nat (listof Nat) -> (listof Nat) 

(define (fewest-coins amt currency)
  (cond [(negative? amt) empty]
        [(empty? currency) empty]
        [(member? amt currency) (list amt)]
        [else
         (local
           [;; defined constant change1, the first possible set of coins
            ;; to produce a given amount using all denominations
            (define change1
              (local
                [;; defined constant leftover that produces the smallest
                 ;; set of coins that sum to an amount - the first denomination
                 (define leftover
                   (fewest-coins (- amt (first currency)) currency))]

                (cond [(empty? leftover) empty]
                      [else (append leftover (list (first currency)))])))

            ;; defined constant change2, the other set of coins to produce
            ;; an amount using all but the first denomination
            (define change2
              (fewest-coins amt (rest currency)))]

           (cond [(empty? change1) change2]
                 [(empty? change2) change1]
                 [(> (length change1) (length change2))
                  change2]
                 [else change1]))]))

;; Tests:

;; defined function to make change lists for testing

;; (make-changelst from to currency) produces fewest-coins for each 
;; integer in interval from and to for currency, inclusive of to,
;; not inclusive of from
;; Examples:
(check-expect (make-changelst 0 11 '(1 5 10 25))
              '((10 1) (10) (5 1 1 1 1) (5 1 1 1) (5 1 1)
                (5 1) (5) (1 1 1 1) (1 1 1) (1 1) (1)))

;; make-changelst: Nat Nat (listof Nat) -> (listof (listof Nat)) 

(define (make-changelst from to currency)
  (cond [(= from to) empty]
        [else (cons (fewest-coins to currency)
                      (make-changelst from (sub1 to) currency))]))


(check-expect (fewest-coins 0 '(1 5 10 25)) empty)
(check-expect (fewest-coins 1 '(1 5 10 25)) (list 1))
(check-expect (fewest-coins 11 '(1 5 10 25)) (list 10 1))
(check-expect (fewest-coins 30 '(1 5 10 25)) (list 25 5))
(check-expect (fewest-coins 11 '(1 9 19)) (list 9 1 1))
(check-expect (fewest-coins 51 '(1 9 19)) (list 19 19 9 1 1 1 1))
(check-expect (fewest-coins 101 '(1 9 19)) (list 19 19 9 9 9 9 9 9 9))
(check-expect (fewest-coins 171 '(1 9 19)) (list 19 19 19 19 19 19 19 19 19))
(check-expect (fewest-coins 8 '(1 3)) (list 3 3 1 1))
(check-expect (fewest-coins 9 '(1 3)) (list 3 3 3))
(check-expect (fewest-coins 5 '(1 2)) (list 2 2 1))
(check-expect (fewest-coins 14 '(1 2)) (list 2 2 2 2 2 2 2))
(check-expect (make-changelst 17 32 '(1 5 14 30 49))
              (list
               (list 30 1 1)(list 30 1)(list 30)(list 14 14 1)(list 14 14)
               (list 14 5 5 1 1 1)(list 14 5 5 1 1)(list 14 5 5 1)(list 14 5 5)
               (list 14 5 1 1 1 1)(list 14 5 1 1 1)(list 14 5 1 1)(list 14 5 1)
               (list 14 5)(list 14 1 1 1 1)))
(check-expect (make-changelst 17 32 '(1 9 19))
              (list
               (list 19 9 1 1 1 1)(list 19 9 1 1 1)(list 19 9 1 1)(list 19 9 1)
               (list 19 9)(list 9 9 9)(list 19 1 1 1 1 1 1 1)
               (list 19 1 1 1 1 1 1)(list 19 1 1 1 1 1)(list 19 1 1 1 1)
               (list 19 1 1 1)(list 19 1 1)(list 19 1)(list 19)(list 9 9)))
(check-expect (make-changelst 17 32 '(1 5 10 25))
              (list
               (list 25 5 1 1)(list 25 5 1)(list 25 5)(list 25 1 1 1 1)
               (list 25 1 1 1)(list 25 1 1)(list 25 1)(list 25)
               (list 10 10 1 1 1 1)(list 10 10 1 1 1)(list 10 10 1 1)
               (list 10 10 1)(list 10 10)(list 10 5 1 1 1 1)(list 10 5 1 1 1)))