;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname brackets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; (balanced? bstr) determines if bstr is balanced, and contains a close 
;; bracket for each open bracket
;; Examples:
(check-expect (balanced? "((<>[])<>)[]") true)
(check-expect (balanced? "(<>[])") true)
(check-expect (balanced? "(<)>") false)

;; balanced?: Str -> Bool

(define (balanced? bstr)
  (local
    [;; (check-balance lob stack) determines if lob is balanced by determining
     ;; if each bracket in lob has its opposite close bracket using bracket
     ;; "holder", stack
     ;; check-balance: (listof Char) (listof Char) -> Bool
     (define (check-balance lob stack)
       (cond [(and (empty? lob)
                   (empty? stack))
              true]
             [(and (empty? lob)
                   (not (empty? stack)))
              false]
             [(empty? stack)
              (check-balance (rest lob)
                             (append (list (first lob)) stack))]
             [(closed? (first stack)) false]
             [(equal? (oppo-brac (first stack)) (first lob))
              (check-balance (rest lob) (rest stack))]
             [else (check-balance (rest lob)
                                  (append (list (first lob)) stack))]))

     ;; (oppo-brac bracket) returns the opposite char to bracket
     ;; oppo-brac: Char -> Char
     (define (oppo-brac bracket)
       (cond [(equal? #\( bracket) #\)]
             [(equal? #\[ bracket) #\]]
             [(equal? #\< bracket) #\>]))

     ;; (closed? bracket) determines if bracket is closed bracket
     ;; Char -> Bool
     (define (closed? bracket)
       (cond [(or (equal? #\) bracket)
                  (equal? #\] bracket)
                  (equal? #\> bracket)) true]
             [else false]))]

    (check-balance (string->list bstr) empty)))

;; Tests:
(check-expect (balanced? "()<>[][]()<>") true)
(check-expect (balanced? "()<>[][]()<") false)
(check-expect (balanced? "()") true)
(check-expect (balanced? "][><)(") false)
(check-expect (balanced? "><") false)
(check-expect (balanced? "") true)

