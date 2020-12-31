;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)

;; Required rkt file for random-animals function
(require "animals.rkt")

;; Defined list of examples for testing
(define seen
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)))





;; (collect-attributes examples) produces list of all attributes 
;; contained in examples

;; Examples:
(check-expect (collect-attributes seen)
              (list 'small 'large 'swims 'medium 'flies 'angry))

;; collect-attributes: (listof Example) -> (listof Sym) 

(define (collect-attributes examples)
  (local
    [;; (remove-duplicates lst) removes all duplicates from lst in order
     ;; remove-duplicates: (listof Sym) -> (listof Sym)
     (define (remove-duplicates lst)
       (cond [(empty? lst) empty]
             [(member? (first lst) (rest lst))
              (remove-duplicates (rest lst))]
             [else (cons (first lst)
                         (remove-duplicates (rest lst)))]))
     
     ;; (get-attributes lst) returns every attribute from lst
     ;; get-attributes: (listof Example) -> (listof Sym)
     (define (get-attributes lst)
       (cond [(empty? lst) empty]
             [else (append (rest (first lst))
                           (get-attributes (rest lst)))]))]

    (remove-duplicates (get-attributes examples))))

;; Tests:
(check-expect (collect-attributes
               (list (list 'penguin 'chef 'notfly 'hungry)
                     (list 'student 'hungry 'notfly)))
              (list 'chef 'hungry 'notfly))
(check-expect (collect-attributes empty)
              empty)







;; (split-examples examples symbol) Splits examples into two lists
;; of examples, one that each example contains the symbol, and one that
;; doesn't contain the symbol

;; Examples:
(check-expect (split-examples seen 'goose)
              (list
               (list (list 'goose 'large 'swims 'flies 'angry)
                     (list 'goose 'large 'swims 'flies 'angry))
               (list (list 'squirrel 'small 'angry)
                     (list 'crow 'medium 'flies 'angry))))
(check-expect (split-examples seen 'small)
              (list
               (list (list 'squirrel 'small 'angry))
               (list (list 'goose 'large 'swims 'flies 'angry)
                     (list 'goose 'large 'swims 'flies 'angry)
                     (list 'crow 'medium 'flies 'angry))))

;; split-examples:
;;(listof Example) -> (listof (listof Example) (listof Example))


(define (split-examples examples symbol)
  (local
    [;; (inlst lst symbol) produces a list of all examples in lst
     ;; that contain symbol
     ;; inlst: (listof Examples) Sym -> (listof Examples)
     (define (inlst lst symbol)
       (cond [(empty? lst) empty]
             [(member? symbol (first lst))
              (append (list (first lst))
                      (inlst (rest lst) symbol))]
             [else (inlst (rest lst) symbol)]))
     
     ;; (notinlst lst symbol) produces a list of all examples in lst
     ;; that do not contain symbol
     ;; notinlst: (listof Examples) Sym -> (listof Examples)
     (define (notinlst lst symbol)
       (cond [(empty? lst) empty]
             [(not (member? symbol (first lst)))
              (append (list (first lst))
                      (notinlst (rest lst) symbol))]
             [else (notinlst (rest lst) symbol)]))]

    (list (inlst examples symbol) (notinlst examples symbol))))


;; Tests:
(check-expect (split-examples empty 'symbol)
              (list empty empty))
(check-expect (split-examples seen 'honk)
              (list empty
                    (list
                     (list 'squirrel 'small 'angry)
                     (list 'goose 'large 'swims 'flies 'angry)
                     (list 'goose 'large 'swims 'flies 'angry)
                     (list 'crow 'medium 'flies 'angry))))
(check-expect (split-examples seen 'angry)
              (list (list
                     (list 'squirrel 'small 'angry)
                     (list 'goose 'large 'swims 'flies 'angry)
                     (list 'goose 'large 'swims 'flies 'angry)
                     (list 'crow 'medium 'flies 'angry))
                    empty))







;; (histogram examples) produces pairs of attributes from examples 
;; that are in the form of a Histogram

;; Examples:
(check-expect (histogram seen)
              (list (list 'small 1) (list 'large 2) (list 'swims 2)
                    (list 'medium 1) (list 'flies 3) (list 'angry 4)))

;; histogram: (listof Examples) -> Histogram

(define (histogram examples)
  (local
    [;; (histo-attributes attribs examples) produces Histogram with pairs
     ;; for each attribs seen in examples
     ;; histo-attributes: (listof Sym) (listof Examples) -> Histogram
     (define (histo-attributes attribs examples)
       (cond [(empty? attribs) empty]
             [else (append (list (list (first attribs)
                                       (get-total (first attribs) examples)))
                           (histo-attributes (rest attribs) examples))]))
     
     ;; (get-total attrib examples) produces number of times
     ;; attrib is seen in examples
     ;; get-total: Sym (listof Examples) -> Nat
     (define (get-total attrib examples)
       (cond [(empty? examples) 0]
             [(member? attrib (first examples))
              (+ 1 (get-total attrib (rest examples)))]
             [else (get-total attrib (rest examples))]))]


    (histo-attributes (collect-attributes examples) examples)))

;; Tests:
(check-expect (histogram empty) empty)
(check-expect (histogram
               (list (list 'penguin 'chef 'notfly 'hungry)
                     (list 'student 'hungry 'notfly)))
              (list (list 'chef 1) (list 'hungry 2) (list 'notfly 2)))








;; (augment-histogram histogram attributes total) Converts histogram 
;; into an AugmentedHistogram in order of attributes using total
;; Requires: total must be greater than or equal to the largest nat in
;;           histogram

;; Examples:
(check-expect
 (augment-histogram
  (list (list 'a 100) (list 'c 50))
  (list 'a 'b 'c)
  200)
 (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))
(check-expect
 (augment-histogram empty (list 'x 'y) 10)
 (list (list 'x 0 10) (list 'y 0 10)))


;; augment-histogram: Histogram (listof Sym) Nat -> AH

(define (augment-histogram histogram attributes total)
  (local [;;(get-seen attrib histogram) returns value of times attrib is seen
          ;; in histogram
          ;; get-seen: Sym Histogram -> Nat
          (define (get-seen attrib histogram)
            (cond [(empty? histogram) 0]
                  [(symbol=? attrib (first (first histogram)))
                   (second (first histogram))]
                  [else (get-seen attrib (rest histogram))]))]

    (cond [(empty? attributes) empty]
          [else (append (list (list (first attributes)
                                    (get-seen (first attributes)
                                              histogram)
                                    (- total (get-seen
                                              (first attributes)
                                              histogram))))
                        (augment-histogram histogram
                                           (rest attributes)
                                           total))])))


;; Tests:
(check-expect
 (augment-histogram
  (list (list 'a 10) (list 'c 2) (list 'd 1))
  (list 'a 'b 'c 'd 'e 'f)
  10)
 (list (list 'a 10 0) (list 'b 0 10) (list 'c 2 8)
       (list 'd 1 9) (list 'e 0 10) (list 'f 0 10)))






;; (entropy positive-counts negative-counts) produces entropy
;; between positive-counts and negative-counts

;; entropy: (listof Sym Nat Nat) (listof Sym Nat Nat) -> Num

(define (entropy positive-counts negative-counts)
  (local
    [;; local definitions for ease of use
     (define a (second positive-counts))
     (define b (second negative-counts))
     (define c (third positive-counts))
     (define d (third negative-counts))

     ;; (p-func n m) produces the probability of count pair n and m
     ;; p-func: Num Num -> Num
     (define (p-func n m)
       (cond [(> (+ n m) 0) (/ n (+ n m))]
             [else 0.5]))

     ;; (e-func prob) produces the e value of prob
     ;; e-func: Num -> Num
     (define (e-func prob)
       (cond [(= prob 0) 0]
             [else (* -1 (* prob (log prob 2)))]))]


    (+ (* (p-func (+ a b) (+ c d))
          (+ (e-func (p-func a b))
             (e-func (p-func b a))))
       (* (p-func (+ c d) (+ a b))
          (+ (e-func (p-func c d))
             (e-func (p-func d c))))
       )))

;; Tests:
(check-within (entropy (list 'large 126 59) (list 'large 146 669))
              0.5663948 0.001)
(check-within (entropy (list 'small 17 168) (list 'small 454 361))
              0.5825593 0.001)
(check-within (entropy (list 'a 0 100) (list 'b 100 0)) 0 0.001)






;; (entropy-attributes positive negative) produces entropy association list
;; for each first pair of values from positive negative


;; entropy-attributes: AH AH -> EAL

(define (entropy-attributes positive negative)
  (local
    [;; (specific-entropy pos-val neg-val) creates element of EAL by
     ;; calculating entropy for pos-val and neg-val
     ;; specific-entropy: (listof Sym Nat Nat) (listof Sym Nat Nat) ->
     ;; (listof Sym Num)
     (define (specific-entropy pos-val neg-val)
       (list (first pos-val) (entropy pos-val neg-val)))]

    (cond [(empty? positive) empty]
          [else (append (list (specific-entropy (first positive)
                                                (first negative)))
                        (entropy-attributes (rest positive)
                                            (rest negative)))])))

;; Tests:
(check-within (entropy-attributes
               (list
                (list 'large 126 59) (list 'angry 161 24)
                (list 'small 17 168) (list 'flies 170 15)
                (list 'swims 162 23) (list 'medium 42 143))
               (list
                (list 'large 146 669) (list 'angry 469 346)
                (list 'small 454 361) (list 'flies 615 200)
                (list 'swims 365 450) (list 'medium 215 600)))
             (list
              (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
              (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
              (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))
             0.001)
                                                               





;; (best-attribute entropies) produces the symbol of the attribute from
;; entropies with the smallest entropy value
;; Examples:
(check-expect
 (best-attribute
   (list
    (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
    (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
    (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)))
 'large)

;; best-attribute: EAL -> Sym 

(define (best-attribute entropies)
  (cond [(empty? (rest entropies)) (first (first entropies))]
        [(> (second (first entropies)) (second (second entropies)))
         (best-attribute (rest entropies))]
        [else (best-attribute (cons (first entropies)
                                     (rest (rest entropies))))]))

;; Tests:
(check-expect
 (best-attribute
   (list
    (list 'op1 1) (list 'op2 0))) 'op2)
(check-expect
 (best-attribute
   (list
    (list 'op1 0) (list 'op2 1))) 'op1)
(check-expect
 (best-attribute
   (list
    (list 'op1 0) (list 'op2 0))) 'op1)
(check-expect
 (best-attribute
   (list
    (list 100 1) (list 200 10))) 100)






;; (build-dt examples label) creates decision tree in terms of label
;; from training on examples
;; Examples:


;; build-dt: (listof Examples) Sym -> DT 

(define (build-dt examples label)
  (local
    [;; (remove-root examples root) removes all occurances of root from
     ;; each example in examples
     ;; remove-root: (listof Examples) Sym -> (listof Examples)
     (define (remove-root examples root)
       (local
         [;; (remove-root-single example root) removes occurances of root
          ;; from example
          ;; remove-root-single: Examples Sym -> (listof Sym)
          (define (remove-root-single example root)
            (cond [(empty? example) empty]
                  [(= (length example) 1) empty]
                  [(equal? (first example) root)
                   (remove-root-single (rest example) root)]
                  [else (cons (first example)
                              (remove-root-single (rest example) root))]))]

         (cond [(empty? examples) empty]
               [else (append (list (remove-root-single (first examples) root))
                             (remove-root (rest examples) root))])))

     ;; Defined constants for ease of use
     (define attributes (collect-attributes examples))
     (define positive-examples (first (split-examples examples label)))
     (define negative-examples (second (split-examples examples label)))]

    (cond [(empty? positive-examples) false]
          [(empty? negative-examples) true]
          [(empty? attributes)
           (cond [(> (length positive-examples)
                     (length negative-examples))
                  true]
                 [else false])]
          [else
           (local
             [
              ;; defined constants for ease of use
              (define root-attribute (best-attribute
                                      (entropy-attributes
                                       (augment-histogram
                                        (histogram positive-examples)
                                        attributes
                                        (length positive-examples))
                                       (augment-histogram
                                        (histogram negative-examples)
                                        attributes
                                        (length negative-examples)))))
              (define with-root (remove-root
                                 (first (split-examples examples
                                                        root-attribute))
                                 root-attribute))
              (define without-root (second (split-examples examples
                                                           root-attribute)))
              (define dt-with-root (build-dt with-root label))
              (define dt-without-root (build-dt without-root label))]

             (cond [(equal? dt-with-root dt-without-root)
                    (list root-attribute dt-with-root)]
                   [else (list root-attribute
                               dt-with-root
                               dt-without-root)]))])))






;; (train-classifier examples label) produces a predicate label by training 
;; on examples using build-dt


;; train-classifier: (listof Example) Sym -> ((listof Sym) -> Bool)

(define (train-classifier examples label)
  (local
    [;; (decision? dt vals) determines if vals produces true or false
     ;; for dt
     ;; decision?: DT (listof Sym) -> Bool
     (define (decision? dt vals)
       (cond [(equal? dt true) true]
             [(equal? dt false) false]
             [(member? (first dt) vals)
              (decision? (second dt) vals)]
             [(= (length dt) 2)
              (decision? (second dt) vals)]
             [else (decision? (third dt) vals)]))

     (define tree (build-dt examples label))

     (define (take-syms los)
       (local
         [;; defined constants to produce function for train-classifier
          (define answer (decision? tree los))]
         answer))]

    take-syms))

;; Tests:
(define goose? (train-classifier (random-animals 1000) 'goose))
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)
(define crow? (train-classifier (random-animals 1000) 'crow))
(check-expect (crow? (list 'angry 'flies 'medium)) true)




;; (performance classifier? examples label) determines sensitivity and
;; specificity of examples on label using classifier?


;; performance: ((listof Sym) -> Bool) (listof Example) Sym ->
;; (listof Sym Nat Nat)

(define (performance classifier? examples label)
  (local
    [
     ;; predefined classifier to prevent reuse of function
     (define class classifier?)
     (define pos-examples (first (split-examples examples label)))
     (define neg-examples (second (split-examples examples label)))

     ;; (remove-first examples) removes first element of all examples
     ;; remove-first: (listof Example) -> (listof Example)
     (define (remove-first examples)
       (cond [(empty? examples) empty]
             [else (cons (rest (first examples))
                         (remove-first (rest examples)))]))

     (define pos-noname (remove-first pos-examples))
     (define neg-noname (remove-first neg-examples))

     (define (total-pos-correct examples class)
            (cond [(empty? examples) 0]
                  [(class (first examples))
                   (+ 1 (total-pos-correct (rest examples) class))]
                  [else (total-pos-correct (rest examples) class)]))

     (define (total-neg-correct examples class)
            (cond [(empty? examples) 0]
                  [(not (class (first examples)))
                   (+ 1 (total-neg-correct (rest examples) class))]
                  [else (total-neg-correct (rest examples) class)]))]

    (list label
          (round (* 100 (/ (total-pos-correct pos-noname class) (length pos-noname))))
          (round (* 100 (/ (total-neg-correct neg-noname class) (length neg-noname)))))))