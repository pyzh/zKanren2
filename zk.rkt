#|
    zKanren2 : A Better MiniKanren
    Copyright (C) 2018  Zaoqi

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#
#lang typed/racket
(module S typed/racket
  (provide (rename-out [my-struct struct]))
  (define-syntax my-struct
    (syntax-rules ()
      [(_ x ...) (struct x ... #:transparent)])))
(require 'S)

; (: list-bind (All (A B) (Listof A) (-> A (Listof B)) -> (Listof B)))
; (define (list-bind xs f) (apply append (map f xs)))

(define-type Count Positive-Integer)
(: count-init Count)
(define count-init 1)
(: count-next (-> Count Count))
(define (count-next x) (+ x 1))
(struct Var ([v : Count]))
(struct State ([=/=s : =/=s]
               [count : Count]
               [goal-applys : (Listof GoalApplyProcedure)]
               [q : Any]))
(struct GoalApplyProcedure ([f : (-> Any * Goal)] [xs : (Listof Any)]))
(struct Goal== ([x : Any] [y : Any]))
(struct Goal=/= ([x : Any] [y : Any]))
(define-type GoalApply (U GoalApplyProcedure Goal== Goal=/=))
(define-type Goal (U GoalApply GoalFresh GoalDisj GoalConj))
(struct GoalFresh ([f : (-> Var Goal)]))
(struct GoalDisj ([x : Goal] [y : Goal]))
(struct GoalConj ([x : Goal] [y : Goal]))
(define-type (Stream A) (U Null (Promise (Stream A)) (Pairof A (Stream A))))
(define-type =/=s (Listof (Listof (Pairof Var Any))))

(define-type Value (U Var Symbol String Char Number Null (Pairof Value Value) (Promise Value)))

(define-syntax goal
  (syntax-rules (== =/= conde all)
    [(_ (all x ...)) (all x ...)]
    [(_ (conde x ...)) (conde x ...)]
    [(_ (== x y)) (Goal== x y)]
    [(_ (=/= x y)) (Goal=/= x y)]
    [(_ (f x ...)) (GoalApplyProcedure f (list x ...))]))
(define-syntax all
  (syntax-rules ()
    [(_ x) (goal x)]
    [(_ x0 x ...) (GoalConj (goal x0) (all x ...))]))
(define-syntax conde
  (syntax-rules ()
    [(_ [x ...]) (all x ...)]
    [(_ [x ...] xs ...) (GoalDisj (all x ...) (conde xs ...))]))
(define-syntax define-relation
  (syntax-rules ()
    [(_ (f arg ...) body ...) (define (f arg ...) (all body ...))]))

(: goal->stream (-> Goal (Stream (Pairof Any =/=s))))
(define (goal->stream g) (goal->stream g)) ; WIP
(: step (-> State (Pairof (Listof (Pairof Any =/=s)) State)))
(define (step s) (step s)) ; WIP

; TypeRacket的问题 BEGIN
(: run-goal (-> Count Goal (Listof (List Count (Listof Goal==) (Listof Goal=/=) (Listof GoalApplyProcedure)))))
(define (run-goal c g)
  (match g
    [(GoalFresh f) (run-goal (count-next c) (f (Var c)))]
    [(and g (Goal== _ _)) (list (list c (list g) '() '()))]
    [(and g (Goal=/= _ _)) (list (list c '() (list g) '()))]
    [(and g (GoalApplyProcedure _ _)) (list (list c '() '() (list g)))]
    [(GoalDisj x y) (append (run-goal c x) (run-goal c y))]
    [(GoalConj x y) (%run-goals%B (run-goal c x)
                                  (c ==s1 =/=s1 apps1)
                                  (%run-goals%B (run-goal c y)
                                                (c ==s2 =/=s2 apps2)
                                                (list (list c (append ==s1 ==s2) (append =/=s1 =/=s2) (append apps1 apps2)))))]))
(: %run-goals%mat% (All (A) (-> Count (Listof Goal==) (Listof Goal=/=) (Listof GoalApplyProcedure) A) ->
                        (-> (List Count (Listof Goal==) (Listof Goal=/=) (Listof GoalApplyProcedure)) A)))
(define (%run-goals%mat% f)
  (λ (x) (apply f x)))
(: %run-goals%bind% (All (A) (Listof (List Count (Listof Goal==) (Listof Goal=/=) (Listof GoalApplyProcedure)))
                         (-> Count (Listof Goal==) (Listof Goal=/=) (Listof GoalApplyProcedure) (Listof A))
                         -> (Listof A)))
(define (%run-goals%bind% xs f) (apply append (map (%run-goals%mat% f) xs)))
(define-syntax-rule (%run-goals%B xs (c ==s =/=s apps) v)
  (%run-goals%bind% xs (λ ([c : Count] [==s : (Listof Goal==)] [=/=s : (Listof Goal=/=)] [apps : (Listof GoalApplyProcedure)]) v)))
; END
(define fold foldl) ; or foldr
(: run-goals (-> Count (Listof Goal) (Listof (List Count (Listof Goal==) (Listof Goal=/=) (Listof GoalApplyProcedure)))))
(define (run-goals c gs)
  (if (null? gs)
      (list (list c '() '() '()))
      (run-goal c (fold GoalConj (car gs) (cdr gs)))))

(: unify0 (-> (Listof (Pairof Value Value)) (Immutable-HashTable Var Value) Value Value
              (U False (Pairof (Immutable-HashTable Var Value) (Listof (Pairof Value Value))))))
(: %unify%history%mem? (-> (Listof (Pairof Value Value)) Value Value Boolean))
(define (%unify%history%mem? set x y)
  (and (pair? set)
       (let ([a (car set)] [d (cdr set)])
         (let ([v1 (car a)] [v2 (cdr a)])
           (cond
             [(eq? v1 x) (eq? v2 y)]
             [(eq? v1 y) (eq? v2 x)]
             [else #f])))))
(define (unify0 history c x y)
  (if (%unify%history%mem? history x y)
      (cons c history)
      (let ([history (cons (cons x y) history)])
        (match* ((hash-ref c x (λ () x)) (hash-ref c y (λ () y)))
          [((and x (Var _)) y) (cons (hash-set c x y) history)]
          [(x (and y (Var _))) (cons (hash-set c y x) history)]
          [((cons xa xd) (cons ya yd))
           (let ([t (unify0 history c xa xd)])
             (and t (let ([c (car t)] [history (cdr t)])
                      (unify0 history c xd yd))))]
          [((? promise? x) y) (unify0 history c (force x) y)] ; BUG? (define _ (delay _))
          [(x (? promise? y)) (unify0 history c x (force y))] ; BUG? (define _ (delay _))
          [(x y) (and (equal? x y) (cons c history))]))))
    
