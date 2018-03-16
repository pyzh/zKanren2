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
(: run-goal (-> Count Goal (Listof (List Count (Listof Goal==) (Listof Goal=/=) (Listof GoalApplyProcedure)))))
(define (run-goal c g)
  (match g
    [(GoalFresh f) (run-goal (count-next c) (f (Var c)))]
    [(and g (Goal== _ _)) (list (list c (list g) (list) (list)))]
    [(and g (Goal=/= _ _)) (list (list c (list) (list g) (list)))]
    [(and g (GoalApplyProcedure _ _)) (list (list c (list) (list) (list g)))]
    [(GoalDisj x y) (run-goal c g)] ; WIP
    [(GoalConj x y) (run-goal c g)]))