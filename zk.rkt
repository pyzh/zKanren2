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
(struct Var ([v : Positive-Integer]))
(struct State ([=/=s : (Listof (Listof (Pairof Var Any)))]
               [count : Positive-Integer]
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
