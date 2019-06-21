# jim-emacs-fun对比学习专家系统: Reasoned Schemer & clojure.core.logic & Prolog & Datalog & Datomic & Neo4J

* https://github.com/clojure/core.logic/wiki/A-Core.logic-Primer

```clj
 (run* [logic-variable]
   &logic-expressions)
 (run* [q]
   (membero q [1 2 3])
   (membero q [2 3 4]))
 (run* [query-variable] ...)
 (fresh [a b c] &logic-expressions)
 (run* [q]
   (constraint-1)
   (constraint-2)
   (constraint-3))
 (== lvar1 lvar2)
 (run* [q]
   (== q 1))
 (run* [q]
   (== q {:a 1 :b 2}))
 (run* [q]
   (== {:a q :b 2} {:a 1 :b 2}))
 (run* [q]
   (== 1 q))
 (run* [q]
   (== q '(1 2 3)))
```

* https://github.com/clojure/core.unify

```clj
    (use 'clojure.core.unify)

    (unifier '((?a * ?x ** 2) + (?b * ?x) + ?c) 
         '(?z + (4 * 5) + 3))

    ;=> ((?a * 5 ** 2) + (4 * 5) + 3)	
```

* https://github.com/clojure/core.logic/wiki/Differences-from-The-Reasoned-Schemer

```clj
(run #f (r)
  (fresh (x y)
    (== (cons x (cons y 'salad)) r)))
(run* [r]
  (fresh [x y]
    (== (lcons x (lcons y 'salad)) r)))
(run 1 (x)
  (listo `(a b c . ,x)))
(run 1 [x]
  (listo (llist 'a 'b 'c x)))
(run* [x]
   (listo (list 'a 'b x 'd)))
```

* https://github.com/clojure/core.logic/wiki/Using-core.logic-with-ClojureScript

```clj
(ns example
  (:require [cljs.core.logic :as m :refer [membero]]))
(m/run* [q]
  (membero q '(:cat :dog :bird :bat :debra)))
```

* https://github.com/clojure/core.logic/wiki/Extending-core.logic-%28Datomic-example%29

```clj
(ns example
  (:use [datomic.api :only [db q] :as d]
        [clojure.core.logic]
        [clojure.core.logic.datomic :only [datom?]]))

(def uri "datomic:dev://localhost:4334/test")
(def conn (d/connect uri))

   (extend-type datomic.Datom
     clojure.core.logic.protocols/IUnifyTerms
     (unify-terms [u v s]
       (unify-with-datom* u v s)))
   (defn unify-with-datom* [u v s]
     (when (and (instance? clojure.lang.PersistentVector v) (> (count v) 1))
       (loop [i 0 v v s s]
         (if (empty? v)
           s
           (when-let [s (unify s (first v) (nth u i))]
             (recur (inc i) (next v) s))))))
   (extend-type clojure.lang.PersistentVector
     clojure.core.logic.protocols/IUnifyTerms
     (unify-terms [u v s]
       (if (datom? v)
         (unify-with-datom* v u s)
         (when (sequential? v)
           (unify-with-sequential* u v s)))))
(defn datomic-rel [q]
  (fn [a]
    (l/to-stream
      (map #(l/unify a % q) (d/datoms (db conn) :eavt)))))
(run* [q]
  (fresh [e a v t]
    (== v true)
    (datomic-rel [e a v t])
    (== q [e a v t])))
```
#### https://github.com/clojure/core.logic/wiki/Projects-using-core.logic

* [parsing-packet-with-logic](http://github.com/tgk/parsing-packet-with-logic) Erlang style packet pattern matching
* [maze.logic](http://github.com/bloat/maze.logic), solving mazes with core.logic 
* [TAPL in miniKanren](http://github.com/namin/TAPL-in-miniKanren-cKanren-core.logic), exploring type theory
* [leanTAP](http://github.com/namin/leanTAP), an implementation of alphaLeanTAP, a theorem prover
* [symbol](http://github.com/timowest/symbol), a typed lisp to C++ compiler for audio DSP software
* [kibit](http://github.com/jonase/kibit), static analysis of Clojure programs
* [webdriver-logic](http://github.com/semperos/webdriver-logic), automated browser testing
* [monotony](http://github.com/aredington/monotony), reasoning about time
* [piplin](http://github.com/dgrnbrg/piplin), hardware description language
* [guzheng](http://github.com/dgrnbrg/guzheng), Clojure code coverage tool
* [damp.qwal](http://github.com/ReinoutStevens/damp.qwal), regular path expressions
* [damp.ekeko](http://github.com/cderoove/damp.ekeko), query the Java AST in Eclipse
* [funnyqt](https://github.com/jgralab/funnyqt), a model querying and transformation library
* [topoged](https://github.com/m0smith/topoged), querying genealogical information

* https://github.com/clojure/core.logic/wiki/Translations-from-prolog

```clj
(defn knight-moves 
"Returns the available moves for a knight (on a 8x8 grid) given its current position." 
 [x y]
(let [xmax 8 ymax 8]
 (run* [q] ;bring back all possible solutions
 (fresh [a b] ;like 'let' but for logic variables
  (conde ;;like OR
    [(< (+ x 1) xmax) (< (+ y 2) ymax) (== a (+ x 1)) (== b (+ y 2))] ;1st possibility
    [(< (+ x 2) xmax) (< (+ y 1) ymax) (== a (+ x 2)) (== b (+ y 1))] ;2nd possibility
    [(< (+ x 2) xmax) (>= (- y 1)   0) (== a (+ x 2)) (== b (- y 1))] ;3rd possibility
    [(< (+ x 1) xmax) (>= (- y 2)   0) (== a (+ x 1)) (== b (- y 2))] ;4th possibility
    [(>= (- x 1)   0) (>= (- y 2)   0) (== a (- x 1)) (== b (- y 2))] ;5th possibility
    [(>= (- x 2)   0) (>= (- y 1)   0) (== a (- x 2)) (== b (- y 1))] ;6th possibility
    [(>= (- x 2)   0) (< (+ y 1) ymax) (== a (- x 2)) (== b (+ y 1))] ;7th possibility
    [(>= (- x 1)   0) (< (+ y 2) ymax) (== a (- x 1)) (== b (+ y 2))] ;8th possibility
  ) 
   (== q [a b]))))) ;return each solution in a vector [x, y]

 [(< (+ x 1) xmax) 
  (< (+ y 2) ymax) 
  (== a (+ x 1)) 
  (== b (+ y 2))]

(ns XXXXX.xxx
       (:refer-clojure :exclude [== >= <= > < =])
       (:use clojure.core.logic 
             clojure.core.logic.arithmetic))

(defn king-moves 
"Returns the available moves for a king (on a 8x8 grid) given its current position."
[x y]
(let [xmax 8 ymax 8]
 (run* [q]
 (fresh [a b]
  (conde 
    [(< (+ x 1) xmax) (< (+ y 1) ymax) (== a (+ x 1)) (== b (+ y 1))] ;1st possibility (diagonally)
    [(>= (- x 1) 0) (>= (- y 1) 0) (== a (- x 1)) (== b (- y 1))]     ;2nd possibility (diagonally)
    [(< (+ y 1) ymax) (== a x) (== b (+ y 1))]                        ;3rd possibility (x is constant)
    [(>= (- y 1) 0) (== a x) (== b (- y 1))]                          ;4th possibility (x is constant)
    [(>= (- x 1) 0) (== b y) (== a (- x 1))]                          ;5th possibility (y is constant)
    [(< (+ x 1) xmax) (== b y) (== a (+ x 1))]                        ;6th possibility (y is constant)
    [(< (+ x 1) xmax) (> (- y 1) 0) (== a (+ x 1)) (== b (- y 1))]    ;7th possibility (diagonally)
    [(>= (- x 1) 0) (< (+ y 1) ymax) (== a (- x 1)) (== b (+ y 1))]   ;8th possibility (diagonally)
  ) 
   (== q [a b]))))) ;return each solution in a vector [x, y]

(def ^:const board (vec (range 8)))

(defn rook-moves 
"Returns the available moves for a rook (on a 8x8 grid) given its current position."
[x y]
 (run* [q]
 (fresh [a b]
 (conde 
  [(membero a board) (!= a x) (== b y)]  ;y remains constant
  [(membero b board) (!= b y) (== a x)]) ;x remains constant
     (== q [a b]))))

(defn bishop-moves 
"Returns the available moves for a bishop (on a 8x8 grid) given its current position and direction."
[x y]
(run* [q] 
(fresh [a b] 
  (membero a board) 
  (membero b board)
   (!= a x) 
   (!= b y)
    (project [x y a b]
    (== (Math/abs (- x a)) 
        (Math/abs (- y b)))
           (== q [a b])))))
```

```prolog
move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X + 1 < Xmax,
Y + 2 < Ymax,
A is X + 1,
B is Y + 2. ;;---------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X + 2 < Xmax,
Y + 1 < Ymax,
A is X + 2,
B is Y + 1.;;------------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X + 2 < Xmax,
Y - 1 >= 0,
A is X + 2,
B is Y - 1.;;------------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X + 1 < Xmax,
Y - 2 >= 0,
A is X + 1,
B is Y - 2. ;--------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X - 1 >= 0,
Y - 2 >= 0,
A is X - 1,
B is Y - 2. ;;------------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X - 2 >= 0,
Y - 1 >= 0,
A is X - 2,
B is Y - 1. ;;--------------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X - 2 >= 0,
Y + 1 < Ymax,
A is X - 2,
B is Y + 1. ;;--------------------

move([X, Y, Xmax, Ymax], [A, B, Xmax, Ymax]) :-
X - 1 >= 0,
Y + 2 < Ymax,
A is X - 1,
B is Y + 2.

;; ----------------
X + 1 < Xmax,
Y + 2 < Ymax,
A is X + 1,
B is Y + 2.
```
* https://github.com/clojure/core.logic/wiki/References

A non-exhaustive list of influences and references:

* [alphaKanren](http://www.cs.indiana.edu/~webyrd/alphamk/alphamk.pdf)
* [Nominal Logic Programming](http://arxiv.org/abs/cs/0609062)
* [Finite Domain Constraint Programming in Oz. A Tutorial](http://www.mozart-oz.org/documentation/fdt/)
* [cKanren](http://scheme2011.ucombinator.org/papers/Alvis2011.pdf)
* [Efficient Constraint Propagation Engines](http://www.gecode.org/paper.html?id=SchulteStuckey:TOPLAS:2008)
* [Techniques for Efficient Constraint Propagation](http://www.gecode.org/paper.html?id=Lagerkvist:Lic:Diss:2008)
* [Operations Research Tools developed at Google](http://code.google.com/p/or-tools/)
* [logilab-constraint](http://hg.logilab.org/logilab/constraint)
* [Solving Every Sudoku Puzzle](http://norvig.com/sudoku.html)
* [Constraint Handling Rules](http://www.informatik.uni-ulm.de/pm/fileadmin/pm/home/fruehwirth/constraint-handling-rules-book.html)
* [The XSB System Version 3.2 - Volume 2: Libraries, Interfaces, and Packages](http://xsb.sourceforge.net/manual2/manual2.pdf), particularly the section on Attributed Variables
* [The XSB System Version 3.2 - Volume 1: Programmer's Manual](http://xsb.sourceforge.net/manual1/manual1.pdf)
* [Concepts, Technqiues, and Models of Computer Programming](http://www.info.ucl.ac.be/~pvr/book.html), Chapters 9 and 12
* [Art of the Propagator](http://dspace.mit.edu/handle/1721.1/44215)
* [Constraint Propagation - Models, Techniques, Implementation](http://people.cs.kuleuven.be/~guido.tack/dissertation.php)
* [Relational Programming in miniKanren: Techniques, Applications, and Implementations](http://pqdtopen.proquest.com/#abstract?dispub=3380156)
* [The Reasoned Schemer](http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=10663)
* [Efficient representations for triangular substitutions: A comparison in miniKanren](https://www.cs.indiana.edu/~lkuper/papers/walk.pdf)
* [A pattern matcher for miniKanren, or, how to get into trouble with CPS macros](http://www.cs.indiana.edu/~lkuper/papers/lambdae.pdf)
* [Kanren](http://kanren.sourceforge.net/)
* [Logical JVM: Implementing the miniKanren logic system in Scala](https://web.archive.org/web/20120607112047/http://hircus.multics.org/kanren/presentation.html)
* [minikanren-scala](https://github.com/hircus/minikanren-scala)
* [Purely Functional Data Strucutres](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.64.3080&rep=rep1&type=pdf)
* [Using Datalog with Binary Decision Diagrams for Program Analysis](http://people.csail.mit.edu/mcarbin/papers/aplas05.pdf)
* [Memoing for Logic Programs](http://portal.acm.org/citation.cfm?id=131299)
* [Efficient bottom-up abstract interpretation of prolog by means of constraint solving over symbolic finite domains](http://portal.acm.org/citation.cfm?id=692605)

### https://github.com/clojure/core.logic/wiki/Examples

```clj
;; A classic AI program:
(ns classic-ai-example
   (:refer-clojure :exclude [==])
   (:use clojure.core.logic))

(defne moveo [before action after]
  ([[:middle :onbox :middle :hasnot]
    :grasp
    [:middle :onbox :middle :has]])
  ([[pos :onfloor pos has]
    :climb
    [pos :onbox pos has]])
  ([[pos1 :onfloor pos1 has]
    :push
    [pos2 :onfloor pos2 has]])
  ([[pos1 :onfloor box has]
    :walk
    [pos2 :onfloor box has]]))

(defne cangeto [state out]
  ([[_ _ _ :has] true])
  ([_ _] (fresh [action next]
           (moveo state action next)
           (cangeto next out))))

(run 1 [q]
  (cangeto [:atdoor :onfloor :atwindow :hasnot] q)) ; (true)
;; Sudoku
(ns sudoku
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

(defn get-square [rows x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
    (get-in rows [x y])))

(defn init [vars hints]
  (if (seq vars)
    (let [hint (first hints)]
      (all
        (if-not (zero? hint)
          (== (first vars) hint)
          succeed)
        (init (next vars) (next hints))))
    succeed))

(defn sudokufd [hints]
  (let [vars (repeatedly 81 lvar)
        rows (->> vars (partition 9) (map vec) (into []))
        cols (apply map vector rows)
        sqs  (for [x (range 0 9 3)
                   y (range 0 9 3)]
               (get-square rows x y))]
    (run 1 [q]
      (== q vars)
      (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
      (init vars hints)
      (everyg fd/distinct rows)
      (everyg fd/distinct cols)
      (everyg fd/distinct sqs))))

(def hints
  [2 0 7 0 1 0 5 0 8
   0 0 0 6 7 8 0 0 0
   8 0 0 0 0 0 0 0 6
   0 7 0 9 0 6 0 5 0
   4 9 0 0 0 0 0 1 3
   0 3 0 4 0 1 0 2 0
   5 0 0 0 0 0 0 0 1
   0 0 0 2 9 4 0 0 0
   3 0 6 0 8 0 4 0 9])

(sudokufd hints)
;=>
; ((2 6 7 3 1 9 5 4 8
;   9 5 4 6 7 8 1 3 2 
;   8 1 3 5 4 2 7 9 6 
;   1 7 2 9 3 6 8 5 4
;   4 9 5 8 2 7 6 1 3
;   6 3 8 4 5 1 9 2 7
;   5 4 9 7 6 3 2 8 1
;   7 8 1 2 9 4 3 6 5
;   3 2 6 1 8 5 4 7 9))
;;; A type inferencer for the simply typed lambda calculus
(ns simple-typed-lambda-calculus
   (:refer-clojure :exclude [==])
   (:use clojure.core.logic))

(defna findo [x l o]
  ([_ [[y :- o] . _] _] 
    (project [x y] (== (= x y) true)))
  ([_ [_ . c] _] (findo x c o)))

(defn typedo [c x t]
  (conda
    [(lvaro x) (findo x c t)]
    [(matche [c x t]
       ([_ [[y] :>> a] [s :> t]]
          (fresh [l]
            (conso [y :- s] c l)
            (typedo l a t)))
       ([_ [:apply a b] _]
          (fresh [s]
            (typedo c a [s :> t])
            (typedo c b s))))]))

(comment
  ;; ([_.0 :> _.1])
  (run* [q]
    (fresh [f g a b t]
     (typedo [[f :- a] [g :- b]] [:apply f g] t)
     (== q a)))

  ;; ([:int :> _.0])
  (run* [q]
    (fresh [f g a t]
     (typedo [[f :- a] [g :- :int]] [:apply f g] t)
     (== q a)))

  ;; (:int)
  (run* [q]
    (fresh [f g a t]
     (typedo [[f :- [:int :> :float]] [g :- a]] 
       [:apply f g] t)
     (== q a)))

  ;; ()
  (run* [t]
    (fresh [f a b]
      (typedo [f :- a] [:apply f f] t)))

  ;; ([_.0 :> [[_.0 :> _.1] :> _.1]])
  (run* [t]
    (fresh [x y]
      (typedo [] 
        [[x] :>> [[y] :>> [:apply y x]]] t)))
  )
``` 
* https://github.com/clojure/core.logic/wiki/Features

```clj
(pldb/db-rel man p)
(pldb/db-rel woman p)
(pldb/db-rel likes p1 p2)
(pldb/db-rel fun p)

(def facts0
  (pldb/db
   [man 'Bob]
   [man 'John]
   [man 'Ricky]

   [woman 'Mary]
   [woman 'Martha]
   [woman 'Lucy]

   [likes 'Bob 'Mary]
   [likes 'John 'Martha]
   [likes 'Ricky 'Lucy]))

(def facts1 (-> facts0 (pldb/db-fact fun 'Lucy)))

(pldb/with-db facts1
  (run* [q]
    (fresh [x y]
      (fun y)
      (likes x y)
      (== q [x y])))) ; ([Ricky Lucy])
(pldb/db-rel likes ^:index p1 ^:index p2)
(unifier '(?x ?y ?z) '(1 2 ?y)) ; (1 2 _.0)
(run* [q]
  (!= q 1)) ; => ((_0 :- (!= _0 1)))
(run* [q]
  (fresh [x y]
    (!= [1 x] [y 2])
    (== q [x y])))
(run* [q]
  (fd/in q (fd/interval 1 5))) ; => (1 2 3 4 5)
(run* [q]
  (fresh [x y]
    (fd/in x y (fd/interval 1 10))
    (fd/+ x y 10)
    (== q [x y]))) ; => ([1 9] [2 8] [3 7] [4 6] [5 5] [6 4] [7 3] [8 2] [9 1])
(run* [q]
  (fresh [x y]
    (fd/in x y (fd/interval 0 9))
    (fd/eq
      (= (+ x y) 9)
      (= (+ (* x 2) (* y 4)) 24))
    (== q [x y])))
(run* [q]
  (fresh [x y]
    (fd/in x y (fd/interval 1 10))
    (fd/+ x y 10)
    (fd/distinct [x y])
    (== q [x y]))) ; => ([1 9] [2 8] [3 7] [4 6] [6 4] [7 3] [8 2] [9 1])
(defne arco [x y]
  ([:a :b])
  ([:b :a])
  ([:b :d]))

(def patho
  (tabled [x y]
    (conde
     [(arco x y)]
     [(fresh [z]
        (arco x z)
        (patho z y))])))

;; (:b :a :d)
(run* [q] (patho :a q))
(defn substo [e new a out]
  (conde
    [(== ['var a] e) (== new out)]
    [(fresh [y]
       (== ['var y] e)
       (== ['var y] out)
       (nom/hash a y))]
    [(fresh [rator ratorres rand randres]
       (== ['app rator rand] e)
       (== ['app ratorres randres] out)
       (substo rator new a ratorres)
       (substo rand new a randres))]
    [(fresh [body bodyres]
       (nom/fresh [c]
         (== ['lam (nom/tie c body)] e)
         (== ['lam (nom/tie c bodyres)] out)
         (nom/hash c a)
         (nom/hash c new)
         (substo body new a bodyres)))]))

(run* [q]
  (nom/fresh [a b]
    (substo ['lam (nom/tie a ['app ['var a] ['var b]])]
            ['var b] a q)))
;; => [['lam (nom/tie 'a_0 '(app (var a_0) (var a_1)))]]

(run* [q]
  (nom/fresh [a b]
    (substo ['lam (nom/tie a ['var b])]
            ['var a]
            b
            q)))
;; => [['lam (nom/tie 'a_0 '(var a_1))]]
(def-->e verb [v]
  ([[:v 'eats]] '[eats]))

(def-->e noun [n]
  ([[:n 'bat]] '[bat])
  ([[:n 'cat]] '[cat]))

(def-->e det [d]
  ([[:d 'the]] '[the])
  ([[:d 'a]] '[a]))

(def-->e noun-phrase [n]
  ([[:np d n]] (det d) (noun n)))

(def-->e verb-phrase [n]
  ([[:vp v np]] (verb v) (noun-phrase np)))

(def-->e sentence [s]
  ([[:s np vp]] (noun-phrase np) (verb-phrase vp)))

(run* [parse-tree]
  (sentence parse-tree '[the bat eats a cat] []))

;; ([:s [:np [:d the] [:n bat]] [:vp [:v eats] [:np [:d a] [:n cat]]]])
```
