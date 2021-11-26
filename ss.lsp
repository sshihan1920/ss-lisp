; REPL
(defun test (f args)
  "Evaluate the function whose name is the symbol F on list of arguments
ARGS and print an informative message."
  (format t "~S => ~S~%" (cons f args) (apply f args)))

(defun repl (f)
  "Run the function whose name is the symbol F in a Read-Eval-Print Loop."
  (format t "Testing ~A~%" (symbol-name f))
  (repl1 f))

(defun repl1 (f)
  (format t "Enter arguments for ~A (enter a list, or ! to stop): " (symbol-name f))
  (finish-output t)
  (repl2 f (read)))

(defun repl2 (f args)
  (cond
    ((eq args '!)
     t)
    (t
     (test f args)
     (repl1 f))))
;-----------------------------------------------------------------------
;;LIST FUNCTIONS
;;;; 1. Append two lists
;;;;    (append '(1 3 x a) '(4 2 b)) => (1 3 x a 4 2 b)

(defun append (x y)
    (appendhelper x y)
)

(defun appendhelper (x y)
    (if (null x)
        y
        (cons (car x) (appendhelper (cdr x) y))
    )
)

;;;; 2. Reverse a lists
;;;;    (reverse '(a b c d)) => (d c b a)

(defun reverse (x)
    (reversehelper x)
)

(defun reversehelper(x)
    (if (null x)
        x
        (appendhelper (reversehelper (cdr x)) (list (car x)))
    )
)

;;;; 3. Map a function over every element in a list (this is called mapcar in Lisp)
;;;;    (defun add3 (x) (+ 3 x))
;;;;    (map 'add3 '(1 2 3 4)) => (4 5 6 7)

(defun add3 (x)
    (+ 3 x)
)

(defun map (x y) ;;not limited to 'add3
    (if y
        (cons (funcall x (car y)) (map x (cdr y)))
        nil
    )
)

;;;; 4. Remove duplicates from a list
;;;;    (nub '(1 1 2 4 1 2 5)) => (1 2 4 5)

(defun nub(x)
    (reverse (nubhelper x nil))
)

(defun nubhelper(x y)
    (if (null x)
        y
        (if (member (car x) y)
            (nubhelper (cdr x) y)
            (nubhelper (cdr x) (cons (car x) y))
        )
    )
)

;;;; 5. Fold-left (arguments are: initial value, function, list)
;;;;    (fold 10 '- '(1 3 2)) => 4

(defun fold (val func list)
    (if (null list)
        list
        (funcall func val (apply '+ list))
    )
)

;;;; 6. Filter
;;;;    (defun lessthan3 (x) (< x 3))
;;;;    (filter 'lessthan3 '(1 4 5 2 1 6)) => (1 2 1)

(defun lessthan3 (x) 
    (< x 3)
)

(defun filter (x y) ;;not limited to 'lessthan3
    (filterhelper x y '())
)

(defun filterhelper (x y z)
    (if y
        (if (eql (funcall x (car y)) t)
            (cons (car y) (filterhelper x (cdr y) z))
            (filterhelper x (cdr y) z)
        )
        z
    )
)

;;;; 7. Merge two sorted lists
;;;;    (merge '(1 3 4 7) '(2 3 6)) => (1 2 3 3 4 6 7)

(defun merge (x y)
    (mergehelper x y '())
)

(defun mergehelper (x y z)
    (if x 
        (if y
            (if (> (car x) (car y))
                (cons (car y) (mergehelper x (cdr y) z))
                (cons (car x) (mergehelper (cdr x) y z))
            )
            (cons (car x) (mergehelper (cdr x) y z))
        )
        (if y
            (cons (car y) (mergehelper x (cdr y) z))
            z
        )
    )
)

;;;; 8. Add an element to the end of a list. Cool hint: try using reverse
;;;;    (addtoend 'd '(a b c)) => (a b c d)

(defun addtoend (x y)
    (reverse (cons x (reverse y)))
)

;;;; 9. Index of
;;;;    (indexof 'a '(b c a d)) => 2
;;;;    (indexof 'a '(b c d f)) => -1
(defun indexof (x y) 
    (indexofhelper x y 0)
)

(defun indexofhelper (x y z)
    (if y
        (if (eql (car y) x)
            z
            (indexofhelper x (cdr y) (+ z 1))
        )
        -1
    )
)

;;;; 10. Remove-all
;;;;    (remove-all 'a '(b a c a a d a)) => (b c d)

(defun remove-all (x y)
    (if (null y)
        y
        (if (eq x (car y))
            (remove-all x (cdr y))
            (cons (car y) (remove-all x (cdr y)))
        )
    )
)
;-----------------------------------------------------------------------
;;SET FUNCTIONS
;1
(defun member(x y)
    (if y   
        (if (eql x (car y)) 
            T        
            (member x (cdr y))
        )
        nil
    )
)
;2
(defun insert (x y) 
    (if (eql (member x y) t)
        y
        (cons x y)
    )

)
;3
(defun intersection (x y) 
    (if x
        (if (eql (member (car x) y) t)
            (cons (car x) (intersection (cdr x) y))
            (intersection (cdr x) y)
        )
    )
)
;4
(defun union (x y)
    (if x
        (union (cdr x) (insert (car x) y))
	y
    )
)
;5
(defun difference (x y) 
    (if x
        (if (eql (member (car x) y) t)
            (difference (cdr x) y)
            (cons (car x) (difference (cdr x) y))
            
        )
    )
)
;6
(defun symdiff (x y)
    (union (difference x y) (difference y x))
)
;7
(defun subsetp (x y)
    (if x
        (if (eql (difference x y) nil)
            T
            nil
        )
        nil
    )
)
;8
(defun supersetp (x y)
    (if x
        (if (eql (difference y x) nil)
            T
            nil
        )
        nil
    )
)
;9
(defun cardinality (x)
    (if (car x)
        (+ (cardinality (cdr x)) 1)
        0
    )
)
;10
(defun powersethelper(x y)
    (if y
        (cons (cons x (car y)) (powersethelper x (cdr y)))
        nil
    )
)
(defun powerset(x)
    (if x
        (append (powerset (cdr x)) (powersethelper (car x) (powerset (cdr x))) )
        '(())
    )
)

;-----------------------------------------------------------------------
MATH FUNCTIONS
(defun mod (x y)
;; do I need to check if x or y is bigger?
	(if (eq 0 x)
		0 ;; returns 0 if no remainder
	(if (< (- x y) y)
		(- x y)	; returns difference between x and y when difference < y
		(mod (- x y) y)
))
)

(defun abs (x) 	
	(if (>= x 0) 
		x ;returns x val if positive
		(- x) ;returns - x val if negative
))

(defun factorial (x)
	(if (or 
		(eq 0 x) 
		(eq 1 x)
		)
		1 ;returns 1 if 0! or 1!
		(* x(factorial(- x 1))) ;returns x!
))

(defun right-tri(a b c)
	(if (or 
		(eq (+(* a a) (* b b)) (* c c)) ;a^2 + b^2 = c^2;
		(eq (+(* b b) (* c c)) (* a a)) ;b^2 + c^2 = a^2;
 		(eq (+(* c c) (* a a)) (* b b)) ;c^2 +a^2 = b^2;
		) 
		t ;returns true if abc are a right triangle combination
  		nil ; returns false if abc are not
)
)

(defun gcd (x y)
	(if (eq x y)
		x ;returns if x = y
	(if (eq 0 x) 
		y ;returns x/y if either opp val is 0
	(if (eq 0 y)
		x ;returns x/y if either opp val is 0
		(if (> x y)
			(gcd y (mod x y)) ;Euclidean Algorithm
		(if (> y x)
			(gcd x (mod y x)) ;Euclidean Algorithm
)))
))
)

(defun lcm (x y)
	(/ (* x y) (gcd x y));Uses product of two #'s and divides by GCD
)

(defun nth-fibo (x)
	(if ;;(or 
		(eq 0 x) 
		;;(eq 1 x)
		;;)
		0
		(if (eq 1 x)
			1
		(+ (nth-fibo (- x 1)) (nth-fibo (- x 2)));; an = an-2 + an-1
	))
)

(defun isprime (x y)
	(if (eq y 1)
		t
		(if (eq (gcd x y) 1)
			(isprime x (- y 1))
			nil
))
)

(defun primep (x)
	(if (eq 1 x)
		t
		(isprime x (- x 1))
	;;(if (> (gcdIter x x) 1)		
		;;nil
		;;t
		;;(primep (- x 1))	
)
)

(defun nth-prime-help (x y z)
    (if (= y (+ z 1))
        (- x 1)
        (if (eql (primep x) t)
            (nth-prime-help (+ x 1) (+ y 1) z)
            (nth-prime-help (+ x 1) y z)
        )
    )
)

(defun nth-prime (x)
    (nth-prime-help 0 0 x)
)

;-----------------------------------------------------------------------
REQUIRED FUNCTIONS
;1
(defun trihelp (x y z) 
    (if (= y (+ z 1))
        x
        (trihelp (+ x y) (+ y 1) z)
    )
)
(defun tri (x) 
    (trihelp 0 1 x)
)
;2
(defun triphelp (x y z) 
    (if (= y (+ z 1))
        nil
        (if (= x z)
            t
            (triphelp (+ x y) (+ y 1) z)
        )
        
    )
)
(defun trip (x) 
    (triphelp 0 1 x)
)
;3
(defun insert (x y) 
    (if (eql (member x y) t)
        y
        (cons x y)
    )

)

(defun maphelper (x y)
    (if y
        (cons (addnum x (car y)) (maphelper x (cdr y)))
        nil
    )
)

(defun addnum (x y) (+ x y))

(defun tritrihelp (x y z a)
    (if (> (+ x y) z)
        nil
        (if (eql (member z (maphelper (+ x y) a)) T)
            T
            (tritrihelp (+ x y) (+ y 1) z a)
        )
    )
)
(defun listoftrinums (x y z a) 
    (if (> (+ x y) z)
        a
        (listoftrinums (+ x y) (+ y 1) z (cons (+ x y) a))
    )
)

(defun tritri (x)
    (if (eql t (tritrihelp 0 1 x (listoftrinums 0 1 x '())))
        t
        nil
    )
)
;-----------------------------------------------------------------------

;;EXAMPLES
(test 'append ((1 3 X A) (2 4 B)))

;;TESTING LIST FUNCTIONS
(format t "~%")
(format t "Now Testing LIST Functions~%")
(repl' append)
(repl' reverse)
(repl' map)
(repl' nub)
(repl' fold)
(repl' filter)
(repl' merge)
(repl' addtoend)
(repl' indexof)
(repl' remove-all)

;;TESTING SET FUNCTIONS
(format t "~%")
(format t "Now Testing SET Functions~%")
(repl' member)
(repl' insert)
(repl' intersection)
(repl' union)
(repl' difference)
(repl' symdiff)
(repl' subsetp)
(repl' supersetp)
(repl' cardinality)
(repl' powerset)

;;TESTING MATH FUNCTIONS
(format t "~%")
(format t "Now Testing MATH Functions~%")
(repl' abs)
(repl' factorial)
(repl' right-tri)
(repl' gcd)
(repl' lcm)
(repl' nth-fibo)
(repl' primep)
(repl' nth-prime)

;;TESTING REQUIRED FUNCTIONS
(format t "~%")
(format t "Now Testing REQUIRED Functions~%")
(repl' tri)
(repl' trip)
(repl' tritri)
