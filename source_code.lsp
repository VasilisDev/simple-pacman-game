;*********************************************************
;  @authors:Vasileios Tsakiris  (151082),
;            Ioannis Mitropoulos(151015).
;  @lab :B4 / friday 11:00 - 14:00.
;
;**********************************************************

; ----------------------------------------------------------------------------
; **** starting search
; **** έναρξη αναζήτησης

(defun searchProblem (start-state goal method )
  (print '____BEGIN_SEARCHING_____ )
   (findSolution
          (MakeFront start-state)
          (MakeQueue start-state)
          ()
          goal
          method))

; ----------------------------------------------------------------------------
; **** Basic recursive function to create search tree (recursive tree expantion)
; **** Βασική αναδρομική συνάρτηση για δημιουργία δέντρου αναζήτησης (αναδρομική επέκταση δέντρου)

(defun FindSolution (front queue closed goal method )
    (cond
      ((null front) 'no_solution)
      ((mymember (car front) closed)
          (FindSolution (cdr front) (cdr queue) closed  goal method  ))
      ((equal (car front) goal) (print "Goal found: ") (reverse (first queue)))
      (T (FindSolution
           (ExpandFront front method goal)
           (ExtendQueue queue method goal)
           (updateClosed (car front)  closed)
                              goal
                              method))))


(defun updateClosed (state closed)
"push the current state in closed"
    (push state closed))

; ----------------------------------------------------------------------------
; **** FRONT
; **** Διαχείριση Μετώπου
; ----------------------------------------------------------------------------
; ** initialization of front
; ** Αρχικοποίηση Μετώπου

(defun MakeFront (node)
     (cons node ())) ;put the starting state into the empty front


; ----------------------------------------------------------------------------
; **** expanding front
; **** επέκταση μετώπου

(defun ExpandFront (front method goal)
  (cond
     ((eq method 'DFS)
          (append(removeNils (findchildren (car front))) (cdr front)))
     ((eq method 'BFS)
          (prepend (removeNils (findchildren (car front))) (cdr front)))
    ((eq method 'BestFS)
       ;set first node of front as parent
       ;call the findchildren (descendant function) in order to find children and put them in front of the front
       ;and after put the the parent node in closed.
       ;After that sort the front in descending order
       ;and pass the new front to lambda expression which compare
       ;the current state with the goal state (compare is the heuristic method).
       ;and returns the distance of current state from the goal.
       ;So we compare our front according to the distance of current from the goal!
         (sort (append (removeNils (findchildren (car front))) (cdr front)) #'> :key  #'(lambda(x)(compare x goal)) ))
    (T "other methods to be added")))


(defun prepend (x y)
  "Prepend y to start of x"
       (append y x))


; ----------------------------------------------------------------------------
; **** QUEUE
; **** Διαχείριση ουράς

; ----------------------------------------------------------------------------
; ** initialization of queue
; ** Αρχικοποίηση ουράς

(defun MakeQueue (node)
    (list (list node)))

; ----------------------------------------------------------------------------
; **** expanding queue
; **** επέκταση ουράς


(defun ExtendQueue (queue method goal)
   (cond
       ((eq method 'DFS)
          (append  (growPath (car queue)) (rest queue)))
       ((eq method 'BFS)
          (prepend (growPath (car queue)) (rest queue)))
       ((eq method 'BestFS)
          (sort (append (growPath (car queue)) (rest queue)) #'> :key #'(lambda(x)(compare (first x) goal))))
       (T "other methods to be added")))



(defun compare (state goal)
"this is the heuristic method for BestFS
 for each element of sorted list ,which is contain the new tree
 compare the state either of queue or front and if one of them  are equal with the goal method
 increment by one and return this number to the reduce function in order to sum the final
 point of pacman in the list"
    (reduce #'+ (mapcar #'(lambda(x y) (if (equal x y) 1 0 )) state goal)))

; ----------------------------------------------------------------------------
; **** growing path towards each different child of the selected parent node
; **** επεκταση μονοπατιου προς καθε διαφορετικό παιδί-κόμβο από τον επιλεγμένο γονέα-κόμβο


(defun growPath (path)
      (mapcar #'(lambda(fn) (cons fn path)) (removeNils (findChildren (car path)))))

; ----------------------------------------------------------------------------
; **** Supportive functions
; **** Υποστηρικτικές συναρτήσεις


(defun myMember(x y)
"custom myMember function if a list contains an atom"
      (some #'(lambda(fn)(equal x fn)) y))


(defun removeNils (list)
"predicate that remove nils from a list"
        (remove-if-not 'identity list))


(defun removecycles (paths)
     (cond
       ((null paths) nil)
       ((member (caar paths) (cdar paths)) (removecycles (cdr paths)))
       (T (cons (car paths) (removecycles (cdr paths))))))

(defun isVeryLeft (state)
 "predicate that checks if packman is in the left most cell"
 (EQ 'p (CAAR state)))


(defun isVeryRight (state)
"predicate that checks if packman is in the right most cell"
 (isVeryLeft (REVERSE state)))



(defun canIeat (state)
"recursive predicate that checks if the current state satisfies our condition.
 Our goal is pacman can eat either one or more fruits in one or more cells"
 (COND
   ((NULL state) NIL)
   ((findPacmanWithFruits state) T) ; βρέθηκε και επιστρέφει TRUE.
   (T (canIeat (CDR state))))) ; συνεχίζει η αναδρομική αναζήτηση



(defun MoveRight(state)
"operator that moves pacman on the right side if and if there is free space"
 (COND
    ((isVeryRight state) NIL) ; μη ικανοποίηση της προϋποθέσεως του τελεστή
    (T (MoveRight_Recursion state))))


(defun MoveRight_Recursion (state)
"recursive function that makes the new list for our problem space"
(COND
 ((EQ 'p (CAAR state)) ; εάν βρέθηκε ο pacman,
    (CONS (CDAR state) ; τότε αφαιρείται και κολλάει
      (CONS (CONS 'p (CADR state)) ; στην επόμενη υπολίστα
        (CDDR state)))) ; και κολλώντας τα υπόλοιπα (CDDR) όπως έχουν
 (T (CONS (CAR state) ; αν δεν βρέθηκε, τότε κολλάει την κεφαλή όπως είναι, με
      (MoveRight_Recursion (CDR state)))))) ; το αποτέλεσμα της αναδρομής του υπολοίπου


(defun moveLeft (state)
"operator that moves pacman on the left side if and if there is free space"
 (REVERSE (moveRight (REVERSE state))))


(defun eat (state) ; γενική συνάρτηση φαγώματος
"operator that eats the fruit if and only if satifies our canIeat predicator"
 (COND
   ((canIeat state) (Eat_Recursion state)) ; εάν μπορεί να φάει, τότε τρώει
   (T NIL))) ; εάν όχι, τότε επιστρέφει NIL


(defun findPacmanWithFruits(state)
"function that finds the pacman in cell with one or more fruits {f}*."
  (AND (EQUAL (CAAR state) 'p)
       (EQUAL (CADAR state) 'f)))

(defun Eat_Recursion (state) ; αναδρομική συνάρτηση φαγώματος
 (COND
   ((NULL state) NIL) ; εάν έφτασε στο τέλος, τότε NIL
     ((findPacmanWithFruits state )  ; εάν βρέθηκε ο pacman με το φρούτο,
     (CONS '(p) ; τότε το αφαιρεί και το κολλάει (με χρήση CONS)
      (CDR state))) ; στην ουρά ως έχει
   (T ; εάν όχι,
     (CONS (CAR state) ; τότε συνεχίζει και κολλάει την κεφαλή ως έχει με
      (Eat_Recursion (CDR state)))))) ; το αποτέλεσμα της αναδρομής του υπολοίπου


(defun findChildren (state)
"descendant function.
 Here we put our operators with a specific order in to create right our tree."
 (LIST
   (eat state)
   (moveLeft state)
   (moveRight state)))

; (eat '( (p) () (f) (f) ) )
; (eat '( () () (p f) (f) ) )
;  (eat '( (p f) () (f) (f) ) )

; (moveleft '( (p) () (f) (f) ) )
; (moveleft '( () () (p f) (f) ) )
; (moveleft '( (p f) () (f) (f) ) )
