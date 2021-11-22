;; Returns 'T' if lst is equal to goal state
;; otherwise returns NIL
(defun goal-state (lst)
(cond
((equal '(1 2 3 8 e 4 7 6 5) lst) T)
(T nil)
))

;; Returns the direction of a move
(defun get-direction (lst) (car lst))

;; Returns the state of the move
(defun get-state (lst) (cadr lst))

;; Returns T if both states are the same,
;; otherwise returns NIL
(defun same-state (lst1 lst2)
(cond
((equal (cadr lst1) (cadr lst2)) T)
(T nil)))

;; Returns the directional path take to go from
;; initial state to goal state as a list
(defun path (lst)
(cond
((equal (get-direction (car lst)) nil) (reverse (caar lst)))
(T (append (path (cdr lst)) (list (get-direction (car lst)))))
))


(defun remove-redundant (lst1 lst2)
(cond
((equal nil lst1) nil)
((same-state (car lst1) (car lst2)) (remove-redundant (cdr lst1) (cdr lst2)))
(T (cons (car lst1) (remove-redundant (cdr lst1) (cdr lst2))))
))

;; Returns a list of all legal moves that can
;; be done from state lst using helper functions

;; 2 helper functions
;; - swap

;; Determine where 'e' is
;; Integer mod for col
;; Use a swap helper method to make move
;;      - subst
;;      - pos

(defun moves (lst)
(cond
(nil lst)
(T (append-moves (position 'e lst) lst))))

;; append-moves helper method
(defun append-moves (x lst)
(cond
((= 0 x) (append (list (list 'R (swap 'e (nth 1 lst) lst))) (list (list 'D (swap 'e (nth 3 lst) lst)))))
((= 1 x) (append (list (list 'L (swap 'e (nth 0 lst) lst))) (list (list 'D (swap 'e (nth 4 lst) lst))) (list (list 'R (swap 'e (nth 2 lst) lst)))))
((= 2 x) (append (list (list 'L (swap 'e (nth 1 lst) lst))) (list (list 'D (swap 'e (nth 5 lst) lst)))))
((= 3 x) (append (list (list 'U (swap 'e (nth 0 lst) lst))) (list (list 'R (swap 'e (nth 4 lst) lst))) (list (list 'D (swap 'e (nth 6 lst) lst)))))
((= 4 x) (append (list (list 'U (swap 'e (nth 1 lst) lst))) (list (list 'R (swap 'e (nth 5 lst) lst))) (list (list 'D (swap 'e (nth 7 lst) lst))) (list (list 'L (swap 'e (nth 3 lst) lst)))))
((= 5 x) (append (list (list 'U (swap 'e (nth 2 lst) lst))) (list (list 'D (swap 'e (nth 8 lst) lst))) (list (list 'L (swap 'e (nth 4 lst) lst)))))
((= 6 x) (append (list (list 'U (swap 'e (nth 3 lst) lst))) (list (list 'R (swap 'e (nth 7 lst) lst)))))
((= 7 x) (append (list (list 'U (swap 'e (nth 4 lst) lst))) (list (list 'R (swap 'e (nth 8 lst) lst))) (list (list 'L (swap 'e (nth 6 lst) lst)))))
((= 8 x) (append (list (list 'U (swap 'e (nth 5 lst) lst))) (list (list 'L (swap 'e (nth 7 lst) lst)))))
))

;; swap helper used in append-moves
;; moves function is COMPLETE

(defun swap (x y lst)
(subst y 9 (subst x y (subst 9 x lst))))

(defun make-open-init (lst)
	(append (list (list (list nil lst))))
)

(defun extend-path (pth)
	(extending pth (remove-redundant (moves (get-state (car pth))) pth))
)

(defun extending (pth mov)
	(cond
		((null mov) nil)
		(T (cons (cons (car mov) pth) (extending pth (cdr mov))))
	)
)

(defun search-bfs (lst)
	(cond
		((null lst) nil)
		((goal-searching (car lst)) (path (car lst)))
		(T (search-bfs (append (cdr lst) (extend-path (car lst)))))
	)
)

;; search-bfs helper function
(defun goal-searching (lst)
	(cond
		((null lst) nil)
		((goal-state (get-state (car lst))))
		(T (goal-searching (cdr lst)))
	)
)

(defun search-dfs-fd (lst d)
	(cond
		((null lst) nil)
		((goal-searching (car lst)) (path (car lst)))
		(T (deep-help lst d))
	)
)

(defun deep-help (lst d)
	(cond
		((null lst) nil)
		((> (length (car lst)) d) (search-dfs-fd (cdr lst) d))
		((search-dfs-fd (append (extend-path (car lst)) (cdr lst)) d))
	)
)

;(defun search-id (lst)
;	(cond
;		((null lst) ())
;		(T (or (search-dfs-fd lst (length (first lst)))
;		       (search-id (append (extend-path (first lst)) (last lst)))))
;	)
;)

(defun sss (lst &key (type 'BFS) (depth 7))
	(cond
	  	((null lst) nil)
		((goal-state lst) nil)
		((equal type 'BFS) (search-bfs (make-open-init lst)))
		((equal type 'DFS) (search-dfs-fd (make-open-init lst) depth))
		((equal type 'ID) (search-id (make-open-init lst)))
		(T nil)
	)
)

(defun out-of-place (lst)
	(cond
		((null lst) nil)
		((goal-state lst) 0)
		(T (out-of-place-help lst '(1 2 3 8 e 4 7 6 5) 0))
	)
)

(defun out-of-place-help (lst goal counter)
	(cond
		((null lst) counter)
		((equal (car lst) 'e) (out-of-place-help (cdr lst) (cdr goal) counter))
		((equal (car lst) (car goal)) (out-of-place-help (cdr lst) (cdr goal) counter))
		(T (out-of-place-help (cdr lst) (cdr goal) (+ 1 counter)))
	)
)

(defun out-of-place-f (pth)
	(cond
		((null pth) nil)
		(T (+ (out-of-place (cadar pth)) (- (length pth) 1)))
	)
)

;	1 2 3	2 1 4
;	8   4	8 6 5
;	7 6 5	3   7
;
; 	(1 + 1 + 1 + 0 + 1 + 1 + 4 + 2)
;
;	(1 2 3 8 e 4 7 6 5)
;	(2 1 4 8 6 5 3 e 7)
;	
;	Length, nth, member


(defun manhattan (state)
	(cond
		; [a1 - b1] + [a2 - b2] + ... + [an - bn]
		((null state) nil)
	;	(T (abs (- (length (member 6 state)) (length (member 6 '(1 2 3 8 e 4 7 6 5))))))
		(T (man-help state '(1 2 3 8 e 4 7 6 5) 4))
	)
)

(defun man-help (state goal &optional (n 0) (sum 0))
	(let* ((a (nth n state)) (b (member a state)) (c (member a goal)) (d (length b)) (e (length c)))
		(cond
	  		((null state) nil)
			((equal n 8) sum)
			(T (abs (- d e)))
		)
	)
)
