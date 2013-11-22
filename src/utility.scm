;;;; Utilities

;;; constant value

(define +pi+ 3.14159265359)
(define +eps+ 1.0E-10)

;;; primitive list processing

(define (append1 lst element)
  (append lst (list element)))

(define (last lst . n)
  (let ((n (if (null? n) 1 (car n))))
    (list-tail lst (- (length lst) n))))

(define (last1 lst)
  (car (last lst 1)))

(define (butlast lst . n)
  (let ((l (length lst))
        (n (if (null? n) 1 (car n))))
    (reverse (last (reverse lst) (- l n)))))

(define (group sequence n)
  (let rec0 ((seq sequence))
    (if (null? seq)
        nil
        (cons (let rec1 ((i 0))
                (if (= i n)
                    nil
                    (cons (list-ref seq i) (rec1 (1+ i)))))
              (rec0 (list-tail seq n))))))

(define (flatten expression)
  (cond ((null? expression) nil)
        ((not (pair? expression)) (list expression))
        (else (append (flatten (car expression))
                      (flatten (cdr expression))))))

(define (depth expression)
  (if (not (pair? expression))
      0
      (max (1+ (depth (car expression)))
           (depth (cdr expression)))))

(define (accumulate operator initial sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (accumulate operator initial (cdr sequence)))))

(define (filter predicate sequence)
  (let ((head (car sequence))
        (tail (cdr sequence)))
    (cond ((null? sequence)
           nil)
          ((predicate head)
           (cons head (filter predicate tail)))
          (else
           (filter predicate tail)))))

(define (some predicate . sequences)
  (define (some-null? seq)
    (if (null? seq)
        #f
        (or (null? (car seq))
            (some-null? (cdr seq)))))
  (if (some-null? sequences)
      #f
      (or (apply predicate (map car sequences))
          (apply some predicate (map cdr sequences)))))

(define (every predicate . sequences)
  (if (some null? sequences)
      #t
      (and (apply predicate (map car sequences))
           (apply every predicate (map cdr sequences)))))

(define (notevery predicate . sequences)
  (not (apply every predicate sequences)))

(define (any predicate sequence)
  (not (apply some predicate sequences)))

(define (pkeys plist)
  (map car (group plist 2)))

(define (pvals plist)
  (map cadr (group plist 2)))

(define (getf place indicator . not-exist-error?)
  (let rec ((twins (group place 2)))
    (let ((twin (car twins)))
      (if (null? twins)
          (if (or (null? not-exist-error?)
                  (car not-exist-error?))
              (error "not found key -- getf" indicator)
              nil)
          (if (eq? (car twin) indicator)
              (cadr twin)
              (rec (cdr twins)))))))

(define (shell-sort sequence predicate)
  (define (gap-insertion-sort sequence predicate gap)
    (let ((length (length sequence)))
      (if (< length 2)
          sequence
          (do ((i 1 (1+ i)))
              ((= i length) sequence)
            (do ((x (list-ref sequence i))
                 (j i (- j gap)))
                ((or (< (- j gap) 0)
                     (not (predicate x (list-ref sequence (1- j)))))
                 (set-car! (list-tail sequence j) x))
              (set-car! (list-tail sequence j)
                        (list-ref sequence (- j gap))))))))
  (let ((gaps '(1750 701 301 132 57 23 10 4 1)))
    (do ((gap gaps (cdr gap)))
        ((null? gap) sequence)
      (gap-insertion-sort sequence predicate (car gap)))))

(define sort shell-sort)

(define (enumerate begin end difference)
  (do ((n begin (+ n difference))
       (result nil (append1 result n)))
      ((>= n end) result)))

;;; math

(define (/= number . numbers)
  (let rec0 ((ns0 (cons number numbers)))
    (if (null? ns0)
        #t
        (and (let rec1 ((ns1 (cdr ns0)))
               (if (null? ns1)
                   #t
                   (and (not (= (car ns0) (car ns1)))
                        (rec1 (cdr ns1)))))
             (rec0 (cdr ns0))))))

(define (radian->degree radian)
  (* radian (/ 180 +pi+)))

(define (degree->radian degree)
  (* degree (/ +pi+ 180)))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

;;; vector

;; vector : (num num ...)

(define (vector? expression)
  (and (list? expression)
       (or (= (depth expression) 1))))

(define (legal-vector? expression)
  (and (vector? expression)
       (every number? expression)))

(define (zero-vector? vector)
  (every zero? vector))

(define (x vector)
  (list-ref vector 0))

(define (y vector)
  (list-ref vector 1))

(define (z vector)
  (list-ref vector 2))

(define (add expression . expressions)
  (let ((exps (cons expression expressions)))
    (cond ((number? expression) (map + exps))
          ((vector? expression) (apply map + exps))
          ((matrix? expression) (apply map add exps))
          (else (error "Unknown data type -- add")))))

(define (sub expression . expressions)
  (let ((exps (cons expression expressions)))
    (cond ((number? expression) (map - exps))
          ((vector? expression) (apply map - exps))
          ((matrix? expression) (apply map sub exps))
          (else (error "Unknown data type -- sub")))))

(define (scl scale vector)
  (if (number? scale)
      (map (lambda (x) (* scale x)) vector)
      (map * scale vector)))

(define (norm vector)
  (sqrt (apply + (map square vector))))

(define (midpoint vector0 vector1 . vectors)
  (scl (/ 1 (+ 2 (length vectors)))
       (apply add vector0 vector1 vectors)))

(define (dot vector0 vector1)
  (apply + (map * vector0 vector1)))

(define (cross vector0 vector1)
  (cond ((= (length vector0) 2)
         (- (* (x vector0) (y vector1)) (* (y vector0) (x vector1))))
        ((= (length vector0) 3)
         (list (- (* (list-ref vector0 1) (list-ref vector1 2))
                  (* (list-ref vector0 2) (list-ref vector1 1)))
               (- (* (list-ref vector0 2) (list-ref vector1 0))
                  (* (list-ref vector0 0) (list-ref vector1 2)))
               (- (* (list-ref vector0 0) (list-ref vector1 1))
                  (* (list-ref vector0 1) (list-ref vector1 0)))))))

(define (cos-angle vector0 vector1)
  (/ (dot vector0 vector1)
     (* (norm vector0) (norm vector1))))

(define (rod axis radian vector)
  (let ((den (if (zero-vector? axis)
                 +eps+
                 (norm axis))))
    (let ((unit (scl (/ 1 den) axis)))
      (let ((nx (x unit))
            (ny (y unit))
            (nz (z unit)))
        (let ((rotmat (list (list (+ (* (square nx) (- 1 (cos radian))) (cos radian))
                                  (+ (* nx ny (- 1 (cos radian))) (* nz (sin radian)))
                                  (- (* nz nx (- 1 (cos radian))) (* ny (sin radian))))
                            (list (- (* nx ny (- 1 (cos radian))) (* nz (sin radian)))
                                  (+ (* (square ny) (- 1 (cos radian))) (cos radian))
                                  (+ (* ny nz (- 1 (cos radian))) (* nx (sin radian))))
                            (list (+ (* nz nx (- 1 (cos radian))) (* ny (sin radian)))
                                  (- (* ny nz (- 1 (cos radian))) (* nx (sin radian)))
                                  (+ (* (square nz) (- 1 (cos radian))) (cos radian))))))
          (matrix->vector (mul rotmat (vector->matrix vector))))))))

(define (display-vector vector)
  (let rec ((v vector))
    (if (null? v)
        vector
        (begin (display (car v))
               (newline)
               (rec (cdr v))))))

;;; matrix

;; matrix : ((num num ...)
;;                 :
;;           (num num ...))

(define (matrix? expression)
  (and (list? expression)
       (= (depth expression) 2)))

(define (legal-matrix? expression)
  (and (matrix? expression)
       (every number? (flatten expression))
       (let ((len (length (car expression))))
         (every (lambda (seq) (= (length seq) len))
                (cdr expression)))))

(define (vector->matrix vector)
  (list vector))

(define (matrix->vector matrix)
  (flatten matrix))

(define (col n matrix)
  (list-ref matrix n))

(define (row n matrix)
  (map (lambda (c) (list-ref c n)) matrix))

(define (transpose matrix)
  (let rec ((n 0))
    (if (= n (length (car matrix)))
        nil
        (cons (map (lambda (v) (list-ref v n))
                   matrix)
              (rec (1+ n))))))

(define (mul matrix0 matrix1)
  (let ((rows (1- (length (car matrix0))))
        (cols (1- (length matrix1))))
    (do ((i rows (if (= i 0) rows (1- i)))
         (j cols (if (= i 0) (1- j) j))
         (result nil))
        ((or (< j 0) (< i 0)) (group result (1+ rows)))
      (set! result
            (cons (dot (row i matrix0) (col j matrix1))
                  result)))))

(define (display-matrix matrix)
  (let display-col ((n 0))
    (if (= n (length (car matrix)))
        matrix
        (begin
          (let display-row ((r (map (lambda (c) (list-ref c n))
                                    matrix)))
            (if (null? r)
                nil
                (begin (display (car r))
                       (display #\space)
                       (display-row (cdr r)))))
          (newline)
          (display-col (1+ n))))))

