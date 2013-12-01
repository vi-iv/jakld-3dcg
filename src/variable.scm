;;;; Variables

;; (start-picture)

(define +origin+ (list 0.0 0.0 0.0))

(define +NL+ (string #\newline))

(define +nt-frame-full+
  (list (cons 0.0 0.0)
        (cons 1.0 0.0)
        (cons 0.0 1.0)))

(define +2d-frame-full+
  (make-2d-frame (list 0.0 0.0)
                 (list 1.0 0.0)
                 (list 0.0 1.0)))

(define +3d-frame-full+
  (make-3d-frame (list 0.0 0.0 0.0)
                 (list 1.0 0.0 0.0)
                 (list 0.0 1.0 0.0)
                 (list 0.0 0.0 1.0)))

(define *camera* nil)

(define *lights* nil)

(define *painter-filled?* #t)

(define *resolution-scale* 10)

(define *latest-model* nil)

(define *render-cache* nil)
