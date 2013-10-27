;;;; File I/O

;;; constant value

(define +NL+ (string #\newline))

;;; general

(define (export painter file-name . file-type)
  (case (car file-type)
    ('scad (export-scad painter file-name))
    (else (export-model painter file-name))))

(define (import file-name . file-type)
  (case (car file-type)
    (else (import-model file-name))))

;;; save as S-expression

(define (export-model painter file-name)
  (call-with-output-file file-name
    (lambda (output)
      (if (write (list (painter 'ml-model)
                       (painter 'll-model))
                 output)
          #t #f))))

(define (import-model file-name)
  (call-with-input-file file-name
    (lambda (input)
      (let ((twin (read input)))
        (make-painter% (list-ref twin 0)
                       (list-ref twin 1))))))

(define (make-painter% ml-model ll-model)
  (lambda (frame-or-key)
    (case frame-or-key
      ('type     (type-of ml-model))
      ('ml-model ml-model)
      ('ll-model ll-model)
      ('painter  (ll-model->painter ll-model))
      (else ((ll-model->painter ll-model)
             frame-or-key)))))

;;; save as SCAD

(define (export-scad painter file-name)
  (let ((ml-model (painter 'ml-model)))
    (call-with-output-file file-name
      (lambda (output)
        (display (ml-model->scad-string ml-model)
                 output)))))

(define (vector->string vector)
  (do ((nums vector (cdr nums))
       (result nil))
      ((null? nums)
       (apply string-append (butlast result 1)))
    (let ((numstr (number->string (car nums))))
      (set! result
            (append result (list numstr ","))))))

(define (ml-model->scad-string ml-model)
  (let ((type (type-of ml-model)))
    (case type
      ('cube
       (let ((size (option ml-model 0)))
         (string-append "cube(size=[" (vector->string size) "],center=false);" +NL+)))
      ('sphere
       (let ((radius (option ml-model 0))
             (resolution (option ml-model 1)))
         (string-append "sphere(r=" (number->string radius)
                        ",$fn=" (number->string resolution) ",center=false);" +NL+)))
      ('cylinder
       (let ((height (option ml-model 0))
             (top-radius (option ml-model 1))
             (bottom-radius (option ml-model 2))
             (resolution (option ml-model 3)))
         (string-append "cylinder(h=" (number->string height)
                        (cond ((null? top-radius)
                               (string-append ",r=" (number->string bottom-radius)))
                              ((null? bottom-radius)
                               (string-append ",r=" (number->string top-radius)))
                              (else
                               (string-append ",r1=" (number->string bottom-radius)
                                              ",r2=" (number->string top-radius))))
                        ",$fn=" (number->string resolution) ",center=false);" +NL+)))
      ('scale
       (let ((scale (option ml-model 0)))
         (string-append "scale(v=[" (vector->string scale) "]) {" +NL+
                        (ml-model->scad-string (child ml-model 0)) "}" +NL+)))
      ('translate
       (let ((vector (option ml-model 0)))
         (string-append "translate(v=[" (vector->string vector) "]) {" +NL+
                        (ml-model->scad-string (child ml-model 0)) "}" +NL+)))
      ('union
       (let ((child-strs (map (lambda (m) (string-append (ml-model->scad-string m) +NL+))
                              (child ml-model))))
         (string-append "union() {" +NL+
                        (apply string-append child-strs) "}" +NL+))))))
