;;;; File I/O

;;; general

(define (export file-name . file-type)
  (case (car file-type)
    ('scad (export-scad *latest-model* file-name))
    (else (export-model *latest-model* file-name))))

(define (import file-name . file-type)
  (case (car file-type)
    (else (import-model file-name))))

;;; save as S-expression

(define (export-model model file-name)
  (call-with-output-file file-name
    (lambda (output)
      (if (write model output)
          #t #f))))

(define (import-model file-name)
  (call-with-input-file file-name
    (lambda (input)
      (make-painter% (read input)))))

(define (make-painter% model)
  (lambda (frame-or-key)
    (case frame-or-key
      ('type (type-of model))
      ('model model)
      (else (draw (transform-model model frame-or-key))))))

;;; save as SCAD

(define (export-scad model file-name)
  (call-with-output-file file-name
    (lambda (output)
      (display (model->scad-string model)
               output))))

(define (vector->string vector)
  (do ((nums vector (cdr nums))
       (result nil))
      ((null? nums)
       (apply string-append (butlast result 1)))
    (let ((numstr (number->string (car nums))))
      (set! result
            (append result (list numstr ","))))))

(define (vectors->string vectors)
  (do ((vecs vectors (cdr vecs))
       (result nil))
      ((null? vecs)
       (apply string-append (butlast result 1)))
    (let ((vecstr (vector->string (car vecs))))
      (set! result
            (append result (list "[" vecstr "]" ","))))))

(define (property model . n)
  (let ((properties (getf model 'properties)))
    (if (null? n)
        properties
        (list-ref properties (car n)))))

(define (model->scad-string model)
  (let ((subtype (getf model 'subtype)))
    (case subtype
      ('polyhedron
       (let ((points (property model 0))
             (triangles (map reverse (property model 1))))
         (string-append "polyhedron(points=[" (vectors->string points) "], " +NL+
                        "triangles=[" (vectors->string triangles) "]);" +NL+)))
      ('union
       (let ((child-strs (map (lambda (m) (string-append (model->scad-string m) +NL+))
                              (property model))))
         (string-append "union() {" +NL+
                        (apply string-append child-strs) "}" +NL+)))
      ('dim3
       (let* ((frame (property model 0))
              (affin (append1 (transpose (append1 (getf frame 'edges) (getf frame 'origin)))
                              (list 0 0 0 1))))
         (string-append "multmatrix(m=[" (vectors->string affin) "]) {" +NL+
                        (model->scad-string (property model 1)) "}" +NL+)))
      ('dim2
       (error "exporting 2d-model as SCAD not allowed"))
      ('cube
       (let ((size (property model 0)))
         (string-append "cube(size=[" (vector->string size) "],center=false);" +NL+)))
      ('sphere
       (let ((radius (property model 0))
             (resolution (* *resolution-scale* (property model 1))))
         (string-append "sphere(r=" (number->string radius)
                        ",$fn=" (number->string resolution) ",center=false);" +NL+)))
      ('cylinder
       (let ((height (property model 0))
             (top-radius (property model 1))
             (bottom-radius (property model 2))
             (resolution (* *resolution-scale* (property model 3))))
         (string-append "cylinder(h=" (number->string height)
                        (string-append ",r1=" (number->string bottom-radius)
                                       ",r2=" (number->string top-radius))
                        ",$fn=" (number->string resolution) ",center=false);" +NL+)))
      ('scale
       (let ((scale (property model 0)))
         (string-append "scale(v=[" (vector->string scale) "]) {" +NL+
                        (model->scad-string (property model 1)) "}" +NL+)))
      ('rotate
       (let ((degree (property model 0))
             (axis (property model 1)))
         (string-append "rotate(" (number->string degree)
                        ",[" (vector->string axis) "]) {" +NL+
                        (model->scad-string (property model 1)) "}" +NL+)))
      ('translate
       (let ((vector (property model 0)))
         (string-append "translate(v=[" (vector->string vector) "]) {" +NL+
                        (model->scad-string (property model 1)) "}" +NL+))))))
