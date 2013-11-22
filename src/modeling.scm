;;;; Modeling

;;; general

(define (type-of epression)
  (getf epression 'type))

(define (make-color r g b)
  (list 'r r 'g g 'b b))

(define (make-attribute color ka kd ks ke shininess)
  (list 'color color 'ka ka 'kd kd 'ks ks 'ke ke 'shininess shininess))

(define (color->hex color)
  (if (number? color)
      color
      (let ((r (getf color 'r))
            (g (getf color 'g))
            (b (getf color 'b)))
        (+ (* (expt 16 4) (floor (* 255 r)))
           (* (expt 16 2) (floor (* 255 b)))
           (floor (* 255 g))))))

(define (hex->color hex)
  (let ((r (/ hex (expt 16 4)))
        (g (/ (remainder hex (expt 16 4)) (expt 16 2)))
        (b (remainder hex (expt 16 2))))
    (make-color (/ r 255) (/ g 255) (/ b 255))))

;;; make vertexes and polygons

(define (make-polygons-tetragon-hexahedron attribute 8vertexes)
  (let ((comb '((0 1 2 3) (0 1 5 4) (1 2 6 5)
                (2 3 7 6) (3 0 4 7) (4 5 6 7)))
        (facis '(0 1 2 3 4 5)))
    (map (lambda (fi)
           (make-polygon attribute
                         (map (lambda (ci) (list-ref 8vertexes ci))
                              (list-ref comb fi))))
         facis)))

(define (make-polygons-cuboid attribute size)
  (let ((x (x size)) (y (y size)) (z (z size)))
    (let* ((a (list 0.0 0.0   z))
           (b (list   x 0.0   z))
           (c (list   x   y   z))
           (d (list 0.0   y   z))
           (e (list 0.0 0.0 0.0))
           (f (list   x 0.0 0.0))
           (g (list   x   y 0.0))
           (h (list 0.0   y 0.0)))
      (make-polygons-tetragon-hexahedron attribute
                                         (list a b c d e f g h)))))

(define (make-polygons-sphere attribute radius resolution)
  (letrec ((rec (lambda (v0 v1 v2 c)
                  (if (<= c 0)
                      (list (make-polygon attribute (list v0 v1 v2)))
                      (let ((v3d (midpoint v1 v2))
                            (v4d (midpoint v2 v0))
                            (v5d (midpoint v0 v1)))
                        (let ((v3 (scl (/ radius (norm v3d)) v3d))
                              (v4 (scl (/ radius (norm v4d)) v4d))
                              (v5 (scl (/ radius (norm v5d)) v5d)))
                          (append (rec v0 v4 v5 (1- c))
                                  (rec v1 v5 v3 (1- c))
                                  (rec v2 v3 v4 (1- c))
                                  (rec v3 v4 v5 (1- c)))))))))
    (let ((sc (ceiling (* resolution 1)))
          (a (list 0.0 0.0 radius))
          (b (list radius 0.0 0.0))
          (c (list 0.0 radius 0.0))
          (d (list (- radius) 0.0 0.0))
          (e (list 0.0 (- radius) 0.0))
          (f (list 0.0 0.0 (- radius))))
      (append (rec a b c sc) (rec a c d sc) (rec a d e sc) (rec a e b sc)
              (rec f b c sc) (rec f c d sc) (rec f d e sc) (rec f e b sc)))))

(define (make-polygons-cylinder attribute height top-radius bottom-radius resolution)
  (let* ((sc (ceiling (* resolution 10)))
         (rad (enumerate 0 (* 2 +pi+) (/ +pi+ sc))))
    (let ((tops (map (lambda (a)
                       (list (* top-radius (cos a))
                             (* top-radius (sin a))
                             height))
                     rad))
          (bots (map (lambda (a)
                       (list (* bottom-radius (cos a))
                             (* bottom-radius (sin a))
                             0))
                     rad)))
      (let ((mkp (cond ((/= (* top-radius bottom-radius) 0)
                        (lambda (tp0 tp1 bp0 bp1)
                          (make-polygon attribute (list tp0 bp0 bp1 tp1))))
                       ((= top-radius 0)
                        (lambda (tp0 tp1 bp0 bp1)
                          (make-polygon attribute (list tp0 bp0 bp1))))
                       ((= bottom-radius 0)
                        (lambda (tp0 tp1 bp0 bp1)
                          (make-polygon attribute (list tp0 bp0 tp1)))))))
        (append (map mkp
                     tops (append1 (cdr tops) (car tops))
                     bots (append1 (cdr bots) (car bots)))
                (if (/= top-radius 0) (list (make-polygon attribute tops)))
                (if (/= bottom-radius 0) (list (make-polygon attribute bots))))))))

(define (make-polygons-polyhedron attribute points triangles)
  (map (lambda (t)
         (make-polygon attribute
                       (map (lambda (p)
                              (list-ref points p))
                            t)))
       triangles))

;;; middle level representation
;;
;; ml-model : <primitive shape> or <transform method>
;; - primitive shape : ('type <type> 'properties <shape-properties>)
;; -- type : 'cube, 'sphere, 'cylinder, or 'polyhedron
;; -- <shape-properties> : ('attribute <attribute> 'options <options>)
;; --- attribute : ('color color 'ka <ka> 'kd <kd> 'ks <ks> 'ke <ke>
;; ---              'shininess <shininess>)
;; ---- color : ('r <r> 'g <g> 'b <b>)
;; ----- r, g, b : value in [0 1]
;; --- options
;; ---- (2d) type == 'polygon : (<vertexes>)
;; ---- (3d) type == 'cube : (<size>)
;; ----- size : <3d-vector>
;; ------ 3d-vector : (<number> <number> <number>)
;; ---- (3d) type == 'sphere : (<radius> <resolution>)
;; ---- (3d) type == 'cylinder : (<height> <top-radius> <bottom-radius> <resolution>)
;; ---- (3d) type == 'polyhedron : (<vertexes>)
;; - transfrom method : ('type <type> 'properties <method-properties>)
;; -- type : 'scale, 'resize, rotate, 'translate, ...
;;           'union, 'difference, 'intersection, ...
;; -- method-properties : ('children children 'options options)
;; --- children : (ml-model ... ml-model)
;; --- options
;; ---- type == 'scale : (<scale>)
;; ----- scale : <3d-vector>
;; ---- type == 'rotate : (<angle> <axis>)
;; ----- angle : <degree>
;; ----- axis : <2d-vector> or <3d-vector>
;; ------ 2d-vector : (<number> <number>)
;; ---- type == 'translate : (<2d-vector>) or (<3d-vector>)
;; ---- type == 'union : ()
;;

(define (option ml-model . n)
  (let ((ops (getf (getf ml-model 'properties) 'options)))
    (if (null? n)
        ops
        (list-ref ops (car n)))))

(define (child ml-model . n)
  (let ((chs (getf (getf ml-model 'properties) 'children)))
    (if (null? n)
        chs
        (list-ref chs (car n)))))

(define (make-painter type properties ml-model->ll-model)
  (let* ((ml-model (list 'type type 'properties properties))
         (ll-model (ml-model->ll-model ml-model))
         (painter (ll-model->painter ll-model)))
    (lambda (frame-or-key)
      (case frame-or-key
        ('type     type)
        ('ml-model ml-model)
        ('ll-model ll-model)
        ('painter painter)
        (else (painter frame-or-key))))))

(define (polygon attribute vertexes)
  (make-painter
   'polygon
   (list 'attribute attribute 'options (list vertexes))
   (lambda (ml-model)
     (make-ll-model '2d-model
                    (list (make-polygon attribute vertexes))))))

(define (cube attribute size)
  (make-painter
   'cube
   (list 'attribute attribute 'options (list size))
   (lambda (ml-model)
     (make-ll-model '3d-model
                    (make-polygons-cuboid attribute size)))))

(define (sphere attribute radius resolution)
  (make-painter
   'sphere
   (list 'attribute attribute 'options (list radius resolution))
   (lambda (ml-model)
     (make-ll-model '3d-model
                    (make-polygons-sphere attribute radius resolution)))))

(define (cylinder attribute height top-radius bottom-radius resolution)
  (make-painter
   'cylinder
   (list 'attribute attribute 'options (list height top-radius bottom-radius resolution))
   (lambda (ml-model)
     (make-ll-model '3d-model
                    (make-polygons-cylinder attribute
                                            height top-radius bottom-radius resolution)))))

(define (polyhedron attribute points triangles)
  (make-painter
   'polyhedron
   (list 'attribute attribute 'options (list points triangles))
   (lambda (ml-model)
     (make-ll-model '3d-model
                    (make-polygons-polyhedron attribute points triangles)))))

(define (scale scale painter)
  (make-painter
   'scale
   (list 'children (list (painter 'ml-model)) 'options (list scale))
   (lambda (ml-model)
     (let ((child-ll-model (painter 'll-model)))
       (make-ll-model (getf child-ll-model 'type)
                      (map (lambda (p)
                             (make-polygon (getf p 'attribute)
                                           (map (lambda (v) (scl scale v))
                                                (getf p 'vertexes))))
                           (getf child-ll-model 'polygons)))))))

(define (rotate degree axis painter)
  (make-painter
   'rotate
   (list 'children (list (painter 'ml-model)) 'options (list degree axis))
   (lambda (ml-model)
     (let ((child-ll-model (painter 'll-model)))
       (make-ll-model (getf child-ll-model 'type)
                      (map (lambda (p)
                             (make-polygon
                              (getf p 'attribute)
                              (map (lambda (v) (rod axis (degree->radian degree) v))
                                   (getf p 'vertexes))))
                           (getf child-ll-model 'polygons)))))))

(define (translate vector painter)
  (make-painter
   'translate
   (list 'children (list (painter 'ml-model)) 'options (list vector))
   (lambda (ml-model)
     (let ((child-ll-model (painter 'll-model)))
       (make-ll-model (getf child-ll-model 'type)
                      (map (lambda (p)
                             (make-polygon (getf p 'attribute)
                                           (map (lambda (v) (add v vector))
                                                (getf p 'vertexes))))
                           (getf child-ll-model 'polygons)))))))

(define (union painter . painters)
  (let ((painters (cons painter painters)))
    (make-painter
     'union
     (list 'children (map (lambda (p) (p 'ml-model))
                          painters))
     (lambda (ml-model)
       (let ((child-ll-models (map (lambda (p) (p 'll-model))
                                   painters)))
         (make-ll-model (getf (car child-ll-models) 'type)
                        (apply append
                               (map (lambda (p) (getf p 'polygons))
                                    child-ll-models))))))))

;;; low level representation
;;
;; ll-model : ('type <model-type> 'polygons <polygons>)
;; - model-type : '3d-model or '2d-model
;; - polygons : (<polygon> ... <polygon>)
;; -- polygon : ('attribute attribute 'vertexes <vertexes>)
;; --- vertexes : (<vertex> ... <vertex>)
;; ---- vertex : <point>
;; ----- point : <2d-point> or <3d-point>
;; ------ 2d-point : <2d-vector>
;; ------- 2d-vector : (<number> <number>)
;; ------ 3d-point : <3d-vector>
;; ------- 3d-vector : (<number> <number> <number>)
;;
;; frame : ('type <type> 'origin <origin> 'edges <edges>
;; - frame-type : '3d-frame or '2d-frame
;; --- origin : 2d-point or 3d-point
;; --- edges : (2d-point 2d-point) or (3d-point 3d-point 3d-point)
;;

(define (nt-frame? frame)
  (null? (getf frame 'type #f)))

(define (2d-frame? frame)
  (eq? (getf frame 'type #f) '2d-frame))

(define (3d-frame? frame)
  (eq? (getf frame 'type #f) '3d-frame))

(define (make-polygon attribute vertexes)
  (list 'attribute attribute
        'vertexes vertexes))

(define (make-ll-model type polygons)
  (list 'type type
        'polygons polygons))

(define (make-2d-frame origin x-edge y-edge)
  (list 'type '2d-frame
        'origin origin
        'edges (list x-edge y-edge)))

(define (make-3d-frame origin x-edge y-edge z-edge)
  (list 'type '3d-frame
        'origin origin
        'edges (list x-edge y-edge z-edge)))

(define (map-point point frame)
  (let ((mat1 (vector->matrix point))
        (orig (getf frame 'origin))
        (edge (getf frame 'edges)))
    (let ((mat0 (map (lambda (e) (sub e orig)) edge)))
      (add (matrix->vector (mul mat0 mat1))
           orig))))

(define (ll-model->painter ll-model)
  (case (type-of ll-model)
    ('2d-model (2d-ll-model->painter ll-model))
    ('3d-model (3d-ll-model->painter ll-model))))

(define (twin->pair twin)
  (cons (list-ref twin 0) (list-ref twin 1)))

(define (2d-ll-model->painter 2d-ll-model)
  (lambda (frame)
    (let ((2d-frame (if (2d-frame? frame) frame +2d-frame-full+))
          (nt-frame (if (nt-frame? frame) frame +nt-frame-full+)))
      (do ((polys (getf 2d-ll-model 'polygons) (cdr polys)))
          ((null? polys) #t)
        (let ((head (car polys)))
          (let ((color (getf (getf head 'attribute) 'color))
                (verts (getf head 'vertexes)))
            (set-color (color->hex color))
            ((vertexes->painter
              (map (lambda (v) (twin->pair (map-point v 2d-frame)))
                   verts)
              *painter-filled?*)
             nt-frame)))))))

(define (3d-ll-model->painter 3d-ll-model)
  (lambda (frame)
    (let ((3d-frame (if (3d-frame? frame) frame +3d-frame-full+))
          (2d-frame (if (or (2d-frame? frame) (nt-frame? frame))
                        frame
                        +2d-frame-full+)))
      (let* ((mkv (lambda (v) (map-point v 3d-frame)))
             (mkp (lambda (p) (make-polygon (getf p 'attribute)
                                            (map mkv (getf p 'vertexes)))))
             (3dll (make-ll-model '3d-model (map mkp (getf 3d-ll-model 'polygons))))
             (2dll (3d-ll-model->2d-ll-model 3dll *camera* *lights*)))
        ((2d-ll-model->painter 2dll) 2d-frame)))))

(define (show painter . frame)
  (clear-picture)
  (painter (if (null? frame) +nt-frame-full+ (car frame))))
