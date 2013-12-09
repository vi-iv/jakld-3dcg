;; =============================================================================
;; The MIT License (MIT)
;;
;; Copyright (c) 2013, Koutarou FURUKAWA (<Furukawa.Koutarou@Gmail.com>)
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
;; =============================================================================

;;;; Modeling

;;; general

(define (type-of expression . not-exist-error?)
  (getf expression 'type (car not-exist-error?)))

(define (type=? expression0 expression1)
  (eq? (type-of expression0) (type-of expression1)))

(define (make-color r g b)
  (list 'r r 'g g 'b b))

(define (make-attribute color ka kd ks ke shininess)
  (list 'color color         ; used for 2d
        'ka ka               ; ambient
        'kd kd               ; diffuse
        'ks ks               ; specular
        'ke ke               ; emission
        'shininess shininess ;
        ))

(define (color->hex color)
  (if (number? color)
      color
      (let ((r (getf color 'r))
            (g (getf color 'g))
            (b (getf color 'b)))
        (+ (* (expt 16 4) (floor (* 255 r)))
           (* (expt 16 2) (floor (* 255 g)))
           (floor (* 255 b))))))

(define (hex->color hex)
  (let ((r (/ hex (expt 16 4)))
        (g (/ (remainder hex (expt 16 4)) (expt 16 2)))
        (b (remainder hex (expt 16 2))))
    (make-color (/ r 255) (/ g 255) (/ b 255))))

;;; frame

(define (make-2d-frame origin x-edge y-edge)
  (list 'type 'dim2
        'origin origin
        'edges (list x-edge y-edge)))

(define (make-3d-frame origin x-edge y-edge z-edge)
  (list 'type 'dim3
        'origin origin
        'edges (list x-edge y-edge z-edge)))

(define (frame? object)
  (and (list? object)))

(define (nt-frame? frame)
  (and (frame? frame)
       (null? (type-of frame))))

(define (2d-frame? frame)
  (and (frame? frame)
       (eq? (type-of frame) 'dim2)))

(define (3d-frame? frame)
  (and (frame? frame)
       (eq? (type-of frame) 'dim3)))

;;; vertexes and polygons

(define (make-polygon attribute vertexes)
  (list 'attribute attribute
        'vertexes vertexes))

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

;;; primitive painter

(define (make-model type subtype properties polygons)
  (list 'type type
        'subtype subtype
        'properties properties
        'polygons polygons))

(define (make-painter type subtype properties polygons)
  (let ((model (make-model type subtype properties polygons)))
    (lambda (frame-or-key)
      (case frame-or-key
        ('type type)
        ('model model)
        (else (draw (transform-model model frame-or-key)))))))

(define (map-point point frame)
  (let ((mat1 (vector->matrix point))
        (orig (getf frame 'origin))
        (edge (getf frame 'edges)))
    (let ((mat0 (map (lambda (e) (sub e orig)) edge)))
      (add (matrix->vector (mul mat0 mat1))
           orig))))

(define (map-polygons procedure polygons)
  (map (lambda (polygon)
         (make-polygon (getf polygon 'attribute)
                       (map procedure (getf polygon 'vertexes))))
       polygons))

(define (transform-model model frame)
  (if (type=? model frame)
      (let ((polygons (getf model 'polygons)))
        (make-model (type-of model)
                    (type-of frame)
                    (list frame model)
                    (map-polygons (lambda (vertex) (map-point vertex frame))
                                  (getf model 'polygons))))))

(define (painter:polyhedron attribute points triangles)
  (make-painter 'dim3
                'polyhedron
                (list points triangles)
                (make-polygons-polyhedron attribute points triangles)))

(define (painter:union painter . painters)
  (let* ((painters (cons painter painters)))
    (make-painter ((car painters) 'type)
                  'union
                  (map (lambda (p) (p 'model)) painters)
                  (mappend (lambda (p) (getf (p 'model) 'polygons))
                           painters))))

(define (painter:transform painter frame)
  (if (type=? (painter 'model) frame)
      (make-painter (painter 'type)
                    (type-of frame)
                    (list frame (painter 'model))
                    (map-polygons (lambda (vertex) (map-point vertex frame))
                                  (getf (painter 'model) 'polygons)))))

;;; optional painter

(define (painter:cube attribute size)
  (make-painter 'dim3
                'cube
                (list size)
                (make-polygons-cuboid attribute size)))

(define (painter:sphere attribute radius resolution)
  (make-painter 'dim3
                'sphere
                (list radius resolution)
                (make-polygons-sphere attribute radius resolution)))

(define (painter:cylinder attribute height top-radius bottom-radius resolution)
  (make-painter 'dim3
                'cylinder
                (list height top-radius bottom-radius resolution)
                (make-polygons-cylinder attribute
                                        height top-radius bottom-radius resolution)))

(define (painter:revolution attribute vertexes resolution)
  (let ((scale (* resolution 1))
        (count (let ((c -1)) (lambda () (set! c (1+ c))))))
    (let* ((sel (lambda (slice n)
                  (car (list-ref slice n))))
           (mks (lambda (v)
                  (let ((x (x v)) (y (y v)) (z (z v)))
                    (map (lambda (th)
                           (list (count)
                                 (list x (* y (sin th)) (* y (cos th)))))
                         (enumerate 0 (* 2 +pi+) (/ 1 scale))))))
           (mkt (lambda (slices)
                  (do ((slices (ring slices) (cdr slices))
                       (result nil))
                      ((single? slices) result)
                    (do ((s0 (ring (list-ref slices 0)) (cdr s0))
                         (s1 (ring (list-ref slices 1)) (cdr s1)))
                        ((or (single? s0) (single? s1)) #t)
                      (set! result
                            (append result
                                    (list (list (sel s0 0) (sel s0 1) (sel s1 0))
                                          (list (sel s1 0) (sel s0 1) (sel s1 1))))))))))
      (let* ((slices (map mks vertexes))
             (ts (mkt slices)))
        (painter:polyhedron attribute (map cadr (apply append slices)) ts)))))

(define (painter:scale scale painter)
  (make-painter (getf (painter 'model) 'type)
                'scale
                (list scale (painter 'model))
                (let ((num 0)
                      (cent0 (list 0.0 0.0 0.0))
                      (cent1 (list 0.0 0.0 0.0)))
                  (map-polygons
                   (lambda (vertex)
                     (sub vertex (scl (/ num) (sub cent1 cent0))))
                   (map-polygons (lambda (vertex)
                                   (set! num (1+ num))
                                   (set! cent0 (add cent0 vertex))
                                   (let ((after (scl scale vertex)))
                                     (set! cent1 (add cent1 after))
                                     after))
                                 (getf (painter 'model) 'polygons))))))

(define (painter:rotate degree axis painter)
  (let ((rad (degree->radian degree)))
    (make-painter (getf (painter 'model) 'type)
                  'rotate
                  (list degree axis (painter 'model))
                  (map-polygons (lambda (vertex) (rod axis rad vertex))
                                (getf (painter 'model) 'polygons)))))

(define (painter:translate vector painter)
  (make-painter (getf (painter 'model) 'type)
                'translate
                (list vector (painter 'model))
                (map-polygons (lambda (vertex) (add vector vertex))
                              (getf (painter 'model) 'polygons))))

;; render interface

(define (remove-redundancy model)
  (make-model (getf model 'type)
              (getf model 'subtype)
              (getf model 'properties)
              (do ((polygons (getf model 'polygons) (cdr polygons))
                   (result nil))
                  ((null? polygons) result)
                (let* ((polygon (car polygons))
                       (new (make-polygon
                             (getf polygon 'attribute)
                             (do ((verts (ring (getf polygon 'vertexes)) (cdr verts))
                                  (result nil))
                                 ((single? verts) result)
                               (if (>= (distance (list-ref verts 0)
                                                 (list-ref verts 1))
                                       +eps+)
                                   (set! result (append1 result (list-ref verts 0))))))))
                  (if (>= (length (getf new 'vertexes)) 3)
                      (set! result (append1 result new)))))))

(define (draw model)
  (let ((model (remove-redundancy model)))
    (let ((2d-model (case (type-of model)
                      ('dim2 model)
                      ('dim3 (3d-model->2d-model model *camera* *lights*)))))
      (do ((polygons (getf 2d-model 'polygons) (cdr polygons)))
          ((null? polygons)
           (set! *latest-model* model)
           (set! *render-cache* 2d-model)
           #t)
        (let ((polygon (car polygons)))
          (let ((color (getf (getf polygon 'attribute) 'color))
                (vertexes (getf polygon 'vertexes)))
            (set-color (color->hex color))
            ((vertexes->painter
              (map (lambda (vertex) (twin->pair vertex))
                   vertexes)
              *painter-filled?*)
             +nt-frame-full+)))))))

(define (redraw)
  (if (null? *render-cache*)
      (error "not drawn any models")
      (draw *render-cache*)))

(define (show painter . frame)
  (clear-picture)
  (painter (if (null? frame)
               (case (painter 'type)
                 ('dim2 +2d-frame-full+)
                 ('dim3 +3d-frame-full+))
               (car frame))))
