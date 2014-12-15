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

;;;; Rendering

;;; light

(define (make-intensity ia id is)
  (list 'ia ia 'id id 'is is))

(define (make-light type position direction intensity)
  (list 'type type
        'position position
        'direction direction
        'intensity intensity))

(define (make-parallel-light direction intensity)
  (make-light 'parallel nil direction intensity))

(define (make-point-light position intensity)
  (make-light 'point position nil intensity))

(define (parallel-light? light)
  (eq? (type-of light) 'parallel))

(define (point-light? light)
  (eq? (type-of light) 'point))

(define (make-simple-light direction)
  (make-parallel-light direction (make-intensity 0.5 0.5 0.5)))

;;; camera

(define (make-camera position direction view-volume)
  (list 'position position
        'direction direction
        'view-volume view-volume))

;;; projection

(define (make-projected-polygon attribute normal depth vertexes)
  (list 'attribute attribute
        'normal normal
        'depth depth
        'vertexes vertexes))

(define (make-projected-polygon% polygon)
  (let ((verts (getf polygon 'vertexes)))
    (make-projected-polygon (getf polygon 'attribute)
                            (normal polygon)
                            (/ (apply + (map (lambda (v) (z v))
                                             verts))
                               (length verts))
                            verts)))

(define (right-side? vec0 vec1)
  (let ((v0 (butlast vec0 1))
        (v1 (butlast vec1 1)))
    (> (cross v0 v1) 0)))

(define (normal projected-polygon)
  (let ((verts (getf projected-polygon 'vertexes)))
    (let ((vec0 (sub (list-ref verts 1) (list-ref verts 0)))
          (vec1 (sub (list-ref verts 2) (list-ref verts 0))))
      (if (right-side? vec0 vec1)
          (cross vec0 vec1)
          (cross vec1 vec0)))))

(define (project type camera polygons)
  (let ((cdir (getf camera 'direction)))
    (let ((zx-vector (list (x cdir) 0.0 (z cdir)))
          (zz-vector (list 0.0 0.0 (z cdir))))
      (let ((zx-zero-p (zero-vector? zx-vector))
            (zz-zero-p (zero-vector? zz-vector))
            (rad (acos (cos-angle zx-vector zz-vector))))
        (let ((screen-origin (getf camera 'position))
              (zxv (cond ((and zx-zero-p (<= 0.0 (y cdir))) (list -1.0 0.0 0.0))
                         ((and zx-zero-p (> 0.0 (y cdir)))  (list 1.0 0.0 0.0))
                         (else (cross cdir zx-vector))))
              (zzv (cond ((and zz-zero-p (<= 0.0 (x cdir))) (list 0.0 1.0 0.0))
                         ((and zz-zero-p (> 0.0 (x cdir)))  (list 0.0 -1.0 0.0))
                         (else (cross zx-vector zz-vector))))
              (zxa (if zx-zero-p
                       (/ +pi+ 2.0)
                       (acos (cos-angle cdir zx-vector))))
              (zza (cond (zz-zero-p (/ +pi+ 2.0))
                         ((>= 0 (z cdir)) rad)
                         (else (+ rad +pi+)))))
          (let ((rpolys (map (lambda (p)
                               (make-polygon (getf p 'attribute)
                                             (map (lambda (v)
                                                    (let* ((a (sub v screen-origin))
                                                           (b (rod zxv zxa a)))
                                                      (rod zzv zza b)))
                                                  (getf p 'vertexes))))
                             polygons)))
            (case type
              ('parallel    (parallel-project rpolys))
              ('perspective (perspective-project camera rpolys)))))))))

(define (parallel-project polygons)
  (map make-projected-polygon% polygons))

(define (perspective-project camera polygons)
  (let ((rate 1)
        (vvz (z (getf camera 'view-volume)))
        (para-polygons (map make-projected-polygon% polygons)))
    (map (lambda (p)
           (make-projected-polygon
            (getf p 'attribute)
            (getf p 'normal)
            (getf p 'depth)
            (map (lambda (v)
                   (let ((s (/ (- vvz (abs (z v))) (* rate vvz))))
                     (list (* s (x v)) (* s (y v)) (z v))))
                 (getf p 'vertexes))))
         para-polygons)))

;;; clipping

(define (notstepover? vertexes view-volume)
  (accumulate (lambda (head rest)
                (and (and (<= (abs (x head)) (/ (x view-volume) 2))
                          (<= (abs (y head)) (/ (y view-volume) 2)))
                     rest))
              #t
              vertexes))

(define (clip projected-polygons camera)
  (do ((vv (getf camera 'view-volume))
       (rest projected-polygons (cdr rest))
       (result nil))
      ((null? rest) result)
    (let* ((head (car rest))
           (depth (getf head 'depth)))
      (if (and (notstepover? (getf head 'vertexes) vv)
               (>= depth (- (z vv)))
               (< depth 0))
          (set! result (append1 result head))))))

;;; hidden surface removal

(define (depth-sort projected-polygons)
  (sort projected-polygons
        (lambda (p0 p1)
          (< (getf p0 'depth) (getf p1 'depth)))))

;;; shading

(define (shade type camera lights projected-polygons)
  (case type
    ('phong-model (flat-shade 'phong-model camera lights projected-polygons))))

(define (level-color color)
  (let ((vals (pvals color)))
    (let ((minv (max (apply min vals) 0)))
      (let ((vals (map (lambda (v) (+ v minv)) vals)))
        (let ((maxv (max (apply max vals) 1)))
          (apply make-color (scl (/ maxv) vals)))))))

(define (flat-shade type camera lights projected-polygons)
  (map (lambda (p)
         (let ((color (case type
                        ('phong-model (phong-model-color camera lights p)))))
           (make-polygon (make-attribute (level-color color) nil nil nil nil nil)
                         (getf p 'vertexes))))
       projected-polygons))

(define (phong-model-color camera lights projected-polygon)
  (let ((verts (getf projected-polygon 'vertexes))
        (attrs (getf projected-polygon 'attribute)))
    (let ((vdir (scl -1 (list-ref verts 0)))
          (ndir (getf projected-polygon 'normal))
          (ka (pvals (getf attrs 'ka)))
          (kd (pvals (getf attrs 'kd)))
          (ks (pvals (getf attrs 'ks)))
          (ke (pvals (getf attrs 'ke)))
          (shininess (getf attrs 'shininess))
          (ia (/ (apply + (map (lambda (l) (getf (getf l 'intensity) 'ia))
                               lights))
                 (length lights))))
      (apply make-color
             (apply add
                    (scl ia ka)
                    (map (lambda (l)
                           (let* ((ldir (cond ((parallel-light? l)
                                               (scl -1 (getf l 'direction)))
                                              ((point-light? l)
                                               (sub (getf l 'position)
                                                    (list-ref verts 0)))))
                                  (rdir (sub (scl (/ (* 2 (dot ldir ndir))
                                                     (square (norm ndir)))
                                                  ndir)
                                             ldir))
                                  (id (getf (getf l 'intensity) 'id))
                                  (is (getf (getf l 'intensity) 'is)))
                             (add (scl (* id (cos-angle ldir ndir)) kd)
                                  (scl (* is (if (>= (cos-angle rdir vdir) 0)
                                                 (abs (expt (cos-angle rdir vdir)
                                                            shininess))
                                                 0))
                                       ks))))
                         lights))))))

;;; 3d -> 2d

(define (adjust polygons camera)
  (let ((vv (getf camera 'view-volume)))
    (let ((vo (list (- (/ (x vv) 2)) (- (/ (y vv) 2)))))
      (map (lambda (p)
             (make-polygon (getf p 'attribute)
                           (map (lambda (v)
                                  (list (/ (- (x v) (x vo)) (* (abs (x vo)) 2))
                                        (/ (- (y v) (y vo)) (* (abs (y vo)) 2))))
                                (getf p 'vertexes))))
           polygons))))

(define (3d-model->2d-model model camera lights)
  (let ((polygons (getf model 'polygons)))
    (let* ((pp0 (project 'perspective camera polygons))
           (pp1 (clip pp0 camera))
           (pp2 (depth-sort pp1))
           (pp3 (shade 'phong-model camera lights pp2))
           (pp4 (adjust pp3 camera)))
      (make-model 'dim2 nil nil pp4))))
