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

;;;; Appendix

;;; general

(define (string-reverse string)
  (list->string (reverse (string->list string))))

(define (string-left-trim char-bag string)
  (let ((clist (string->list char-bag))
        (slist (string->list string)))
    (let rec ((slist slist))
      (if (member (car slist) clist)
          (rec (cdr slist))
          (list->string slist)))))

(define (string-right-trim char-bag string)
  (string-reverse (string-left-trim char-bag (string-reverse string))))

(define (string-trim char-bag string)
  (string-right-trim char-bag (string-left-trim char-bag string)))

(define (split char string)
  (let rec ((string string) (result nil))
    (let ((sub (member char (string->list string))))
      (if sub
          (let ((i (length sub))
                (j (string-length string)))
            (rec (substring string (- j i -1) j)
                 (append1 result (substring string 0 (- j i)))))
          (append1 result string)))))

(define (join char strings)
  (let* ((lst (repeat (string char) (length strings)))
         (strs (butlast (mappend list strings lst))))
    (apply string-append strs)))

;;; i/o

(define (read-lines port)
  (let rec ((char (read-char port))
            (lines nil)
            (line ""))
    (cond ((eof-object? char) (append1 lines line))
          ((char=? char #\newline)
           (rec (read-char port) (append1 lines line) ""))
          (#t
           (rec (read-char port) lines (string-append line (string char)))))))

(define (remove-pgm-comment-lines lines)
  (let rec ((lines lines)
            (result nil))
    (if (null? lines)
        result
        (let* ((line (car lines))
               (head (car (string->list (string-left-trim " " line)))))
          (if (and (char? head) (char=? #\# head))
              (rec (cdr lines) result)
              (rec (cdr lines) (append1 result line)))))))

(define (painter:pgm-picture file-name attribute unit-size max-height)
  (call-with-input-file file-name
    (lambda (input)
      (let* ((lines (remove-pgm-comment-lines (read-lines input)))
             (elems% (split #\space (join #\space lines)))
             (elems (filter (lambda (e) (not (string=? e ""))) elems%)))
        (let ((magic-number (list-ref elems 0))
              (width        (string->number (list-ref elems 1)))
              (height       (string->number (list-ref elems 2)))
              (gray-max     (string->number (list-ref elems 3)))
              (body         (map string->number (list-tail elems 4))))
          (if (not (string=? magic-number "P2"))
              (error "Unsupported file type -- painter:pgm-picture"))
          (let ((cubes (map (lambda (v)
                              (if (zero? v)
                                  nil
                                  (painter:cube attribute
                                                (list unit-size
                                                      (* max-height (/ v gray-max))
                                                      unit-size))))
                            body))
                (result nil))
            (set! *hoge* body)

            (apply painter:union
                   (do ((i 0 (1+ i)))
                       ((>= i height) result)
                     (do ((j 0 (1+ j)))
                         ((>= j width) nil)
                       (let ((p (list-ref cubes (+ (* i width) j))))
                         (if (not (null? p))
                             (set! result
                                   (append1 result
                                            (painter:translate
                                             (list (* i (- unit-size +eps+))
                                                   0
                                                   (* j (- unit-size +eps+)))
                                             p))))))))))))))

;;;

(define pgm-picture-001
  (let ((unit-size 1) (max-height 4))
    (painter:translate (list (- (* 8 unit-size))
                             0
                             (- (* 8 unit-size)))
                       (painter:pgm-picture "../mod/pgms/circle_b.pgm"
                                            attribute-001
                                            unit-size
                                            max-height))))
