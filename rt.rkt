#lang racket/gui
;; basic structs -------------------------------------------
(struct pixel (r g b))
(struct vec (x y z))
(struct ray (from dir))
(struct int-res (int? t p))
(struct sphere3D (center radius col))
(struct point (col norm mat))
(struct color (r g b a))

;; helper funcs --------------------------------------------
(define (format-vec v)
  (format "(~a, ~a, ~a)" (vec-x v) (vec-y v) (vec-z v)))

(define (print-sphere3D s)
  (printf "sphere3D: (center: ~a, R: ~a)\n " (format-vec (sphere3D-center s))
          (sphere3D-radius s)))

(define (format-point p)
  (format "(color: ~a, norm: ~a, material: ~a)" 
          (point-col p)
          (point-norm p)
          (point-mat p)))

(define (print-int-res ir)
  (printf "intersection: (~a, dis: ~a, point: ~a)\n" 
          (int-res-int? ir)
          (int-res-t ir)
          (format-point (int-res-p ir))))


;; basic constatns -----------------------------------------
(define cam-direction (vec 0.0 0.0 1.0))
(define screen-width  640)
(define screen-height 480)

(define col-red (color 255 0 0 0))
(define col-navy (color 0 155  120 0))
(define col-green (color 0 255  0 0))
(define col-blue (color 0 100 200 0))
(define col-black (color 0 0 0 0))


(define sph-list 
  (list (sphere3D (vec 100.0 100.0 0.0) 130 col-red)
        (sphere3D (vec 200 150 22) 120 col-blue)
        (sphere3D (vec 500 300 0) 49 col-navy)))


;; vector stuff --------------------------------------------
(define (scalar-mult v1 v2)
  (+ (* (vec-x v2) (vec-x v1) )
     (* (vec-y v2) (vec-y v1) )
     (* (vec-z v2) (vec-z v1) )))

(define (sqr-norm v)
  (scalar-mult v v))

(define (vec-length v)
  (sqrt (sqr-norm v)))

(define (normalize-vec v)
  (let* ([x (vec-x v)]	[y (vec-y v)] [z (vec-z v)] 
                           [len (vec-length v)])
    (vec (/ x len) (/ y len) (/ z len))))

(define (subtract-vec v2 v1)
  (vec (- (vec-x v2) (vec-x v1) )
       (- (vec-y v2) (vec-y v1) )
       (- (vec-z v2) (vec-z v1) )))

(define (get-closer-res i1 i2)
  (if (and (> (int-res-t i1) (int-res-t i2))
           (int-res-int? i2))
      i2
      i1))

;; sphere3D intersection
(define (screen-ray x y)
  (ray (vec x y -1000.0) cam-direction))

(define null-point
  (point (color 0 0 0 0) 0 0))

(define (hit-sphere3D r s)
  (let* ([dist-vector (subtract-vec (sphere3D-center s) (ray-from r))]
         [B (scalar-mult dist-vector (ray-dir r))]
         [D (+ (* B B) (- (sqr-norm dist-vector)) (sqr (sphere3D-radius s)))])
    (if (> D 0) 
        (let ([t0 (- B (sqrt D))] 
              [t1 (+ B (sqrt D))])
          (if (and (> t0 0.1) 
                   (< t0 t1))
              (int-res #t t0 (point (sphere3D-col s) 0 0))
              (int-res #t t1 (point (sphere3D-col s) 0 0))))
        (int-res #f 0 null-point))))


(define (ray-cast x y object-list)
  (let ([view-ray (screen-ray x y)])
    (for/fold ([closest-int (int-res #f 10000 null-point)])
      ([obj object-list])
      (get-closer-res closest-int 
                      (hit-sphere3D view-ray obj)))))


(require racket/gui)

; Make a frame
(define frame (new frame% [label "racket ray tracer"]
                   [width screen-width]
                   [height screen-height]))

; Make the drawing area
(define canvas (new canvas% [parent frame]))
; Get the canvas's drawing context
(define dc (send canvas get-dc))

(define (put-pixel dc x y col)
  (send dc set-pen 
        (make-object color% (color-r col) (color-g col) (color-b col))
        1
        'solid)
  (send dc draw-point x y))

(define (render-scene object-list dc)
  (for* ([x screen-width]
         [y screen-height])
    (let* ([ray-res (ray-cast x y object-list)]
           [pix-col (point-col (int-res-p ray-res))])
      (put-pixel dc x y pix-col))))

(define (run-tracer)
  ; Show the frame
  (send frame show #t)
  ; Wait a second to let the window get ready
  (sleep/yield 1)
  ; Draw the scene
  ;; (draw-scene dc)
  (render-scene sph-list dc))

(run-tracer)