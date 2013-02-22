#lang racket/gui
(require racket/flonum)
(require racket/future)
(require future-visualizer)

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
(define screen-width  640.0)
(define screen-height 480.0)
(define scene-width  640)
(define scene-height 480)


(define col-red (make-object color% 255 0 0 0))
(define col-navy (make-object color% 0 155  120 0))
(define col-green (make-object color% 0 255  0 0))
(define col-blue (make-object color% 0 100 200 0))
(define col-black (make-object color% 0 0 0 0))

(define sph-list 
  (list (sphere3D (vec 100.0 100.0 0.0) 130.0 col-red)
	  (sphere3D (vec 200.0 150.0 22.0) 120.0 col-blue)
	  (sphere3D (vec 500.0 300.0 0.0) 49.0 col-navy)))

;; vector stuff --------------------------------------------
(define (scalar-mult v1 v2)
  (fl+ (fl* (vec-x v2) (vec-x v1) )
       (fl+   (fl* (vec-y v2) (vec-y v1) )
	      (fl* (vec-z v2) (vec-z v1) ))))

(define (sqr-norm v)
  (scalar-mult v v))

(define (vec-length v)
  (flsqrt (sqr-norm v)))

(define (normalize-vec v)
  (let* ([x (vec-x v)]	[y (vec-y v)] [z (vec-z v)] 
                           [len (vec-length v)])
    (vec (fl/ x len) (fl/ y len) (fl/ z len))))

(define (subtract-vec v2 v1)
  (vec (fl- (vec-x v2) (vec-x v1) )
       (fl- (vec-y v2) (vec-y v1) )
       (fl- (vec-z v2) (vec-z v1) )))

(define (get-closer-res i1 i2)
  (if (and (fl> (int-res-t i1) (int-res-t i2))
           (int-res-int? i2))
      i2
      i1))

;; sphere3D intersection
(define (screen-ray x y)
  (ray (vec  x  y -1000.0) cam-direction))

(define null-point
  (point col-black 0 0))

(define (hit-sphere3D r s)
  (let* ([dist-vector (subtract-vec (sphere3D-center s) (ray-from r))]
         [B (scalar-mult dist-vector (ray-dir r))]
         [D (fl+ (fl* B B) (fl+ (fl- 0.0 (sqr-norm dist-vector)) (flexpt (sphere3D-radius s) 2.0)))])
    (if (fl> D 0.0) 
        (let ([t0 (fl- B (flsqrt D))] 
              [t1 (fl+ B (flsqrt D))])
          (if (and (fl> t0 0.1) 
                   (fl< t0 t1))
              (int-res #t t0 (point (sphere3D-col s) 0.0 0.0))
              (int-res #t t1 (point (sphere3D-col s) 0.0 0.0))))
        (int-res #f 0.0 null-point))))

(define (ray-cast x y object-list)
    (let ([view-ray (screen-ray x y)])
      (let loop ([closest-int  (int-res #f 10000.0 null-point)]
                 [object-list  object-list])
        (cond [(empty? object-list)  closest-int]
              [else
               (define obj (first object-list))
               (loop (get-closer-res closest-int (hit-sphere3D view-ray obj))
                     (rest object-list))]))))


(define (render-half-scene object-list x-start x-end bmp)
    (let x-loop ([x x-start])
      (cond
        [(x . fl< . x-end)
         (let y-loop ([y 0.0])
           (cond
             [(y . fl< . screen-height)
              (let* ([ray-res (ray-cast x y object-list)]
                     [pix-col (point-col (int-res-p ray-res))]
		     [xp (fl->exact-integer x)]
		     [yp (fl->exact-integer y)])
		(send bmp set-pixel xp yp pix-col)
                (y-loop (fl+ y 1.0)))]
             [else  (x-loop (fl+ x 1.0))]))]
        [else  #t])))
 
  ;; (: render-scene ((Listof sphere3D) -> Void))
  (define (render-scene object-list bmp)
    (define screen-width/4 (fl/ screen-width 4.0))
    (define screen-width/2 (fl/ screen-width 2.0))
    (define 3screen-width/4 (fl* screen-width 0.75))
    (define f1
      (future
       (lambda ()
         (render-half-scene object-list
                                  0.0
                                  screen-width/4 bmp))))
    (define f2
      (future
       (lambda ()
         (render-half-scene object-list
                                  screen-width/4
                                  screen-width/2 bmp))))

    (define f3
      (future
       (lambda ()
         (render-half-scene object-list
                                  screen-width/2
                                  3screen-width/4 bmp))))

    (define f4
      (future
       (lambda ()
         (render-half-scene object-list
                                  3screen-width/4
                                  screen-width bmp))))


    (touch f1)
    (touch f2)
    (touch f3)
    (touch f4)
    (void))

(define (render-half-scene-dummy object-list x-start x-end)
    (let x-loop ([x x-start])
      (cond
        [(x . fl< . x-end)
         (let y-loop ([y 0.0])
           (cond
             [(y . fl< . screen-height)
              (let* ([ray-res (ray-cast x y object-list)]
                     [pix-col (point-col (int-res-p ray-res))])
                (y-loop (fl+ y 1.0)))]
             [else  (x-loop (fl+ x 1.0))]))]
        [else  #t])))
 
  (define (render-scene-dummy object-list)
    (define screen-width/4 (fl/ screen-width 4.0))
    (define screen-width/2 (fl/ screen-width 2.0))
    (define 3screen-width/4 (fl* screen-width 0.75))
    (define f1
      (future
       (lambda ()
         (render-half-scene-dummy object-list
                                  0.0
                                  screen-width/4))))
    (define f2
      (future
       (lambda ()
         (render-half-scene-dummy object-list
                                  screen-width/4
                                  screen-width/2))))

    (define f3
      (future
       (lambda ()
         (render-half-scene-dummy object-list
                                  screen-width/2
                                  3screen-width/4))))

    (define f4
      (future
       (lambda ()
         (render-half-scene-dummy object-list
                                  3screen-width/4
                                  screen-width))))


    (touch f1)
    (touch f2)
    (touch f3)
    (touch f4)
    (void))

(define (make-scene-bitmap w h)
  (new bitmap-dc% [bitmap (make-object bitmap% w h)])
)

(define (run-no-render)
  (render-scene-dummy sph-list))

(require racket/gui)

(define (run-tracer)
  (let* ([frame (new frame% [label "racket ray tracer"]
		     [width scene-width]
		     [height scene-height])]
	 [canvas (new canvas% [parent frame])]
	 [dc (send canvas get-dc)]
	 [bmp (make-scene-bitmap scene-width scene-height)])
; Show the frame
    (send frame show #t)
; Wait a second to let the window get ready
    (sleep/yield 1)
; Draw the scene
    (printf "rendering scene...")
    (time
     (render-scene sph-list bmp))
    (send dc draw-bitmap (send bmp get-bitmap) 0 0)
    frame))

;; (run-tracer)
