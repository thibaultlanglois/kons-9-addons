(in-package #:kons-9)

;;;---Utilities----------------------------------------------------------

;; to use λ in emacs: (define-key key-translation-map (kbd "C-c l") (kbd "λ"))
(defmacro λ (lambda-list &body code) 
  `(function (lambda ,lambda-list ,@code)))


(defgeneric get-shape (name scene-or-group)
  (:documentation
   "Given a scene and a name, returns the shape with that name if it exists.")
  (:method ((name symbol) (a-scene scene))
    (get-shape name (shape-root a-scene)))
  (:method ((name symbol) (group shape-group))
    (loop
      with found = nil
      for c across (children group)
      while (not found)
      do
         (cond ((and (subtypep (type-of c) 'scene-item)
                     (eq (name c) name))
                (setq found c))
               ((subtypep (type-of c) 'shape-group)
                (setq found (get-shape name c))))
      finally (return found))))

(defmacro make-show-hide-function (v n)
  (let ((show-f (intern (strcat "SHOW-" (symbol-name n))))
        (hide-f (intern (strcat "HIDE-" (symbol-name n)))))
    `(progn (defun ,show-f () (setq ,v t))
            (defun ,hide-f () (setq ,v nil)))))

(eval-when (:load-toplevel :compile-toplevel)
  (loop
    for (special-v name) in '((*display-filled?* faces)
                              (*display-wireframe?* wireframe)
                              (*display-points?* points)
                              (*display-ground-plane?* ground)
                              (*display-axes?* axes))
    do (eval `(make-show-hide-function ,special-v ,name))))

(defmacro def-viewpoint (name &key x-rot y-rot side-dist up-dist fwd-dist)
  `(defun ,name ()
     (setq *cam-x-rot* ,(or x-rot *cam-x-rot*)
           *cam-y-rot* ,(or y-rot *cam-y-rot*)
           *cam-side-dist* ,(or side-dist *cam-side-dist*)
           *cam-up-dist* ,(or up-dist *cam-up-dist*)
           *cam-fwd-dist* ,(or fwd-dist *cam-fwd-dist*))
     t))

;;;---Point(s) utilities-------------------------------------------------

(defun is-left (p0 p1 p2)
  (- (* (- (aref p1 0) (aref p0 0))
        (- (aref p2 1) (aref p0 1)))
     (* (- (aref p2 0) (aref p0 0))
        (- (aref p1 1) (aref p0 1)))))

(defgeneric is-point-in-polygon? (points point)
  (:documentation
   "Given POINTS a set of points (list or vector) that belong to the XY plane 
and define a polygon or closed curve (i.e. points are ordered clockwise or 
anti-clockwise) and a POINT a 3D point also in the XY plane, return T if 
POINT is inside the polygon, else return NIL.")
  (:method ((points vector) (p vector))
    (let ((n (length points)))
      (loop
        with winding-number = 0
        for i below n
        for p0 = (aref points i)
        for p1 = (aref points (mod (+ i 1) n))
        do
           (when p1
             (let ((y0 (aref p0 1))
                   (y1 (aref p1 1))
                   (y (aref p 1)))
               (if (<= y0 y)
                   (when (and (> y1 y) (plusp (is-left p0 p1 p)))
                     (incf winding-number))
                   (when (and (<= y1 y) (minusp (is-left p0 p1 p)))
                     (decf winding-number)))))
        finally (return (/= winding-number 0)))))
  (:method ((points cons) (p vector))
    (let ((n (length points)))
      (loop
        with winding-number = 0
        for i below n
        for p0 in points
        for p1 = (nth (mod (+ i 1) n) points)
        do
           (when p1
             (let ((y0 (aref p0 1))
                   (y1 (aref p1 1))
                   (y (aref p 1)))
               (if (<= y0 y)
                   (when (and (> y1 y) (plusp (is-left p0 p1 p)))
                     (incf winding-number))
                   (when (and (<= y1 y) (minusp (is-left p0 p1 p)))
                     (decf winding-number)))))
        finally (return (/= winding-number 0)))))
  (:method ((c curve) (p vector))
    (is-point-in-polygon? (points c) p)))

(defgeneric is-point-in-polygon?* (points point)
  (:documentation
   "Given POINTS a set of co-planar points (list or vector) that define a 
polygon or closed curve (i.e. points are ordered clockwise or 
anti-clockwise) and a POINT a 3D point co-planar with POINTS, return T if 
POINT is inside the polygon, if not return NIL.")
  (:method ((points vector) (point vector))
    (let* ((pa (aref points 0))
           (pb (aref points 1))
           (pc (aref points 2))
           (normal (triangle-normal pa pb pc))
           (points-xy (project-to-xy points normal))
           (p-xy (project-to-xy (list point) normal)))
      (is-point-in-polygon? points-xy p-xy)))
  (:method ((points cons) (point vector))
    (let* ((pa (first points))
           (pb (second points))
           (pc (third points))
           (normal (triangle-normal pa pb pc))
           (points-xy (project-to-xy points normal))
           (p-xy (first (project-to-xy (list point) normal))))
      (format t "normal: ~A~%" normal)
      (format t "points: ~A~%" points)
      (format t "points-xy: ~A~%" points-xy)
      (format t "p: ~A~%" point)
      (format t "p-xy: ~A~%" p-xy)
      (is-point-in-polygon? points-xy p-xy)))
  (:method ((c curve) (p vector))
    (is-point-in-polygon?* (points c) p)))

;;;---Curve utilities----------------------------------------------------

(defgeneric insert-curve (c1 c2 where)
  (:documentation
   "Insert curve C2 into C1 at place or point. C1's point is removed.")
  (:method ((c1 curve) (c2 curve) (i fixnum))
    "Replace the ith point in c1 by c2."
    (let ((l1 (coerce (points c1) 'list))
          (l2 (coerce (points c2) 'list)))
      (setf (points c1)
            (apply #'vector
                   (append (subseq l1 0 i) l2 (subseq l1 (1+ i)))))))
  (:method ((c1 curve) (c2 curve) (p vector))
    "Replace the point P in c1 by c2."
    (let ((l1 (coerce (points c1) 'list))
          (l2 (coerce (points c2) 'list))
          (i (position p (points c1) :test #'equal)))
      (setf (points c1)
            (apply #'vector
                   (append (subseq l1 0 i) l2 (subseq l1 (1+ i))))))))

(defmethod reverse-curve-points ((c curve))
  "Reverse the order of points in C. Return the modified curve."
  (setf (points c)
        (apply #'vector (reverse (coerce (points c) 'list))))
  c)

(defun make-rounded-rectangle-curve (width height radius)
  (let* ((r1 (make-rectangle-curve width height 1))
         (p0 (aref (points r1) 0))
         (p1 (aref (points r1) 1))
         (p2 (aref (points r1) 2))
         (p3 (aref (points r1) 3))
         (arc1 (make-arc-curve (* radius 2) 90 0 10))
         (arc2 (make-arc-curve (* radius 2) 0 90 10))
         (arc3 (make-arc-curve (* radius 2) 0 90 10))
         (arc4 (make-arc-curve (* radius 2) 90 0 10)))
      (translate-to arc1 (p! (- (/ width 2) radius) (- (/ height 2) radius) 0))
      (rotate-to arc2 (p! 0 180 0))
      (translate-to arc2 (p! (- (- (/ width 2) radius)) (- (/ height 2) radius) 0))
      (rotate-to arc3 (p! 0 180 90))
      (translate-to arc3 (p! (- (- (/ width 2) radius)) (- (- (/ height 2) radius)) 0))
      (rotate-to arc4 (p! 0 0 -90))
      (translate-to arc4 (p! (- (/ width 2) radius) (- (- (/ height 2) radius)) 0))
      (apply-transform arc2)
      (apply-transform arc1)
      (apply-transform arc3)
      (apply-transform arc4)
      (insert-curve r1 arc1 p0)
      (insert-curve r1 arc2 p1)
      (insert-curve r1 arc3 p2)
      (insert-curve r1 arc4 p3)
    r1))

;;;----------------------------------------------------------------------
