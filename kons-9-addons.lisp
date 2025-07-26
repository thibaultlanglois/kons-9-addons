(in-package #:kons-9)

;;;---Utilities----------------------------------------------------------

;; to use λ in emacs: (define-key key-translation-map (kbd "C-c l") (kbd "λ"))
(defmacro λ (lambda-list &body code) 
  `(function (lambda ,lambda-list ,@code)))

(defparameter *epsilon* 1.e-7)

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

;;;--Faces---------------------------------------------------------------

(defgeneric adjacent-faces (vertex faces-or-polyh)
  (:documentation "Returns the list of faces adjacent to a vertex among a list 
of faces or a polyhedron.")
  (:method ((vertex-index fixnum) (faces cons))
      (remove-if
       (lambda (f)
         (not (member vertex-index f)))
       faces))
  (:method ((vertex-index fixnum) (a-shape polyhedron))
    (loop
      with adj-faces = nil
      with adj-faces-normals = nil
      for face across (faces a-shape)
      for face-normal across (face-normals a-shape)
      for belongs-to-face = (loop
                              for p in face
                              for is-eq = (= p vertex-index)
                              until is-eq
                              finally (return is-eq))
      do (when belongs-to-face
           (push face adj-faces)
           (push face-normal adj-faces-normals))
      finally (return (values (nreverse adj-faces)
                              (nreverse adj-faces-normals))))))

(defun faces-coplanar-p (normals)
  "Given a list of (face) normals, returns T if they are parallel."
  (loop
    with n0 = (first normals)
    for n in (rest normals)
    for cross = (p:cross n0 n)
    do (unless (p:parallel-p n0 n)
         (format t "prod: ~A × ~A ~A~%" n0 n (p:parallel-p n0 n))
         (return-from faces-coplanar-p nil))
    finally (return t)))

(defun edge-equal (e1 e2)
  (or (and (equal (first e1) (first e2))
           (equal (second e1) (second e2)))
      (and (equal (first e1) (second e2))
           (equal (second e1) (first e2)))))

(defun face-edges (face)
  "Given a face returns a list of pairs of point indices that correspond to 
face' edges."
  (labels ((face-edges-aux
               (face n first)
             (cond ((> n 1)
                    (cons (subseq face 0 2)
                          (face-edges-aux (cdr face) (- n 1) first)))
                   (t (list (list (first face) first))))))
    (face-edges-aux face (length face) (first face))))

(defun polyhedron-edges (polyhedron)
  ;; given a polyhedron returns the list of edges without duplicates.
  (let ((es (loop
              for face across (faces polyhedron)
              append (face-edges face))))
    (remove-duplicates es :test #'edge-equal)))

(defun edge-faces (polyhedron edge)
  ;; given a polyhedron and an edge, returns the list of faces that contain
  ;; EDGE. 
  (loop
    with faces = nil
    for f across (faces polyhedron)
    do (when (member edge (face-edges f)
                     :test #'edge-equal)
         (push f faces))
    finally (return faces)))

(defun sort-edges (edges)
  "Given a list of edges returns a list of contiguous edges.
  for example given:
   ((412 471) (471 259) (71 467) (467 412) (455 313) (10 455) (387 388)
    (63 389) (389 347) (388 89) (73 387) (259 121) (313 73) (121 10)
    (89 63))
  returns:
   ((347 71) (71 467) (467 412) (412 471) (471 259) (259 121)
    (121 10) (10 455) (455 313) (313 73) (73 387) (387 388) (388 89)
    (89 63))
   this is useful for making a curve from a list of edges that
   belong to a contour (for example)"
  (loop
    with sorted-edges = (list (first edges))
    with edges = (rest edges)
    do
       (let* ((e (first (last sorted-edges)))
              (next (find-if (λ (e_) (equal (first e_) (second e)))
                             edges)))
         ;; (format t "e: ~A next: ~A~%" e next)
         ;; (format t "edges: ~A~%" edges)
         ;; (format t "sorted-edges: ~A~%" sorted-edges)
         (if next
             (progn (setq sorted-edges (append sorted-edges (list next)))
                    (setq edges (remove next edges :test #'equal)))
             ))
       ;; (print "contonue?")
       ;; (read)
    while (plusp (length edges))
    finally (return sorted-edges)))

;;;--Polyhedrons---------------------------------------------------------

(defun contour-edges (polyhedron)
  "Returns the list of edges that are on the contour of polyhedron." 
  (loop
    with edges = nil
    for e in (polyhedron-edges polyhedron)
    for faces = (edge-faces polyhedron e)
    do
       (when (= 1 (length faces))
         (push e edges))
    finally (return (sort-edges edges))))

(defun contour-curve (polyhedron)
  "Makes a curve that corresponds to the contour of POLYHEDRON."
  (let* ((edges (contour-edges polyhedron))
         (points-refs (mapcar #'first edges))
         (points (loop
                   for ref in points-refs
                   collect (aref (points polyhedron) ref))))
    (make-instance 'curve
                   :points (apply #'vector points))))

(defun contour-point-refs (polyhedron)
  "Returns a list of points indices that corresponds to the contour of POLYHEDRON."
  (mapcar #'first (contour-edges polyhedron)))

(defun contour-points (polyhedron)
 "Returns a list of points that corresponds to the contour of POLYHEDRON."
  (let* ((edges (contour-edges polyhedron))
         (points-refs (mapcar #'first edges)))
    (loop
      for ref in points-refs
      collect (aref (points polyhedron) ref))))
