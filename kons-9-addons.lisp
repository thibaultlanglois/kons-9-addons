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

;;;----------------------------------------------------------------------
