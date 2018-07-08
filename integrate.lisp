;;; -*- Package: PROJ -*-

;;;****************************************************************
;;;Patches for Nichael
;;;****************************************************************


(defmethod (:associated-window basic-projector) ()
  window)

(defmethod (:dump-data-to-projector basic-projector) (OTHER-PROJECTOR)
  (send OTHER-PROJECTOR :initialize-data MAX-NUMBER-POINTS)
  (loop for INDEX below CURRENT-POINT-PNTR
	do (send OTHER-PROJECTOR :load-one-point
		 (raw-x INDEX) (raw-y INDEX) (raw-z INDEX) (point-data INDEX))))

(defmethod (:draw-axis alternating-b&w-projector) (&optional PENDING?)
  (let ((DRER (if PENDING? (pending-drawer) (displaying-drawer))))
    (let ((LENG (* SCALE-FACTOR 10)))
      (multiple-value-bind (ORIG-X ORIG-Y)
	  (multiple-value-call
	    SCREENER :blast-project-one-point
	    (send TRANSFORMER :blast-transform-one-point 0 0 0))
	(multiple-value-bind (X-X X-Y)
	    (multiple-value-call
	      SCREENER :blast-project-one-point
	      (send TRANSFORMER :blast-transform-one-point LENG 0 0))
	  (send DRER :draw-axis-line ORIG-X ORIG-Y X-X X-Y))
	(multiple-value-bind (Y-X Y-Y)
	    (multiple-value-call
	      SCREENER :blast-project-one-point
	      (send TRANSFORMER :blast-transform-one-point 0 LENG 0))
	  (send DRER :draw-axis-line ORIG-X ORIG-Y Y-X Y-Y))
	(multiple-value-bind (Z-X Z-Y)
	    (multiple-value-call
	      SCREENER :blast-project-one-point
	      (send TRANSFORMER :blast-transform-one-point 0 0 LENG))
	  (send DRER :draw-axis-line ORIG-X ORIG-Y Z-X Z-Y)))))
  )

(defmethod (:convert-world-coord-to-scr-coord alternating-b&w-projector)
	   (WORLD-X WORLD-Y WORLD-Z)
  (declare (values SCR-X SCR-Y))
  (multiple-value-call
    SCREENER :blast-project-one-point
    (send TRANSFORMER :blast-transform-one-point
	  (* scale-factor WORLD-X)
	  (* scale-factor WORLD-Y)
	  (* scale-factor WORLD-Z))))

(defmethod (:draw-axis-line b&w-array-drawer) (X0 Y0 X1 Y1)
  (send WINDOW :draw-line X0 Y0 X1 Y1))

(defmethod (:prepare-for-immediate-point-plotting alternating-b&w-projector)
	   (NUM-POINTS LINE-MODE SCALE &rest ROTATION-COMMANDS)
  (send SELF :initialize-data NUM-POINTS)
  (send SELF :set-line-mode LINE-MODE)
  (send SELF :set-scale-factor SCALE nil)  
  (setup-transformation-matrix ROTATION-COMMANDS)
  (prepare-for-immediate-draw)
  (send SELF :draw-axis))

;;;++This does not multiply the scale-factor like :convert-world-coord-to-scr-coord
(defmethod (:pos-to-screen basic-alternating-color-projector) (x y z)
  (declare (values SCR-X SCR-Y))
  (multiple-value-call SCREENER :blast-project-one-point
		       (send TRANSFORMER :blast-transform-one-point x y z)))

;;;So we need this guy
(defmethod (:conv-to-scale basic-alternating-color-projector) (x y z)
  (declare (values sx sy sz))
  (values
    (/ x scale-factor)
    (/ y scale-factor)
    (/ z scale-factor)))


;;;++This does not multiply the scale-factor like :convert-world-coord-to-scr-coord
(defmethod (:pos-to-screen  alternating-b&w-projector) (x y z)
  (declare (values SCR-X SCR-Y))
  (multiple-value-call SCREENER :blast-project-one-point
		       (send TRANSFORMER :blast-transform-one-point x y z)))

;;;So we need this guy
(defmethod (:conv-to-scale alternating-b&w-projector) (x y z)
  (declare (values sx sy sz))
  (values
    (/ x scale-factor)
    (/ y scale-factor)
    (/ z scale-factor)))


;;;****************************************************************

(defvar *PROJECTOR-B&W-WINDOW* (tv:make-window 'tv:window
					       :blinker-p nil
					       :borders  nil
					       :label    nil
					       :save-bits t)
  )

(defvar *PROJECTOR-COLOR-WINDOW*
	(tv:make-window 'tv:window
			:superior (color:find-color-screen :create-p t)
			:blinker-p nil
			:borders  nil
			:label    nil)
  )

(defvar *PROJECTOR-COLOR-WINDOW1*
	(tv:make-window 'tv:window
			:superior (color:find-color-screen :create-p t)
			:blinker-p nil
			:borders  2
			:label    nil)
  )

(defvar *MULTICOLOR-PROJECTOR* (make-alternating-multiple-color-projector))
(send *MULTICOLOR-PROJECTOR* :setup-screen)
(send *MULTICOLOR-PROJECTOR* :set-default-alus)
(send *MULTICOLOR-PROJECTOR* :setup-window-info *PROJECTOR-COLOR-WINDOW*)

(defvar *MULTICOLOR-PROJECTOR1* (make-alternating-multiple-color-projector))
(send *MULTICOLOR-PROJECTOR1* :setup-screen)
(send *MULTICOLOR-PROJECTOR1* :set-default-alus)
(send *MULTICOLOR-PROJECTOR1* :setup-window-info *PROJECTOR-COLOR-WINDOW1*)

#+ignore 
(setup-amcdd-colors
  '(( 00.  0. 1023.)
    (1023.  00.  00.)
  
    (00.  1023.    0.)
    ( 0.  1023. 	   1023.) (1023.  1023.    0.)
    (1023. 0. 	   1023.)
    ;( 0.  1023. 	   1023.)
    ( 307. 1023. 	   0.)
	  
    (   0.  1023.    0.)
    (   0.  1023.  307.)
    (   0.  1023.  614.)
    (   0.  1023. 1023.)
    (   0.   614. 1023.)
    (   0.   307. 1023.)
	  
    ( 614.   614. 1023.)
    (1023.  1023. 1023.)))
 

(defvar *alternating-B&W-PROJECTOR* (make-b&w-alternating-projector))
(send *alternating-B&W-PROJECTOR* :setup-window-info *PROJECTOR-B&W-WINDOW*)

#||
(defvar *FAST-PROJECTOR* (make-instance 'fast-projector))
(defvar *FAST-PROJECTOR-B&W-WINDOW* (tv:make-window 'tv:window
						    :blinker-p nil
						    :borders  nil
						    :label    nil
						    :save-bits t
						    :width 512.
						    :height 512.)
  )
(send *FAST-PROJECTOR* :setup-window-info *FAST-PROJECTOR-B&W-WINDOW*)
||#
(defvar p)
(defvar w)
(defvar p1)
(defvar w1)
;;(setq w *FAST-PROJECTOR-B&W-WINDOW*)
(setq w *PROJECTOR-COLOR-WINDOW* #+ignore *PROJECTOR-B&W-WINDOW*)
(setq p *MULTICOLOR-PROJECTOR* #+ignore *DRIVEN-COLOR-PROJECTOR*
      #+ignore *DRIVEN-B&W-PROJECTOR*)

(setq p1 *MULTICOLOR-PROJECTOR1*)
(setq w1 *PROJECTOR-COLOR-WINDOW1*)

;(send p :driver-loop)

(DEFFLAVOR 3d-equation
	((scale 1)
	 (conv (/ -180. pi))
	 (pinc 1)
	 (x-offset 1/2)
	 (y-offset 1/2)
	 (delta 0.1)
	 (rotx -45)
	 (roty -45)
	 (rotz -45))
	()
  (:conc-name nil)
  :INITABLE-INSTANCE-VARIABLES :readable-instance-variables :writable-instance-variables)

(defmethod (offsets 3d-equation) (window)
  (multiple-value-bind (x y) (send window :inside-size)
    (values (* x x-offset) (* y y-offset))))

(defmethod (VERY-SIMPLE-INTEGRATE 3d-equation) (p w ix iy iz transient total int-color)
  w
    #+ignore
    (send p :prepare-for-shuffle rtot :points 10)
    (loop with x = ix
	  with y = iy
	  with z = iz
	  with angx and angy
	  with rinc = 0
	  for i from 0 below (+ transient total)
	  do (multiple-value-setq (ix iy iz)
	       (integration-step self delta x y z))
	  when (> i transient)
	    when (zerop (mod i pinc))
	      do (setq angx (atan (- iy y) (- iz z))) and
	      do (setq angy (atan (- ix x) (- iz z))) and
	      do (send p :add-and-plot-immediate ix iy iz int-color)
		 #+ignore
		 (send p :add-one-point-and-shuffle rinc ix iy iz t
		       :transx (- x) :transy (- y) :transz (- z)
		       ;:rotx (+ (* conv angx) 0) :roty (+ (* conv angy) 0)
		       ) and
	      do (incf rinc)
	  do (setq x ix y iy z iz)))

(defmethod (simple-integrate 3d-equation) (p w ix iy iz iter-inc transient
					     total int-color)
  w conv
  (loop with x = ix
	  with y = iy
	  with z = iz
	  with rinc = 0
 	  for i from 0 below (+ transient total) by iter-inc
	  do (multiple-value-setq (ix iy iz)
	       (loop with a and b and c
		     for ixx in x and
		     for iyy in y and
		     for izz in z
		     do (multiple-value-setq (a b c)
			  (integration-step self delta ixx iyy izz))
		     collect a into xv
		     collect b into yv
		     collect c into zv
		     finally (return (values xv yv zv))))
	  when (> i transient)
	    when (zerop (mod i pinc))
	      do (loop for i from int-color
		       for xv in ix
		       for yv in iy
		       for zv in iz
		       do (send p :add-and-plot-immediate xv yv zv i)
			  #+IGNORE
			  (send p :add-and-plot-single-point rinc xv yv zv i)
			  ) and
	  do (incf rinc iter-inc)
	  do (setq x ix y iy z iz)))

(DEFFLAVOR lorenz
	((scale 15)
	 (delta .005)
	 (x-offset .3)
	 (y-offset .3)
	 (rotz 0)
	 (pinc 2)
	 (s 10.0)
	 (r 28.0)
	 (b 8/3))
	(3d-equation)
  (:conc-name nil)
  :INITABLE-INSTANCE-VARIABLES :readable-instance-variables :writable-instance-variables)


(defmethod (integration-step lorenz) (delta x y z)
  (values  #x + delta*s*(y - x) 
	    #y + delta*(r*x - y - x*z) 
	    #z + delta*(x*y - b*z)  ))

(DEFFLAVOR rossler
	((scale 100)
	 (delta 0.01)
	 (x-offset 1/2)
	 (y-offset 1/2)
	 (pinc 5)
	 (rotz 0)
	 (a  .398 #+ignore 3/8)
	 (b 2)
	 (c 4))
	(3d-equation)
  (:conc-name nil)
  :INITABLE-INSTANCE-VARIABLES :readable-instance-variables :writable-instance-variables)


(defmethod (integration-step rossler) (delta x y z)
  (values #x - delta*(y + z)
	    #y + delta*(x + a*y) 
	    #z + delta*(b + z*(x - c)) ))

(DEFFLAVOR rossler-corkscrew
	((scale 75)
	 (delta 0.01)
	 (x-offset .3)
	 (y-offset .3)
	 (pinc 5)
	 (rotz 0)
	 (a   #+ignore .398 .55 #+ignore 3/8)
	 (b 2)
	 (c 4))
	(rossler)
  (:conc-name nil)
  :INITABLE-INSTANCE-VARIABLES :readable-instance-variables :writable-instance-variables)

(DEFFLAVOR forced-pendulum
	((scale 5)
	 (delta .1)
	 (a 0.5)
	 (b 0.1))
	(3d-equation)
  (:conc-name nil)
  :INITABLE-INSTANCE-VARIABLES :readable-instance-variables :writable-instance-variables)

(defmethod (integration-step forced-pendulum) (delta x y z)
  (values   #x + delta*y
	    #y - delta*(a*a + b*cos(z))*sin(x) 
	    #mod (z + delta,pi*2)))
  
(defun integrate (fcn)
  (let* ((eqn (make-instance fcn))
	 (ix 1.0) (iy 1.0) (iz 1.0)
	 (rtot 10000)
	 (transient 0 #+ignore 15000)
	 (total (1+ (* rtot (pinc eqn))))
	 )
    (send w :expose)
    (multiple-value-bind (x-offset y-offset) (offsets eqn w)
      (send p :set-scr-cent 
	    :cent-x x-offset 
	    :cent-y y-offset))
    (send p :prepare-for-immediate-point-plotting
	  rtot '(:points 2) (scale eqn) :roty (roty eqn) :rotx (rotx eqn) :rotz (rotz eqn)
	  )
    (very-simple-integrate eqn p w ix iy iz transient total 0)))



(defun integrate1 (fcn)
  (let* ((eqn (make-instance fcn))
	 (ix '(1.0 1.001 1.002)) (iy '(1.0 1.001 1.002)) (iz '(1.0 1.001 1.002))
	 (iter-inc (length ix))
	 (rtot (* iter-inc 5000))
	 (transient (* iter-inc 000))
	 (total (1+ (* rtot (pinc eqn)))))
    (send w :expose) (send w :clear-window)
    (multiple-value-bind (x-offset y-offset) (offsets eqn w)
      (send p :set-scr-cent 
	    :cent-x x-offset 
	    :cent-y y-offset))
    (send p :prepare-for-immediate-point-plotting
	  rtot '(:points 2) (scale eqn) :roty (roty eqn) :rotx (rotx eqn) :rotz (rotz eqn)
	  )
    (simple-integrate eqn p w  ix iy iz iter-inc transient total 0)))

(defun intmouse (fcn &optional (over nil) (points 200))
  (let* ((eqn (make-instance fcn))
	 (ix nil) (iy nil) (iz nil)
	 (iter-inc (length ix))
	 (rtot 10000 #+ignore (* iter-inc 5000))
	 (transient (* iter-inc 000))
	 (total (1+ (* rtot (pinc eqn)))))
    (send w :expose)
    (unless over
      (send w :clear-window)
      (multiple-value-bind (x-offset y-offset) (offsets eqn w)
	(send p :set-scr-cent 
	      :cent-x x-offset 
	      :cent-y y-offset))
      (send p :prepare-for-immediate-point-plotting
	    rtot '(:points 2) (scale eqn) :roty (roty eqn)
	    :rotx (rotx eqn) :rotz (rotz eqn)))
    (loop 
      do (multiple-value-bind (x y z)
	     (mouse-specify-3d-point)
	   (setq ix nil) (setq iy nil) (setq iz nil)
	   (push x ix) (push y iy) (push z iz)
	   (setq iter-inc (length ix))
	   (setq rtot (* iter-inc points))
	   (setq total (1+ (* rtot (pinc eqn))))
	   (simple-integrate eqn  p w ix iy iz
			   iter-inc  transient total 4)))))

(defun intmouse1 (fcn &optional (over nil) (points 200))
  (let* ((eqn (make-instance fcn))
	 (ix nil) (iy nil) (iz nil)
	 (iter-inc (length ix))
	 (rtot 10000 #+ignore (* iter-inc 5000))
	 (transient (* iter-inc 000))
	 (total (1+ (* rtot (pinc eqn)))))
    (send w :expose)
    (send w1 :expose)
    (send w1 :clear-window)
    (multiple-value-bind (x-offset y-offset) (offsets eqn w1)
	(send p1 :set-scr-cent 
	      :cent-x x-offset 
	      :cent-y y-offset))
    (send p1 :prepare-for-immediate-point-plotting
	    rtot '(:points 2) (scale eqn) :roty (+ (roty eqn) 0)
	    :rotx (rotx eqn) :rotz (+ (rotz eqn) 90))
    (send p :dump-data-to-projector p1)
    (send p1 :transform-project-and-draw nil
	  :roty (+ (roty eqn) 0) :rotx (rotx eqn) :rotz (+ (rotz eqn) 90))
    ;(send p1 :rotate-project-and-draw nil
    (unless over
      (send w :clear-window)
      (multiple-value-bind (x-offset y-offset) (offsets eqn w)
	(send p :set-scr-cent 
	      :cent-x x-offset 
	      :cent-y y-offset))
      (send p :prepare-for-immediate-point-plotting
	    rtot '(:points 2) (scale eqn) :roty (roty eqn)
	    :rotx (rotx eqn) :rotz (rotz eqn)))
    (loop 
      do (multiple-value-bind (x y z)
	     (mouse-specify-3d-point)
	   (setq ix nil) (setq iy nil) (setq iz nil)
	   (push x ix) (push y iy) (push z iz)
	   (setq iter-inc (length ix))
	   (setq rtot (* iter-inc points))
	   (setq total (1+ (* rtot (pinc eqn))))
	   (simple-integrate eqn  p w ix iy iz
			   iter-inc  transient total 4)
	   (simple-integrate eqn  p1 w1 ix iy iz
			   iter-inc  transient total 4)))))

(defun bar ()
  (loop do
  (loop for ang from 0 by 20 ;below 50
	do (when (send *terminal-io* :listen) (return-from bar))
	do (send *multicolor-projector* :transform-project-and-draw t
		 :rotz (* 0.3 ang) :roty (* 2 ang)))))

(defvar *AGB-DATA1* (with-open-file (STRM "CONGER:>Nichael>new-projector>agb-data-file.lisp") (read STRM)))
;;;

(defun fast-projector-agb-demo (&optional (DATA-LIST *AGB-DATA1*))
  (send *FAST-PROJECTOR* :setup DATA-LIST
	:window *FAST-PROJECTOR-B&W-WINDOW*
	:scale-factor 10.0)
  (loop (loop for Y-INDEX below 72
	      do (loop for X-INDEX below 72
		       do (send *FAST-PROJECTOR* :transform-and-draw
				X-INDEX (mod (+ X-INDEX X-INDEX Y-INDEX) 72)))))
  )

(defun integrate-fast (fcn)
  (let* ((eqn (make-instance fcn))
	 (ix 1.0) (iy 1.0) (iz 1.0)
	 (rtot 10000)
	 (p *FAST-PROJECTOR*)
	 (w *FAST-PROJECTOR-B&W-WINDOW*)
	 (transient 0 #+ignore 15000)
	 (total (1+ (* rtot (pinc eqn))))
	 )
    (send w :expose)
    (multiple-value-bind (x-offset y-offset) (offsets eqn w)
      (send p :set-scr-cent 
	    :cent-x x-offset 
	    :cent-y y-offset))
    (send p :prepare-for-immediate-point-plotting
	  rtot '(:points 2) (scale eqn) :roty (roty eqn) :rotx (rotx eqn) :rotz (rotz eqn)
	  )
    (very-simple-integrate eqn p w ix iy iz transient total 0)))

(defmethod (fast-INTEGRATE 3d-equation) (p w ix iy iz transient total int-color)
  w
    #+ignore
    (send p :prepare-for-shuffle rtot :points 10)
    (loop with x = ix
	  with y = iy
	  with z = iz
	  with angx and angy
	  with rinc = 0
	  for i from 0 below (+ transient total)
	  do (multiple-value-setq (ix iy iz)
	       (integration-step self delta x y z))
	  when (> i transient)
	    when (zerop (mod i pinc))
	      do (setq angx (atan (- iy y) (- iz z))) and
	      do (setq angy (atan (- ix x) (- iz z))) and
	      do (send p :add-and-plot-immediate ix iy iz int-color)
		 #+ignore
		 (send p :add-one-point-and-shuffle rinc ix iy iz t
		       :transx (- x) :transy (- y) :transz (- z)
		       ;:rotx (+ (* conv angx) 0) :roty (+ (* conv angy) 0)
		       ) and
	      do (incf rinc)
	  do (setq x ix y iy z iz)))

#+ignore
(fast-projector-agb-demo)
#||

(defmethod (:initialize-data basic-fast-projector) (NUMBER-OF-POINTS)
  (send SELF :set-number-points NUMBER-OF-POINTS)
  (setq CURRENT-POINT-PNTR 0))

(defmethod (:load-one-point basic-fast-projector) (XXX YYY ZZZ)
  (cond ((< CURRENT-POINT-PNTR MAX-NUMBER-POINTS)
	 (load-one-data-point CURRENT-POINT-PNTR XXX YYY ZZZ)
	 (convert-raw-data-1 CURRENT-POINT-PNTR) 
	 (incf CURRENT-POINT-PNTR))
	(t
	 (beep))))
(defmethod (:setup-simple-draw simple-drawer-mixin)
	   (DATA-LIST &key WINDOW SCALE-FACTOR (X-OFFSET 0.) (Y-OFFSET 0.) (Z-OFFSET 0.))
  (when WINDOW
    (send SELF :setup-window-info WINDOW))
  (send SELF :set-initial-offsets :xoff X-OFFSET :yoff Y-OFFSET :zoff Z-OFFSET)

  (send SELF :load-point-list DATA-LIST)

  (when SCALE-FACTOR
    (send SELF :set-scale-factor SCALE-FACTOR))
  (send SELF :setup-bit-array-parameters)

  (setup-axis-values)


  (when WINDOW
    (send WINDOW :expose))
  )
||#
			   
(defun find-plane (p)
  (sort (mapcar #'(lambda (data)
		      (multiple-value-bind (x0 y0)
			  (send p :pos-to-screen 0 0 0)
			(multiple-value-bind (x y)
			    (send p :pos-to-screen  (first data) (second data) (third data))
			  (setq x (- x x0))
			  (setq y (- y y0))
			  (list (sqrt (+ (* x x) (* y y))) (fourth data)))))
		      '((100 0 0 x) (0 100 0 y) (0 0 100 z))) #'> :key #'first))

;;;****************************************************************
;;; 3D mouse interaction
;;;****************************************************************
(DEFFLAVOR 3d-axis-blinker
	((for-z nil)
	 (plane-offset 0)
	 (curr-x 0)
	 (curr-y 0)
	 (curr-z 0)
	 (x-scale 1.0)
	 (y-scale 1.0)
	 (z-scale 1.0)
	 (x-scale-offset 0)
	 (y-scale-offset 0)
	 (z-scale-offset 0)
	 (associated-blinkers nil)
	 (blink-following nil)
	 (projector nil))
	(tv:MOUSE-BLINKER-MIXIN  tv:CHARACTER-BLINKER)
  :INITABLE-INSTANCE-VARIABLES :settable-instance-variables)



(defmethod (:pos-to-screen 3d-axis-blinker) (x y z)
  (declare (values SCR-X SCR-Y))
  (send projector :pos-to-screen x y z))

(defmethod (:position 3d-axis-blinker) ()
  (values curr-x curr-y curr-z))
 

(defmethod (:draw-line 3d-axis-blinker) (from-x from-y to-x to-y
					&optional (alu :draw) (draw-end-point t))
  (let ((tv:currently-prepared-sheet tv:sheet)
	(tv:screen tv:sheet))
    (multiple-value-bind (from-x from-y to-x to-y)
	(tv::clip-and-offset-line-internal tv:sheet from-x from-y to-x to-y)
       (when
	 from-x
	(tv:sheet-draw-line from-x from-y to-x to-y  alu draw-end-point tv:sheet)))))

(defmethod (:draw-floating-axis 3d-axis-blinker) (x-off y-off)
  (macrolet ((with-offsets (&body body)
	       `(multiple-value-bind (xp yp)
		    ,@body
		  (values (- (+ x-off xp) x-org)
			  (- (+ y-off yp) y-org)))))
    (multiple-value-bind (x-size y-size) (send tv:sheet :inside-size)
      x-size y-size
      (let ((LENG 10000))
	(multiple-value-bind (x-org y-org)
	    (send self :pos-to-screen  0 0 0)
	  (multiple-value-bind (X-X X-Y)
	      (with-offsets (send self :pos-to-screen  LENG 0 0))
	    (multiple-value-bind (X-Xp X-Yp)
		(with-offsets (send self :pos-to-screen  (- LENG) 0 0))
	      (send self :draw-line X-Xp X-Yp X-X X-Y tv:blinker-alu)))
	  (multiple-value-bind (Y-X Y-Y)
	      (with-offsets (send self :pos-to-screen  0 LENG 0))
	    (multiple-value-bind (Y-Xp Y-Yp)
		(with-offsets (send self :pos-to-screen  0 (- LENG) 0))
	      (send self :draw-line Y-Xp Y-Yp Y-X Y-Y tv:blinker-alu)))
	  (multiple-value-bind (Z-X Z-Y)
	      (with-offsets (send self :pos-to-screen  0 0 LENG))
	    (multiple-value-bind (Z-Xp Z-Yp)
		(with-offsets (send self :pos-to-screen  0 0 (- LENG)))
	      (send self :draw-line Z-Xp Z-Yp Z-X Z-Y tv:blinker-alu))))))))

(defmethod (:set-scaling 3d-axis-blinker) ()
  (multiple-value-bind (x0 y0)
      (send self :pos-to-screen 0 0 0)
    x0 y0
    (multiple-value-bind (xmax ymax)
	(send tv:sheet :inside-size)
      (multiple-value-bind (xcx ycx)
	  (send self :pos-to-screen xmax 0 0)
	(multiple-value-bind (xcy ycy)
	    (send self :pos-to-screen 0 ymax 0)
	  (multiple-value-bind (xcz ycz)
	      (send self :pos-to-screen 0 0 xmax)
	    ;(setq x-scale-offset 0 y-scale-offset 0 z-scale-offset 0)
	    (setq x-scale-offset (- xmax)
		  y-scale-offset (- ymax)
		  z-scale-offset (- xmax))
	    	    
	    #+ignore
	    (setq x-scale 1 y-scale 1 z-scale 1)
	    (setq x-scale (/ xmax (* .5 (max xcx ycx)))
		  y-scale (/ ymax (* .5 (max xcy ycy)))
		  z-scale (/ xmax (* .5 (max xcz ycz ))))))))))


(DEFMETHOD (:compute-x-y-z  3d-axis-blinker) ()
  (cond (blink-following
	 (values curr-x curr-y curr-z))
	(for-z
	 (multiple-value-bind (x0 y0)
	     (send self :pos-to-screen 0 0 0)
	   (let ((xd plane-offset)
		 (yd (- y0 (+ (* y-scale tv:y-pos) y-scale-offset)))
		 (zd (- (+ (* z-scale tv:x-pos) z-scale-offset) x0)))
	     (values xd yd zd))))
	(T (multiple-value-bind (x0 y0)
	     (send self :pos-to-screen 0 0 0)
	   (let ((xd (- (+ (* x-scale tv:x-pos) x-scale-offset) x0))
		 (yd (- y0 (+ (* y-scale tv:y-pos) y-scale-offset)))
		 (zd plane-offset))
	     (values xd yd zd))))))

(DEFMETHOD (:blink 3d-axis-blinker) ()
  (multiple-value-bind (xd yd zd)
      (send self :compute-x-y-z)
    (multiple-value-bind (x y)
	(send self :pos-to-screen xd yd zd)
      (send self :draw-with-offset x y tv:index tv:font)
      (send self :draw-floating-axis x y)
      (setq curr-x xd curr-y yd curr-z zd))))



(defmethod (:blink 3d-axis-blinker :after) ()
  (cond (for-z
	 (multiple-value-bind (xd yd zd)
	     (send self :compute-x-y-z)
	   (multiple-value-bind (x y)
	       (send self :pos-to-screen xd yd 0)
	     (send self :draw-with-offset x y (char-code #\Y) fonts:cptfontb))
	   (multiple-value-bind (x y)
	       (send self :pos-to-screen xd 0 zd)
	     (send self :draw-with-offset x y (char-code #\Z) fonts:cptfontb))))
	(t
	 (multiple-value-bind (xd yd zd)
	     (send self :compute-x-y-z)
	   (multiple-value-bind (x y)
	       (send self :pos-to-screen xd 0 zd)
	     (send self :draw-with-offset x y (char-code #\X) fonts:cptfontb))
	   (multiple-value-bind (x y)
	       (send self :pos-to-screen 0  yd zd)
	     (send self :draw-with-offset x y (char-code #\Y) fonts:cptfontb))))))


(defwhopper (:blink 3d-axis-blinker) ()
  (continue-whopper)
  (dolist (bl associated-blinkers)
    (send bl :set-3d-pos curr-x curr-y curr-z)
    (send bl :blink)))

(defmethod (:set-3d-pos 3d-axis-blinker) (x y z)
  (setq curr-x x curr-y y curr-z z))

(DEFMETHOD (:mark 3d-axis-blinker) ()
  (multiple-value-bind (sx sy sz)
      (send self :conv-to-scale)
    (send projector :add-and-plot-immediate sx sy sz 14)))

(DEFMETHOD (:conv-to-scale 3d-axis-blinker) ()
  (multiple-value-bind (x y z)
      (send self :position)
    (multiple-value-bind (sx sy sz)
	(send projector :conv-to-scale x y z)
      (values sx sy sz))))


(DEFMETHOD (:draw-with-offset 3d-axis-blinker) (x y font-index font)
  (if (eq font tv:font)
      (tv:sheet-draw-glyph font-index font (- x tv::X-OFFSET) (- y tv::Y-OFFSET)
			 tv:blinker-alu
			 tv:sheet)
      (let ((x-off (let ((fit (zl:font-indexing-table font)))
		     (if fit (- (aref fit (1+ font-index)) (aref fit font-index))
			 (zl:font-raster-width font))))
	    (y-off (round (/ (zl:font-raster-height font)))))
	(setq x-off (round (/ x-off 2)))
	(tv:sheet-draw-glyph font-index font (- x x-off) (- y y-off)
			     tv:blinker-alu
			     tv:sheet))))

(defmethod (:go-to-3d 3d-axis-blinker) (x y z)
 (multiple-value-bind (x y)
     (send self :pos-to-screen x y z)
   (TV:MOUSE-WARP x y )))

#+ignore
(defwhopper (:go-to-3d 3d-axis-blinker) (x y z)
  (continue-whopper x y z)
  (dolist (bl associated-blinkers)
    (send bl :go-to-3d x y z)))

#+ignore
(defwhopper (:set-visibility 3d-axis-blinker) (val)
  (continue-whopper val)
  (dolist (bl associated-blinkers)
    (send bl :set-visibility val)))

(defun mouse-specify-3d-point (&optional (plot? t) (sheet w) abortable)
  (declare (special w p))
  (send w :expose)
  abortable
  (multiple-value-bind (x y z)
      (tv:mouse-set-sheet-then-call sheet 'mouse-specify-3d-point1 sheet #+ignore abortable)
    (when plot? (send p :add-and-plot-immediate x y z 14))
    (values x y z)))


#+ignore
(defun window-set-blinker (type window &optional x-off y-off  (mouse tv::main-mouse))
  (let ((bl (tv:mouse-get-blinker type window tv::main-mouse)))
    (and x-off (send bl ':set-offsets x-off y-off))
    (send bl ':set-cursorpos (- (tv:mouse-x mouse) (or x-off 0)
				(tv:sheet-inside-left window))
			     (- (tv:mouse-y mouse) (or y-off 0)
				(tv:sheet-inside-top window)))
    (send bl ':set-visibility t)
    (send bl ':track-mouse)
    bl))

(defmethod (:initial-sets 3d-axis-blinker) (p)
  (send self :set-visibility nil)
  (send self :set-projector p)
  (send self :set-curr-x 0)
  (send self :set-curr-y 0)
  (send self :set-curr-z 0)
  (send self :set-plane-offset 0)
  (send self :set-scaling))

(defmethod (:loop-sets 3d-axis-blinker) (set-for-z)
  (multiple-value-bind (x y z)   (send self :position)
    y z
    (send self :set-visibility nil)
    (send self :set-for-z set-for-z)
    (send self :set-visibility t)
    (send self :go-to-3d x y z)))

(defmethod (:loop-end 3d-axis-blinker) (set-for-z p sheet)
  (multiple-value-bind (x y z)     (send self :position)
    (multiple-value-bind (xp yp)   (send p :pos-to-screen x y z)
      (tv:prepare-sheet (sheet)
	#+ignore
	(send self :draw-with-offset Xp Yp (if set-for-z 118 100) fonts:mouse)))
    ;(send self :mark)
    (send self :set-plane-offset (if set-for-z z  x))))


; (tv:key-state #\space)
; (tv:wait-for-mouse-button-down "Select X-Y Shift for Z" nil nil mouse)
(defun mouse-specify-3d-point1 (&optional (sheet w)
				&aux (mouse (tv:sheet-mouse sheet)))
  (declare (special w p w1 p1))
  (tv:with-this-mouse-and-buttons-grabbed (mouse)
    (tv:MOUSE-SET-BLINKER :3d-axis  6 6)
    (loop with bl = (tv:MOUSE-GET-BLINKER :3d-axis w MOUSE) and button and foo = 0
	  initially (send bl :initial-sets p)
	  until (and button (zl:bit-test 2 button))
	  when (not (zerop (mod foo 2)))
	    do (send bl :loop-sets t) and
	    do (setq button (tv:wait-for-mouse-button-down
			      "Select Y-Z Click for X" nil nil mouse)) and
	    do (incf foo) and
	  do (send bl :loop-end t p sheet)
	  else do (send bl :loop-sets nil) and
	       do (setq button (tv:wait-for-mouse-button-down
				  "Select X-Y. Click for Z. M to end." nil nil mouse)) and
	       do (incf foo) and
	  do (send bl :loop-end nil p sheet)
	  finally  (return (send bl :conv-to-scale)))))

#+ignore
(defun mouse-specify-3d-point1 (&optional (sheet w)
				&aux (mouse (tv:sheet-mouse sheet)))
  (declare (special w p w1 p1))
  (tv:with-this-mouse-and-buttons-grabbed (mouse)
    (tv:MOUSE-SET-BLINKER :3d-axis  6 6)
    (loop with bl = (tv:MOUSE-GET-BLINKER :3d-axis w MOUSE) and
	       bl2 = *other-blinker* and button and foo = 0
	  initially (send bl2 ':set-offsets 6 6)
	  initially (send bl :initial-sets p)
	  initially (send bl2 :initial-sets p1)
	  initially (send bl :set-associated-blinkers (list  bl2))
	  until (and button (zl:bit-test 2 button))
	  when (not (zerop (mod foo 2)))
	    do (send bl :loop-sets t) and
	    do (send bl2 :loop-sets t) and
	    do (setq button (tv:wait-for-mouse-button-down
			      "Select Y-Z Click for X" nil nil mouse)) and
	    do (incf foo) and
	  do (send bl :loop-end t p sheet) and
	  do (send bl2 :loop-end t p1 sheet)
	  else do (send bl :loop-sets nil) and
	       do (send bl2 :loop-sets nil) and
	       do (setq button (tv:wait-for-mouse-button-down
				  "Select X-Y. Click for Z. M to end." nil nil mouse)) and
	       do (incf foo) and
	  do (send bl :loop-end nil p sheet) and
	  do (send bl2 :loop-end nil p1 sheet)
	  finally  (return (send bl :conv-to-scale)))))


(tv:MOUSE-DEFINE-BLINKER-TYPE
  ':3d-axis
  #'(lambda (screen)
      (tv:make-blinker screen '3d-axis-blinker
		       :projector p
		       :for-z nil
		       :CHAR #\mouse:Maltese-Cross
		       :visibility nil)))


;#+ignore
(defvar *other-blinker*
	(tv:make-blinker w1 '3d-axis-blinker
			 ;:for-z nil
			 :blink-following t
			 :CHAR #\mouse:Maltese-Cross
			 :visibility nil))

#+ignore
(defun set-up-3d-blinker (w)
  (let* ((screen (send w :superior))
	 (bls    (SEND SCREEN ':MOUSE-BLINKERS))
	 bl)
    (unless (setq bl (assoc :3d-axis bls))
      (push (cons :3d-axis (setq bl (tv:make-blinker screen '3d-axis-blinker
						      ;:for-z nil
						      :CHAR #\mouse:Maltese-Cross
						      :visibility nil)))
	    bls)
      (SEND SCREEN ':SET-MOUSE-BLINKERS bls))
    (TV:BLINKER-SET-SHEET bl SCREEN)
    bl))


#||

;;;from lox:>rel-7-2>color>zoom.lisp
(defun zoom (&optional (zoomed-window (send w ':screen)) &aux (old-sheet tv:mouse-sheet))
  "use the mouse to specify a rectangle, and pan&zoom to center it"
  (when (eq self zoomed-window)
    (send self ':set-pan-and-zoom 0 0 0 0))
  (unwind-protect
    (let* ((scr (send zoomed-window :screen))
	   (specify-window zoomed-window))
      (tv:mouse-set-sheet specify-window)
      (multiple-value-bind (left top right bottom)
	  (tv:mouse-specify-rectangle nil nil nil nil specify-window 0 0 t)
	(when (and left top right bottom)
		(let* ((zoomed-width (- right left))
		       (zoomed-height (- bottom top))
		       (x-ratio (// zoomed-width (float width)))
		       (y-ratio (// zoomed-height (float height)))
		       (cent-x ))
		  (setq left (floor (* left x-ratio))
			top (floor (* top y-ratio))
			right (floor (- zoomed-width (* x-ratio (- my-width right))))
			bottom (floor (- zoomed-height (* y-ratio (- my-height bottom))))))
	(send zoomed-window :set-scr-cent 
	      :cent-x   #+ignore 0 200
	      :cent-y   #+ignore 0 200)
	(send :set-scale-factor 
	  (send self ':pan-and-zoom-from-rectangle left top right bottom t zoomed-window))
	)))
    (tv:mouse-set-sheet old-sheet))
  )


(defun mouse-specify-3d-point1 (&optional (sheet mouse-sheet) abortable
				&aux (mouse (sheet-mouse sheet)) button abort mx my)
  (declare (special p))
  (tv:with-this-mouse-and-buttons-grabbed (mouse)
    (do () (nil)
      (multiple-value-bind (x y)
	    (send p :pos-to-screen 0 0 0)
	  (TV:MOUSE-WARP x y ))
      (tv:mouse-set-blinker-definition-internal mouse
					     ':character 0 0
					     ':on ':set-character #\mouse:Maltese-Cross)
      (setf (tv:who-line-mouse-grabbed-documentation mouse)
	    (if abortable
		"Position initial conditions.  Middle aborts.  Right is smart."
		"Position initial conditions.  Right is smart."))
      #+ignore
      (multiple-value-setq (button mx my)
	(tv:wait-for-mouse-button-down "Button" nil nil mouse))
      ;; The first click determines the point in x-y plane
      #+ignore
      (and abortable (zl:bit-test 2 button) (return (setq abort t)))
      (multiple-value (left1 top1)
	(tv:mouse-specified-point sheet mx my (zl:bit-test 4 button) nil))
      (tv:mouse-warp (+ left1 width) (+ top1 height) mouse)
      (tv:mouse-set-blinker-definition-internal mouse
					     :box-tracking-corner-blinker 0 0 :on
					     :set-edges-and-moving-corner
					     left1 top1 (+ left1 width) (+ top1 height)
					     :lower-right)
      (setf (tv:who-line-mouse-grabbed-documentation mouse)
	    (if abortable
		"Position lower right corner of rectangle.  Middle aborts.  Right is smart."
		"Position lower right corner of rectangle.  Right is smart."))
      ;; The next click fixes the lower right corner.
      (multiple-value (button mx my)
	(tv:wait-for-mouse-button-down "Button" nil nil mouse))
      (tv:mouse-standard-blinker nil mouse)
      (setf (tv:who-line-mouse-grabbed-documentation mouse) nil)
      (and abortable (zl:bit-test 2 button) (return (setq abort t)))
      (multiple-value-bind (x y)
	  (tv:mouse-specified-point sheet (1+ mx) (1+ my) (zl:bit-test 4 button) t)
	(setq width (- x left1)
	      height (- y top1)))
      (cond ((and (plusp width) (plusp height))
	     (multiple-value-bind (xoff yoff)
		 (sheet-mouse-offsets sheet)
	       (setq left1 (- left1 xoff)
		     top1 (- top1 yoff)))
	     (if (or (< width minimum-width) (< height minimum-height)
		     (minusp left1) (minusp top1)
		     (> (+ left1 width) (sheet-width sheet))
		     (> (+ top1 height) (sheet-height sheet)))
		 (beep)
		 (return nil)))
	    (t (setq left1 (tv:sheet-inside-left sheet)
		     top1 (tv:sheet-inside-top sheet)
		     width (tv:sheet-inside-width sheet)
		     height (tv:sheet-inside-height sheet))
	       (return nil)))))
  (unless abort
    (values left1 top1 (+ left1 width) (+ top1 height))))



(defmethod (:TRACK-internal basic-slider) (draw-message erase-message &optional mouse-line
							&aux old-type old-sheet old-x old-y
							mouse-x mouse-y graph
							left bottom right top)
  (setq old-type  tv:mouse-blinker-name
	old-sheet tv:mouse-sheet
	old-x	  tv:mouse-x
	old-y 	  tv:mouse-y
	graph     (first graphs))
  (multiple-value (left bottom right top) (send self :bounds))
  (unwind-protect
      ;; Usurp control of the mouse from the mouse process, you must do everything.
      (TV:WITH-MOUSE-USURPED			
	(tv:mouse-set-sheet (send graph :stream))
	(send tv:mouse-blinker :set-visibility nil)
	(setq tv:who-line-mouse-grabbed-documentation (and mouse-line mouse-line))
	(multiple-value-bind (sx sy)
	    (send graph :xy-to-mouse x y)
	  (TV:MOUSE-WARP sx sy))
	(multiple-value (nil mouse-x mouse-y)
	  (tv:wait-for-mouse-button-up))
	(do ((start-flag t nil)			;guarantee do body at least once
	     (buttons-newly-pushed 0))
	    ((and (not start-flag) ( 0 buttons-newly-pushed))
	     (let ((button (tv:mouse-button-encode buttons-newly-pushed)))
	       (send self erase-message)	; !KRA below clobbers x and last-x!
	       (setq lastx x lasty y)		;remember as the "last point"
	       (values x y button)))
	  (unless start-flag			;erase the old
	    (send self erase-message))
	  (multiple-value (x y)
	    (send graph :mouse-to-xy mouse-x mouse-y))
	  (send self :constrain-position left bottom right top)
	  #+ignore
	  (multiple-value (x y)
	    (send self :constrain-point x y))
	  (send self draw-message)
	  ;; Erasing/drawing has awakened the mouse; put it back to sleep and
	  ;; read new mouse state, when it changes...
	  (setq tv:mouse-wakeup nil)
	  (multiple-value (nil nil buttons-newly-pushed nil mouse-x mouse-y)
	    (tv:mouse-input))
	  ))
    (tv:mouse-set-blinker old-type)
    (tv:mouse-set-sheet old-sheet)
    (tv:mouse-warp old-x old-y)))

(loop for X = (tv:key-state :left-meta)
      do (if X (format t "X")
	     (format t "O")))

#\mouse:Big-Triangle  28
#\mouse:Boxed-Down-Triangle  117
#\mouse:Boxed-Up-Triangle  110
#\mouse:Circle-Plus  20
#\mouse:Circle-Times  26
#\mouse:Dot  96
#\mouse:Down-Arrow  2
#\mouse:Down-Arrow-To-Bar  106
#\mouse:Fat-Circle  100
#\mouse:Fat-Circle-Minus  101
#\mouse:Fat-Circle-Plus  102
#\mouse:Fat-Double-Horizontal-Arrow  14
#\mouse:Fat-Double-Vertical-Arrow  12
#\mouse:Fat-Down-Arrow  10
#\mouse:Fat-Left-Arrow  11
#\mouse:Fat-Plus  118
#\mouse:Fat-Right-Arrow  9
#\mouse:Fat-Times  97
#\mouse:Fat-Up-Arrow  8
#\mouse:Filled-Circle  99
#\mouse:Filled-Lozenge  45
#\mouse:Hollow-NE-Arrow  89
#\mouse:Hollow-NW-Arrow  70
#\mouse:Hollow-UP-Arrow  64
#\mouse:Horizontal-Double-Arrow  5
#\mouse:Hourglass  19
#\mouse:Inverse-Down-Arrow  33
#\mouse:Inverse-Up-Arrow  31
#\mouse:Left-Arrow  3
#\mouse:Maltese-Cross  119
#\mouse:Medium-Triangle  29
#\MOUSE:NE-Arrow  25
#\MOUSE:NW-Arrow  6
#\MOUSE:NW-Corner  17
#\mouse:Paintbrush  21
#\mouse:Paragraph  16
#\mouse:Plus  43
#\mouse:Right-Arrow  1
#\mouse:Scissors  22
#\MOUSE:SE-Corner  18
#\mouse:Short-Down-Arrow  107
#\mouse:Short-Up-Arrow  109
#\mouse:Small-Filled-Circle  98
#\mouse:Small-Triangle  30
#\mouse:Times  7
#\mouse:Trident  23
#\mouse:Up-Arrow  0
#\mouse:Up-Arrow-To-Bar  108
#\mouse:Vertical-Double-Arrow  4
||#