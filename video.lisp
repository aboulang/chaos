;;; -*- Mode:LISP; Package:HACKS; Lowercase: T; Base:10.-*-

;;; about 5 seconds of solid bitblt time.  Rotate takes 2 + 15*log(N) bitblts.



(defun rotate (myself w)
  (macrolet ((copy-all-to (from xoffset yoffset to alu)
			  `(bitblt ,alu (- width ,xoffset) (- width ,yoffset)
				   ,from 0 0 ,to ,xoffset ,yoffset))
	     
	     (copy-all-from (to xoffset yoffset from alu)
			    `(bitblt ,alu (- width ,xoffset) (- width ,yoffset)
				     ,from ,xoffset ,yoffset ,to 0 0)))
    
    
    (let* ((width (decode-raster-array myself)))
      (with-stack-list (temp-dims width width)
	(tv:with-temp-sheet-rasters
	  ((mask temp-dims ':type (array-type myself))
	   (temp temp-dims ':type (array-type myself)))
	  
	  (copy-all-to mask 0 0 mask TV:ALU-SETZ)
	  (copy-all-from mask (// width 2) (// width 2) mask TV:ALU-SETO)
	  (do ((quad (// width 2) (// quad 2)))
	      ((< quad 1))
	    (copy-all-to mask 0 0 temp TV:ALU-SETA)	; 1
	    (copy-all-to mask 0 quad temp TV:ALU-IOR)	; 2
	    (copy-all-to myself 0 0 temp TV:ALU-AND)	; 3
	    (copy-all-to temp 0 0 myself TV:ALU-XOR)	; 4
	    (copy-all-from temp quad 0 myself TV:ALU-XOR)	; 5
	    (copy-all-from myself quad 0 myself TV:ALU-IOR)	; 6
	    (copy-all-to temp quad 0 myself TV:ALU-XOR)	; 7
	    (copy-all-to myself 0 0 temp TV:ALU-SETA)	; 8
	    (copy-all-from temp quad quad myself TV:ALU-XOR)	; 9
	    (copy-all-to mask 0 0 temp TV:ALU-AND)	; 10
	    (copy-all-to temp 0 0 myself TV:ALU-XOR)	; 11
	    (copy-all-to temp quad quad myself TV:ALU-XOR)	; 12
	    (copy-all-from mask (// quad 2) (// quad 2) mask TV:ALU-AND)	; 13
	    (copy-all-to mask quad 0 mask TV:ALU-IOR)	; 14
	    (copy-all-to mask 0 quad mask TV:ALU-IOR)	; 15)
	  )
	    (funcall w ':bitblt tv:alu-ior width width myself 0 0 0 0)))
    myself)))

(defun f ()
  (dotimes (i 20)
    (fatten tv:main-screen)))


(defun fatten (window)
  (multiple-value-bind (w h)
	(if (instancep window)
	    (funcall window ':inside-size)
	    (decode-raster-array window))
      (let* ((array-type (if (instancep window)
			     (tv:sheet-array-type window)
			     (array-type window)))
	     (el-size (if (instancep window)
			  (send (send window :screen) :bits-per-pixel)
			  (array-element-size window)))
	     (h2 (+ h 2))
	     (w2 (+ w 2))
	     (w32 (* (ceiling (* w2 el-size) 32)
		     (// 32 el-size)))
	     (dims (list h2 w32)))
	(tv:with-temp-sheet-rasters 
	  ((myself dims ':type array-type)
	   (res    dims ':type array-type))
	  
	  (if (instancep window)
	      (funcall window ':bitblt-from-sheet tv:alu-seta w h 0 0 myself 0 0)
	      (bitblt tv:alu-seta w h window 0 0 myself 0 0))
	  (bitblt TV:ALU-XOR w2 h2 res   0 0 res  0 0)
	  (dolist (l '((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2)))
	    (bitblt TV:ALU-ior  w h myself 0 0 res (car l) (cadr l)))
	    
	    (bitblt TV:ALU-ior  w  h  res 1 1 myself 0 0)
	    (if (not (instancep window))
		(bitblt tv:alu-seta w h myself 0 0 window 0 0)
		(funcall window ':bitblt tv:alu-seta w h myself 0 0 0 0)
		))
	  ))
      window)

(defun life (window  &optional (gens 100000))
  (*catch 'exit
    (multiple-value-bind (w h)
	(if (instancep window)
	    (funcall window ':inside-size)
	    (decode-raster-array window))
      (let* ((array-type (if (instancep window)
			     (tv:sheet-array-type window)
			     (array-type window)))
	     (el-size (if (instancep window)
			  (send (send window :screen) :bits-per-pixel)
			  (array-element-size window)))
	     (h2 (+ h 2))
	     (w2 (+ w 2))
	     (w32 (* (ceiling (* w2 el-size) 32)
		     (// 32 el-size)))
	     (dims (list h2 w32)))
	(tv:with-temp-sheet-rasters 
	  ((myself dims ':type array-type)
	   (nbr1   dims ':type array-type)
	   (nbr2   dims ':type array-type)
	   (nbr4   dims ':type array-type)
	   (carry2 dims ':type array-type)
	   (carry4 dims ':type array-type))
	  
	  (if (instancep window)
	      (funcall window ':bitblt-from-sheet tv:alu-seta w h 0 0 myself 0 0)
	      (bitblt tv:alu-seta w h window 0 0 myself 0 0))
	  
	  (dotimes (generation gens)
	    (bitblt TV:ALU-XOR w2 h2 nbr1   0 0 nbr1   0 0)
	    (bitblt TV:ALU-XOR w2 h2 nbr2   0 0 nbr2   0 0)
	    (bitblt TV:ALU-XOR w2 h2 nbr4   0 0 nbr4   0 0)
	    (bitblt TV:ALU-XOR w2 h2 carry2 0 0 carry2 0 0)
	    (bitblt TV:ALU-XOR w2 h2 carry4 0 0 carry4 0 0)
	    (dolist (l '((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2)))
	      (bitblt TV:ALU-SETA w2 h2 nbr1   0 0 carry2 0       0       )
	      
	      ;; carry2 = nbr1 AND carry2
	      (bitblt TV:ALU-AND  w  h  myself   0 0 carry2 (car l) (cadr l))
	      
	      ;; nbr1   = nbr1 TV:ALU-XOR myself
	      (bitblt TV:ALU-XOR  w  h  myself   0 0 nbr1   (car l) (cadr l))
	      (bitblt TV:ALU-SETA w2 h2 nbr2   0 0 carry4 0       0       )
	      
	      ;; carry4 = nbr2 AND carry4
	      (bitblt TV:ALU-AND  w2 h2 carry2 0 0 carry4 0       0       )
	      
	      ;; nbr2   = nbr2 TV:ALU-XOR carry2
	      (bitblt TV:ALU-XOR  w2 h2 carry2 0 0 nbr2   0       0       )
	      
	      ;; nbr4   = nbr4 TV:ALU-XOR carry4
	      (bitblt TV:ALU-XOR  w2 h2 carry4 0 0 nbr4   0       0       ))
	    
	    ;; myself = myself AND nbr2
	    (bitblt TV:ALU-AND    w  h  nbr2 1 1 myself 0 0)
	    
	    ;; nbr1 = nbr1 AND nbr2
	    (bitblt TV:ALU-AND    w2 h2 nbr2 0 0 nbr1 0 0)
	    
	    ;; myself = (myself AND nbr2) OR (nbr1 AND nbr2)
	    (bitblt TV:ALU-IOR     w  h  nbr1 1 1 myself 0 0)
	    
	    ;; myself = (NOT nbr4) AND ((myself AND nbr2) OR (nbr1 AND nbr2))
	    (bitblt TV:ALU-ANDCA w  h  nbr4 1 1 myself 0 0)
	    (if (not (instancep window))
		(bitblt tv:alu-seta w h myself 0 0 window 0 0)
		(funcall window ':bitblt tv:alu-seta w h myself 0 0 0 0)
		(funcall window ':home-cursor)
		(format window "~D" generation)
		(if (funcall window ':tyi-no-hang) (*throw 'exit t))
		))
	  ))
      window)
    
    ))


(defun run-life ()
  (hof-window-call (*little-hof-window* :deactivate)
    (with-real-time
      (funcall *little-hof-window* ':set-label "Life Window")
      (multiple-value-bind (width height) (funcall *little-hof-window* ':inside-size)
	(funcall *little-hof-window* ':clear-screen)
	(funcall *little-hof-window* ':draw-line
		 100 (// height 2)
		 (- width 100) (// height 2)))
      (life *little-hof-window*))))

(defdemo "Life"
	 "Conway's game of /"Life/", a cellular automaton demonstration.  By CMB."
  (run-life))


(defvar *rotate-size* 512)

(defun run-rotate ()
  (hof-window-call (*hof-window* :deactivate)
    (with-real-time
      (funcall *hof-window* ':set-label "Rotate")
      (tv:with-temp-sheet-raster
	(source (list *rotate-size* *rotate-size*)
		:type (tv:sheet-array-type *hof-window*))
	(funcall *hof-window* ':clear-screen)
	(bitblt tv:alu-xor *rotate-size* *rotate-size* source 0 0 source 0 0) 
	(multiple-value-bind (w h) (send *hof-window* :inside-size)
	  ;(dotimes (y h) (send *hof-window* :draw-line 0 0 (1- w) y tv:alu-xor))
	  ;(dotimes (x h) (send *hof-window* :draw-line 0 0 x (1- h) tv:alu-xor))
	  (let ((string "Symbolics"))
	    (scl:with-character-style ('(:eurex :italic :huge) *hof-window*)
	      (multiple-value-bind (ww hh)
		  (progn (send *hof-window* :set-cursorpos 0 0)
			 (send *hof-window* :compute-motion string))
		(send *hof-window* :set-cursorpos (- (// w 2) ww) (// (- h hh) 2))
		(send *hof-window* :string-out string)))))
	(loop doing
	  (when (eq (send *hof-window* :any-tyi-no-hang) #\End) (return nil))	;gobble it
	  (funcall *hof-window* ':bitblt-from-sheet
		   tv:alu-seta *rotate-size* *rotate-size* 0 0 source 0 0)
	  (rotate source *hof-window*)
	  (fatten *hof-window*))))))
