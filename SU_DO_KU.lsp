
;Name: Vedanshi Patel
;Date: March 28th 2018
;purpose: Create SU-DO-KU game.


; To creat and initialize array 

(setq table (make-array '(4 4) :initial-contents '((0 2 4 0) (1 0 0 3) (4 0 0 2) (0 1 3 0))))	;#8
;(setq table (make-array '(4 4) :initial-contents '((0 2 0 0) (1 0 0 3) (4 0 0 2) (0 0 3 0))))	;#6
;(setq table (make-array '(4 4) :initial-contents '((0 0 0 2) (4 0 0 0) (0 0 0 1) (2 0 0 0))))	;#4
;(setq table (make-array '(4 4) :initial-contents '((1 2 0 0) (3 4 0 0) (0 0 1 4) (0 0 4 3))))	;not solvable


; To print SU-DO-KU table
(defun printTable()
    (dotimes (i 4)
        (dotimes (j 4)
            (format t " ~A |" (aref table i j)))
        (format t "~%")))

;To Check if num is valid to input for that specific block in a SU-DO-KU table
(defun checkInputValidity(row col num)
	
	; To check if num already exists in ROW
	(loop for k from 0 to 3 
		do (if (= (aref table row k) num)
				(return-from checkInputValidity (not t))))
	
	; To check if num already exits in COLUMN
	(loop for k from 0 to 3 
		do (if (= (aref table k col) num)
				(return-from checkInputValidity (not t))))
	
	; To check if num already exits in Submetrix - 2*2 box
	(setq startRow (- row (mod row 2)))
	(setq startCol (- col (mod col 2)))

	(loop for i from startRow to (+ startRow 1)
		do (loop for j from startCol to (+ startCol 1)
			do (if (= (aref table i j) num)
				(return-from checkInputValidity (not t))))) ; returns true if num is not allowed to input in the SU-DO-KU table
	
	;returns true if num is valid to input in the SU-DO-KU table
	(return-from checkInputValidity t) 
)		
		
(defun solveSUDOKU(blockNum)
	(let 
		(	; To find ROW from block number
			(ROW (truncate (/ blockNum 4)))
			;To find COLUMN from block number
			(COLUMN (mod blockNum 4)))

			  ; In total this su-do-ku table have blockNum from 0 to 15. 
			  ;if blockNum is 16, then it's out of bound, and the puzzle is complete. 
			  (if (= blockNum 16) 
					t
                  
                  ;if value is already assign to that block then increment to next block
                  (if (/= (aref table ROW COLUMN) 0)
                        (solveSUDOKU (+ blockNum 1))
                        
						; To find valid Input num to fill the empty block. 
						(loop for num from 1 to 4
						  do (and (checkInputValidity ROW COLUMN num)
								  (progn
                                      
                                      ;if num is valid then it is assigned to that specific empty block
                                      (setf (aref table ROW COLUMN) num)
                                      
                                      ; Rechecking by Backtracking using recursive method
                                      (and (solveSUDOKU (+ blockNum 1)) 
                                           (return t))))
						  finally  (progn
                                      ; if assigned input num wasn't valid then it is set back to 0.
                                      (setf (aref table ROW COLUMN) 0) 
                                      (return nil)))))))


(format t "*** SU-DO-KU ***~%~%")
(format t "Puzzle to solve: ~%~%")
(printTable)

(format t " ~%Result: ~%~%")

(if (solveSUDOKU 0)
	(printTable)
	(format t "Initial setup unsolvable.~%"))

(format t "~%--- End Of Program --- ~%")