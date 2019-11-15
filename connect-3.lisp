(setf arraySize 16)		; size of the board
(setf tempBoard (make-array arraySize))		; initiliazing tempBoard
(setf board (make-array 16		; initializing board
	:initial-contents '(_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
	)
)
; keep track of how big each column
(setf column0 0)
(setf column1 0)
(setf column2 0)
(setf column3 0)

; keep track of how big each column is for the temporary board
(setf tempColumn0 0)
(setf tempColumn1 0)
(setf tempColumn2 0)
(setf tempColumn3 0)

; print any array/board
(defun printBoard (myBoard)
	(dotimes (i arraySize)
		(if (= 0 (mod i 4))
			(progn
				(terpri)
				(princ (aref myBoard i))
				(princ #\SPACE)
			)
			(progn
				(princ (aref myBoard i))
				(princ #\SPACE)
			)
		)
	)
)

; copy the main board to another board
(defun	duplicateBoard(newBoard)
	(dotimes (i arraySize)
		(setf (aref newBoard i) (aref board i))
	)
	(setf tempColumn0 column0)
	(setf tempColumn1 column1)
	(setf tempColumn2 column2)
	(setf tempColumn3 column3)
)

; takes a disk and places it into the appropriate row and column
(defun insert-in-board (diskColor column myBoard)
	(if (and (> column0 3)(> column1 3)(> column2 3)(> column3 3))
		(return-from insert-in-board nil)		; if board is full then return false/nil
		(continue)
	)

	; make sure the user picks a valid column
	(loop while (or (< column 0)(> column 3)) do
		(princ "invalid column choice, please try again")
		(terpri)
		(princ "enter column: ")
		(setq column (read))
	)
	
	; make sure the user doesn't pick a full column
	(loop while (or (and (> column0 3)(= column 0))(and (> column1 3)(= column 1))(and (> column2 3)(= column 2))(and (> column3 3)(= column 3))) do
		(princ "the column you have chosen is full!")
		(terpri)
		(princ "enter column:")
		(setq column (read))
		
		; make sure the user picks a valid column
		(loop while (or (< column 0)(> column 3)) do
			(princ "invalid column choice, please try again")
			(terpri)
			(princ "enter column: ")
			(setq column (read))
		)
	)
	
	; place the disk in the appropriate column and row
	(cond
		((= column 0) 
			(progn
				(setf (aref myBoard (- 12 (* 4 column0))) diskColor)		; places the disk in the appropriate row
				(setf column0 (+ column0 1))		;; increment the size of the column
			)
		)
		((= column 1) 
			(progn
				(setf (aref myBoard (- 13 (* 4 column1))) diskColor)
				(setf column1 (+ column1 1))
			)
		)
		((= column 2) 
			(progn
				(setf (aref myBoard (- 14 (* 4 column2))) diskColor)
				(setf column2 (+ column2 1))
			)
		)
		((= column 3) 
			(progn
				(setf (aref myBoard (- 15 (* 4 column3))) diskColor)
				(setf column3 (+ column3 1))
			)
		)
	)
	(return-from insert-in-board t)		; if properly inserted return true
)

(defun check-for-win (c)
	; check rows for a winning game
	(dotimes (i 4)
		(if (and (char-equal (coerce c 'character) (coerce (aref board (* 4 i)) 'character)) (char-equal (coerce c 'character) (coerce (aref board (+ (* 4 i) 1)) 'character)) (char-equal (coerce c 'character) (coerce (aref board (+ (* 4 i) 2)) 'character)))
			(progn
				(return-from check-for-win t)
			)
			(progn
				(continue)
			)
		)
		(if (and (char-equal (coerce c 'character) (coerce (aref board (+ (* 4 i) 1)) 'character)) (char-equal (coerce c 'character) (coerce (aref board (+ (* 4 i) 2)) 'character)) (char-equal (coerce c 'character) (coerce (aref board (+ (* 4 i) 3)) 'character)))
			(progn
				(return-from check-for-win t)
			)
			(progn
				(continue)
			)
		)
	)
	
	; check columns for a winning game
	(dotimes (i 4)
		(if (and (char-equal (coerce c 'character) (coerce (aref board (+ i 0)) 'character)) (char-equal (coerce c 'character) (coerce (aref board (+ i 4)) 'character)) (char-equal (coerce c 'character) (coerce (aref board (+ i 8)) 'character)))
			(progn
				(return-from check-for-win t)
			)
			(progn
				(continue)
			)
		)
		(if (and (char-equal (coerce c 'character) (coerce (aref board (+ i 4)) 'character)) (char-equal (coerce c 'character) (coerce (aref board (+ i 8)) 'character)) (char-equal (coerce c 'character) (coerce (aref board (+ i 12)) 'character)))
			(progn
				(return-from check-for-win t)
			)
			(progn
				(continue)
			)
		)
	)
	
	; check for diagonals 
	(cond
		((and (char-equal (coerce c 'character) (coerce (aref board 0) 'character))(char-equal (coerce c 'character) (coerce (aref board 5) 'character))(char-equal (coerce c 'character) (coerce (aref board 10) 'character))) (return-from check-for-win t))
		((and (char-equal (coerce c 'character) (coerce (aref board 1) 'character))(char-equal (coerce c 'character) (coerce (aref board 6) 'character))(char-equal (coerce c 'character) (coerce (aref board 11) 'character))) (return-from check-for-win t))
		((and (char-equal (coerce c 'character) (coerce (aref board 4) 'character))(char-equal (coerce c 'character) (coerce (aref board 9) 'character))(char-equal (coerce c 'character) (coerce (aref board 14) 'character))) (return-from check-for-win t))
		((and (char-equal (coerce c 'character) (coerce (aref board 5) 'character))(char-equal (coerce c 'character) (coerce (aref board 10) 'character))(char-equal (coerce c 'character) (coerce (aref board 15) 'character))) (return-from check-for-win t))
		((and (char-equal (coerce c 'character) (coerce (aref board 9) 'character))(char-equal (coerce c 'character) (coerce (aref board 6) 'character))(char-equal (coerce c 'character) (coerce (aref board 3) 'character))) (return-from check-for-win t))
		((and (char-equal (coerce c 'character) (coerce (aref board 8) 'character))(char-equal (coerce c 'character) (coerce (aref board 5) 'character))(char-equal (coerce c 'character) (coerce (aref board 2) 'character))) (return-from check-for-win t))
		((and (char-equal (coerce c 'character) (coerce (aref board 13) 'character))(char-equal (coerce c 'character) (coerce (aref board 10) 'character))(char-equal (coerce c 'character) (coerce (aref board 7) 'character))) (return-from check-for-win t))
		((and (char-equal (coerce c 'character) (coerce (aref board 12) 'character))(char-equal (coerce c 'character) (coerce (aref board 9) 'character))(char-equal (coerce c 'character) (coerce (aref board 6) 'character))) (return-from check-for-win t))
	)
	
	(return-from check-for-win nil)
)

(defun AI-turn ()
	(duplicateBoard tempBoard)
	; check that there is enough room to insert a disk in each column
	(if (> column0 3)
		(continue)
		(insert-in-board 'r 0 tempBoard)
	)
	(if (> column1 3)
		(continue)
		(insert-in-board 'r 1 tempBoard)
	)
	(if (> column2 3)
		(continue)
		(insert-in-board 'r 2 tempBoard)
	)
	(if (> column3 3)
		(continue)
		(insert-in-board 'r 3 tempBoard)
	)
	; reset the main board's column values after wee have inserted
	(setf column0 tempColumn0)
	(setf column1 tempColumn1)
	(setf column2 tempColumn2)
	(setf column3 tempColumn3)
	; all potential moves are inserted at this point
	
	; checking for potential moves related to column0
	(if (> column0 3)
		(continue)		; if column is full move onto the next move
		(if (and (char-equal #\B (coerce (aref board 4) 'character)) (char-equal #\B (coerce (aref board 8) 'character)))		; block potential column0 wins
			(progn
				(insert-in-board 'r 0 board)
				(return-from AI-turn "AI placed disk in column 0")
			)
			(if (and (char-equal #\B (coerce (aref board 8) 'character)) (char-equal #\B (coerce (aref board 12) 'character)) (char-not-equal #\R (coerce (aref board 4) 'character)))		;block potential column0 wins
				(progn
					(insert-in-board 'r 0 board)
					(return-from AI-turn "AI placed disk in column 0")
				)
				(continue)
			)
		)
	)
	
	; checking for potential moves related to column1
	(if (> column1 3)
		(continue)		; if column is full move onto the next move
		(if (and (char-equal #\B (coerce (aref board 5) 'character)) (char-equal #\B (coerce (aref board 9) 'character)))		; block potential column1 wins
			(progn
				(insert-in-board 'r 1 board)
				(return-from AI-turn "AI placed disk in column 1")
			)
			(if (and (char-equal #\B (coerce (aref board 9) 'character)) (char-equal #\B (coerce (aref board 13) 'character)) (char-not-equal #\R (coerce (aref board 5) 'character)))		;block potential column1 wins
				(progn
					(insert-in-board 'r 1 board)
					(return-from AI-turn "AI placed disk in column 1")
				)
				(progn
					(dotimes (i 4)
						(if (and (char-equal #\B (coerce (aref board (* i 4)) 'character)) (char-equal #\_ (coerce (aref board (+ (* i 4) 1)) 'character)))		; block potential row wins related to column1 moves
							(progn
								(insert-in-board 'r 1 board)
								(return-from AI-turn "AI placed disk in column 1")
							)
							(if (and (char-equal #\B (coerce (aref board (+ (* i 4) 2)) 'character)) (char-equal #\_ (coerce (aref board (+ (* i 4) 1)) 'character)))
								(progn
									(insert-in-board 'r 1 board)
									(return-from AI-turn "AI placed disk in column 1")
								)
								(continue)
							)
						)
					)
				)
			)
		)
	)
	
	
	; checking for potential moves related to column2
	(if (> column2 3)
		(continue)		; if column is full move onto the next move
		(if (and (char-equal #\B (coerce (aref board 6) 'character)) (char-equal #\B (coerce (aref board 10) 'character)))		; block potential column1 wins
			(progn
				(insert-in-board 'r 2 board)
				(return-from AI-turn "AI placed disk in column 2")
			)
			(if (and (char-equal #\B (coerce (aref board 10) 'character)) (char-equal #\B (coerce (aref board 14) 'character)) (char-not-equal #\R (coerce (aref board 6) 'character)))		;block potential column1 wins
				(progn
					(insert-in-board 'r 2 board)
					(return-from AI-turn "AI placed disk in column 2")
				)
				(progn
					(dotimes (i 4)
						(if (and (char-equal #\B (coerce (aref board (+ (* i 4) 1)) 'character)) (char-equal #\_ (coerce (aref board (+ (* i 4) 2)) 'character)))		; block potential row wins related to column1 moves
							(progn
								(insert-in-board 'r 2 board)
								(return-from AI-turn "AI placed disk in column 2")
							)
							(if (and (char-equal #\B (coerce (aref board (+ (* i 4) 3)) 'character)) (char-equal #\_ (coerce (aref board (+ (* i 4) 2)) 'character)))
								(progn
									(insert-in-board 'r 2 board)
									(return-from AI-turn "AI placed disk in column 2")
								)
								(continue)
							)
						)
					)
				)
			)
		)
	)	
	
	; checking for potential moves related to column3
	(if (> column3 3)
		(continue)		; if column is full move onto the next move
		(if (and (char-equal #\B (coerce (aref board 7) 'character)) (char-equal #\B (coerce (aref board 11) 'character)))		; block potential column0 wins
			(progn
				(insert-in-board 'r 3 board)
				(return-from AI-turn "AI placed disk in column 3")
			)
			(if (and (char-equal #\B (coerce (aref board 11) 'character)) (char-equal #\B (coerce (aref board 15) 'character)))		;block potential column0 wins
				(progn
					(insert-in-board 'r 3 board)
					(return-from AI-turn "AI placed disk in column 3")
				)
				(continue)
			)
		)
	)
	
	; place disk anywhere if previous conditions failed
	(if (> column0 3)
		(continue)
		(progn
			(insert-in-board 'r 0 board)
			(return-from AI-turn "AI placed disk in column 0")
		)
	)
	(if (> column1 3)
		(continue)
		(progn
			(insert-in-board 'r 1 board)
			(return-from AI-turn "AI placed disk in column 1")
		)
	)
	(if (> column2 3)
		(continue)
		(progn
			(insert-in-board AI-turn 'r 2 board)
			(return-from "AI placed disk in column 2")
		)
	)
	(if (> column3 3)
		(continue)
		(progn
			(insert-in-board AI-turn 'r 3 board)
			(return-from "AI placed disk in column 3")
		)
	)
)

; checks to see if the board is full
(defun isBoardFull ()
	(if (and (> column0 3)(> column1 3)(> column2 3)(> column3 3))
		(return-from isBoardFull t)
		(return-from isBoardFull nil)
	)
)

(defun connect-3 ()
	(princ "Welcome to connect 3!")
	(terpri)
	(princ "B - is you")
	(terpri)
	(princ "R - is the AI")
	(terpri)
	(princ "Try your best to beat the AI!")
	(printBoard board)
	
	(loop while (eq (check-for-win 'b)(check-for-win 'r)) do
		(terpri)
		(if (isBoardFull)		; check to see if board is full before player's turn
			(return-from connect-3 "DRAW!")
			(continue)
		)
		(princ "Player's turn")
		(terpri)
		(princ "Pick a column to drop your disk: ")
		(setq col (read))
		(insert-in-board 'b col board)		; player's turn
		(printBoard board)
		
		(if (isBoardFull)		; check to see if board is full before AI's turn
			(return-from connect-3 "DRAW!")
			(continue)
		)
		(terpri)
		(princ "AI's turn")
		(AI-turn)		; AI's turn
		(printBoard board)
	)
	(if (eql t (check-for-win 'b))		; check for winner
		(return-from connect-3 "YOU WIN")
		(return-from connect-3 "AI WINS")
	)
)