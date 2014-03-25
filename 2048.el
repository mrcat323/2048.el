;;; 2048.el --- play 2048 in Emacs

;; Copyright 2014 Zachary Kanfer

(defun 2048-game () "Major mode for playing 2048 in Emacs"
  (interactive)
  (switch-to-buffer "2048")
  (kill-all-local-variables)
  (setq major-mode '2048-mode)
  (setq mode-name "2048")
  (toggle-read-only t)
  (2048-init))

(defvar *2048-board* nil
  "The board itself. If a number is in the square, the number is stored. Otherwise, 0 is stored.
   You should access this with 2048-get-cell.")

(defvar *2048-columns* 4
  "The width of the board. It could be customized, if you wanted to make the game very very hard, or very very easy.")

(defvar *2048-rows* 4
  "The height of the board. It could be customized, if you wanted to make the game very very tall, or very very short.")

(defvar *2048-random-4-threshold* 90
  "When a new number is inserted into the board, insert a 4 if (>= (random 100) *2048-random-4-threshold*). Otherwise, 2.")

(defun 2048-init ()
  "Begin a game of 2048."
  (setq *2048-board* (make-vector (* *2048-columns* *2048-rows*)
                                  0))
  (2048-insert-random-cell)
  (2048-insert-random-cell)
  (2048-print-board))

(defun 2048-get-cell (row col)
  "Gets the value in (row, col)."
  (elt *2048-board*
       (+ (* row *2048-columns*)
          col)))

(defun 2048-set-cell (row column val)
  "Sets the value in (row, column)."
  (aset *2048-board*
        (+ (* row *2048-columns*)
           column)
        val))

(defun 2048-insert-random-cell ()
  "Picks a number randomly, and inserts it into a random cell."
  (let ((number-to-insert (if (>= (random 100)
                                  *2048-random-4-threshold*)
                              4
                            2))
        (row (random *2048-rows*))
        (column (random *2048-columns*)))
    (while (not (eq (2048-get-cell row column)
                    0))
      (setq row (random *2048-rows*))
      (setq column (random *2048-columns*)))
    (2048-set-cell row column number-to-insert)))


(defun 2048-print-board ()
  "Wipes the entire field, and prints the board."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *2048-rows*)

      ;;print the separator lineon top of, and between cells
      (dotimes (col *2048-columns*)
        (insert-string "+-------"))
      (insert-string "+")
      (insert "\n")

      ;;print the numbers
      (dotimes (col *2048-columns*)
        (insert-string (format "|%5d  " (2048-get-cell row col))))
      (insert "|")
      (insert "\n"))

    ;;print the separator line on the bottom of the last row.
    (dotimes (col *2048-columns*)
      (insert-string "+-------"))
    (insert-string "+")))
