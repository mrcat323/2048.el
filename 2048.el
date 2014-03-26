;;; 2048.el --- play 2048 in Emacs

;; Copyright 2014 Zachary Kanfer

(defun 2048-game () "Major mode for playing 2048 in Emacs"
  (interactive)
  (switch-to-buffer "2048")
  (kill-all-local-variables)
  (setq major-mode '2048-mode)
  (setq mode-name "2048")
  (use-local-map *2048-keymap*)
  (toggle-read-only t)
  (2048-init))

(defvar *2048-keymap*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") '2048-up)
    (define-key map (kbd "C-p") '2048-up)
    (define-key map (kbd "<up>") '2048-up)
    (define-key map (kbd "n") '2048-down)
    (define-key map (kbd "C-n") '2048-down)
    (define-key map (kbd "<down>") '2048-down)
    (define-key map (kbd "b") '2048-left)
    (define-key map (kbd "C-b") '2048-left)
    (define-key map (kbd "<left>") '2048-left)
    (define-key map (kbd "f") '2048-right)
    (define-key map (kbd "C-f") '2048-right)
    (define-key map (kbd "<right>") '2048-right)
    map))


(defvar *2048-board* nil
  "The board itself. If a number is in the square, the number is stored. Otherwise, 0 is stored.
   You should access this with 2048-get-cell.")

(defvar *2048-columns* 4
  "The width of the board. It could be customized, if you wanted to make the game very very hard, or very very easy.")

(defvar *2048-rows* 4
  "The height of the board. It could be customized, if you wanted to make the game very very tall, or very very short.")

(defvar *2048-random-4-threshold* 90
  "When a new number is inserted into the board, insert a 4 if (>= (random 100) *2048-random-4-threshold*). Otherwise, 2.")

(defvar *2048-debug* nil
  "when 't, print debugging information.")

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

      (dotimes (col *2048-columns*)
        (insert-string "|       "))
      (insert "|")
      (insert "\n")

      ;;print the numbers
      (dotimes (col *2048-columns*)
        (insert-string (format "|%5d  " (2048-get-cell row col))))
      (insert "|")
      (insert "\n")

      (dotimes (col *2048-columns*)
        (insert-string "|       "))
      (insert "|")
      (insert "\n"))

    ;;print the separator line on the bottom of the last row.
    (dotimes (col *2048-columns*)
      (insert-string "+-------"))
    (insert-string "+")))

(defun 2048-move (from-row from-column to-row to-column)
  "Tries to move the number in (from-row, from-column) to (to-row, to-column).
   This succeeds when (to-row, to-column) either is 0,
   or is the same value as (from-row, from-column).
   Returns t if we were able to move; otherwise nil."
  (2048-debug (format "moving the cell (%d, %d) to (%d, %d)" from-row from-column to-row to-column))
  (let ((from-val (2048-get-cell from-row from-column))
        (to-val (2048-get-cell to-row to-column)))
    (cond ((eq from-val to-val)
           (unless (eq from-val 0)
             (2048-set-cell to-row to-column (* from-val 2))
             (2048-set-cell from-row from-column 0)))
          ((eq to-val 0)
           (2048-set-cell to-row to-column from-val)
           (2048-set-cell from-row from-column 0))
          (t nil))))




(defun 2048-up ()
  "Shifts the board up"
  (interactive)
  (2048-for row 1 (1- *2048-rows*)
            (2048-for col 0 (1- *2048-columns*)
                      (2048-move row col (- row 1) col)))
  (2048-print-board))

(defun 2048-down ()
  "Shifts the board down"
  (interactive)
  (2048-for-down row (- *2048-rows* 2) 0
                 (2048-for col 0 (1- *2048-columns*)
                           (2048-move row col (+ row 1) col)))
  (2048-print-board))

(defun 2048-left ()
  "Shifts the board left."
  (interactive)
  (2048-for row 0 (1- *2048-rows*)
            (2048-for col 1 (1- *2048-columns*)
                      (2048-move row col row (- col 1))))
  (2048-print-board))


(defun 2048-right ()
  "Shifts the board right."
  (interactive)
  (2048-for row 0 (1- *2048-rows*)
            (2048-for-down col (- *2048-columns* 2) 0
                           (2048-move row col row (+ col 1))))
  (2048-print-board))

(defmacro 2048-for (var init end &rest body)
  "Helper function. executes 'body repeatedly, with 'var assigned values starting at 'init, and ending at 'end, increasing by one each iteration."
  `(let ((,var ,init)
	 (end-val ,end))
     (while (<= ,var end-val)
       ,@body
       (setq ,var (1+ ,var)))))

(defmacro 2048-for-down (var init end &rest body)
  "Helper function, executes 'body repeatedly, with 'var assigned values starting at 'init, and ending at 'end, decreasing by one each iteration."
  `(let ((,var ,init)
	 (end-val ,end))
     (while (>= ,var end-val)
       ,@body
       (setq ,var (1- ,var)))))

(defmacro 2048-debug (&rest body)
  "If *2048-debug* is 't, log ,@body as a string to the buffer named '2048-debug'"
  `(when *2048-debug*
     (print (concat ,@body)
	    (get-buffer-create "2048-debug"))))
