;;; 2048.el --- play 2048 in Emacs

;; Copyright 2014 Zachary Kanfer

(defun 2048-game () "Major mode for playing 2048 in Emacs"
  (interactive)
  (switch-to-buffer "2048")
  (kill-all-local-variables)
  (setq major-mode '2048-mode)
  (setq mode-name "2048")
  (toggle-read-only t)
  (2048-init)
  (2048-print-board))

(defvar *2048-board* nil
  "The board itself. If a number is in the square, the number is stored. Otherwise, 0 is stored.
   You should access this with 2048-get-cell.")

(defvar *2048-columns* 4
  "The width of the board. It could be customized, if you wanted to make the game very very hard, or very very easy.")

(defvar *2048-rows* 4
  "The height of the board. It could be customized, if you wanted to make the game very very tall, or very very short.")

(defun 2048-init ()
  "Begin a game of 2048."
  (setq *2048-board* (make-vector (* *2048-columns* *2048-rows*)
                                  0)))

(defun 2048-get-cell (row col)
  "Gets the value in (row, col)."
  (elt *2048-board*
       (+ (* row *2048-columns*)
          col)))

(defun 2048-print-board ()
  "Wipes the entire field, and prints the board."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *2048-rows*)
      (dotimes (col *2048-columns*)
        (insert-string (2048-get-cell row col) " "))
      (insert "\n"))))
