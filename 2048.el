;;; 2048.el --- play 2048 in Emacs

;; Copyright 2014 Zachary Kanfer

(defun 2048-game () "Major mode for playing 2048 in Emacs"
  (interactive)
  (switch-to-buffer "2048")
  (kill-all-local-variables)
  (setq major-mode '2048-mode)
  (setq mode-name "2048")
  (toggle-read-only t))

(defvar *2048-board* nil
  "The board itself. If a number is in the square, the number is stored. Otherwise, 0 is stored.")
