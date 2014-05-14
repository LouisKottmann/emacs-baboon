(defun tabbar-buffer-groups-baboon ()
"Returns the name of the tab group for a given buffer"
(list
 (cond
  (or (get-buffer-process (current-buffer))
      (string-equal "*hackernews*" buffer-name)
   "news")
  ((memq major-mode
         '(twittering-mode erc-mode))
   "news")
  ((memq major-mode
         '(dired-mode eshell-mode))
   "emacs")
  ((string-equal "*" (substring (buffer-name) 0 1))
   "emacs")
  (t "baboon"))))

;; (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-baboon)
