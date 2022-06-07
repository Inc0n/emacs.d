
;;; Commentary::
;; check for python implementation
;; ~/sources/projects/sway-ipc-emacs/swayipc.py

;;; Code:

(defun sway-focus (app-id)
  "Let sway focus on window with APP-ID."
  (shell-command (format "swaymsg [app_id=%s] focus" app-id)))

(defun sway-alacritty-cd ()
  "Tell sway to focus on alacritty with DEFAULT-DIRECTORY."
  (interactive)
  ;; (kill-new )
  (shell-command
   (format "swaymsg [app_id=Alacritty] focus && virtual-kbd type %S send enter"
           (format "cd %S" default-directory))))

(defun sway-send-keys ()
  "Tell sway to focus on alacritty with DEFAULT-DIRECTORY."
  (interactive)
  (call-interactively 'describe-key)
  (shell-command
   (format "virtual-kbd send %s" ;; C-S-v
           (key-description (vector (read-key "Key: "))))))

(provide 'swayipc)
;;; swayipc.el ends here
