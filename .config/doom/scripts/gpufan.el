;;; ../../.dotfiles/.config/doom/scripts/gpufan.el -*- lexical-binding: t; -*-

(defun ross/gpu-fan-speed (speed password)
  "Set the fan speed of the GPU"
  (interactive "sEnter the desired GPU speed as a percentage: \nsEnter the superuser password: \n")
    (with-temp-buffer
      (shell-command (format "%s %s %s %s" "echo " password " | sudo -k -S ~/bin/gpufan" speed))))
(provide 'gpufan)
