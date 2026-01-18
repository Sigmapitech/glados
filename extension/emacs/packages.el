;; -*- no-byte-compile: t; -*-
;;; extension/emacs/packages.el

;; Quant mode package definition for Doom Emacs
;; Note: This assumes the glados repo is cloned/symlinked to ~/.doom.d/local-packages/
(package! quant-mode
  :recipe (:local-repo "local-packages/glados/extension/emacs"
           :files ("*.el")))
