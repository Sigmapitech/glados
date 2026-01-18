;;; extension/emacs/config.el -*- lexical-binding: t; -*-

;; Load Quant mode
(use-package! quant-mode
  :mode "\\.qa\\'"
  :config
  ;; Optional: Add custom hooks or keybindings here
  (setq quant-indent-offset 4))
