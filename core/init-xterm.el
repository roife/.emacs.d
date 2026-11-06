;;; -*- lexical-binding: t -*-

;; warp/truncation indicator in tty
(set-display-table-slot standard-display-table
                        'wrap
                        (make-glyph-code ?↩ 'shadow))

(set-display-table-slot standard-display-table
                        'truncation
                        (make-glyph-code ?… 'shadow))

;; [Kitty Graphics Protocol] Implements support for Kitty's "graphics protocol",
;; which allows the terminal to display images and videos inline.
(use-package kitty-graphics
  :straight (:type git :host github :repo "cashmeredev/kitty-graphics.el")
  :hook (tty-setup . kitty-graphics-setup)
  :init
  (setq kitty-graphics-enable-video t))


;; [xterm]
(use-package term/xterm
  :hook (tty-setup . xterm-mouse-mode)
  :init
  ;; Probe capabilities so native Kitty keyboard support can be negotiated.
  (setq xterm-extra-capabilities 'check
        xterm-set-window-title t)

  (defun +xterm-report-background ()
    "Query the terminal background color and reload the matching theme."
    (interactive)
    (unless (display-graphic-p)
      (require 'term/xterm)
      (xterm--query "\e]11;?\e\\"
                    '(("\e]11;" . xterm--report-background-handler))
                    t)
      (let ((bg-color (terminal-parameter nil 'xterm--background-color)))
        (when bg-color
          (apply #'xterm--set-background-mode bg-color)))
      (+load-theme)
      (message "Reported terminal background as %s"
               (terminal-parameter nil 'background-mode)))))
