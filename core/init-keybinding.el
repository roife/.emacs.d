;;; -*- lexical-binding: t -*-

;; translate [C-SPC] to [M-SPC] for mark
;; (define-key key-translation-map (kbd "C-'") (kbd "C-SPC"))
;; (bind-key "C-\"" #'pop-global-mark)


;; Chinese prunc mapping
(cl-loop for prefix in '("C-" "M-" "s-" "H-")
         do
         (cl-loop for cpunc in '("，" "。" "？" "！" "；" "：" "、" "（" "）" "【" "】" "《" "》" "—")
                  for epunc in '("," "." "?" "!" ";" ":" "," "(" ")" "[" "]" "<" ">" "_")
                  do (define-key key-translation-map (kbd (concat prefix cpunc)) (kbd (concat prefix epunc)))))


(global-set-key (kbd "s-a") #'mark-whole-buffer)
(global-set-key (kbd "s-x") #'kill-region)
(global-set-key (kbd "s-s") #'save-buffer)
(global-set-key (kbd "s-v") #'yank)
(global-set-key (kbd "s-c") #'copy-region-as-kill)
(global-set-key (kbd "s-z") #'undo)
(global-set-key (kbd "s-Z") #'undo-redo)
(global-set-key (kbd "s-f") #'isearch-forward)
(global-set-key (kbd "s-w") #'tab-close)
(global-set-key (kbd "s-t") #'tab-new)
(global-set-key (kbd "s-o") #'other-window)
