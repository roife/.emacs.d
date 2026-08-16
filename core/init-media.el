;;; init-media.el --- Multimedia playback with EMMS -*- lexical-binding: t; -*-

(defun +emms-source-file-directory-tree-fd (dir regex)
  "Return files below DIR whose absolute names match REGEX."
  (let ((directory (expand-file-name dir)))
    (when (file-directory-p directory)
      (if-let* ((fd (executable-find "fd")))
          (with-temp-buffer
            (let ((status
                   (call-process fd nil t nil
                                 "--type" "f" "--absolute-path" "--print0"
                                 "." directory)))
              (unless (zerop status)
                (error "fd failed: %s" (string-trim (buffer-string))))
              (seq-filter
               (lambda (file) (string-match-p regex file))
               (split-string (buffer-string) "\0" t))))
        (directory-files-recursively directory regex)))))

(defun +emms-extract-embedded-cover (track)
  "Return TRACK's embedded artwork as a large cover file."
  (let ((cover (expand-file-name "cover_large.png"
                                 (file-name-directory track))))
    (unless (file-readable-p cover)
      (unless (zerop
               (call-process
                (executable-find "ffmpeg") nil nil nil "-nostdin" "-v" "error" "-y"
                "-i" track "-map" "0:v:0" "-frames:v" "1" "-update" "1" cover))
        (when (file-exists-p cover) (delete-file cover))))
    (and (file-readable-p cover) cover)))

(defun +emms-extract-embedded-covers ()
  "Extract missing large covers from embedded artwork."
  (interactive)
  (require 'emms-browser)
  (let (albums)
    (dolist (track
             (+emms-source-file-directory-tree-fd
              emms-source-file-default-directory (emms-source-file-regex)))
      (unless (member (file-name-directory track) albums)
        (push (file-name-directory track) albums)
        (+emms-extract-embedded-cover track))))
  (when (hash-table-p emms-browser--cache-hash)
    (emms-browser-clear-cache-hash))
  (when (buffer-live-p emms-browser-buffer)
    (kill-buffer emms-browser-buffer)))

(defun +emms-browser-cover (directory size)
  "Return an EMMS SIZE cover from DIRECTORY, extracting large artwork on demand."
  (if (eq size 'large)
      (let ((cover (expand-file-name "cover_large.png" directory)))
        (or (and (file-readable-p cover) cover)
            (when-let* ((track (car (directory-files
                                     directory t (emms-source-file-regex)))))
              (+emms-extract-embedded-cover track))))
    (emms-browser-cache-thumbnail-async directory size)))

(defun +emms-lyrics-find-with-info-lyric (file)
  "Find external lyric FILE, falling back to the current track's tag."
  (or (emms-lyrics-find-lyric file)
      (when-let* ((track (emms-playlist-current-selected-track))
                  (_ (or (emms-track-get track 'info-lyrics)
                         (emms-info-exiftool track)))
                  (lyrics (emms-track-get track 'info-lyrics))
                  (cache-file (expand-file-name
                               (concat (md5 (emms-track-name track)) ".lrc")
                               (expand-file-name "lyrics/" emms-directory)))
                  ((stringp lyrics))
                  ((not (string-empty-p lyrics))))
        (when (or (not (file-exists-p cache-file))
                  (file-newer-than-file-p (emms-track-name track)
                                          cache-file))
          (make-directory (file-name-directory cache-file) t)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file cache-file
              (insert lyrics))))
        cache-file)))

(autoload 'emms-playlist-mode-go "emms-playlist-mode" nil t)
(autoload 'emms-tag-editor-edit "emms-tag-editor" nil t)
(autoload 'emms-lyrics-visit-lyric "emms-lyrics" nil t)
(autoload 'emms-info-exiftool "emms-info-exiftool")
(autoload 'emms-last-played-update-current "emms-last-played")

(defun +emms-ui-now-playing-view ()
  "Switch the EMMS UI Now Playing view from the invoking key."
  (interactive)
  (emms-ui-now-playing-view
   (alist-get last-command-event
              '((?C . cover) (?M . metadata)
                (?T . lyrics) (?P . playlist)))))

(use-package emms
  :straight t
  :bind (("C-c m b" . emms-smart-browse)
         ("C-c m c" . +emms-extract-embedded-covers)
         ("C-c m e" . emms-tag-editor-edit)
         ("C-c m i" . emms-show)
         ("C-c m l" . emms-playlist-mode-go)
         ("C-c m p" . emms-previous)
         ("C-c m n" . emms-next)
         ("C-c m P" . emms-pause)
         ("C-c m r" . emms-toggle-random-playlist)
         ("C-c m s" . emms-stop)
         ("C-c m y" . emms-lyrics-visit-lyric)
         ("C-c m +" . emms-volume-mode-plus)
         ("C-c m -" . emms-volume-mode-minus)
         ("<XF86AudioPlay>" . emms-pause)
         ("<XF86AudioStop>" . emms-stop)
         ("<XF86AudioPrev>" . emms-previous)
         ("<XF86AudioNext>" . emms-next))
  :init
  (setq emms-directory (no-littering-expand-var-file-name "emms/")
        emms-cache-file (no-littering-expand-var-file-name "emms/cache")
        emms-history-file (no-littering-expand-var-file-name "emms/history")
        emms-source-file-default-directory "~/Music/"
        emms-player-list '(emms-player-mpv)
        emms-player-mpv-parameters '("--quiet" "--no-video" "--force-window=no"))
  (make-directory emms-directory t)
  :config
  (require 'emms-player-mpv)
  (require 'emms-cache)
  (require 'emms-history)

  (add-to-list 'emms-track-initialize-functions
               #'emms-info-initialize-track)
  (add-hook! emms-player-started-hook #'emms-last-played-update-current)

  (with-eval-after-load 'emms-info-exiftool
    (add-to-list 'emms-info-exiftool-field-map '(info-lyrics . Lyrics)))

  (setq emms-browser-covers #'+emms-browser-cover
        emms-browser-thumbnail-small-size 64
        emms-browser-thumbnail-medium-size 128
        emms-info-functions '(emms-info-exiftool)
        emms-track-description-function #'emms-info-track-description
        emms-lyrics-find-lyric-function #'+emms-lyrics-find-with-info-lyric
        emms-lyrics-scroll-p nil
        emms-source-file-directory-tree-function #'+emms-source-file-directory-tree-fd
        emms-playlist-buffer-name "*Music*"
        emms-playlist-mode-center-when-go t
        emms-info-asynchronously t
        emms-info-auto-update t
        emms-volume-change-amount 5
        emms-volume-change-function #'emms-volume-mpv-change
        emms-show-format "♪ %s")

  (emms-cache 1)
  (setopt emms-player-mpv-update-metadata t))

(use-package consult-emms
  :straight (:host github :repo "Hugo-Heagren/consult-emms")
  :after (consult emms)
  :bind (("C-c m a" . consult-emms-library)
         ("C-c m j" . consult-emms-current-playlist))
  :config
  (setq consult-emms--sort-album-function #'string<))

(use-package emms-ui
  :straight (:host github :repo "roife/emms-ui")
  :commands (emms-ui emms-ui-albums
                     emms-ui-list emms-ui-now-playing)
  :custom
  (emms-ui-album-cover-size 220)
  (emms-ui-track-columns
   '(status info-title info-playing-time info-artist
            info-album info-genre info-year play-count))
  (emms-ui-now-playing-cover-max-size 640)
  (emms-ui-now-playing-default-view 'cover)
  :config
  (define-key emms-ui-albums-mode-map [remap meow-left]
              #'emms-ui-albums-previous)
  (define-key emms-ui-albums-mode-map [remap meow-next]
              #'emms-ui-albums-down)
  (define-key emms-ui-albums-mode-map [remap meow-prev]
              #'emms-ui-albums-up)
  (define-key emms-ui-albums-mode-map [remap meow-right]
              #'emms-ui-albums-next)

  (dolist (map (list emms-ui-albums-mode-map
                     emms-ui-list-mode-map
                     emms-ui-now-playing-mode-map))
    (define-key map (kbd "A") #'emms-ui-albums)
    (define-key map (kbd "L") #'emms-ui-list)
    (define-key map (kbd "N") #'emms-ui-now-playing))

  (define-key emms-ui-now-playing-mode-map [remap meow-left]
              #'emms-seek-backward)
  (define-key emms-ui-now-playing-mode-map [remap meow-next] #'emms-next)
  (define-key emms-ui-now-playing-mode-map [remap meow-prev] #'emms-previous)
  (define-key emms-ui-now-playing-mode-map [remap meow-right]
              #'emms-seek-forward)
  (define-key emms-ui-now-playing-mode-map [remap meow-keypad]
              #'emms-pause)
  (define-key emms-ui-now-playing-mode-map (kbd "r")
              #'emms-ui-now-playing-toggle-random)
  (define-key emms-ui-now-playing-mode-map (kbd "R")
              #'emms-ui-now-playing-toggle-repeat)

  (dolist (key '("C" "M" "T" "P"))
    (define-key emms-ui-now-playing-mode-map (kbd key)
                #'+emms-ui-now-playing-view)))
