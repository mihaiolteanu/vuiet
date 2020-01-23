;;; emacs-lastfm-player.el --- The minimalistic and stupid emacs music player -*- lexical-binding: t -*-

;; (lastfm--defmethod album.getInfo (artist album)
;;   "Get the metadata and tracklist for an album on Last.fm using the album name."
;;   :no ("track > name" "duration"))

;; (lastfm--defmethod artist.getInfo (artist)
;;   "Get the metadata for an artist. Includes biography, max 300 characters."
;;   :no ("bio summary" "listeners" "playcount" "similar artist name" "tags tag name"))


(require 'lastfm)
(require 's)
(require 'cl-lib)
(require 'mpv)
(require 'ivy-youtube)
(require 'memoize)

;; Modify to (set-buffer-multibyte t) in elquery-read-string for correct displaying of special chars

(setq ivy-youtube-key "AIzaSyCQlbsX0-ftN3SXba2fhYZu-lWfMxSAgJ0")
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")
(setq org-confirm-elisp-link-function nil)

(defun youtube-response-id (*qqJson*)
  "Copied from ivy-youtube-wrapper in mpv.el"
  (let ((search-results (cdr (ivy-youtube-tree-assoc 'items *qqJson*))))
    (cdr (ivy-youtube-tree-assoc 'videoId (aref search-results 0)))))

(defun browse-youtube (video-id)
  (browse-url (concat "https://www.youtube.com/watch?v="
                      video-id)))

(defun play-youtube-video (video-id)
  (mpv-start "--no-video"
             (concat "https://www.youtube.com/watch?v="
                     video-id)))

(defun my-as-string (in)
  (cond ((stringp in) in)
        ((and (consp in) (cl-every #'stringp in))
         (cl-reduce #'concat in))
        (t nil)))

(defun find-youtube-id (artist+song)
  (let ((youtube-id)
        (query-str (cond ((stringp artist+song) artist+song)
                         ((and (consp artist+song)
                               (cl-every #'stringp artist+song))
                          (cl-reduce #'concat artist+song))
                         (t nil))))
    (when query-str
      (request
       "https://www.googleapis.com/youtube/v3/search"
       :params `(("part" . "snippet")
                 ("q" . ,query-str)
                 ("type" . "video")
                 ("maxResults" . 1)
                 ("key" .  ,ivy-youtube-key))
       :parser 'json-read
       :sync t
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (setf youtube-id (youtube-response-id data))))
       :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                      ;; (200 . (lambda (&rest _) (message "Got 200.")))
                      (418 . (lambda (&rest _) (message "Got 418.")))
                      (403 . (lambda (&rest _)
                               (message "403: Unauthorized. Maybe you need to enable your youtube api key"))))))
    youtube-id))

(memoize #'find-youtube-id)

(defun counsel-similar-artists (artist)
  (ivy-read "Select Artist: "
          (lastfm-artist-get-similar artist :limit 30)
          :action (lambda (a)
                    (display-artist (cl-first a)))))

(defun display-artist (artist)
  (let ((b (generate-new-buffer artist))
        (artist-info (lastfm-artist-get-info artist))
        (top-songs (lastfm-artist-get-top-tracks artist)))
    (with-current-buffer b
      (org-mode)
      (let ((bio-summary (cl-first artist-info))
            (listeners (cl-second artist-info))
            (playcount (cl-third artist-info))
            (similar-artists (cl-subseq artist-info 3 7))
            (tags (cl-subseq artist-info 8 12)))
        ;; (put-text-property 0 (length artist) 'face 'info-title-1 artist)
        (insert (format "* %s\n\n" artist))
        (insert (s-word-wrap 75 bio-summary))
        (newline) (newline)
        (insert (format "*Listeners*: %s" listeners))
        (insert (format "   *Playcount*: %s\n" playcount))
        (insert "\n** Similar artists: \n")
        (mapcar (lambda (a)
             (insert (format "[[elisp:(display-artist \"%s\")][%s]] | " a a)))
           similar-artists)
        (newline)
        (insert "\n** Popular tags: \n")
        (mapcar (lambda (a)
             (insert (format "[[elisp:(display-tag \"%s\")][%s]] | "
                             a a)))
           tags)
        (newline)
        (insert "\n** Top Songs: \n")
        (mapcar (lambda (a)
             (let ((song (cl-first a))
                   (listeners (cl-third a)))
               (insert (format "[[elisp:(listen-on-youtube \"%s %s\")][%s]] (%s)\n"
                               artist song song listeners))))
         top-songs))
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key (kbd "C-5") (lambda ()
                                   (interactive)
                                   (counsel-similar-artists artist))))
    (switch-to-buffer b)))

(defun display-tag (tag)
  (let ((b (generate-new-buffer tag))
        (tag-info (lastfm-tag-get-info tag))
        (tag-top-artists (lastfm-tag-get-top-artists tag :limit 15))
        (tag-top-songs (lastfm-tag-get-top-tracks tag :limit 15))
        ;; (tag-similar (lastfm-tag-get-similar tag)) ;; Empty response
        )
    (with-current-buffer b
      (org-mode)
      (insert (format "* %s\n\n" tag))
      (insert (s-word-wrap 75 (cl-first tag-info)))
      (newline)
      (insert "\n** Top Artists: \n")
      (mapcar (lambda (a)
           (insert (format "[[elisp:(display-artist \"%s\")][%s]]\n"
                           (cl-first a) (cl-first a))))
         tag-top-artists)
      (insert "\n** Top Songs: \n")
      (mapcar (lambda (a)
           (let ((artist (cl-first a))
                 (song (cl-second a)))
             (insert (format "[[elisp:(listen-on-youtube \"%s %s\")][%s - %s]]\n"
                             artist song artist song))))
         tag-top-songs))
    (switch-to-buffer b)))

(defun choose-from-user-loved-songs ()
  (interactive)
  (ivy-read "Select Artist: "
            (mapcar (lambda (song)
                      (format "%s - %s" (cl-first song) (cl-second song)))
                    (lastfm-user-get-loved-tracks :limit 500))
            :action (lambda (s)
                      (find-song-on-youtube s #'browse-youtube))))

(defun display-user-loved-songs (page)
  (let ((b (generate-new-buffer lastfm--username))
        (songs (lastfm-user-get-loved-tracks :limit 50 :page page)))
    (with-current-buffer b
      (org-mode)
      (insert (format "* %s \n\n" lastfm--username))
      (insert (format "** Loved Songs (Page %s): \n" page))
      (mapcar (lambda (a)
           (let ((artist (cl-first a))
                 (song (cl-second a)))
             (insert (format "[[elisp:(browse-youtube \"%s %s\")][%s - %s]]\n"
                             artist song artist song))))
              songs)
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key (kbd "=") (lambda ()
                                 (interactive)
                                 (kill-buffer)
                                 (display-user-loved-songs (1+ page))))
      (local-set-key (kbd "-") (lambda ()
                                 (interactive)
                                 (when (> page 1)
                                   (kill-buffer)
                                   (display-user-loved-songs (1- page)))))
      (local-set-key (kbd "C-5") #'choose-from-user-loved-songs))
    
    (switch-to-buffer b)))

(defun display-album (artist album)
  (let ((b (generate-new-buffer lastfm--username))
        (songs'(("About Butterflies And Children" "182") ("Places Remained" "269")
                ("The Misplay" "272") ("From Silence To Noise" "934")
                ("Someone Starts to Fade Away" "535") ("Kites" "475") ("Lightdark" "525")
                ("Cold Afterall" "379") ("Like the elephant ?" "343")
                ("YOU SAID â€˜I AM..â€™" "280") ("Clouds" "634"))
              ;; (lastfm-album-get-info artist album)
              ))
    (with-current-buffer b
      (org-mode)
      (insert (format "* %s - %s \n\n" artist album))
      (mapcar (lambda (a)
                (let ((song (cl-first a))
                      (duration (cl-second a)))
                  (insert (format "[[elisp:(browse-youtube (find-youtube-id \"%s %s\"))][%s]] (%s)\n"
                                  artist song song (format-seconds "%m:%02s" (string-to-number duration))))))
              songs))
    (switch-to-buffer b)))

(let ((playing-song)      
      (keep-playing t))

  (defun play (songs)
    (when keep-playing
      (setf playing-song (iter-next songs))
      (play-youtube-video (find-youtube-id playing-song))))

  (defun open-youtube ()
    (browse-url (concat "https://www.youtube.com/watch?v="
                        (find-youtube-id playing-song))))

  (defun stop-playing ()
    (setf keep-playing nil
          playing-song nil)
    (mpv-kill))

  (defun skip-song () (mpv-kill))
  (defun playing-song () playing-song)

  (defun play-songs (songs)    
    ;; Play the next song after the player exits (song finished, song skipped, etc.)
    (setf mpv-on-exit-hook
          (lambda (&rest event)
            (unless event
              ;; A kill event took place.
              (play songs))))
    (play songs)))
               
(iter-defun user-top-songs ()
  (let ((songs (lastfm-user-get-loved-tracks :limit 50)))
    (while t
      (let ((song (seq-random-elt songs)))
        (iter-yield (format "%s %s" (cl-first song) (cl-second song)))))))

(iter-defun artist-similar-songs (name)
  (while t
    (let* ((artist (cl-first
                    (seq-random-elt
                     (lastfm-artist-get-similar name))))
           (song (cl-first
                  (seq-random-elt
                   (lastfm-artist-get-top-tracks artist)))))
      (iter-yield (format "%s %s" artist song)))))

(iter-defun tag-similar-songs (name)
  (while t
    (let* ((artist (cl-first
                    (seq-random-elt
                     (lastfm-tag-get-top-artists name))))
           (song (cl-first
                  (seq-random-elt
                   (lastfm-artist-get-top-tracks artist)))))
      (iter-yield (format "%s %s" artist song)))))

(defun play-user-loved-songs ()
  (play-songs (user-top-songs)))

(defun play-artist-similar-songs (name)
  (play-songs (artist-similar-songs name)))

(defun play-tag-similar-songs (name)
  (play-songs (tag-similar-songs)))
