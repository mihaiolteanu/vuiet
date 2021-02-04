;;; vuiet.el --- The music player and explorer for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Mihai Olteanu

;; Author: Mihai Olteanu <mihai_olteanu@fastmail.fm>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (lastfm "1.1") (versuri "1.0") (s "1.12.0") (bind-key "2.4") (mpv "0.1.0"))
;; Keywords: multimedia
;; URL: https://github.com/mihaiolteanu/vuiet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Discover and play new music with Emacs.

;; All the info for top tracks, top artists, tags and all similarities between
;; them are taken from last.fm

;;; Code:

(require 'lastfm)
(require 'versuri)
(require 's)
(require 'cl-lib)
(require 'bind-key)
(require 'mpv)
(require 'generator)

(defgroup vuiet ()
  "Emacs music player."
  :group 'music
  :prefix "vuiet-")

(defcustom vuiet-scrobble-timeout 30
  "Time, in seconds, for the same song to play before scrobbling it."
  :type '(number :tag "seconds")
  :group 'vuiet)

(defcustom vuiet-scrobble-enabled t
  "Enable/disable last.fm scrobbling.
Decide if the currently playing track should appear in your list
of recently played tracks on last.fm."
  :type '(boolean :tag "enabled")
  :group 'vuiet)

(defcustom vuiet-update-mode-line-automatically t
  "Enable/disable the automatic update of the mode-line.
If enabled, the mode-line is automatically updated after
`vuiet-update-mode-line-interval' seconds. More specifically,
`vuiet-update-mode-line' is called periodically while a track is
playing to update it's current playback position."
  :type '(boolean :tab "enabled")
  :group 'vuiet)

(defcustom vuiet-update-mode-line-interval 10
  "Timeout, in seconds, after which to update the mode-line.
See the `vuiet-update-mode-line-automatically' custom variable
for details."
  :type '(number :tag "seconds")
  :group 'vuiet)

(defcustom vuiet-artist-similar-limit 15
  "Number of artists similar to the given artist.
When considering artists similar to a given artist, take as many
into consideration as this limit.  A lower value might mean
artists and tracks you already know and love.  A higher value
increases the chances you'll discover something totally new."
  :type '(number :tag "count")
  :group 'vuiet)

(defcustom vuiet-artist-tracks-limit 15
  "Number of tracks for the given artist.
When considering the top tracks for a given artist, take as many
into consideration as this limit.  A lower value might mean
tracks from this artist that you already know and love.  A higher
value increases the chances you'll discover something totally new
but it also increases the chances that you'll get wrongly
scrobbled songs and youtube will find something totally unrelated
as a result."
  :type '(number :tag "count")
  :group 'vuiet)

(defcustom vuiet-tag-artists-limit 15
  "Number of artists for the given tag.
When considering the top artists for a given tag, take as many
into consideration as this limit."
  :type '(number :tag "count")
  :group 'vuiet)

(defcustom vuiet-loved-tracks-limit 500
  "Number of tracks to take into consideration when playing user loved tracks.
A number higher than your actual lastfm loved tracks, will take
all of them into consideration.  A lower values is useful for
taking into consideration only the most recently loved tracks."
  :type '(number :tag "count")
  :group 'vuiet)

(cl-defstruct vuiet-track
  artist name duration)

(defun vuiet--new-track (artist name &optional duration)
  "Prepare the ARTIST and NAME before creating a TRACK object."
  (make-vuiet-track :artist   (s-trim artist)
                    :name     (s-trim name)
                    :duration (or duration "")))

(defun vuiet--track-as-string (track)
  "Return TRACK as a human-readable string."
  (format "%s %s" (vuiet-track-artist track) (vuiet-track-name track)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro vuiet--artist-from-minibuffer-if-nil (artist)
  "If ARTIST is nil, request an artist from the minibuffer.
Use lastfm for autocomplete if, while in the minibuffer, the TAB
key is pressed."
  (declare (debug t)
           (indent defun))
  `(unless ,artist
     (let ((keymap (copy-keymap minibuffer-local-map)))
       (define-key keymap (kbd "<tab>")
         (lambda () (interactive)
           (ivy-read "Artist: "
                     (lastfm-artist-search (minibuffer-contents))
                     :action (lambda (s)   
                               (setf ,artist (car s))
                               (delete-minibuffer-contents)
                               (exit-minibuffer)))))
       (let ((mini (read-from-minibuffer
                    "Artist (TAB for completion): " nil keymap)))
         (unless (string-empty-p mini)
           ;; if completion was used, the minibuffer returns an empty string.
           (setf ,artist mini))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vuiet-mode-map
  (let ((keymap (make-sparse-keymap)))
    (bind-keys :map keymap
           ("C-m" . org-open-at-point)
           ("q"   . kill-current-buffer)
           ("j"   . next-line)
           ("k"   . previous-line)
           ("l"   . forward-char)
           ("h"   . backward-char))
    keymap))

(define-derived-mode vuiet-mode org-mode "Vuiet")

(defmacro vuiet--with-vuiet-buffer (name &rest body)
  "Basic setup for a VUIET-MODE buffer.
Create a new buffer with name NAME if it does not exist.  Turn
on VUIET-MODE for that buffer, eval BODY and then switch to it."
  (declare (debug t)
           (indent defun))
  (let ((b (make-symbol "buffer")))
    `(aif (get-buffer ,name)
         ;; Don't create a new buffer if one already exists.
         (switch-to-buffer it)
       (let ((,b (generate-new-buffer ,name)))
         (with-current-buffer ,b
           (vuiet-mode)
           ,@body)
         (switch-to-buffer ,b)
         (org-previous-visible-heading 1)))))

(defmacro vuiet--local-set-keys (&rest bindings)
  "Set multiple key BINDINGS at once.
BINDINGS is a list of (KEY . EXPR) forms.  The expansion sets up
a local binding such that KEY executes EXPR."
  (declare (debug t)
           (indent defun))
  `(progn
     ,@(mapcar (lambda (binding)
                 `(local-set-key
                   (kbd ,(car binding))
                   (lambda () (interactive)
                     ,(cdr binding))))
               bindings)))

(defun vuiet-ivy-similar-artists (artist)
  "Search similar artists to ARTIST with ivy."
  (interactive "sArtist: ")
  (ivy-read "Select Artist: "
            (lastfm-artist-get-similar
             artist
             :limit vuiet-artist-similar-limit)
          :action (lambda (a)
                    (vuiet-artist-info (car a)))))

;;;###autoload
(defun vuiet-artist-info (&optional artist)
  "Display info about ARTIST in a new buffer.

p   play all the artist songs, sequentially.
s   select and display info for a similar artist with ivy.
l   visit the artist's lastfm page."
  (interactive)
  (vuiet--artist-from-minibuffer-if-nil artist)
  (vuiet--with-vuiet-buffer artist
    (let* ((artist-info (lastfm-artist-get-info artist))
           (songs (lastfm-artist-get-top-tracks
                   artist
                   :limit vuiet-artist-tracks-limit))
           (bio-summary (car artist-info))
           ;; The subseq indices are based on the standard lastfm.el response
           ;; for artist.info
           (similar-artists (cl-subseq artist-info 3 7))
           (tags (cl-subseq artist-info 8)))
      (insert (format "* %s\n\n %s"
                      artist
                      (s-word-wrap 75 (replace-regexp-in-string
                                       "<a.*a>" "" bio-summary))))

      (insert "\n\n* Similar artists: \n")
      (dolist (artist similar-artists)
        (insert (format "|[[elisp:(vuiet-artist-info \"%s\")][%s]]| "
                        artist artist)))
      
      (insert "\n\n* Popular tags: \n")
      (dolist (tag tags)
        (insert (format "|[[elisp:(vuiet-tag-info \"%s\")][%s]]| "
                        tag tag)))
      
      (insert "\n\n* Top Songs: \n")
      (cl-loop for i from 1
               for track in songs
	       as song = (s-replace-all '(("[" . "(") ("]" . ")")) (cadr track))
               do (insert
                   (format "%2s. [[elisp:(vuiet-play '((\"%s\" \"%s\")))][%s]]\n"
                           i artist song song)))

      (vuiet--local-set-keys
        ("p" . (vuiet-play songs))
        ("s" . (vuiet-ivy-similar-artists artist))
        ("l" . (vuiet-artist-lastfm-page artist))))))

;;;###autoload
(defun vuiet-artist-info-search (artist)
  "Search ARTIST and display info about the selected item.
Similar to `vuiet-artist-info', but search for ARTIST on last.fm
first and then display the info about it."
  (interactive "sArtist: ")
  (ivy-read "Info for artist: "
            (mapcar #'car (lastfm-artist-search artist))
            :action #'vuiet-artist-info))

;;;###autoload
(defun vuiet-tag-info (tag)
  "Display info about TAG in a new buffer."
  (interactive "sTag: ")
  (vuiet--with-vuiet-buffer tag
    (let ((info (lastfm-tag-get-info tag))
          ;; (songs (lastfm-tag-get-top-tracks tag :limit 15)) ;; Ignore it
          ;; (tag-similar (lastfm-tag-get-similar tag)) ;; Empty response
          (artists (lastfm-tag-get-top-artists
                    tag
                    :limit vuiet-tag-artists-limit)))
      (insert (format "* %s\n\n %s \n"
                      tag
                      (s-word-wrap 75 (replace-regexp-in-string
                                       "<a.*a>" "" (car info)))))
     
      (insert "\n* Top Artists: \n")
      (cl-loop for i from 1
               for artist in artists
               do (insert
                   (format "%2s. [[elisp:(vuiet-artist-info \"%s\")][%s]]\n"
                           i (car artist) (car artist))))
      ;; Top songs for tags are usually just songs for 2, maximum 3 artists and
      ;; are usually bullshit and non-relevant for that tag. Skip it.
      )))

(defun vuiet--ivy-play-song (songs)
  "Choose a song from SONGS with ivy and play it."
  (cl-multiple-value-bind (artist-max-len song-max-len)
      (cl-loop for entry in songs
               maximize (length (car entry)) into artist
               maximize (length (cadr entry)) into song
               finally (return (cl-values artist song)))
    (ivy-read "Play song: "
              (mapcar (lambda (song)
                   (list (format (s-format  "%-$0s   %-$1s" 'elt
                                            ;; Add the padding
                                            `(,artist-max-len ,song-max-len))
                                 ;; Add the actual artist and song.
                                 (car song) (cadr song))
                         (list (car song) (cadr song))))
                 songs)
              :action (lambda (selection)
                        (vuiet-play (list (cadr selection)))))))

(cl-defun vuiet-loved-tracks-info (&key (page 1) (n 50))
  "Display N tracks from the user loved tracks in a new buffer.
If the user has more than N loved tracks, PAGE can be used to show
the next PAGE * N tracks.

<enter>  On a song entry, plays that song only.
i        Display the next PAGE * N songs.
u        Display the previous PAGE * N songs, if N > 1
s        Choose a song to play, with ivy."
  (interactive)
  (vuiet--with-vuiet-buffer "loved-songs"
    (let* ((songs (lastfm-user-get-loved-tracks :limit n :page page))
           (max-len (cl-loop for entry in songs
                             maximize (length (car entry)))))
      (insert (format "** Loved Songs (Page %s): \n" page))
      (cl-loop for i from (+ 1 (* (1- page) n))
               for entry in songs
               for artist = (car entry)
               for song = (cadr entry)
               do (insert
                   (format (concat "%3s. [[elisp:(vuiet-play '((\"%s\" \"%s\")))][%-"
                                   (number-to-string max-len)
                                   "s  %s]]\n")
                           i artist song artist song))

      (vuiet--local-set-keys
        ("i" . (progn (kill-buffer)
                      (vuiet-loved-tracks-info (1+ page))))
        ("u" . (when (> page 1)
                 (kill-buffer)
                 (vuiet-loved-tracks-info (1- page))))
        ("s" . (vuiet--ivy-play-song songs)))))))

(defun vuiet-album-info (&optional artist album)
  "Display info about the ARTIST's ALBUM in a new buffer.

s   choose a song with ivy.
a   pick another album with ivy.
p   play all songs from the album.
l   save lyrics for this album."
  (interactive)
  (vuiet--artist-from-minibuffer-if-nil artist)
  (unless album
    (ivy-read (format "%s Album:" artist)
            (lastfm-artist-get-top-albums artist)
            :action (lambda (a)
                      (setf album (car a)))))
  (vuiet--with-vuiet-buffer (format "%s - %s" artist album)
    (let* ((songs (lastfm-album-get-info artist album))
           ;; Align song duration in one nice column. For this, I need to know
           ;; the longest song name from the album.
           (max-len (cl-loop for entry in songs
                             maximize (length (cadr entry)))))
      
      (insert (format "* %s - %s \n\n" artist album))
      (cl-loop for i from 1
               for entry in songs
	       for song = (s-replace-all '(("[" . "(") ("]" . ")")) (cadr entry))
               for duration = (format-seconds
                               "%m:%02s" (string-to-number (caddr entry)))
               do (insert
                   (format (concat "%2s. [[elisp:(vuiet-play '((\"%s\" \"%s\")))][%-"
                                   (number-to-string (1+ max-len))
                                   "s]] %s\n")
                           i artist song song duration)))

      (vuiet--local-set-keys
        ("s" . (vuiet--ivy-play-song songs))
        ("a" . (vuiet-album-info-search artist)) ;try another album.
        ("p" . (vuiet-play songs))
        ("l" . (versuri-save-bulk songs 10))))))

(defun vuiet-album-info-search (artist)
  "Search all albums from ARTIST and display the selected one.
The album is displayed in a dedicated buffer.  See
`vuiet-album-info' for details regarding the active keybindings
inside this buffer."
  (interactive "sArtist: ")
  (ivy-read "Select Album: "
            (lastfm-artist-get-top-albums artist)
            :action (lambda (album)
                      (vuiet-album-info artist (car album)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Player
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let (timer)
  (defun vuiet--set-update-mode-line-timer ()
    (unless timer
      (when vuiet-update-mode-line-automatically
        (setf timer
         (run-at-time vuiet-update-mode-line-interval
                      vuiet-update-mode-line-interval
                      #'vuiet-update-mode-line)))))

  (defun vuiet--reset-update-mode-line-timer ()
    (when timer
      (cancel-timer timer)
      (setf timer nil))))

(let (playing-track)
  (defun vuiet--playing-track-set (track)
    "Update the currently playing track."
    (setf playing-track track))

  (defun vuiet--playing-track ()
    "Return the currently playing track."
    playing-track))

(defun vuiet-enable-scrobbling ()
  "Enable last.fm scrobbling."
  (interactive)
  (setf vuiet-scrobble-enabled t))

(defun vuiet-disable-scrobbling ()
  "Disable last.fm scrobbling."
  (interactive)
  (setf vuiet-scrobble-enabled nil))

(defun vuiet-update-mode-line (&optional position)
  "Update the mode-line."
  (interactive)
  (let ((track (vuiet--playing-track)))
    (when track
      (setq-default mode-line-misc-info
        (list (format "%s - %s [%s/%s] "
                (vuiet-track-artist   track)
                (vuiet-track-name     track)
                (format-time-string "%M:%S"
                 (or position
                     ;; At startup, the running track may be set, by the
                     ;; file might not be loaded yet.
                     (condition-case nil
                         (mpv-get-playback-position)
                       (error 0))))
                (vuiet-track-duration track))))))
  (force-mode-line-update t))

(defun vuiet-stop ()
  "Stop playing and clear the mode line."
  (interactive)
  (vuiet--playing-track-set nil)
  (setf mpv-on-exit-hook nil)
  (mpv-kill)
  (vuiet--reset-update-mode-line-timer)
  (setq-default mode-line-misc-info nil))

(defun vuiet-playing-artist ()
  "Return the currently playing artist."
  (awhen (vuiet--playing-track)
    (vuiet-track-artist it)))

(defun vuiet-playing-track-name ()
  "Return the currently playing track name."
  (awhen (vuiet--playing-track)
    (vuiet-track-name it)))

(defun vuiet-playing-track-str ()
  "Return the playing TRACK as a human-readable string."
  (awhen (vuiet--playing-track)
    (vuiet--track-as-string it)))

(defun vuiet-next ()
  "Skip the currently playing track and play the next."
  (interactive)
  (condition-case nil
      (mpv-run-command "playlist-next")
    (error (display-message-or-buffer "No track available; Try again"))))

(defun vuiet-peek-next ()
  "Display the next track in the mode-line for a few seconds."
  (interactive)
  (let ((urls (vuiet--mpv-playlist-remaining-urls)))
    (when (> (length urls) 1)
      (let ((track (vuiet--track-from-youtube-url
                    (cadr (vuiet--mpv-playlist-remaining-urls)))))
        (setq-default mode-line-misc-info
         (list (format "%s - %s (%s) "
                       (vuiet-track-artist   track)
                       (vuiet-track-name     track)
                       (vuiet-track-duration track))))
        (run-at-time 3 nil #'vuiet-update-mode-line)))))

(defun vuiet-previous ()
  "Replay the previous track.
It only considers tracks from the current playlist."
  (interactive)
  (condition-case nil
      (mpv-run-command "playlist-prev")
    (error (display-message-or-buffer "This is the first track"))))

(defun vuiet-replay ()
  "Play the currently playing track from the beginning."
  (interactive)  
  (mpv-seek 1)
  (vuiet-update-mode-line))

(defun vuiet-seek-backward (arg)
  "Seek backward the given number of ARG.  ARG defaults to 5 seconds."
  (interactive "p")
  (mpv-seek-backward (if current-prefix-arg arg 5))
  (vuiet-update-mode-line))

(defun vuiet-seek-forward (arg)
  "Seek forward the given number of ARG.  ARG defaults to 5 seconds."
  (interactive "p")
  (mpv-seek-forward (if current-prefix-arg arg 5))
  (vuiet-update-mode-line))

(defun vuiet-seek-backward-rate (arg)
  "Seek backward ARG% of the track.  ARG defaults to 10%."
  (interactive "p")
  (mpv-seek-backward (round (* (if current-prefix-arg arg 10)
			       (/ (mpv-get-duration) 100))))
  (vuiet-update-mode-line))

(defun vuiet-seek-forward-rate (arg)
  "Seek forward ARG% of the track.  ARG defaults to 10%."
  (interactive "p")
  (mpv-seek-forward (round (* (if current-prefix-arg arg 10)
			      (/ (mpv-get-duration) 100))))
  (vuiet-update-mode-line))

(defun vuiet-play-pause ()
  "Toggle the play/pause status."
  (interactive)
  (mpv-pause)
  (vuiet-update-mode-line))

(defun vuiet-player-volume ()
  "Get the music player volume, between 0% and 100%."
  (mpv-get-property "volume"))

(cl-defun vuiet-player-volume-inc (&key (step 1))
  "Increase the music player volume by STEP percent."
  (interactive)
  (let* ((volume (vuiet-player-volume))
         (new-volume (+ volume step)))
    (when (<= new-volume 100)
      (mpv-set-property "volume" new-volume))))

(cl-defun vuiet-player-volume-dec (&key (step 1))
  "Decrease the mpv player volume by STEP percent."
  (interactive)
  (let* ((volume (vuiet-player-volume))
         (new-volume (- volume step)))
    (when (>= new-volume 0)
      (mpv-set-property "volume" new-volume))))

(defun vuiet-playing-artist-info ()
  "Display info for the currently playing artist in a new buffer."
  (interactive)
  (awhen (vuiet-playing-artist)
    (vuiet-artist-info it)))

(defun vuiet-playing-track-search-youtube ()
  "Open a youtube search for the currently playing track."
  (interactive)
  (awhen (vuiet-playing-track-str)
    (browse-url
     (format "https://www.youtube.com/results?search_query=%s" it))))

(defun vuiet--youtube-link-at-position ()
"Return youtube link of the current track at the playback position."
  (concat "https://www.youtube.com/"
  	  (mpv-get-property "filename")
  	  "&t=" (int-to-string
		 (round (mpv-get-playback-position)))))

(defun vuiet-playing-track-continue-on-youtube ()
  "Pause vuiet and continue playing on youtube."
  (interactive)
  (vuiet-play-pause)
  (when (vuiet-playing-artist)
    (browse-url
     (vuiet--youtube-link-at-position))))

(defun vuiet-playing-track-continue-with-mpv ()
  "Pause vuiet and continue playing with mpv as a new process."
  (interactive)
  (vuiet-play-pause)
  (when (vuiet-playing-artist)
    (start-process "vuiet" nil "mpv"
		   (vuiet--youtube-link-at-position))))

(defun vuiet-artist-lastfm-page (artist)
  "Visit the ARTIST lastfm page."
  (interactive "sArtist: ")
  (browse-url
   (format "https://last.fm/music/%s"
           (s-replace " " "+" artist))))

(defun vuiet-playing-artist-lastfm-page ()
  "Visit he currently playing artist lastfm page."
  (interactive)
  (awhen (vuiet-playing-artist)
    (vuiet-artist-lastfm-page it)))

(defun vuiet-love-track ()
  "Add the currently playing track to the user loved songs."
  (interactive)
  (when (vuiet--playing-track)
    (lastfm-track-love (vuiet-playing-artist)
                       (vuiet-playing-track-name))))

(defun vuiet-unlove-track ()
  "Remove the currently playing track from the user loved songs."
  (interactive)
  (when (vuiet--playing-track)
    (lastfm-track-unlove (vuiet-playing-artist)
                         (vuiet-playing-track-name))))

(defun vuiet-playing-track-lyrics ()
  "Display the lyrics for the currently playing track in a new buffer.
See `versuri-display' for the active keybindings inside this buffer."
  (interactive)
  (when (vuiet--playing-track)
    (versuri-display (vuiet-playing-artist)
                     (vuiet-playing-track-name))))

(defun vuiet--scrobble-track (track)
  "Scrobble TRACK on lastfm, if it's the same as the playing track."
  (when (equal track (vuiet--playing-track))
    (let ((timestamp (round (time-to-seconds (current-time)))))
      (lastfm-track-scrobble (vuiet-track-artist track)
                             (vuiet-track-name track)
                             (int-to-string timestamp)))))

(defun vuiet--next-track (tracks)
  "Yield the next VUIET-TRACK object from the TRACKS list.
If no more objects available, return nil."
  (condition-case nil
      (iter-next tracks)
    (iter-end-of-sequence nil)))

;;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Playlists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(iter-defun vuiet--make-generator (songs random)
  "Make a generator of VUIET-TRACK objects from the SONGS list.
If RANDOM is true, each call to the generator will yield a random
song and the generator is infinite.  Otherwise, the generator
will yield each of the (length songs) elements, sequentially."
  (while songs
    (let ((song (if random
                    (seq-random-elt songs)
                  (prog1 (car songs)
                    (setf songs (cdr songs))))))
      (iter-yield
       (vuiet--new-track (car song) (cadr song))))))

;; Keep a hash table of youtube track ids and vuiet-tracks
(let ((id-tracks (make-hash-table :test 'equal)))
  (defun vuiet--add-id-track (id track)
    (puthash id track id-tracks))

  (defun vuiet--get-track-from-id (id)
    (gethash id id-tracks)))

(defun vuiet--mpv-playlist-remaining-urls ()
  "Return the remaining mpv urls, including this one."
  (let ((pos   (mpv-get-property "playlist-pos"))
        (count (mpv-get-property "playlist-count")))
    (cl-loop for c from pos to (- count 1)
             collect (mpv-get-property
                      (format "playlist/%s/filename" c)))))

(defun vuiet--mpv-playlist-remaining-urls-count ()
  (- (mpv-get-property "playlist-count")
     (mpv-get-property "playlist-pos-1")))

(defun vuiet--track-from-youtube-url (url)
  "Return the track object associated with the youtube id."
  (vuiet--get-track-from-id
   (cadr (s-split "=" url))))

(defun vuiet--mpv-playing-track ()
  "Return the currently mpv playing track."
  (vuiet--track-from-youtube-url
   (mpv-get-property "filename")))

(defun vuiet--mpv-add-track (generator &optional keep-old)
  "Append the next track url from GENERATOR to mpv.
If the GENERATOR is not empty, get the next track from it, find
it's youtube url and append it to the currently running mpv
instance playlist.  When the url is played in the future, I want
to also have a means to recover the track name from it, thus, I
am saving the track for the given youtube url in a hash table."
  (let ((track (vuiet--next-track generator)))
    (when track
      (set-process-filter
       (start-process "ytdl" nil "youtube-dl"
                      (format "ytsearch:%s" (vuiet--track-as-string track))
                      "--get-id"
                      "--get-duration")
       (lambda (_ id-duration-str)
         (let* ((id-duration (s-split "\n" id-duration-str))
                (id (car id-duration))
                (url (concat "https://www.youtube.com/watch?v=" id))
                (duration (cadr id-duration)))
           (setf (vuiet-track-duration track) duration)
           (vuiet--add-id-track id track)
           (if keep-old
               (mpv-run-command "loadfile" url "append-play")
             (mpv-run-command "loadfile" url "replace"))))))))

(let (gen)
  (defun vuiet--play (generator)
    "Start mpv and play the tracks from the GENERATOR."
    (unless (mpv-live-p)
      (mpv-start "--no-video"
                 "--idle=yes"
                 "--keep-open=yes")
      (setf mpv-on-event-hook
       (lambda (event)        
         (pcase (cdr (car event))
           ("playback-restart"
            (mpv-set-property "pause" "no"))
           ("start-file"
            ;; Buffer some tracks beforehand, if needed.
            (when (< (vuiet--mpv-playlist-remaining-urls-count) 1)
              (vuiet--mpv-add-track gen t)))
           ("file-loaded"
            (let ((track (vuiet--mpv-playing-track)))
              (vuiet--playing-track-set track)
              (vuiet-update-mode-line 0)
              (mpv-set-property "pause" "no")
              (when vuiet-scrobble-enabled
                (run-at-time vuiet-scrobble-timeout nil
                             #'vuiet--scrobble-track track))))))))
    ;; If the player was already started, change the generator and clear the
    ;; mpv playlist but leave the hooks and everything else in place.
    (setf gen generator)
    (if current-prefix-arg
        ;; Calling any play command with prefix arg, postpones the changing of
        ;; playlists until the current track ends.
        (progn          
          (setf current-prefix-arg nil)
          ;; Keep only the currently playing track in the playlist.
          (mpv-run-command "playlist-clear")
          (vuiet--mpv-add-track generator t))                        
      (mpv-set-property "pause" "yes")
      (vuiet--mpv-add-track generator nil))
    (vuiet--set-update-mode-line-timer)))

;;;###autoload
(cl-defun vuiet-play (songs &key (random nil))
  "Play everyting in the SONGS list, randomly or sequentially.
SONGS is a list of type ((artist1 song1) (artist2 song2) ...)."
  (vuiet--play
   (vuiet--make-generator songs random)))

;;;###autoload
(defun vuiet-play-artist (&optional artist)
  "Play the ARTIST top tracks, RANDOM or sequentially."
  (interactive)
  (vuiet--artist-from-minibuffer-if-nil artist)
  (vuiet-play (lastfm-artist-get-top-tracks
               artist
               :limit vuiet-artist-tracks-limit)
              :random (y-or-n-p (format "%s: Play random? " artist))))

;;;###autoload
(defun vuiet-play-playing-artist ()
  "Play the currently playing artist's top tracks."
  (interactive)
  (when (vuiet--playing-track)
    (vuiet-play-artist (vuiet-playing-artist))))

(defun vuiet-play-playing-track-album ()
  "Play the full album of the currently playing track."
  (interactive)
  (when (vuiet--playing-track)
    (let* ((artist (vuiet-playing-artist))
           (song (vuiet-playing-track-name))
           (album (car (lastfm-track-get-info artist song))))
      (vuiet-play (lastfm-album-get-info artist album)))))

(defun vuiet-info-playing-track-album ()
  "Open an info buffer for the currently playing track album."
  (interactive)
  (when (vuiet--playing-track)
    (let* ((artist (vuiet-playing-artist))
           (song (vuiet-playing-track-name))
           (album (car (lastfm-track-get-info artist song))))
      (vuiet-album-info artist album))))

;;;###autoload
(defun vuiet-play-album (&optional artist album)
  "Play the whole ALBUM of the given ARTIST.
If called interactively, the album can be picked interactively
from the ARTIST's top albums, where ARTIST is given in the
minibuffer."
  (interactive)
  (if (and artist album)
      (vuiet-play (lastfm-album-get-info artist album))
    (vuiet--artist-from-minibuffer-if-nil artist)
    (ivy-read (format "Play %s Album" artist)
       (lastfm-artist-get-top-albums artist)
       :action (lambda (album)
                 (vuiet-play
                  (lastfm-album-get-info artist (car album)))))))

(iter-defun vuiet--artists-similar-tracks (artists)
  "Return a generator of tracks based on the given ARTISTS.
The generator yields top tracks from artists similar to the given
artist or the given list of artists."
  (while t
    (let* ((artists (lastfm-artist-get-similar
                     (seq-random-elt artists)
                     :limit vuiet-artist-similar-limit))
           (artist (car (seq-random-elt artists)))
           (track  (cadr (seq-random-elt
                       (lastfm-artist-get-top-tracks
                        artist
                        :limit vuiet-artist-tracks-limit)))))
      (iter-yield (vuiet--new-track artist track)))))

;;;###autoload
(defun vuiet-play-artist-similar (&optional artists)
  "Play tracks from artists similar to ARTISTS.
ARTISTS is a list of strings of the form '(artist1 artist2 etc.)
If called interactively, multiple artists can be provided in the
minibuffer if they are sepparated by commas."
  (interactive)
  (vuiet--artist-from-minibuffer-if-nil artists)
  (vuiet--play
   (vuiet--artists-similar-tracks
    (if (stringp artists)
        ;; The function was called interactively.
        (mapcar #'s-trim (s-split "," artists))
      artists))))

(defun vuiet-play-playing-artist-similar ()
  "Play tracks from artists similar to the playing artist.
This function is similar to `vuiet-play-artist-similar', only the
list of artists is limited to the artist of the currently playing
track."
  (interactive)
  (vuiet-play-artist-similar (vuiet-playing-artist)))

(iter-defun vuiet--tags-similar-tracks (tags)
  "Return a generator of tracks based on the given TAGS.
Return a random track from a random artist for a random tag in
the list of TAGS."
  (while t
    (let* ((artists (lastfm-tag-get-top-artists
                     (seq-random-elt tags)
                     :limit vuiet-tag-artists-limit))
           (artist (car (seq-random-elt artists)))
           (track  (cadr (seq-random-elt
                       (lastfm-artist-get-top-tracks
                        artist
                        :limit vuiet-artist-tracks-limit)))))
      (iter-yield (vuiet--new-track artist track)))))

;;;###autoload
(defun vuiet-play-tag-similar (tags)
  "Play tracks from artists similar to TAGS.
TAGS is a list of strings of the form '(tag1 tag2 etc.)
If called interactively, multiple tags can be provided in the
minibuffer if they are sepparated by commas."
  (interactive "sTag(s): ")
  (vuiet--play
   (vuiet--tags-similar-tracks
    (if (stringp tags)
        ;; The function was called interactively.
        (mapcar #'s-trim (s-split "," tags))
      tags))))

(defun vuiet-play-playing-tags-similar ()
  "Play tracks from artists with similar tags as the current tags.
Play tracks from random artists that have tags equal to one of
the tags of the currently playing artist."
  (interactive)
  (vuiet--play
   (vuiet--tags-similar-tracks
    (mapcar #'car (lastfm-artist-get-top-tags
            (vuiet-playing-artist))))))

;;;###autoload
(defun vuiet-play-track (&optional artist name)
  "Play the song NAME from the given ARTIST.
If called interactively, let the user select and play one of the
ARTIST's top songs, where ARTIST is given in the minibuffer."
  (interactive)
  (if (and artist name)
      (vuiet-play `((,artist ,name)))
    (vuiet--artist-from-minibuffer-if-nil artist)
    (vuiet--ivy-play-song
     (lastfm-artist-get-top-tracks artist
      :limit vuiet-artist-tracks-limit))) )

;;;###autoload
(defun vuiet-play-track-search (track)
  "Search TRACK and play the selected item.
Similar to `vuiet-play-track', but search for TRACK on last.fm
first and then let the user select one of the results."
  (interactive "sTrack: ")
  (vuiet--ivy-play-song (lastfm-track-search track)))

;;;###autoload
(defun vuiet-play-track-by-lyrics (lyrics)
  "Search a track by LYRICS and play it."
  (interactive "sLyrics: ")
  (let ((track (versuri-ivy-search lyrics)))
    (vuiet-play (list track))))

;;;###autoload
(defun vuiet-play-loved-track ()
  "Select a track from the user loved tracks and play it.
The user loved tracks list is the one associated with the
username given in the setup of the lastfm.el package."
  (interactive)
  (vuiet--ivy-play-song (lastfm-user-get-loved-tracks
                         :limit vuiet-loved-tracks-limit)))

;;;###autoload
(defun vuiet-play-loved-tracks (random)
  "Play all the tracks from the user loved tracks.
If RANDOM is t, play the tracks at random, indefinitely.
The user loved tracks list is the one associated with the
username given in the setup of the lastfm.el package."
  (interactive (list (y-or-n-p "Play random? ")))
  (vuiet-play (lastfm-user-get-loved-tracks
               :limit vuiet-loved-tracks-limit)
              :random random))

;;;###autoload
(defun vuiet-play-artist-loved-tracks (&optional artist random)
  "Play all the ARTIST tracks found in the user loved tracks.
Similar to `vuiet-play-loved-tracks', but play only the tracks
from the given ARTIST."
  (interactive)
  (unless artist
    (setf artist
          (ivy-read "Artist: "
                    (seq-uniq
                     (mapcar #'car (lastfm-user-get-loved-tracks
                             :limit vuiet-loved-tracks-limit))))))
  (unless random
    (setf random (y-or-n-p "Play random? ")))
  (vuiet-play
   (seq-filter (lambda (item)
                 (string-match artist (car item)))
               (lastfm-user-get-loved-tracks
                :limit vuiet-loved-tracks-limit))
   :random random))

;;;###autoload
(defun vuiet-play-recent-track ()
  "Play one of the recent listened tracks."
  (interactive)
  (vuiet--ivy-play-song (lastfm-user-get-recent-tracks
                         :limit 25)))

(iter-defun vuiet--loved-tracks-similar-tracks ()
  "Return a generator of tracks based on the user's loved tracks.
Return a random track from a random artist from the user's loved
tracks list."
  (while t
    (let* ((artist  (car (seq-random-elt
                          (lastfm-user-get-loved-tracks
                           :limit vuiet-loved-tracks-limit))))
           (similar (car (seq-random-elt
                          (lastfm-artist-get-similar
                           artist
                           :limit vuiet-artist-similar-limit))))
           (track   (cadr (seq-random-elt
                           (lastfm-artist-get-top-tracks
                            similar
                            :limit vuiet-artist-tracks-limit)))))
      (iter-yield (vuiet--new-track similar track)))))

;;;###autoload
(defun vuiet-play-loved-tracks-similar ()
  "Play tracks based on artists similar to loved tracks artists.
Play tracks from random artists similar to a random artist from
the list of user loved tracks."
  (interactive)
  (vuiet--play (vuiet--loved-tracks-similar-tracks)))

(provide 'vuiet)

;;; vuiet.el ends here
