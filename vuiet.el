;;; vuiet.el --- Discover and play music -*- lexical-binding: t -*-

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

(defgroup vuiet ()
  "Emacs music player."
  :group 'music
  :prefix "vuiet-")

(defcustom vuiet-scrobble-timeout 30
  "Time, in seconds, for the same song to play before scrobbling it.
A gigantic value basically disables scrobbling altogether."
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
value increases the changes you'll discover something totally new
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
  artist name)

(defun vuiet--new-track (artist name)
  "Prepare the ARTIST and NAME before creating a TRACK object."
  (make-vuiet-track :artist (s-trim artist)
                    :name   (s-trim name)))

(defun vuiet--track-as-string (track)
  "Return TRACK as a human-readable string."
  (format "%s %s" (vuiet-track-artist track) (vuiet-track-name track)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browser

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

(defun vuiet-artist-info (artist)
  "Display info about ARTIST in a new buffer.

p   play all the artist songs, sequentially.
s   select and display info for a similar artist with ivy.
l   visit the artist's lastfm page."
  (interactive "sArtist: ")
  (vuiet--with-vuiet-buffer artist
    (let* ((artist-info (lastfm-artist-get-info artist))
           (songs (lastfm-artist-get-top-tracks
                   artist
                   :limit vuiet-artist-tracks-limit))
           (bio-summary (car artist-info))
           ;; The subseq indices are based on the standard lastfm.el response
           ;; for artist.info
           (similar-artists (cl-subseq artist-info 3 7))
           (tags (cl-subseq artist-info 8 12)))
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
               for song in songs
               do (insert
                   (format "%2s. [[elisp:(vuiet-play '(\"%s\" \"%s\"))][%s]]\n"
                           i artist (cadr song) (cadr song))))

      (vuiet--local-set-keys
        ("p" . (vuiet-play songs))
        ("s" . (vuiet-ivy-similar-artists artist))
        ("l" . (vuiet-artist-lastfm-page artist))))))

(defun vuiet-artist-info-search (artist)
  "Search ARTIST and display info about the selected item.
Similar to `vuiet-artist-info', but search for ARTIST on last.fm
first and then let the user select one artist from the resulting
list of artists.  Vuiet then displays the info about the user
selected artist.  Useful if you don't know the exact name of the
artist."
  (interactive "sArtist: ")
  (ivy-read "Info for artist: "
            (mapcar #'car (lastfm-artist-search artist))
            :action #'vuiet-artist-info))

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
                   (list (format (s-format  "%-$0s %-$1s" 'elt
                                            ;; Add the padding
                                            `(,artist-max-len ,song-max-len))
                                 ;; Add the actual artist and song.
                                 (car song) (cadr song))
                         (vuiet--new-track (car song) (cadr song))))
                 songs)
              :action (lambda (selection)
                        (vuiet-play (cadr selection))))))

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
                   (format (concat "%3s. [[elisp:(vuiet-play '(\"%s\" \"%s\"))][%-"
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

(defun vuiet-album-info (artist album)
  "Display info about the ARTIST's ALBUM in a new buffer.

s   choose a song with ivy.
a   pick another album with ivy.
p   play all songs from the album.
l   save lyrics for this album."
  (vuiet--with-vuiet-buffer (format "%s - %s" artist album)
    (let* ((songs (lastfm-album-get-info artist album))
           ;; Align song duration in one nice column. For this, I need to know
           ;; the longest song name from the album.
           (max-len (cl-loop for entry in songs
                             maximize (length (cadr entry)))))
      
      (insert (format "* %s - %s \n\n" artist album))
      (cl-loop for i from 1
               for entry in songs
               for song = (cadr entry)
               for duration = (format-seconds
                               "%m:%02s" (string-to-number (caddr entry)))
               do (insert
                   (format (concat "%2s. [[elisp:(vuiet-play '(\"%s\" \"%s\"))][%-"
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
;; Player

(defconst vuiet--playing-track nil "Currently playing track.")

(defun vuiet--set-playing-track (track)
  "Update the currently playing track to TRACK."
  (setf vuiet--playing-track track))

(defun vuiet--playing-track ()
  "Return the currently playing track."
  vuiet--playing-track)

(defun vuiet-stop ()
  "Stop playing and clear the mode line."
  (interactive)
  (setf vuiet--playing-track nil
        mpv-on-exit-hook nil)
  (setq-default mode-line-misc-info nil)
  (mpv-kill))

(defun vuiet-playing-artist ()
  "Return the currently playing artist."
  (vuiet-track-artist (vuiet--playing-track)))

(defun vuiet-playing-track-name ()
  "Return the currently playing track name."
  (vuiet-track-name (vuiet--playing-track)))

(defun vuiet-playing-track-str ()
  "Return the playing TRACK as a human-readable string."
  (vuiet--track-as-string (vuiet--playing-track)))

(defun vuiet-next ()
  "Skip the currently playing track and play the next."
  (interactive)
  ;; The on-kill-event hook ensures the functions does what it says.
  (mpv-kill))

(defun vuiet-replay ()
  "Play the currently playing track from the beginning."
  (interactive)
  ;; No such feature in mpv.el available. Simulate one.
  (mpv-seek-backward 9999))

(defun vuiet-play-pause ()
  "Toggle the play/pause status."
  (interactive)
  (mpv-pause))

(defun vuiet-playing-artist-info ()
  "Display info for the currently playing artist in a new buffer."
  (interactive)
  (when (vuiet--playing-track)
    (vuiet-artist-info (vuiet-playing-artist))))

(defun vuiet-playing-track-search-youtube ()
  "Open a youtube search for the currently playing track."
  (interactive)
  (when (vuiet--playing-track)
    (browse-url (format "https://www.youtube.com/results?search_query=%s"
                        (vuiet-playing-track-str)))))

(defun vuiet-artist-lastfm-page (artist)
  "Visit the ARTIST lastfm page."
  (interactive "sArtist: ")
  (browse-url
   (format "https://last.fm/music/%s"
           (s-replace " " "+" artist))))

(defun vuiet-playing-artist-lastfm-page ()
  "Visit he currently playing artist lastfm page."
  (interactive)
  (when (vuiet--playing-track)
    (vuiet-artist-lastfm-page (vuiet-playing-artist))))

(defun vuiet-love-track ()
  "Add the currently playing track to the loved songs list."
  (interactive)
  (when (vuiet--playing-track)
    (lastfm-track-love (vuiet-playing-artist)
                       (vuiet-playing-track-name))))

(defun vuiet-unlove-track ()
  "Remove the currently playing track from the loved songs list."
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
      (lastfm-track-scrobble (vuiet-track-artist track) (vuiet-track-name track)
                             (int-to-string timestamp)))))

(defun vuiet--play-track (track)
  "Play the TRACK in the background with mpv and ytdl."
  (mpv-start
   "--no-video"
   (format "ytdl://ytsearch:%s" (vuiet--track-as-string track))))

(defun vuiet--next-track (tracks)
  "Yield the next VUIET-TRACK object from the TRACKS list.
If no more objects available, return nil."
  (condition-case nil
      (iter-next tracks)
    (iter-end-of-sequence nil)))

(iter-defun vuiet--make-generator (songs random)
  "Make a generator of VUIET-TRACK objects from the SONGS list.
If RANDOM is true, each call to the generator will yield a random
song and the generator is infinite.  Otherwise, the generator
will yield each (length songs) elements, sequentially."
  (while songs
    (let ((song (if random
                    (seq-random-elt songs)
                  (prog1 (car songs)
                    (setf songs (cdr songs))))))
      (iter-yield
       (vuiet--new-track (car song) (cadr song))))))

(cl-defun vuiet-play (item &key (random nil))
  "Play the ITEM with mpv and scrobble to lastfm.
RANDOM is used only if the ITEM list is not already a generator.

If ITEM is a VUIET-TRACK object, play it.

If ITEM is a (ARTIST SONG) form, where ARTIST and SONG are
strings, create a VUIET-TRACK object and call this function again
with this object.

If ITEM is a list of (ARTIST SONG) forms, create a generator of
VUIET-TRACK objects and call VUIET-PLAY again with the generator.

If ITEM is a generator, play the next VUIET-TRACK object from
that generator and set an mpv hook on exit.  When the hook is
called (mvp exists, track finished playing) call VUIET-PLAY again
with the same generator."
  (vuiet-stop)           ;Clear hooks, leave in a clean state for a new start.
  (cl-case (type-of item)
    ;; The following case was added later after a bug report. It repeats
    ;; functionality implemented already in this function. This is a quick way
    ;; to introduce the possibility that the generation function is compiled, in
    ;; which case, what is passed to vuiet-play is not a cons byt a
    ;; compiled-function
    (compiled-function (vuiet-play (vuiet--next-track item))
                       (setf mpv-on-exit-hook
                             (lambda (&rest event)
                               (unless event
                                 ;; A kill event (mpv closes) is "registered" as nil.
                                 (vuiet-play item)))))
    ;; A track can be played directly.
    (vuiet-track (let ((artist (vuiet-track-artist item))
                       (name (vuiet-track-name item)))
                   (vuiet--set-playing-track item)
                   ;; If, after timeout, the same song is playing, scrobble it.
                   (run-at-time vuiet-scrobble-timeout nil
                                #'vuiet--scrobble-track item)
                   (setq-default mode-line-misc-info
                                 (format "%s - %s        "  artist name))
                   (vuiet--play-track item)))
    (cons (cl-case (type-of (car item))
            ;; A list of '(("artist1" "song1") ("artist2" "song2") ...)
            ;; Transform them into a generator of tracks and try again.
            (cons (vuiet-play (vuiet--make-generator item random)))
            ;; A single ("artist1" "song1").
            (string (vuiet-play (vuiet--new-track (car item) (cadr item))))
            ;; A generator of track structs.
            (symbol (vuiet-play (vuiet--next-track item))
                    ;; Play the rest of the songs, after this one finishes.
                    (setf mpv-on-exit-hook
                          (lambda (&rest event)
                            (unless event
                              ;; A kill event (mpv closes) is "registered" as nil.
                              (vuiet-play item))))))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Playlists

(defun vuiet-play-artist (artist random)
  "Play the ARTIST top tracks.
If RANDOM is t, play the tracks at random, indefinitely.
The number of tracks is equal to VUIET-ARTIST-TRACKS-LIMIT."
  (interactive (list (read-string "Artist: ")
                     (y-or-n-p "Play random? ")))
  (vuiet-play (vuiet--make-generator
               (lastfm-artist-get-top-tracks
                artist
                :limit vuiet-artist-tracks-limit)
               random)))

(defun vuiet-play-playing-artist (random)
  "Play the currently playing artist's top tracks.
If RANDOM is t, play the tracks at random, indefinitely.
The number of tracks is equal to VUIET-ARTIST-TRACKS-LIMIT."
  (interactive (list (y-or-n-p "Play random? ")))
  (when (vuiet--playing-track)
    (vuiet-play-artist (vuiet-playing-artist)
                       random)))

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

(defun vuiet-play-artist-similar (artists)
  "Play tracks from artists similar to ARTISTS.
Random tracks from random artists similar to one of the ARTISTS
are played.
The number of similar artists taken into account is equal to
VUIET-ARTIST-SIMILAR-LIMIT and the number of tracks is equal to
VUIET-ARTIST-TRACKS-LIMIT."
  (interactive "sArtist(s): ")
  (vuiet-play (vuiet--artists-similar-tracks
               (mapcar #'s-trim (s-split "," artists)))))

(defun vuiet-play-playing-artist-similar ()
  "Play tracks from artists similar to the playing artist.
Random tracks from random artists similar to the currently
playing artist are played.
The number of similar artists taken into account is equal to
VUIET-ARTIST-SIMILAR-LIMIT and the number of tracks is equal to
VUIET-ARTIST-TRACKS-LIMIT."
  (interactive)
  (vuiet-play (vuiet--artists-similar-tracks
               (list (vuiet-playing-artist)))))

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

(defun vuiet-play-tag-similar (tags)
  "Play tracks from artists similar to TAGS.
Random tracks from random artists that have tags equal to one of
the TAGS are played.
The number of artists with the given tag taken into account is
equal to VUIET-TAG-ARTISTS-LIMIT while the number of tracks is
equal to VUIET-ARTIST-TRACKS-LIMIT."
  (interactive "sTag(s): ")
  (vuiet-play (vuiet--tags-similar-tracks
               (mapcar #'s-trim (s-split "," tags)))))

(defun vuiet-play-playing-tags-similar ()
  "Play tracks from artists with similar tags as the current tags.
Random tracks from random artists that have tags equal to one of
the tags of the currently playing artist are played.
The number of artists with the given tag taken into account is
equal to VUIET-TAG-ARTISTS-LIMIT while the number of tracks is
equal to VUIET-ARTIST-TRACKS-LIMIT."
  (interactive)
  (vuiet-play (vuiet--tags-similar-tracks
               (mapcar #'car (lastfm-artist-get-top-tags
                       (vuiet-playing-artist))))))

(defun vuiet-play-track (artist name)
  "Play track NAME from ARTIST."
  (interactive (list (read-string "Artist: ")
                     (read-string "Track Name: ")))
  (vuiet-play (vuiet--new-track artist name)))

(defun vuiet-play-track-search (track)
  "Search TRACK and play the selected item.
Similar to `vuiet-play-track', but search for TRACK on last.fm
first and then let the user select one of the results.  The
selected item is what is played by vuiet.  Useful if you don't
know the exact name and/or artist of the song."
  (interactive "sTrack: ")
  (vuiet--ivy-play-song (lastfm-track-search track)))

(defun vuiet-play-track-by-lyrics (lyrics)
  "Search a track by LYRICS and play it."
  (interactive "sLyrics: ")
  (let ((track (versuri-ivy-search lyrics)))
    (vuiet-play track)))

(defun vuiet-play-loved-track ()
  "Select a track from the user loved tracks and play it.
The user loved tracks list is the one associated with the
username given in the setup of the lastfm.el package."
  (interactive)
  (vuiet--ivy-play-song (lastfm-user-get-loved-tracks
                         :limit vuiet-loved-tracks-limit)))
      
(defun vuiet-play-loved-tracks (random)
  "Play all the tracks from the user loved tracks.
If RANDOM is t, play the tracks at random, indefinitely.
The user loved tracks list is the one associated with the
username given in the setup of the lastfm.el package."
  (interactive (list (y-or-n-p "Play random? ")))
  (vuiet-play (lastfm-user-get-loved-tracks
               :limit vuiet-loved-tracks-limit)
              :random random))

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

(defun vuiet-play-loved-tracks-similar ()
  "Play tracks based on artists similar to loved tracks artists.
Random tracks from random artists similar to a random artist from
the list of user loved tracks are played.
The user loved tracks list is the one associated with the
username given in the setup of the lastfm.el package.
The number of similar artists taken into account is equal to
VUIET-ARTIST-SIMILAR-LIMIT and the number of tracks is equal to
VUIET-ARTIST-TRACKS-LIMIT."
  (interactive)
  (vuiet-play (vuiet--loved-tracks-similar-tracks)))

(provide 'vuiet)

;;; vuiet.el ends here
