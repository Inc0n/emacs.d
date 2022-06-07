;;; Commentary:
;;; Code:

(defgroup music nil
  "A music package that can play music files iteratively in a directory.")

(defvar music-playlists '("~/Music/playlist"))
(defvar music-player-program "mpv")

(defvar music--timer nil)
(defvar music--current-playlist nil)
(defvar music--current-music-file nil)
(defvar music--current-player-process nil)

(defun music-list-playlist-directory (dir)
  (split-string (shell-command-to-string
                 (format "%s %S"
                         insert-directory-program
                         (expand-file-name dir)))
                "[\r\n]+" t))

(defun music-probe-song-duration (file)
  "Shell command taken from https://superuser.com/a/1164869."
  (string-to-number
   (shell-command-to-string
    (format
     "ffprobe -show_entries stream=duration -of compact=p=0:nk=1 -v fatal %S"
     file))))

(defun music-current-file (&optional use-playlist)
  (if use-playlist
      (expand-file-name music--current-music-file music--current-playlist)
    music--current-music-file))

(defun music--set-next-music-file ()
  (let* ((music-files 
          (music-list-playlist-directory music--current-playlist))
         (match (member music--current-music-file music-files)))
    (if (or (null match)
            (null (cdr match)))
        (setq music--current-music-file (car music-files))
      (setq music--current-music-file (cadr music-files)))
    music--current-music-file))

(defun music-play-next-song ()
  (interactive)
  (if (process-live-p music--current-player-process)
      (progn (if music--timer
                 (cancel-timer music--timer))
             (setq music--timer (run-with-timer 1 nil #'music-play-next-song)))
    (music-play-song
     (music--set-next-music-file))))

(defun music-play-song (file)
  (let ((full-path (expand-file-name file music--current-playlist)))
    (message "playing: %s, duration: %d"
             file
             (music-probe-song-duration full-path))
    (setq music--timer
          (run-with-timer 
           ;; use ceiling to give a bit more wiggle room
           (ceiling (music-probe-song-duration full-path))
           nil
           #'music-play-next-song))
    (setq music--current-player-process
          (start-process "music-player" nil music-player-program
                         "--no-video"
                         full-path))))

(defun music-play-this-playlist (playlist-dir)
  (interactive (list (completing-read music-playlists)))
  (music-stop)
  (setq music--current-playlist playlist-dir)
  (setq music--current-music-file
        (car (music-list-playlist-directory music--current-playlist)))
  (music-play-song (music-current-file)))

(defun music-stop ()
  (interactive)
  (if (process-live-p music--current-player-process)
      (delete-process music--current-player-process))
  (if music--timer
      (cancel-timer music-timer)))

(defun music-continue ()
  ;; TODO: record and continue from paused seconds
  (music-play-song (music-current-file)))
  ;; (music-play-this-playlist (car music-playlists))
  
(provide 'music)