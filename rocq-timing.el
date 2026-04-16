(defvar rocq-timing-regexp
  "^Chars \\([[:digit:]]+\\) - \\([[:digit:]]+\\).*\\([[:digit:]]+\\.[[:digit:]]*\\) secs (\\([[:digit:]]+\\.[[:digit:]]*\\)u,\\([[:digit:]]+\\.[[:digit:]]*\\)s)"
  "Regex to parse lines in timing file")

(defvar rocq-timing-highlight-overlays nil
  "Whether to highlight the regions with timing information, mostly for debug purposes")

(defvar rocq-timing-colour-overlays t
  "Highlight regions based on how much time they take")

(defvar rocq-timing-q1-face '(:background "green"))
(defvar rocq-timing-q2-face '(:background "yellow"))
(defvar rocq-timing-q3-face '(:background "red"))

(defun rocq-timing-match-to-info ()
  "Extract timing and region information from matched string into a plist."
  (let ((start (string-to-number (match-string-no-properties 1)))
        (end (string-to-number (match-string-no-properties 2)))
        (time (string-to-number (match-string-no-properties 3))))
    `(:start ,start :end ,end :time ,time)))

(defun rocq-timing-parse-file (FILE)
  "Parse a FILE to get a list of timing values for segments of the file."
  (with-temp-buffer
    (insert-file-contents FILE)
    (let (matches)
      (while (re-search-forward rocq-timing-regexp nil t)
        (push (rocq-timing-match-to-info) matches))
      matches)))

(defun rocq-timing-info-to-overlay (BUFFER INFO &optional QUARTS)
  "Make an overlay in a given buffer adding a tooltip with timing information."
  (let
      ((ov (make-overlay (plist-get INFO :start) (plist-get INFO :end) BUFFER))
       (time (plist-get INFO :time)))
    (overlay-put ov 'rocq-timing t)
    (overlay-put ov 'help-echo (format "%s" time))
    (when rocq-timing-highlight-overlays (overlay-put ov 'face 'highlight))
    (when (and rocq-timing-colour-overlays QUARTS)
      (let ((q1 (plist-get QUARTS :q1))
            (q2 (plist-get QUARTS :q2))
            (q3 (plist-get QUARTS :q3)))
        (if (and (>= time q1) (< time q2))
            (overlay-put ov 'face rocq-timing-q1-face)
          (if (and (>= time q2) (< time q3))
              (overlay-put ov 'face rocq-timing-q2-face)
          (if (>= time q3)
              (overlay-put ov 'face rocq-timing-q3-face))))))))

(defun rocq-timing-info-quartiles (INFO)
  "Calculate Q1/Q2/Q3 for the timings in a chunk of info."
  (let* ((sl (sort (mapcar (lambda (i) (plist-get i :time)) INFO)))
         (len (length sl)))
    (when sl
      `(:q1 ,(nth (/ len 4) sl) :q2 ,(nth (/ len 2) sl) :q3 ,(nth (* 3 (/ len 4)) sl)))))

(defun rocq-timing-overlays ()
  "Add tooltip overlays with timing information from a .v.timing file, if it exists."
  (interactive)
  (let* ((buffer (current-buffer))
         (current-file (buffer-file-name))
         (file (concat current-file ".timing"))
         (info (rocq-timing-parse-file file))
         (quartiles (rocq-timing-info-quartiles info)))
    (remove-overlays (point-min) (point-max) 'rocq-timing t)
    (mapcar (lambda (i) (rocq-timing-info-to-overlay buffer i quartiles)) info)))

(defun rocq-timing-overlays-clear ()
  "Clear the overlays in the current buffer related to rocq-timing"
  (interactive)
  (remove-overlays (point-min) (point-max) 'rocq-timing t))

(provide 'rocq-timing)
