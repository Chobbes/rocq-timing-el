;;; rocq-timing.el --- Display timing of rocq commands in buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Calvin Beck

;; Author:  Calvin Beck <hobbes@ualberta.ca>
;; URL: https://github.com/Chobbes/rocq-timing-el
;; Created: 2026
;; Version: 0.0.1
;; Keywords: convenience, tools, rocq, coq, proofs, profiling
;; Package-Requires: ((emacs "24"))

;; Copyright 2026 Calvin Beck

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; rocq-timing-el is used to overlay timing information from
;; `.v.timing` files in the current buffer

;;; Code:

(defvar rocq-timing-regexp
  "^Chars \\([[:digit:]]+\\) - \\([[:digit:]]+\\).*\\([[:digit:]]+\\.[[:digit:]]*\\) secs (\\([[:digit:]]+\\.[[:digit:]]*\\)u,\\([[:digit:]]+\\.[[:digit:]]*\\)s)"
  "Regex to parse lines in timing file.")

(defvar rocq-timing-highlight-overlays nil
  "Whether to highlight the regions with timing information.
This is mostly
for debug purposes")
(defvar rocq-timing-colour-overlays t
  "Highlight regions based on how much time they take.")

(defvar rocq-timing-q1-face '(:background "green")
  "Face for commands that take a short time.")
(defvar rocq-timing-q2-face '(:background "yellow")
  "Face for commands that take a moderate amount of time.")
(defvar rocq-timing-q3-face '(:background "red")
  "Face for commands that take a long.")

(defun rocq-timing-match-to-info ()
  "Extract timing and region information from matched string into a plist."
  (let ((start (+ 1 (string-to-number (match-string-no-properties 1))))
        (end (+ 1 (string-to-number (match-string-no-properties 2))))
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

(defun rocq-timing-lookup-face (BRACKETS time &optional prev-face)
  "Given a list of timing brakckets ((time-0 face-0) ... (time-N face-N))
in time-sorted order, returns face-i where time-0 <= time < time-N, 
prev-face if the time is too small and time-N if the time is too big"
  (if (null BRACKETS)
      prev-face
    (let* ((curr-time (caar BRACKETS))
	   (curr-face (cadar BRACKETS))
	   (rest (cdr BRACKETS)))
      (if (< time curr-time)
	  prev-face
	(rocq-timing-lookup-face rest time curr-face)))))


(defun rocq-timing-info-to-overlay (BUFFER INFO &optional BRACKETS)
  "Make an overlay in a given buffer adding a tooltip with timing information.
The timing information comes from INFO, which is a PLIST that must
contain :start, :end, and :time properties.  BRACKETS provides a map
from time ranges to face properties."
  (let
      ((ov (make-overlay (byte-to-position (plist-get INFO :start)) (byte-to-position (plist-get INFO :end)) BUFFER))
       (time (plist-get INFO :time)))
    (overlay-put ov 'rocq-timing t)
    (overlay-put ov 'help-echo (format "%s" time))
    (when rocq-timing-highlight-overlays (overlay-put ov 'face 'highlight))
    (when (and rocq-timing-colour-overlays BRACKETS)
      (let ((timing-face (rocq-timing-lookup-face BRACKETS time nil)))
	(if timing-face (overlay-put ov 'face timing-face))))))

;; (let ((q1 (plist-get QUARTS :q1))
;;             (q2 (plist-get QUARTS :q2))
;;             (q3 (plist-get QUARTS :q3)))
;;         (if (and (>= time q1) (< time q2))
;;             (overlay-put ov 'face rocq-timing-q1-face)
;;           (if (and (>= time q2) (< time q3) (> q2 q1))
;;               (overlay-put ov 'face rocq-timing-q2-face)
;;           (if (and (>= time q3) (> q3 q2))
;;               (overlay-put ov 'face rocq-timing-q3-face))))))))

(defun rocq-timing-info-quartiles (INFO)
  "Calculate Q1/Q2/Q3 for the timings in a chunk of INFO."
  (let* ((sl (sort (mapcar (lambda (i) (plist-get i :time)) INFO)))
         (len (length sl)))
    (when sl
      ;; (message "len sl: %s" len)
      ;; (message "sl:\n%s" sl)
      `(:q1 ,(nth (/ len 4) sl) :q2 ,(nth (/ len 2) sl) :q3 ,(nth (* 3 (/ len 4)) sl)))))


(defcustom rocq-timing-bracket-factor 2
  "A number >= 2.  When n, color brackets will be [0,(n-1)/n] [0,
n^2-1/n^2] ... and there are log_n #times brackets.  Larger values mean
coarser-grained but sharper boundaries.")


(defun rocq-timing-info-get-nth-bracket-face (n num-brackets)
  "Calculate a face for the nth bracket out of num-brackets"
  (let ((face-color "green"))
    '(:background face-color))
  )


(defun rocq-timing-get-time-index (k len)
  (let* ((n (float (- k 1)))
	 (m (float k))
	 (r (/ n m))
	 (i (floor (* r (float len)))))
    i
    ))

(defun rocq-make-color-from-rgb (rgb)
    (color-rgb-to-hex (car rgb) (cadr rgb) (caddr rgb) 2))

(defun rocq-make-color-ratio (k)
  "returns (N^k - 1)/(N^k) as floating point where N is
   the rocq-timing-bracket-factor"
  (let* ((m (expt (float rocq-timing-bracket-factor)  (float k)))
	 (n (- m 1.0))
	 (r (/ n m)))
    r))

(defun rocq-make-color-logistic (k)
  "returns (1 / (1 + (1/k - 1)^2)"
  (/ 1.0 (+ 1.0 (expt (- (/ 1.0 k) 1.0) 2.0))))

(defun rocq-make-logistic-color-gradient (start stop step-number)
  "Return a list with STEP-NUMBER colors from START to STOP.
The color list builds a color gradient starting at color START to color
STOP. Unlike color-gradient, it includes START in the list and scales
skews the distribution exponentially in the index.  START and STOP
should be (r g b) components with values between 0.0 and 1.0 inclusive
"
  (let* ((r-start (nth 0 start))
	 (g-start (nth 1 start))
	 (b-start (nth 2 start))
	 (r-stop  (nth 0 stop))
	 (g-stop  (nth 1 stop))
	 (b-stop  (nth 2 stop))
	 (r-gap (- r-stop r-start))  ; gap to fill for red
	 (g-gap (- g-stop g-start))
	 (b-gap (- b-stop b-start))
	 result)
    ;; (message "start = %s" start)
    ;; (message "stop = %s" stop)
    (dotimes (k step-number)
      (let* ((factor (rocq-make-color-logistic (/ (float k) (float step-number)))))
	(push (list (+ r-start (* factor r-gap))
		    (+ g-start (* factor g-gap))
		    (+ b-start (* factor b-gap)))
	      result)))
    ;; (message "result = %s" result)
    (nreverse result)))


(defun rocq-make-exponential-color-gradient (start stop step-number)
  "Return a list with STEP-NUMBER colors from START to STOP.
The color list builds a color gradient starting at color START to color
STOP. Unlike color-gradient, it includes START in the list and scales
skews the distribution exponentially in the index.  START and STOP
should be (r g b) components with values between 0.0 and 1.0 inclusive
"
  (let* ((r-start (nth 0 start))
	 (g-start (nth 1 start))
	 (b-start (nth 2 start))
	 (r-stop  (nth 0 stop))
	 (g-stop  (nth 1 stop))
	 (b-stop  (nth 2 stop))
	 (r-gap (- r-stop r-start))  ; gap to fill for red
	 (g-gap (- g-stop g-start))
	 (b-gap (- b-stop b-start))
	 result)
    ;; (message "start = %s" start)
    ;; (message "stop = %s" stop)
    (dotimes (k step-number)
      (let* ((factor (rocq-make-color-ratio k)))
	(push (list (+ r-start (* factor r-gap))
		    (+ g-start (* factor g-gap))
		    (+ b-start (* factor b-gap)))
	      result)))
    ;; (message "result = %s" result)
    (nreverse result)))


(defun rocq-timing-info-brackets (INFO)
  "Calculate a heat map for timings in a chunk of info. Assumes an exponential distribution of timings."
  (let* ((sl (sort (mapcar (lambda (i) (plist-get i :time)) INFO)))
         (len (length sl))
	 (max-time (last sl))
	 (num-brackets (ceiling (log len rocq-timing-bracket-factor)))
	 (bg-color (frame-parameter nil 'background-color))
	 (brackets (rocq-make-logistic-color-gradient (color-name-to-rgb (frame-parameter nil 'background-color)) (color-name-to-rgb "#FF0000") num-brackets)))	 
	 ;; (brackets (rocq-make-exponential-color-gradient (color-name-to-rgb (frame-parameter nil 'background-color)) (color-name-to-rgb "#FF0000") num-brackets)))
	 ;; (brackets (cons (color-name-to-rgb bg-color) (color-gradient (color-name-to-rgb bg-color) (color-name-to-rgb "red") num-brackets))))
    (message "max-time = %s len = %s, num-brackets = %s" max-time len num-brackets)
    (setq k 1)
    (mapcar (lambda (color)
	      (set 'k (* k rocq-timing-bracket-factor))
	      ;; (message "k = %s" k)
	      (let* ((index (rocq-timing-get-time-index k len))
		     (time (nth index sl)))
		;; (message "i = %s time = %s" index time)
		(list time (list :background (rocq-make-color-from-rgb color)))))
	    brackets)))
    

(defun rocq-timing-overlays-clear ()
  "Clear the overlays in the current buffer related to rocq-timing."
  (interactive)
  (remove-overlays (point-min) (point-max) 'rocq-timing t))


(defun rocq-timing-overlays ()
  "Add tooltip overlays with timing information from a `.v.timing` file."
  (interactive)
  (let* ((buffer (current-buffer))
         (current-file (buffer-file-name))
         (file (concat current-file ".timing"))
         (info (rocq-timing-parse-file file))
	 (brackets (rocq-timing-info-brackets info)))
    (rocq-timing-overlays-clear)    
    (message "%s" brackets)
    (remove-overlays (point-min) (point-max) 'rocq-timing t)
    (mapcar (lambda (i) (rocq-timing-info-to-overlay buffer i brackets)) info)))

(provide 'rocq-timing)
;;; rocq-timing.el ends here
