;;; weather-gc-ca.el --- Weather data from weather.gc.ca in Emacs -*- lexical-binding: t -*-

(require 'url)
(require 'xml)

(defgroup weather-gc-ca nil
  "Get weather from rss feeds provided at https://weather.gc.ca"
  :prefix "weather-gc-ca-"
  :group 'Applications)

(defcustom *weather-gc-ca-uri*
  "https://weather.gc.ca/rss/city/qc-147_e.xml"
  "weather.gc.ca rss uri for weather forecasts. default is for montreal.")
(defvar *weather-gc-ca-feed* nil)
(defvar *weather-gc-ca-current-conditions* nil)
(defvar *weather-gc-ca-last-updated* nil)

(defun weather-gc-ca-update ()
  "read the rss feed for montreal from weather-gc-ca"
  (with-current-buffer (url-retrieve-synchronously *weather-gc-ca-uri*)
    (let* ((parsed-xml (car (xml-parse-region (point-min) (point-max))))
	   (entries (seq--into-list
		     (mapcar (lambda (e)
			       (let ((cs (xml-get-children e 'title)))
				 (if (and (listp cs) (> (length cs) 0))
				     (car cs)
				   '())))
			     (xml-get-children parsed-xml 'entry))))
	   (current-conditions (cadr (split-string (caddr (cadr entries)) ": "))))
      (setq *weather-gc-ca-feed* parsed-xml
	    *weather-gc-ca-current-conditions* current-conditions
	    *weather-gc-ca-last-updated* (current-time)))))

(defun weather-gc-ca-put-mode-line-misc-info ()
  "Write contents of current conditions variable to mode line buffer.
   This will overwrite any info in the misc info variable."
  (setq mode-line-misc-info
	`(,(format "{%s}" *weather-gc-ca-current-conditions*))))

(defvar *weather-gc-ca-timer*
  (run-at-time (current-time) 900
	       (lambda ()
		 (weather-gc-ca-update)
		 (weather-gc-ca-put-mode-line-misc-info))))

(defun weather-gc-ca-cancel ()
  (setq mode-line-misc-info '())
  (cancel-timer *weather-gc-ca-timer*))

(provide 'weather-gc-ca)


