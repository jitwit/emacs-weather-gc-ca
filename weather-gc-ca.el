;;; weather-gc-ca.el --- Weather data from weather.gc.ca in Emacs -*- lexical-binding: t -*-

(require 'url)
(require 'xml)

(defgroup weather-gc-ca nil
  "Get weather from rss feeds provided at https://weather.gc.ca"
  :prefix "weather-gc-ca-"
  :group 'apps
  )

(defconst *weather-gc-ca-uri*
  "https://weather.gc.ca/rss/city/qc-147_e.xml")
(defvar *weather-gc-ca-feed* nil)
(defvar *weather-gc-ca-current-conditions* nil)
(defvar *weather-gc-ca-last-updated*)

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
	    *weather-gc-ca-current-conditions* current-conditions))))

(define-minor-mode weather-gc-ca-mode-line
  "Display current conditions in mode-line"
  :global t
  :group 'weather-gc-ca
  (unless *weather-gc-ca-current-conditions*
    (weather-gc-ca-update))
  (add-to-list 'mode-line-misc-info
	       (format "{%s}" *weather-gc-ca-current-conditions*)
	       t))

(provide 'weather-gc-ca)
(provide 'weather-gc-ca-mode-line)



