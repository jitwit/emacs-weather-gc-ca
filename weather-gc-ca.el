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

;; TODO: add startup buffers showing today's jet_stream
;; "https://weather.gc.ca/data/jet_stream/tempmapwx_e.gif"
(defvar *weather-gc-ca-feed* nil
  "Raw dump of http response from weather.gc.ca")
(defvar *weather-gc-ca-current-conditions* nil
  "Data in node giving current conditions")
(defvar *weather-gc-ca-last-updated* nil
  "Time of last fetch")
(defvar *weather-gc-ca-timer* nil
  "Object responsible for updating feed every 1800 seconds")

(defun weather-gc-ca-update ()
  "read the rss feed for montreal from weather-gc-ca"
  (with-current-buffer (url-retrieve-synchronously *weather-gc-ca-uri*)
    (let* ((parsed-xml (car (xml-parse-region (point-min) (point-max))))
	   (entries (seq--into-list
		     (mapcar (lambda (e)
			       (let ((cs (xml-get-children e 'title)))
				 (if (and (listp cs)
					  (> (length cs) 0))
				     (cddar cs)
				   '())))
			     (xml-get-children *weather-gc-ca-feed* 'entry))))
	   (current-conditions
	    (cadr
	     (split-string (car (seq-find (lambda (s) (string-prefix-p "Current" (car s)))
					  entries))
			   ": "))))
      (setq *weather-gc-ca-feed* parsed-xml
	    *weather-gc-ca-current-conditions* current-conditions
	    *weather-gc-ca-last-updated* (current-time)))))

(defun weather-gc-ca-put-mode-line-misc-info ()
  "Write contents of current conditions variable to mode line buffer.
   This will overwrite any info in the misc info variable."
  (setq mode-line-misc-info
	`(,(format "{%s}" *weather-gc-ca-current-conditions*))))

(defun weather-gc-ca-cancel ()
  "Cancel the weather-gc-ca timer and clearn misc-info in mode line buffer."
  (when *weather-gc-ca-timer*
    (setq mode-line-misc-info '())
    (cancel-timer *weather-gc-ca-timer*)))

(defun weather-gc-ca-init ()
  "Start weather-gc-ca timer now and update data every 30 minutes"
  (weather-gc-ca-cancel)
  (setq *weather-gc-ca-timer*
	(run-at-time (current-time)
		     1800
		     (lambda ()
		       (weather-gc-ca-update)
		       (weather-gc-ca-put-mode-line-misc-info)))))

(defun weather-gc-ca-new-uri (uri)
  (setq *weather-gc-ca-uri* uri)
  (weather-gc-ca-init))

(provide 'weather-gc-ca)
;;(weather-gc-ca-update)
;;( weather-gc-ca-put-mode-line-misc-info)


