;;; drizzle-forecast.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Henrik Solgaard
;;
;; Author: Henrik Solgaard <henrik.solgaard@gmail.com>
;; Maintainer: Henrik Solgaard <henrik.solgaard@gmail.com>
;; Created: March 28, 2025
;; Version: 0.0.1
;; Keywords: weather
;; Homepage: https://github.com/hsolg/emacs-drizzle-forecast
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Display weather forecast from met.no.
;;
;;; Code:

(require 'json)
(require 'url)
(require 'iso8601)
(require 'drizzle-forecast-icons)

(defun drizzle-forecast--translate-symbol (symbol)
  "Translate SYMBOL to escape sequence."
  (cond ((symbolp symbol) (format "\x1b[%dm" (cdr (assoc symbol drizzle-forecast--color-codes))))
        (t symbol)))

(defun drizzle-forecast--map-symbols (symbols)
  "Map SYMBOLS to output strings."
  (mapcar #'drizzle-forecast--translate-symbol symbols))

(defvar drizzle-forecast--base-dir
  (file-name-directory (file-truename load-file-name))
  "Base directory of the drizzle-forecast package.")

(define-derived-mode drizzle-forecast-mode special-mode "Drizzle forecast"
  "Major mode for displaying weather forecasts.")

(defun drizzle-forecast--fetch-location-forecast (latitude longitude)
  "Fetch weather forecast for a given LATITUDE and LONGITUDE from met.no API."
  (let* ((url-request-extra-headers '(("User-Agent" . "emacs-drizzle-forecast")))
         (url (format "https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=%s&lon=%s"
                      latitude longitude))
         (buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n") ;; Skip HTTP headers
        (let ((json-object-type 'alist))
          (json-read))))))

(defun drizzle-forecast--image-for-symbol (symbol)
  "Get image for SYMBOL."
  (let* ((filename (format "%s/icons/weather/%s.svg" drizzle-forecast--base-dir symbol)))
    (create-image filename 'svg nil :height 35)))

(defun flatten (lst)
  "Flatten a nested list LST."
  (cond
   ((null lst) nil)
   ((listp (car lst))
    (append (flatten (car lst)) (flatten (cdr lst))))
   (t (cons (car lst) (flatten (cdr lst))))))

(defun drizzle-forecast--glyphs-for-symbol (symbol)
  "Get glyphs for SYMBOL."
  (let* ((codes (cdr (assoc symbol drizzle-forecast--weather-symbols-list))))
    (if codes
        (let ((sky (flatten (nth 0 codes)))
              (air (flatten (nth 1 codes))))
          `(,(apply #'concat (drizzle-forecast--map-symbols sky))
            ,(apply #'concat (drizzle-forecast--map-symbols air))))
      '(symbol ""))))

(defun drizzle-forecast--format-time (utc-string)
  "Format UTC-STRING as local time."
  (let* ((utc-time (iso8601-parse utc-string)))
    (format-time-string "%H:%M:%S %Z" (apply 'encode-time utc-time))))

(defun drizzle-forecast--format-date (utc-string)
  "Format UTC-STRING as local date."
  (let* ((utc-time (iso8601-parse utc-string)))
    (format-time-string "%A, %d %B" (apply 'encode-time utc-time))))

(defun drizzle-forecast--display-all-icons ()
  "Display all console weather icons."
  (dolist (pair drizzle-forecast--weather-symbols-list)
    (let* ((name (car pair))
           (glyphs (drizzle-forecast--glyphs-for-symbol name)))
      (insert (format "%s\n" name))
      (insert (format "%s\n" (nth 0 glyphs)))
      (insert (format "%s\n\n" (nth 1 glyphs)))))
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun drizzle-forecast-show-forecast (arg)
  "Show forecast for a preset location."
  (interactive "p")
  (when (/= arg 1)
    (message "Select location"))

  (let* ((latitude "59.9127")
         (longitude "10.7460")
         (buffer-name "*Weather forecast*")
         (forecast (drizzle-forecast--fetch-location-forecast latitude longitude)))
    (with-current-buffer-window buffer-name nil nil
      (drizzle-forecast-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let* ((timeseries (alist-get 'timeseries (alist-get 'properties forecast)))
               (date-name ""))
          (dotimes (i (length timeseries))
            (let* ((reading (aref timeseries i))
                   (time (alist-get 'time reading))
                   (data (alist-get 'data reading))
                   (instant-details (alist-get 'details (alist-get 'instant data)))
                   (next-1-hour-summary (alist-get 'summary (alist-get 'next_1_hours data)))
                   (air-temperature (alist-get 'air_temperature instant-details))
                   (symbol (alist-get 'symbol_code next-1-hour-summary))
                   (dn (drizzle-forecast--format-date time)))
              (if (display-graphic-p)
                  (progn
                    (when (not (string= dn date-name))
                      (when (not (string= date-name ""))
                        (insert "\n"))
                      (insert (propertize (format "%s\n" dn) 'face '(:height 1.5)))
                      (setq date-name dn))
                    (insert (propertize (format "%s    " (drizzle-forecast--format-time (format "%s" time))) 'display '(raise -0.3)))
                    (when symbol
                      (insert-image (drizzle-forecast--image-for-symbol (format "%s" symbol))))
                    (insert (propertize (format "    %3d °C\n" (round air-temperature)) 'display '(raise -0.3))))
                (progn
                  (when (not (string= dn date-name))
                    (when (not (string= date-name ""))
                      (insert "\n"))
                    (insert (format "%s\n" dn))
                    (setq date-name dn))
                  (insert (format "%s    " (drizzle-forecast--format-time (format "%s" time))))
                  (let* ((glyphs (when symbol (drizzle-forecast--glyphs-for-symbol (format "%s" symbol))))
                         (upper (when glyphs (nth 0 glyphs)))
                         (lower (when glyphs (nth 1 glyphs)))
                         (pos (current-column)))
                    (when upper
                      (insert upper))
                    (insert (format "    %3d °C\n" (round air-temperature)))
                    (when lower
                      (insert (make-string pos ?\s))
                      (insert lower)
                      (insert "\n")))))))
          (unless (display-graphic-p)
            (ansi-color-apply-on-region (point-min) (point-max)))
          (goto-char (point-min)))))
    (pop-to-buffer buffer-name)))

(provide 'drizzle-forecast)
;;; drizzle-forecast.el ends here
