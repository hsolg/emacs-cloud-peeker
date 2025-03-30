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
(require 'evil)
(require 'iso8601)

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

(defun drizzle-forecast--format-time (utc-string)
  "Format UTC-STRING as local time."
  (let* ((utc-time (iso8601-parse utc-string)))
    (format-time-string "%Y-%m-%d %H:%M:%S %Z" (apply 'encode-time utc-time))))

(defun drizzle-forecast-show-forecast ()
  "Show forecast for a preset location."
  (interactive)
  (let* ((latitude "59.91272")
         (longitude "10.74609")
         (buffer-name "*Weather forecast*")
         (forecast (drizzle-forecast--fetch-location-forecast latitude longitude)))
    (with-current-buffer-window buffer-name nil nil
      (drizzle-forecast-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let* ((timeseries (alist-get 'timeseries (alist-get 'properties forecast))))
          (dotimes (i (length timeseries))
            (let* ((reading (aref timeseries i))
                   (time (alist-get 'time reading))
                   (data (alist-get 'data reading))
                   (instant-details (alist-get 'details (alist-get 'instant data)))
                   (next-1-hour-summary (alist-get 'summary (alist-get 'next_1_hours data)))
                   (air-temperature (alist-get 'air_temperature instant-details))
                   (symbol (alist-get 'symbol_code next-1-hour-summary)))
              (insert (propertize (format "%s\t" (drizzle-forecast--format-time (format "%s" time))) 'display '(raise -0.3)))
              (when symbol
                (if (display-graphic-p)
                    (insert-image (drizzle-forecast--image-for-symbol (format "%s" symbol)))
                  (insert (format "%s\t%s" symbol drizzle-forecast--base-dir))))
              (insert (propertize (format "\t%.1f °C\n" air-temperature) 'display '(raise -0.3)))))
          (goto-char (point-min)))))
    (pop-to-buffer buffer-name)))

(provide 'drizzle-forecast)
;;; drizzle-forecast.el ends here
