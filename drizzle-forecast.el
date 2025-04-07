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
(require 'persist)
(require 'drizzle-forecast-icons)

(persist-defvar drizzle-forecast-selected-location nil "Selected location (lat, lon)")

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

(defun drizzle-forecast--search-locations (string)
  "Search for location names that contain STRING with the GeoNames API."
  (let* ((url-request-method "GET")
         (url (format "http://api.geonames.org/search?q=%s&type=json&style=full&isNameRequired=true&username=emacs_cloud_peeker" string))
         (url-request-extra-headers '(("Et-Client-Name" . "emacs-drizzle-forecast")))
         (buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n") ;; Skip HTTP headers
        (let* ((json-text (buffer-substring-no-properties (point) (point-max))))
          (decode-coding-string json-text 'utf-8)
          ;; (message (assoc 'geonames (json-parse-string json-text :object-type 'alist)))
          (json-parse-string json-text :object-type 'alist))))))

(defun drizzle-forecast--get-locations (string)
  "Get location with name that contain STRING."
  (let* ((res (drizzle-forecast--search-locations string))
         (geonames (cdr (assoc 'geonames res)))
         (locations (seq-map (lambda (item) (let* ((lat (cdr (assoc 'lat item)))
                                                   (lon (cdr (assoc 'lng item)))
                                                   (name (cdr (assoc 'name item)))
                                                   (adminName1 (cdr (assoc 'adminName1 item)))
                                                   (adminName2 (cdr (assoc 'adminName2  item)))
                                                   (countryName (cdr (assoc 'countryName  item))))
                                              (cons (format "%s, %s, %s, %s" name adminName2 adminName1 countryName)
                                                    `((name . ,name)
                                                      (city . ,adminName2)
                                                      (county . ,adminName1)
                                                      (country . ,countryName)
                                                      (lat . ,lat)
                                                      (lon . ,lon)))))
                             geonames)))
    locations))

(defun drizzle-forecast--fetch-location-forecast (latitude longitude)
  "Fetch weather forecast for a given LATITUDE and LONGITUDE from met.no API."
  (message (format "%s %s" latitude longitude))
  (let* ((rounded-latitude (round-coordinate latitude))
         (rounded-longitude (round-coordinate longitude))
         (url-request-extra-headers '(("User-Agent" . "emacs-drizzle-forecast")))
         (url (format "https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=%s&lon=%s"
                      rounded-latitude rounded-longitude))
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

(defun round-coordinate (str)
  "Round the decimal number in STR to 4 decimal places and return it as a string."
  (let* ((num (string-to-number str))
         (rounded (/ (float (round (* num 10000))) 10000.0)))
    (format "%.4f" rounded)))

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

(defun drizzle-forecast--select-location (choices)
  "Select location from CHOICES."
  (let* ((selection (completing-read "Select location: " choices nil t)))
    (cdr (assoc selection choices))))

(defun drizzle-forecast--prompt-location ()
  "Prompt the user for a location."
  (let* ((user-input (read-string "Location name: "))
         (choices (drizzle-forecast--get-locations user-input))
         (selected-location-coordinates (drizzle-forecast--select-location choices)))
    selected-location-coordinates))

(defun drizzle-forecast-show-forecast (arg)
  "Show forecast for a selected location."
  (interactive "p")
  (when (/= arg 1)
    (message "Select location"))

  (let* ((location (if (or (/= arg 1) (not drizzle-forecast-selected-location))
                       (drizzle-forecast--prompt-location)
                     drizzle-forecast-selected-location))
         (latitude (cdr (assoc 'lat location)))
         (longitude (cdr (assoc 'lon location)))
         (name (cdr (assoc 'name location)))
         (city (cdr (assoc 'city location)))
         (county (cdr (assoc 'county location)))
         (country (cdr (assoc 'country location)))
         (buffer-name "*Weather forecast*")
         (forecast (drizzle-forecast--fetch-location-forecast latitude longitude)))
    (setq drizzle-forecast-selected-location location)
    (persist-save 'drizzle-forecast-selected-location)
    (with-current-buffer-window buffer-name nil nil
      (drizzle-forecast-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let* ((timeseries (alist-get 'timeseries (alist-get 'properties forecast)))
               (date-name ""))
          (if (display-graphic-p)
              (insert (propertize (format "%s\n" name) 'face '(:height 1.8)))
            (insert (format "%s\n\n" name)))
          (dotimes (i (length timeseries))
            (let* ((reading (aref timeseries i))
                   (time (alist-get 'time reading))
                   (data (alist-get 'data reading))
                   (instant-details (alist-get 'details (alist-get 'instant data)))
                   (next-1-hour-summary (alist-get 'summary (alist-get 'next_1_hours data)))
                   (next-6-hour-summary (alist-get 'summary (alist-get 'next_6_hours data)))
                   (air-temperature (alist-get 'air_temperature instant-details))
                   (symbol (or (alist-get 'symbol_code next-1-hour-summary) (alist-get 'symbol_code next-6-hour-summary)))
                   (dn (drizzle-forecast--format-date time)))
              (when symbol (if (display-graphic-p)
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
                                 (when (not (string-empty-p lower))
                                   (insert (make-string pos ?\s))
                                   (insert lower)
                                   (insert "\n"))))))))
          (unless (display-graphic-p)
            (ansi-color-apply-on-region (point-min) (point-max)))
          (insert "\nData from MET Norway")
          (when (display-graphic-p)
            (insert "\nThe icons are copyright (c) 2015-2017 Yr and licensed under the MIT License"))
          (goto-char (point-min)))))
    (pop-to-buffer buffer-name)))

(provide 'drizzle-forecast)
;;; drizzle-forecast.el ends here
