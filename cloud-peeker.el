;;; cloud-peeker.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Henrik Solgaard

;; Author: Henrik Solgaard <henrik.solgaard@gmail.com>
;; Maintainer: Henrik Solgaard <henrik.solgaard@gmail.com>
;; Created: March 28, 2025
;; Version: 0.0.1
;; Homepage: https://github.com/hsolg/emacs-cloud-peeker
;; Package-Requires: ((emacs "27.1") (persist "0.6.1"))
;; License: GPL-3+

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Display weather forecast from met.no.
;;
;;; Code:

(require 'json)
(require 'url)
(require 'iso8601)
(require 'persist)
(require 'cloud-peeker-icons)

(persist-defvar cloud-peeker-selected-location nil "Selected location name and coordinates")

(defgroup cloud-peeker nil
  "Customization group for `cloud-peeker.el`."
  :prefix "cloud-peeker-"
  :group 'applications)

(defcustom cloud-peeker-geonames-account-name nil
  "Set your own GeoNames account name (recommended because of rate limit)."
  :type 'string
  :group 'cloud-peeker)

(defun cloud-peeker--translate-symbol (symbol)
  "Translate SYMBOL to escape sequence."
  (cond ((symbolp symbol) (format "\x1b[%dm" (cdr (assoc symbol cloud-peeker--color-codes))))
        (t symbol)))

(defun cloud-peeker--map-symbols (symbols)
  "Map SYMBOLS to output strings."
  (mapcar #'cloud-peeker--translate-symbol symbols))

(defvar cloud-peeker--base-dir
  (file-name-directory (file-truename load-file-name))
  "Base directory of the cloud-peeker package.")

(define-derived-mode cloud-peeker-mode special-mode "Cloud Peeker"
  "Major mode for displaying weather forecasts.")

(defun cloud-peeker--search-locations (string)
  "Search for location names that contain STRING with the GeoNames API."
  (let* ((url-request-method "GET")
         (geonames-account-name (or cloud-peeker-geonames-account-name "emacs_cloud_peeker"))
         (url (format "http://api.geonames.org/search?q=%s&type=json&style=full&isNameRequired=true&username=%s" string geonames-account-name))
         (url-request-extra-headers '(("Et-Client-Name" . "emacs-cloud-peeker")))
         (buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n") ;; Skip HTTP headers
        (let* ((json-text (buffer-substring-no-properties (point) (point-max))))
          (decode-coding-string json-text 'utf-8)
          (json-parse-string json-text :object-type 'alist))))))

(defun cloud-peeker--remove-duplicates (lst)
  "Remove consecutive duplicates from LST."
  (cl-reduce (lambda (acc x)
               (if (equal (car acc) x)
                   acc
                 (cons x acc)))
             (reverse lst)
             :initial-value '()))

(defun cloud-peeker--format-location (&rest parts)
  "Format location consisting of multiple, possibly duplicated PARTS."
  (string-join (cloud-peeker--remove-duplicates (remove "" parts)) ", "))

(defun cloud-peeker--get-locations (string)
  "Get location with name that contain STRING."
  (let* ((res (cloud-peeker--search-locations string))
         (geonames (alist-get 'geonames res))
         (locations (seq-map (lambda (item) (let* ((lat (alist-get 'lat item))
                                                   (lon (alist-get 'lng item))
                                                   (name (alist-get 'name item))
                                                   (adminName1 (alist-get 'adminName1 item))
                                                   (adminName2 (alist-get 'adminName2  item))
                                                   (countryName (alist-get 'countryName  item)))
                                              (cons (cloud-peeker--format-location name adminName2 adminName1 countryName)
                                                    `((name . ,name)
                                                      (city . ,adminName2)
                                                      (county . ,adminName1)
                                                      (country . ,countryName)
                                                      (lat . ,lat)
                                                      (lon . ,lon)))))
                             geonames))
         (unique-locations (cl-remove-duplicates
                            locations
                            :key #'car :test #'equal)))
    unique-locations))

(defun cloud-peeker--fetch-location-forecast (latitude longitude)
  "Fetch weather forecast for a given LATITUDE and LONGITUDE from met.no API."
  (message (format "%s %s" latitude longitude))
  (let* ((rounded-latitude (round-coordinate latitude))
         (rounded-longitude (round-coordinate longitude))
         (url-request-extra-headers '(("User-Agent" . "emacs-cloud-peeker")))
         (url (format "https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=%s&lon=%s"
                      rounded-latitude rounded-longitude))
         (buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n") ;; Skip HTTP headers
        (let ((json-object-type 'alist))
          (json-read))))))

(defun cloud-peeker--image-for-symbol (symbol)
  "Get image for SYMBOL."
  (let* ((filename (format "%s/icons/weather/%s.svg" cloud-peeker--base-dir symbol)))
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

(defun cloud-peeker--glyphs-for-symbol (symbol)
  "Get glyphs for SYMBOL."
  (let* ((codes (cdr (assoc symbol cloud-peeker--weather-symbols-list))))
    (if codes
        (let ((sky (flatten (nth 0 codes)))
              (air (flatten (nth 1 codes))))
          `(,(apply #'concat (cloud-peeker--map-symbols sky))
            ,(apply #'concat (cloud-peeker--map-symbols air))))
      '(symbol ""))))

(defun cloud-peeker--wind-arrow (direction)
  "Get wind arrow for wind DIRECTION."
  (let ((offset (/ 45 2)))
    (cond
     ((< direction (- 45 offset)) "↓")
     ((< direction (- 90 offset)) "↙")
     ((< direction (- 135 offset)) "←")
     ((< direction (- 180 offset)) "↖")
     ((< direction (- 225 offset)) "↑")
     ((< direction (- 270 offset)) "↗")
     ((< direction (- 315 offset)) "→")
     ((< direction (- 360 offset)) "↘")
     (t "↓"))))

(defun cloud-peeker--format-time (utc-string)
  "Format UTC-STRING as local time."
  (let* ((utc-time (iso8601-parse utc-string)))
    (format-time-string "%H:%M:%S %Z" (apply 'encode-time utc-time))))

(defun cloud-peeker--format-date (utc-string)
  "Format UTC-STRING as local date."
  (let* ((locale (or (getenv "LC_TIME") (getenv "LANG") "en_US"))
         (date-format (cond
                       ((string-match-p "\\`\\(en_US\\|en_PH\\|en_BZ\\)" locale) "%A, %B %e")
                       ((string-match-p "\\`\\(zh\\|ja\\|ko\\)" locale) "%m月%e日")
                       (t "%A, %e %B")))
         (utc-time (iso8601-parse utc-string)))
    (format-time-string date-format (apply 'encode-time utc-time))))

(defun cloud-peeker--display-all-icons ()
  "Display all console weather icons."
  (dolist (pair cloud-peeker--weather-symbols-list)
    (let* ((name (car pair))
           (glyphs (cloud-peeker--glyphs-for-symbol name)))
      (insert (format "%s\n" name))
      (insert (format "%s\n" (nth 0 glyphs)))
      (insert (format "%s\n\n" (nth 1 glyphs)))))
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun cloud-peeker--select-location (choices)
  "Select location from CHOICES."
  (let* ((selection (completing-read "Select location: " choices nil t)))
    (cdr (assoc selection choices))))

(defun cloud-peeker--prompt-location ()
  "Prompt the user for a location."
  (let* ((user-input (read-string "Location name: "))
         (choices (cloud-peeker--get-locations user-input))
         (selected-location-coordinates (cloud-peeker--select-location choices)))
    selected-location-coordinates))

(defun cloud-peeker-show-forecast (arg)
  "Show forecast for a selected location.

With a prefix ARG, select a new location."
  (interactive "p")

  (let* ((location (if (or (/= arg 1) (not cloud-peeker-selected-location))
                       (cloud-peeker--prompt-location)
                     cloud-peeker-selected-location))
         (latitude (alist-get 'lat location))
         (longitude (alist-get 'lon location))
         (name (alist-get 'name location))
         (city (alist-get 'city location))
         (county (alist-get 'county location))
         (country (alist-get 'country location))
         (buffer-name "*Weather forecast*")
         (forecast (cloud-peeker--fetch-location-forecast latitude longitude)))
    (setq cloud-peeker-selected-location location)
    (persist-save 'cloud-peeker-selected-location)
    (with-current-buffer-window buffer-name nil nil
      (cloud-peeker-mode)
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
                   (next-1-hour (alist-get 'next_1_hours data))
                   (next-6-hour (alist-get 'next_6_hours data))
                   (next-period (or next-1-hour next-6-hour))
                   (period-summary (alist-get 'summary next-period))
                   (period-details (alist-get 'details next-period))
                   (air-temperature (alist-get 'air_temperature instant-details))
                   (wind-from-direction (alist-get 'wind_from_direction instant-details))
                   (wind-speed (alist-get 'wind_speed instant-details))
                   (symbol (alist-get 'symbol_code period-summary))
                   (precipitation-amount (alist-get 'precipitation_amount period-details))
                   (dn (cloud-peeker--format-date time)))
              (when symbol (if (display-graphic-p)
                               (progn
                                 (when (not (string= dn date-name))
                                   (when (not (string= date-name ""))
                                     (insert "\n"))
                                   (insert (propertize (format "%s\n" dn) 'face '(:height 1.5)))
                                   (setq date-name dn))
                                 (insert (propertize (format "%s    " (cloud-peeker--format-time (format "%s" time))) 'display '(raise -0.3)))
                                 (when symbol
                                   (insert-image (cloud-peeker--image-for-symbol (format "%s" symbol))))
                                 (insert (propertize (format "    %3d °C" (round air-temperature)) 'display '(raise -0.3)))
                                 (let ((precipitation (if (> precipitation-amount 0)
                                                          (propertize (format "%4.1f mm" precipitation-amount) 'display '(raise -0.3))
                                                        "       ")))
                                   (insert (propertize (format "    %s" precipitation) 'display '(raise -0.3))))
                                 (insert (propertize (format "    %2d m/s %s\n" (round wind-speed) (cloud-peeker--wind-arrow wind-from-direction)) 'display '(raise -0.3))))
                             (progn
                               (when (not (string= dn date-name))
                                 (when (not (string= date-name ""))
                                   (insert "\n"))
                                 (insert (format "%s\n" dn))
                                 (setq date-name dn))
                               (insert (format "%s    " (cloud-peeker--format-time (format "%s" time))))
                               (let* ((glyphs (when symbol (cloud-peeker--glyphs-for-symbol (format "%s" symbol))))
                                      (upper (when glyphs (nth 0 glyphs)))
                                      (lower (when glyphs (nth 1 glyphs)))
                                      (pos (current-column)))
                                 (when upper
                                   (insert upper))
                                 (insert (format "    %3d °C" (round air-temperature)))
                                 (let ((precipitation (if (> precipitation-amount 0)
                                                          (format "%4.1f mm" precipitation-amount)
                                                        "       ")))
                                   (insert (format "    %s" precipitation)))
                                 (insert (format "    %2d m/s %s\n" (round wind-speed) (cloud-peeker--wind-arrow wind-from-direction)))
                                 (when (not (string-empty-p lower))
                                   (insert (make-string pos ?\s))
                                   (insert lower)
                                   (insert "\n"))))))))
          (unless (display-graphic-p)
            (ansi-color-apply-on-region (point-min) (point-max)))
          (insert "\nData from MET Norway")
          (when (display-graphic-p)
            (insert "\nThe icons are copyright (c) 2015-2017 Yr and licensed under the MIT License"))
          (insert "\nSearch result from GeoNames")
          (unless cloud-peeker-geonames-account-name
            (insert "\n\nCreating you own GeoNames account and setting cloud-peeker-geonames-account-name\nis recommended because of rate limits."))
          (goto-char (point-min)))))
    (pop-to-buffer buffer-name)))

(provide 'cloud-peeker)
;;; cloud-peeker.el ends here
