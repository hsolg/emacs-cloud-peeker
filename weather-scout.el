;;; weather-scout.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Henrik Solgaard

;; Author: Henrik Solgaard <henrik.solgaard@gmail.com>
;; Maintainer: Henrik Solgaard <henrik.solgaard@gmail.com>
;; Created: March 28, 2025
;; Version: 0.0.1
;; Homepage: https://github.com/hsolg/emacs-weather-scout
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
;; Display weather forecast from MET Norway
;;
;; The command weather-scout-show-forecast displays weather forecast for a
;; selected location from MET Norway. The first time the command is invoked, it
;; will display a search prompt in the minibuffer. The next time it is invoked,
;; it will remember the previous selection. To select a new location, invoke the
;; command with a prefix argument (typically C-u) to display the search prompt
;; again.
;;
;; Location searches use the free GeoNames service. By default, weather-scout
;; uses its own GeoNames account. Because of rate limits, creating you own
;; account is recommended. Follow the steps below to create and configure your
;; own account.
;;
;; - Create a free account here: https://www.geonames.org/.
;; - Enable the new account for the free webservices on the account page:
;;   https://www.geonames.org/manageaccount.
;; - Set this variable in your Emacs configuration:
;;   (setq weather-scout-geonames-account-name "your-account-name-here")

;;; Code:

(require 'json)
(require 'url)
(require 'iso8601)
(require 'ansi-color)
(require 'persist)
(require 'weather-scout-icons)

(persist-defvar weather-scout-selected-location nil "Selected location name and coordinates")

(defgroup weather-scout nil
  "Customization group for `weather-scout.el`."
  :prefix "weather-scout-"
  :group 'applications)

(defcustom weather-scout-geonames-account-name nil
  "Set your own GeoNames account name (recommended because of rate limit)."
  :type 'string
  :group 'weather-scout)

(defun weather-scout--translate-symbol (symbol)
  "Translate SYMBOL to escape sequence."
  (cond ((symbolp symbol) (format "\x1b[%dm" (cdr (assoc symbol weather-scout--color-codes))))
        (t symbol)))

(defun weather-scout--map-symbols (symbols)
  "Map SYMBOLS to output strings."
  (mapcar #'weather-scout--translate-symbol symbols))

(defvar weather-scout--base-dir
  (file-name-directory (file-truename load-file-name))
  "Base directory of the weather-scout package.")

(define-derived-mode weather-scout-mode special-mode "Weather Scout"
  "Major mode for displaying weather forecasts.")

(defun weather-scout--search-locations (string)
  "Search for location names that contain STRING with the GeoNames API."
  (let* ((url-request-method "GET")
         (geonames-account-name (or weather-scout-geonames-account-name "emacs_weather_scout"))
         (url (format "http://api.geonames.org/search?q=%s&type=json&style=full&isNameRequired=true&username=%s" string geonames-account-name))
         (url-request-extra-headers '(("Et-Client-Name" . "emacs-weather-scout")))
         (buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n") ;; Skip HTTP headers
        (let* ((json-text (buffer-substring-no-properties (point) (point-max))))
          (decode-coding-string json-text 'utf-8)
          (json-parse-string json-text :object-type 'alist))))))

(defun weather-scout--remove-duplicates (lst)
  "Remove consecutive duplicates from LST."
  (cl-reduce (lambda (acc x)
               (if (equal (car acc) x)
                   acc
                 (cons x acc)))
             (reverse lst)
             :initial-value '()))

(defun weather-scout--format-location (&rest parts)
  "Format location consisting of multiple, possibly duplicated PARTS."
  (string-join (weather-scout--remove-duplicates (remove "" parts)) ", "))

(defun weather-scout--get-locations (string)
  "Get location with name that contain STRING."
  (let* ((res (weather-scout--search-locations string))
         (geonames (alist-get 'geonames res))
         (locations (seq-map (lambda (item) (let* ((lat (alist-get 'lat item))
                                                   (lon (alist-get 'lng item))
                                                   (name (alist-get 'name item))
                                                   (adminName1 (alist-get 'adminName1 item))
                                                   (adminName2 (alist-get 'adminName2  item))
                                                   (countryName (alist-get 'countryName  item)))
                                              (cons (weather-scout--format-location name adminName2 adminName1 countryName)
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

(defun weather-scout--fetch-location-forecast (latitude longitude)
  "Fetch weather forecast for a given LATITUDE and LONGITUDE from met.no API."
  (let* ((rounded-latitude (weather-scout--round-coordinate latitude))
         (rounded-longitude (weather-scout--round-coordinate longitude))
         (url-request-extra-headers '(("User-Agent" . "emacs-weather-scout")))
         (url (format "https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=%s&lon=%s"
                      rounded-latitude rounded-longitude))
         (buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n") ;; Skip HTTP headers
        (let ((json-object-type 'alist))
          (json-read))))))

(defun weather-scout--image-for-symbol (symbol)
  "Get image for SYMBOL."
  (let* ((filename (format "%s/icons/weather/%s.svg" weather-scout--base-dir symbol)))
    (create-image filename 'svg nil :height 35)))

(defun weather-scout--flatten (lst)
  "Flatten a nested list LST."
  (cond
   ((null lst) nil)
   ((listp (car lst))
    (append (weather-scout--flatten (car lst)) (weather-scout--flatten (cdr lst))))
   (t (cons (car lst) (weather-scout--flatten (cdr lst))))))

(defun weather-scout--round-coordinate (str)
  "Round the decimal number in STR to 4 decimal places and return it as a string."
  (let* ((num (string-to-number str))
         (rounded (/ (float (round (* num 10000))) 10000.0)))
    (format "%.4f" rounded)))

(defun weather-scout--glyphs-for-symbol (symbol)
  "Get glyphs for SYMBOL."
  (let* ((codes (cdr (assoc symbol weather-scout--weather-symbols-list))))
    (if codes
        (let ((sky (weather-scout--flatten (nth 0 codes)))
              (air (weather-scout--flatten (nth 1 codes))))
          `(,(apply #'concat (weather-scout--map-symbols sky))
            ,(apply #'concat (weather-scout--map-symbols air))))
      '(symbol ""))))

(defun weather-scout--wind-arrow (direction)
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

(defun weather-scout--format-time (utc-string)
  "Format UTC-STRING as local time."
  (let* ((utc-time (iso8601-parse utc-string)))
    (format-time-string "%H:%M:%S %Z" (apply 'encode-time utc-time))))

(defun weather-scout--format-date (utc-string)
  "Format UTC-STRING as local date."
  (let* ((locale (or (getenv "LC_TIME") (getenv "LANG") "en_US"))
         (date-format (cond
                       ((string-match-p "\\`\\(en_US\\|en_PH\\|en_BZ\\)" locale) "%A, %B %e")
                       ((string-match-p "\\`\\(zh\\|ja\\|ko\\)" locale) "%m月%e日")
                       (t "%A, %e %B")))
         (utc-time (iso8601-parse utc-string)))
    (format-time-string date-format (apply 'encode-time utc-time))))

(defun weather-scout--display-all-icons ()
  "Display all console weather icons."
  (dolist (pair weather-scout--weather-symbols-list)
    (let* ((name (car pair))
           (glyphs (weather-scout--glyphs-for-symbol name)))
      (insert (format "%s\n" name))
      (insert (format "%s\n" (nth 0 glyphs)))
      (insert (format "%s\n\n" (nth 1 glyphs)))))
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun weather-scout--select-location (choices)
  "Select location from CHOICES."
  (let* ((selection (completing-read "Select location: " choices nil t)))
    (cdr (assoc selection choices))))

(defun weather-scout--prompt-location ()
  "Prompt the user for a location."
  (let* ((user-input (read-string "Location name: "))
         (choices (weather-scout--get-locations user-input))
         (selected-location-coordinates (weather-scout--select-location choices)))
    (setq weather-scout-selected-location selected-location-coordinates)
    (persist-save 'weather-scout-selected-location)
    selected-location-coordinates))

(defun weather-scout-show-forecast (arg)
  "Show forecast for a selected location.

With a prefix ARG, select a new location."
  (interactive "p")

  (let* ((location (if (or (/= arg 1) (not weather-scout-selected-location))
                       (weather-scout--prompt-location)
                     weather-scout-selected-location))
         (latitude (alist-get 'lat location))
         (longitude (alist-get 'lon location))
         (name (alist-get 'name location))
         ;; (city (alist-get 'city location))
         ;; (county (alist-get 'county location))
         ;; (country (alist-get 'country location))
         (buffer-name "*Weather forecast*")
         (forecast (weather-scout--fetch-location-forecast latitude longitude)))
    (with-current-buffer-window buffer-name nil nil
      (weather-scout-mode)
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
                   (dn (weather-scout--format-date time)))
              (when symbol (if (display-graphic-p)
                               (progn
                                 (when (not (string= dn date-name))
                                   (when (not (string= date-name ""))
                                     (insert "\n"))
                                   (insert (propertize (format "%s\n" dn) 'face '(:height 1.5)))
                                   (setq date-name dn))
                                 (insert (propertize (format "%s    " (weather-scout--format-time (format "%s" time))) 'display '(raise -0.3)))
                                 (when symbol
                                   (insert-image (weather-scout--image-for-symbol (format "%s" symbol))))
                                 (insert (propertize (format "    %3d °C" (round air-temperature)) 'display '(raise -0.3)))
                                 (let ((precipitation (if (> precipitation-amount 0)
                                                          (propertize (format "%4.1f mm" precipitation-amount) 'display '(raise -0.3))
                                                        "       ")))
                                   (insert (propertize (format "    %s" precipitation) 'display '(raise -0.3))))
                                 (insert (propertize (format "    %2d m/s %s\n" (round wind-speed) (weather-scout--wind-arrow wind-from-direction)) 'display '(raise -0.3))))
                             (progn
                               (when (not (string= dn date-name))
                                 (when (not (string= date-name ""))
                                   (insert "\n"))
                                 (insert (format "%s\n" dn))
                                 (setq date-name dn))
                               (insert (format "%s    " (weather-scout--format-time (format "%s" time))))
                               (let* ((glyphs (when symbol (weather-scout--glyphs-for-symbol (format "%s" symbol))))
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
                                 (insert (format "    %2d m/s %s\n" (round wind-speed) (weather-scout--wind-arrow wind-from-direction)))
                                 (when (not (string-empty-p lower))
                                   (insert (make-string pos ?\s))
                                   (insert lower)
                                   (insert "\n"))))))))
          (unless (display-graphic-p)
            (ansi-color-apply-on-region (point-min) (point-max)))
          (insert "\nData from MET Norway.")
          (when (display-graphic-p)
            (insert "\nThe icons are copyright (c) 2015-2017 Yr and licensed under the MIT License."))
          (insert "\nSearch result from GeoNames.")
          (if (and (stringp weather-scout-geonames-account-name) (not (string-empty-p weather-scout-geonames-account-name)))
              (insert "\n\nThank you for using your own GeoNames account.")
            (insert "\n\nCreating you own GeoNames account and setting weather-scout-geonames-account-name\nis recommended because of rate limits."))
          (goto-char (point-min)))))
    (pop-to-buffer buffer-name)
    (message "To select a different location, invoke with a prefix argument")
    (run-at-time "3 sec" nil #'message nil)))

(provide 'weather-scout)
;;; weather-scout.el ends here
