;;; weather-scout-icons.el --- Console based weather icons -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Henrik Solgaard

;; Author: Henrik Solgaard <henrik.solgaard@gmail.com>
;; Maintainer: Henrik Solgaard <henrik.solgaard@gmail.com>
;; Homepage: https://github.com/hsolg/emacs-weather-scout
;; Package-Requires: ((emacs "27.1"))
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
;; Console based weather icons

;;; Code:

(defconst weather-scout--color-codes
  '((black . 30)
    (red . 31)
    (green . 32)
    (yellow . 33)
    (blue . 34)
    (magenta . 35)
    (cyan . 36)
    (white . 37)
    (reset . 0)))

(defun weather-scout--sun ()
  "Sun."
  '(yellow "☀" reset))

(defun weather-scout--moon ()
  "Moon."
  '(yellow "☽" reset))

(defun weather-scout--polartwilight ()
  "Sun below horizon."
  '(yellow "◒" reset))

(defun weather-scout--clear (body)
  "BODY with no clouds."
  `(" " ,body " "))

(defun weather-scout--cloudy (&optional body)
  "Cloud around optional BODY."
  `("c" ,(or body "O") "ɔ"))

(defun weather-scout--fair (body)
  "Small clouds around BODY."
  `(" " ,body "ɔ"))

(defun weather-scout--dry ()
  "No precipitation."
  '())

(defun weather-scout--fog ()
  "Fog."
  '("==="))

(defun weather-scout--lightning ()
  "Lightning."
  '(yellow "ϟ" reset))

(defun weather-scout--raindrop ()
  "Raindrop."
  '("/"))

(defun weather-scout--raindrop-small-left ()
  "Small raindrop, left."
  '("'"))

(defun weather-scout--raindrop-small-right ()
  "Small raindrop, right."
  '(","))

(defun weather-scout--snowflake ()
  "Snowflake."
  '("❄"))

(defun weather-scout--snowflake-small ()
  "Small snowflake."
  '("•"))

(defun weather-scout--triple (glyph)
  "Repeat GLYPH three times."
  `(,glyph ,glyph ,glyph))

(defun weather-scout--light-rain (&optional body)
  "Light rain with optional BODY."
  `(,(weather-scout--raindrop-small-left) ,(or body " ") ,(weather-scout--raindrop-small-right)))

(defun weather-scout--rain (&optional body)
  "Medium rain with optional BODY."
  `(,(weather-scout--raindrop) ,(or body " ") ,(weather-scout--raindrop-small-right)))

(defun weather-scout--heavy-rain (&optional body)
  "Heavy rain with optional BODY."
  (if body
      `(,(weather-scout--raindrop) ,body ,(weather-scout--raindrop))
    (weather-scout--triple (weather-scout--raindrop))))

(defun weather-scout--light-snow (&optional body)
  "Light snow with optional BODY."
  (if body
      `(" " ,body ,(weather-scout--snowflake))
    `(" " ,(weather-scout--snowflake) " ")))

(defun weather-scout--snow (&optional body)
  "Medium snow with optional BODY."
  `(,(weather-scout--snowflake) ,(or body " ") ,(weather-scout--snowflake-small)))

(defun weather-scout--heavy-snow (&optional body)
  "Heavy snow with optional BODY."
  (if body
      `(,(weather-scout--snowflake) ,body ,(weather-scout--snowflake))
    (weather-scout--triple (weather-scout--snowflake))))

(defun weather-scout--light-sleet (&optional body)
  "Light sleet with optional BODY."
  `(,(weather-scout--snowflake-small) ,(or body " ") ,(weather-scout--raindrop-small-right)))

(defun weather-scout--sleet (&optional body)
  "Medium sleet with optional BODY."
  `(,(weather-scout--snowflake) ,(or body " ") ,(weather-scout--raindrop-small-right)))

(defun weather-scout--heavy-sleet (&optional body)
  "Heavy sleet with optional BODY."
  (if body
      `(,(weather-scout--snowflake) ,body ,(weather-scout--raindrop))
      `(,(weather-scout--raindrop) ,(weather-scout--snowflake) ,(weather-scout--raindrop))))

(defconst weather-scout--weather-symbols-list
  `(("clearsky_day" . (,(weather-scout--clear (weather-scout--sun)) ,(weather-scout--dry)))
    ("clearsky_night" . (,(weather-scout--clear (weather-scout--moon)) ,(weather-scout--dry)))
    ("clearsky_polartwilight" . (,(weather-scout--clear (weather-scout--polartwilight)) ,(weather-scout--dry)))
    ("cloudy" . (,(weather-scout--cloudy) ,(weather-scout--dry)))
    ("fair_day" . (,(weather-scout--fair (weather-scout--sun)) ,(weather-scout--dry)))
    ("fair_night" . (,(weather-scout--fair (weather-scout--moon)) ,(weather-scout--dry)))
    ("fair_polartwilight" . (,(weather-scout--fair (weather-scout--polartwilight)) ,(weather-scout--dry)))
    ("fog" . (,(weather-scout--cloudy) ,(weather-scout--fog)))
    ("heavyrain" . (,(weather-scout--cloudy) ,(weather-scout--heavy-rain)))
    ("heavyrainandthunder" . (,(weather-scout--cloudy) (,(weather-scout--raindrop) ,(weather-scout--lightning) ,(weather-scout--raindrop))))
    ("heavyrainshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--heavy-rain)))
    ("heavyrainshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--heavy-rain)))
    ("heavyrainshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--heavy-rain)))
    ("heavyrainshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--heavy-rain (weather-scout--lightning))))
    ("heavyrainshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--heavy-rain (weather-scout--lightning))))
    ("heavyrainshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--heavy-rain (weather-scout--lightning))))
    ("heavysleet" . (,(weather-scout--cloudy) ,(weather-scout--heavy-sleet)))
    ("heavysleetandthunder" . (,(weather-scout--cloudy) ,(weather-scout--heavy-sleet (weather-scout--lightning))))
    ("heavysleetshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--heavy-sleet)))
    ("heavysleetshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--heavy-sleet)))
    ("heavysleetshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--heavy-sleet)))
    ("heavysleetshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--heavy-sleet (weather-scout--lightning))))
    ("heavysleetshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--heavy-sleet (weather-scout--lightning))))
    ("heavysleetshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--heavy-sleet (weather-scout--lightning))))
    ("heavysnow" . (,(weather-scout--cloudy) ,(weather-scout--heavy-snow)))
    ("heavysnowandthunder" . (,(weather-scout--cloudy) ,(weather-scout--heavy-snow (weather-scout--lightning))))
    ("heavysnowshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--heavy-snow)))
    ("heavysnowshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--heavy-snow)))
    ("heavysnowshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--heavy-snow)))
    ("heavysnowshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--heavy-snow (weather-scout--lightning))))
    ("heavysnowshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--heavy-snow (weather-scout--lightning))))
    ("heavysnowshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--heavy-snow (weather-scout--lightning))))
    ("lightrain" . (,(weather-scout--cloudy) ,(weather-scout--light-rain)))
    ("lightrainandthunder" . (,(weather-scout--cloudy) ,(weather-scout--light-rain (weather-scout--lightning))))
    ("lightrainshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--light-rain)))
    ("lightrainshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--light-rain)))
    ("lightrainshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--light-rain)))
    ("lightrainshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--light-rain (weather-scout--lightning))))
    ("lightrainshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--light-rain (weather-scout--lightning))))
    ("lightrainshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--light-rain (weather-scout--lightning))))
    ("lightsleet" . (,(weather-scout--cloudy) ,(weather-scout--light-sleet)))
    ("lightsleetandthunder" . (,(weather-scout--cloudy) ,(weather-scout--light-sleet (weather-scout--lightning))))
    ("lightsleetshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--light-sleet)))
    ("lightsleetshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--light-sleet)))
    ("lightsleetshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--light-sleet)))
    ("lightsnow" . (,(weather-scout--cloudy) ,(weather-scout--light-snow)))
    ("lightsnowandthunder" . (,(weather-scout--cloudy) ,(weather-scout--light-snow (weather-scout--lightning))))
    ("lightsnowshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--light-snow)))
    ("lightsnowshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--light-snow)))
    ("lightsnowshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--light-snow)))
    ("lightssleetshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--light-sleet (weather-scout--lightning))))
    ("lightssleetshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--light-sleet (weather-scout--lightning))))
    ("lightssleetshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--light-sleet (weather-scout--lightning))))
    ("lightssnowshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--light-snow (weather-scout--lightning))))
    ("lightssnowshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--light-snow (weather-scout--lightning))))
    ("lightssnowshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--light-snow (weather-scout--lightning))))
    ("partlycloudy_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--dry)))
    ("partlycloudy_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--dry)))
    ("partlycloudy_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--dry)))
    ("rain" . (,(weather-scout--cloudy) ,(weather-scout--rain)))
    ("rainandthunder" . (,(weather-scout--cloudy) ,(weather-scout--rain (weather-scout--lightning))))
    ("rainshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--rain)))
    ("rainshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--rain)))
    ("rainshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--rain)))
    ("rainshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--rain (weather-scout--lightning))))
    ("rainshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--rain (weather-scout--lightning))))
    ("rainshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--rain (weather-scout--lightning))))
    ("sleet" . (,(weather-scout--cloudy) ,(weather-scout--sleet)))
    ("sleetandthunder" . (,(weather-scout--cloudy) ,(weather-scout--sleet (weather-scout--lightning))))
    ("sleetshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--sleet)))
    ("sleetshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--sleet)))
    ("sleetshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--sleet)))
    ("sleetshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--sleet (weather-scout--lightning))))
    ("sleetshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--sleet (weather-scout--lightning))))
    ("sleetshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--sleet (weather-scout--lightning))))
    ("snow" . (,(weather-scout--cloudy) ,(weather-scout--snow)))
    ("snowandthunder" . (,(weather-scout--cloudy) ,(weather-scout--snow (weather-scout--lightning))))
    ("snowshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--snow)))
    ("snowshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--snow)))
    ("snowshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--snow)))
    ("snowshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--snow (weather-scout--lightning))))
    ("snowshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--snow (weather-scout--lightning))))
    ("snowshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--snow (weather-scout--lightning))))))

(provide 'weather-scout-icons)
;;; weather-scout-icons.el ends here
