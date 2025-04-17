;;; weather-scout-icons.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Henrik Solgaard

;; Author: Henrik Solgaard <henrik.solgaard@gmail.com>
;; Maintainer: Henrik Solgaard <henrik.solgaard@gmail.com>

;; This file is not part of GNU Emacs.

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
  (if body
      `("c" ,body "ɔ")
    '("cOɔ")))

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

(defconst weather-scout--weather-symbols-list
  `(("clearsky_day" . (,(weather-scout--clear (weather-scout--sun)) ,(weather-scout--dry)))
    ("clearsky_night" . (,(weather-scout--clear (weather-scout--moon)) ,(weather-scout--dry)))
    ("clearsky_polartwilight" . (,(weather-scout--clear (weather-scout--polartwilight)) ,(weather-scout--dry)))
    ("cloudy" . (,(weather-scout--cloudy) ,(weather-scout--dry)))
    ("fair_day" . (,(weather-scout--fair (weather-scout--sun)) ,(weather-scout--dry)))
    ("fair_night" . (,(weather-scout--fair (weather-scout--moon)) ,(weather-scout--dry)))
    ("fair_polartwilight" . (,(weather-scout--fair (weather-scout--polartwilight)) ,(weather-scout--dry)))
    ("fog" . (,(weather-scout--cloudy) ,(weather-scout--fog)))
    ("heavyrain" . (,(weather-scout--cloudy) ("///")))
    ("heavyrainandthunder" . (,(weather-scout--cloudy) ("/" ,(weather-scout--lightning) "/")))
    ("heavyrainshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("///")))
    ("heavyrainshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("///")))
    ("heavyrainshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("///")))
    ("heavyrainshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("/" ,(weather-scout--lightning) "/")))
    ("heavyrainshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("/" ,(weather-scout--lightning) "/")))
    ("heavyrainshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("/" ,(weather-scout--lightning) "/")))
    ("heavysleet" . (,(weather-scout--cloudy) ("/❄/")))
    ("heavysleetandthunder" . (,(weather-scout--cloudy) ("/" ,(weather-scout--lightning) "❄")))
    ("heavysleetshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("/❄/")))
    ("heavysleetshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("/❄/")))
    ("heavysleetshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("/❄/")))
    ("heavysleetshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("/" ,(weather-scout--lightning) "❄")))
    ("heavysleetshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("/" ,(weather-scout--lightning) "❄")))
    ("heavysleetshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("/" ,(weather-scout--lightning) "❄")))
    ("heavysnow" . (,(weather-scout--cloudy) ("❄❄❄")))
    ("heavysnowandthunder" . (,(weather-scout--cloudy) ("❄" ,(weather-scout--lightning) "❄")))
    ("heavysnowshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("❄❄❄")))
    ("heavysnowshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("❄❄❄")))
    ("heavysnowshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("❄❄❄")))
    ("heavysnowshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("❄" ,(weather-scout--lightning) "❄")))
    ("heavysnowshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("❄" ,(weather-scout--lightning) "❄")))
    ("heavysnowshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("❄" ,(weather-scout--lightning) "❄")))
    ("lightrain" . (,(weather-scout--cloudy) ("' ,")))
    ("lightrainandthunder" . (,(weather-scout--cloudy) ("'" ,(weather-scout--lightning) ",")))
    ("lightrainshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("' ,")))
    ("lightrainshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("' ,")))
    ("lightrainshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("' ,")))
    ("lightrainshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("'" ,(weather-scout--lightning) ",")))
    ("lightrainshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("'" ,(weather-scout--lightning) ",")))
    ("lightrainshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("'" ,(weather-scout--lightning) ",")))
    ("lightsleet" . (,(weather-scout--cloudy) ("❄ ,")))
    ("lightsleetandthunder" . (,(weather-scout--cloudy) ("❄" ,(weather-scout--lightning) ",")))
    ("lightsleetshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("❄ ,")))
    ("lightsleetshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("❄ ,")))
    ("lightsleetshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("❄ ,")))
    ("lightsnow" . (,(weather-scout--cloudy) ("❄ ❄")))
    ("lightsnowandthunder" . (,(weather-scout--cloudy) (" " ,(weather-scout--lightning) "❄")))
    ("lightsnowshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) (" ❄ ")))
    ("lightsnowshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) (" ❄ ")))
    ("lightsnowshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) (" ❄ ")))
    ("lightssleetshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("❄" ,(weather-scout--lightning) ",")))
    ("lightssleetshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("❄" ,(weather-scout--lightning) ",")))
    ("lightssleetshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("❄" ,(weather-scout--lightning) ",")))
    ("lightssnowshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) (" " ,(weather-scout--lightning) "❄")))
    ("lightssnowshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) (" " ,(weather-scout--lightning) "❄")))
    ("lightssnowshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) (" " ,(weather-scout--lightning) "❄")))
    ("partlycloudy_day" . (,(weather-scout--cloudy (weather-scout--sun)) ,(weather-scout--dry)))
    ("partlycloudy_night" . (,(weather-scout--cloudy (weather-scout--moon)) ,(weather-scout--dry)))
    ("partlycloudy_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ,(weather-scout--dry)))
    ("rain" . (,(weather-scout--cloudy) ("/ /")))
    ("rainandthunder" . (,(weather-scout--cloudy) (" " ,(weather-scout--lightning) "/")))
    ("rainshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("/ /")))
    ("rainshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("/ /")))
    ("rainshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("/ /")))
    ("rainshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("'" ,(weather-scout--lightning) "/")))
    ("rainshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("'" ,(weather-scout--lightning) "/")))
    ("rainshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("'" ,(weather-scout--lightning) "/")))
    ("sleet" . (,(weather-scout--cloudy) ("❄ /")))
    ("sleetandthunder" . (,(weather-scout--cloudy) ("❄" ,(weather-scout--lightning) "/")))
    ("sleetshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("❄ /")))
    ("sleetshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) (" /")))
    ("sleetshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("❄ /")))
    ("sleetshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("❄" ,(weather-scout--lightning) "/")))
    ("sleetshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("❄" ,(weather-scout--lightning) "/")))
    ("sleetshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("❄" ,(weather-scout--lightning) "/")))
    ("snow" . (,(weather-scout--cloudy) ("❄ ❄")))
    ("snowandthunder" . (,(weather-scout--cloudy) (" " ,(weather-scout--lightning) "❄")))
    ("snowshowers_day" . (,(weather-scout--cloudy (weather-scout--sun)) ("❄ ❄")))
    ("snowshowers_night" . (,(weather-scout--cloudy (weather-scout--moon)) ("❄ ❄")))
    ("snowshowers_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) ("❄ ❄")))
    ("snowshowersandthunder_day" . (,(weather-scout--cloudy (weather-scout--sun)) (" " ,(weather-scout--lightning) "❄")))
    ("snowshowersandthunder_night" . (,(weather-scout--cloudy (weather-scout--moon)) (" " ,(weather-scout--lightning) "❄")))
    ("snowshowersandthunder_polartwilight" . (,(weather-scout--cloudy (weather-scout--polartwilight)) (" " ,(weather-scout--lightning) "❄")))))

(provide 'weather-scout-icons)
;;; weather-scout-icons.el ends here
