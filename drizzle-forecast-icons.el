;;; drizzle-forecast-icons.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Henrik Solgaard
;;
;; Author: Henrik Solgaard <henrik.solgaard@gmail.com>
;; Maintainer: Henrik Solgaard <henrik.solgaard@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Console based weather icons
;;
;;; Code:

(defconst drizzle-forecast--color-codes
  '((black . 30)
    (red . 31)
    (green . 32)
    (yellow . 33)
    (blue . 34)
    (magenta . 35)
    (cyan . 36)
    (white . 37)
    (reset . 0)))

(defun drizzle-forecast-sun ()
  "Sun."
  '(yellow "O" reset))

(defun drizzle-forecast--moon ()
  "Moon."
  '(yellow ")" reset))

(defun drizzle-forecast--polartwilight ()
  "Sun below horizon."
  '(yellow "0" reset))

(defun drizzle-forecast--clear (body)
  "BODY with no clouds."
  `(" " ,body " "))

(defun drizzle-forecast--cloudy (&optional body)
  "Cloud around optional BODY."
  (if body
      `("c" ,body "ɔ")
    '("cOɔ")))

(defun drizzle-forecast--fair (body)
  "Small clouds around BODY."
  `(" " ,body "ɔ"))

(defun drizzle-forecast--dry ()
  "No precipitation."
  '("   "))

(defun drizzle-forecast--fog ()
  "Fog."
  '("==="))

(defun drizzle-forecast--lightning ()
  "Lightning."
  '(yellow "ϟ" reset))

(defconst drizzle-forecast--weather-symbols-list
  `(("clearsky_day" . (,(drizzle-forecast--clear (drizzle-forecast-sun)) ,(drizzle-forecast--dry)))
    ("clearsky_night" . (,(drizzle-forecast--clear (drizzle-forecast--moon)) ,(drizzle-forecast--dry)))
    ("clearsky_polartwilight" . (,(drizzle-forecast--clear (drizzle-forecast--polartwilight)) ,(drizzle-forecast--dry)))
    ("cloudy" . (,(drizzle-forecast--cloudy) ,(drizzle-forecast--dry)))
    ("fair_day" . (,(drizzle-forecast--fair (drizzle-forecast-sun)) ,(drizzle-forecast--dry)))
    ("fair_night" . (,(drizzle-forecast--fair (drizzle-forecast--moon)) ,(drizzle-forecast--dry)))
    ("fair_polartwilight" . (,(drizzle-forecast--fair (drizzle-forecast--polartwilight)) ,(drizzle-forecast--dry)))
    ("fog" . (,(drizzle-forecast--cloudy) ,(drizzle-forecast--fog)))
    ("heavyrain" . (,(drizzle-forecast--cloudy) ("///")))
    ("heavyrainandthunder" . (,(drizzle-forecast--cloudy) ("/" ,(drizzle-forecast--lightning) "/")))
    ("heavyrainshowers_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("///")))
    ("heavyrainshowers_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("///")))
    ("heavyrainshowers_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("///")))
    ("heavyrainshowersandthunder_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("/" ,(drizzle-forecast--lightning) "/")))
    ("heavyrainshowersandthunder_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("/" ,(drizzle-forecast--lightning) "/")))
    ("heavyrainshowersandthunder_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("/" ,(drizzle-forecast--lightning) "/")))
    ("heavysleet" . (,(drizzle-forecast--cloudy) ("/*/")))
    ("heavysleetandthunder" . (,(drizzle-forecast--cloudy) ("/" ,(drizzle-forecast--lightning) "*")))
    ("heavysleetshowers_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("/*/")))
    ("heavysleetshowers_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("/*/")))
    ("heavysleetshowers_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("/*/")))
    ("heavysleetshowersandthunder_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("/" ,(drizzle-forecast--lightning) "*")))
    ("heavysleetshowersandthunder_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("/" ,(drizzle-forecast--lightning) "*")))
    ("heavysleetshowersandthunder_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("/" ,(drizzle-forecast--lightning) "*")))
    ("heavysnow" . (,(drizzle-forecast--cloudy) ("***")))
    ("heavysnowandthunder" . (,(drizzle-forecast--cloudy) ("*" ,(drizzle-forecast--lightning) "*")))
    ("heavysnowshowers_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("***")))
    ("heavysnowshowers_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("***")))
    ("heavysnowshowers_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("***")))
    ("heavysnowshowersandthunder_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("*" ,(drizzle-forecast--lightning) "*")))
    ("heavysnowshowersandthunder_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("*" ,(drizzle-forecast--lightning) "*")))
    ("heavysnowshowersandthunder_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("*" ,(drizzle-forecast--lightning) "*")))
    ("lightrain" . (,(drizzle-forecast--cloudy) ("' ,")))
    ("lightrainandthunder" . (,(drizzle-forecast--cloudy) ("'" ,(drizzle-forecast--lightning) ",")))
    ("lightrainshowers_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("' ,")))
    ("lightrainshowers_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("' ,")))
    ("lightrainshowers_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("' ,")))
    ("lightrainshowersandthunder_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("'" ,(drizzle-forecast--lightning) ",")))
    ("lightrainshowersandthunder_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("'" ,(drizzle-forecast--lightning) ",")))
    ("lightrainshowersandthunder_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("'" ,(drizzle-forecast--lightning) ",")))
    ("lightsleet" . (,(drizzle-forecast--cloudy) ("* ,")))
    ("lightsleetandthunder" . (,(drizzle-forecast--cloudy) ("*" ,(drizzle-forecast--lightning) ",")))
    ("lightsleetshowers_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("* ,")))
    ("lightsleetshowers_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("* ,")))
    ("lightsleetshowers_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("* ,")))
    ("lightsnow" . (,(drizzle-forecast--cloudy) ("* *")))
    ("lightsnowandthunder" . (,(drizzle-forecast--cloudy) (" " ,(drizzle-forecast--lightning) "*")))
    ("lightsnowshowers_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) (" * ")))
    ("lightsnowshowers_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) (" * ")))
    ("lightsnowshowers_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) (" * ")))
    ("lightssleetshowersandthunder_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("*" ,(drizzle-forecast--lightning) ",")))
    ("lightssleetshowersandthunder_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("*" ,(drizzle-forecast--lightning) ",")))
    ("lightssleetshowersandthunder_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("*" ,(drizzle-forecast--lightning) ",")))
    ("lightssnowshowersandthunder_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) (" " ,(drizzle-forecast--lightning) "*")))
    ("lightssnowshowersandthunder_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) (" " ,(drizzle-forecast--lightning) "*")))
    ("lightssnowshowersandthunder_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) (" " ,(drizzle-forecast--lightning) "*")))
    ("partlycloudy_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ,(drizzle-forecast--dry)))
    ("partlycloudy_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ,(drizzle-forecast--dry)))
    ("partlycloudy_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ,(drizzle-forecast--dry)))
    ("rain" . (,(drizzle-forecast--cloudy) ("/ /")))
    ("rainandthunder" . (,(drizzle-forecast--cloudy) (" " ,(drizzle-forecast--lightning) "/")))
    ("rainshowers_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("/ /")))
    ("rainshowers_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("/ /")))
    ("rainshowers_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("/ /")))
    ("rainshowersandthunder_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("'" ,(drizzle-forecast--lightning) "/")))
    ("rainshowersandthunder_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("'" ,(drizzle-forecast--lightning) "/")))
    ("rainshowersandthunder_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("'" ,(drizzle-forecast--lightning) "/")))
    ("sleet" . (,(drizzle-forecast--cloudy) ("* /")))
    ("sleetandthunder" . (,(drizzle-forecast--cloudy) ("*" ,(drizzle-forecast--lightning) "/")))
    ("sleetshowers_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("* /")))
    ("sleetshowers_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("* /")))
    ("sleetshowers_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("* /")))
    ("sleetshowersandthunder_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("*" ,(drizzle-forecast--lightning) "/")))
    ("sleetshowersandthunder_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("*" ,(drizzle-forecast--lightning) "/")))
    ("sleetshowersandthunder_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("*" ,(drizzle-forecast--lightning) "/")))
    ("snow" . (,(drizzle-forecast--cloudy) ("* *")))
    ("snowandthunder" . (,(drizzle-forecast--cloudy) (" " ,(drizzle-forecast--lightning) "*")))
    ("snowshowers_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) ("* *")))
    ("snowshowers_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) ("* *")))
    ("snowshowers_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) ("* *")))
    ("snowshowersandthunder_day" . (,(drizzle-forecast--cloudy (drizzle-forecast-sun)) (" " ,(drizzle-forecast--lightning) "*")))
    ("snowshowersandthunder_night" . (,(drizzle-forecast--cloudy (drizzle-forecast--moon)) (" " ,(drizzle-forecast--lightning) "*")))
    ("snowshowersandthunder_polartwilight" . (,(drizzle-forecast--cloudy (drizzle-forecast--polartwilight)) (" " ,(drizzle-forecast--lightning) "*")))))

(provide 'drizzle-forecast-icons)
;;; drizzle-forecast-icons.el ends here
