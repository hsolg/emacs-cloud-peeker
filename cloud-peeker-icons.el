;;; cloud-peeker-icons.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Henrik Solgaard

;; Author: Henrik Solgaard <henrik.solgaard@gmail.com>
;; Maintainer: Henrik Solgaard <henrik.solgaard@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Console based weather icons

;;; Code:

(defconst cloud-peeker--color-codes
  '((black . 30)
    (red . 31)
    (green . 32)
    (yellow . 33)
    (blue . 34)
    (magenta . 35)
    (cyan . 36)
    (white . 37)
    (reset . 0)))

(defun cloud-peeker-sun ()
  "Sun."
  '(yellow "☀" reset))

(defun cloud-peeker--moon ()
  "Moon."
  '(yellow "☽" reset))

(defun cloud-peeker--polartwilight ()
  "Sun below horizon."
  '(yellow "◒" reset))

(defun cloud-peeker--clear (body)
  "BODY with no clouds."
  `(" " ,body " "))

(defun cloud-peeker--cloudy (&optional body)
  "Cloud around optional BODY."
  (if body
      `("c" ,body "ɔ")
    '("cOɔ")))

(defun cloud-peeker--fair (body)
  "Small clouds around BODY."
  `(" " ,body "ɔ"))

(defun cloud-peeker--dry ()
  "No precipitation."
  '())

(defun cloud-peeker--fog ()
  "Fog."
  '("==="))

(defun cloud-peeker--lightning ()
  "Lightning."
  '(yellow "ϟ" reset))

(defconst cloud-peeker--weather-symbols-list
  `(("clearsky_day" . (,(cloud-peeker--clear (cloud-peeker-sun)) ,(cloud-peeker--dry)))
    ("clearsky_night" . (,(cloud-peeker--clear (cloud-peeker--moon)) ,(cloud-peeker--dry)))
    ("clearsky_polartwilight" . (,(cloud-peeker--clear (cloud-peeker--polartwilight)) ,(cloud-peeker--dry)))
    ("cloudy" . (,(cloud-peeker--cloudy) ,(cloud-peeker--dry)))
    ("fair_day" . (,(cloud-peeker--fair (cloud-peeker-sun)) ,(cloud-peeker--dry)))
    ("fair_night" . (,(cloud-peeker--fair (cloud-peeker--moon)) ,(cloud-peeker--dry)))
    ("fair_polartwilight" . (,(cloud-peeker--fair (cloud-peeker--polartwilight)) ,(cloud-peeker--dry)))
    ("fog" . (,(cloud-peeker--cloudy) ,(cloud-peeker--fog)))
    ("heavyrain" . (,(cloud-peeker--cloudy) ("///")))
    ("heavyrainandthunder" . (,(cloud-peeker--cloudy) ("/" ,(cloud-peeker--lightning) "/")))
    ("heavyrainshowers_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("///")))
    ("heavyrainshowers_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("///")))
    ("heavyrainshowers_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("///")))
    ("heavyrainshowersandthunder_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("/" ,(cloud-peeker--lightning) "/")))
    ("heavyrainshowersandthunder_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("/" ,(cloud-peeker--lightning) "/")))
    ("heavyrainshowersandthunder_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("/" ,(cloud-peeker--lightning) "/")))
    ("heavysleet" . (,(cloud-peeker--cloudy) ("/*/")))
    ("heavysleetandthunder" . (,(cloud-peeker--cloudy) ("/" ,(cloud-peeker--lightning) "*")))
    ("heavysleetshowers_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("/*/")))
    ("heavysleetshowers_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("/*/")))
    ("heavysleetshowers_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("/*/")))
    ("heavysleetshowersandthunder_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("/" ,(cloud-peeker--lightning) "*")))
    ("heavysleetshowersandthunder_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("/" ,(cloud-peeker--lightning) "*")))
    ("heavysleetshowersandthunder_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("/" ,(cloud-peeker--lightning) "*")))
    ("heavysnow" . (,(cloud-peeker--cloudy) ("***")))
    ("heavysnowandthunder" . (,(cloud-peeker--cloudy) ("*" ,(cloud-peeker--lightning) "*")))
    ("heavysnowshowers_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("***")))
    ("heavysnowshowers_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("***")))
    ("heavysnowshowers_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("***")))
    ("heavysnowshowersandthunder_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("*" ,(cloud-peeker--lightning) "*")))
    ("heavysnowshowersandthunder_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("*" ,(cloud-peeker--lightning) "*")))
    ("heavysnowshowersandthunder_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("*" ,(cloud-peeker--lightning) "*")))
    ("lightrain" . (,(cloud-peeker--cloudy) ("' ,")))
    ("lightrainandthunder" . (,(cloud-peeker--cloudy) ("'" ,(cloud-peeker--lightning) ",")))
    ("lightrainshowers_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("' ,")))
    ("lightrainshowers_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("' ,")))
    ("lightrainshowers_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("' ,")))
    ("lightrainshowersandthunder_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("'" ,(cloud-peeker--lightning) ",")))
    ("lightrainshowersandthunder_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("'" ,(cloud-peeker--lightning) ",")))
    ("lightrainshowersandthunder_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("'" ,(cloud-peeker--lightning) ",")))
    ("lightsleet" . (,(cloud-peeker--cloudy) ("* ,")))
    ("lightsleetandthunder" . (,(cloud-peeker--cloudy) ("*" ,(cloud-peeker--lightning) ",")))
    ("lightsleetshowers_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("* ,")))
    ("lightsleetshowers_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("* ,")))
    ("lightsleetshowers_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("* ,")))
    ("lightsnow" . (,(cloud-peeker--cloudy) ("* *")))
    ("lightsnowandthunder" . (,(cloud-peeker--cloudy) (" " ,(cloud-peeker--lightning) "*")))
    ("lightsnowshowers_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) (" * ")))
    ("lightsnowshowers_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) (" * ")))
    ("lightsnowshowers_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) (" * ")))
    ("lightssleetshowersandthunder_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("*" ,(cloud-peeker--lightning) ",")))
    ("lightssleetshowersandthunder_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("*" ,(cloud-peeker--lightning) ",")))
    ("lightssleetshowersandthunder_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("*" ,(cloud-peeker--lightning) ",")))
    ("lightssnowshowersandthunder_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) (" " ,(cloud-peeker--lightning) "*")))
    ("lightssnowshowersandthunder_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) (" " ,(cloud-peeker--lightning) "*")))
    ("lightssnowshowersandthunder_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) (" " ,(cloud-peeker--lightning) "*")))
    ("partlycloudy_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ,(cloud-peeker--dry)))
    ("partlycloudy_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ,(cloud-peeker--dry)))
    ("partlycloudy_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ,(cloud-peeker--dry)))
    ("rain" . (,(cloud-peeker--cloudy) ("/ /")))
    ("rainandthunder" . (,(cloud-peeker--cloudy) (" " ,(cloud-peeker--lightning) "/")))
    ("rainshowers_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("/ /")))
    ("rainshowers_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("/ /")))
    ("rainshowers_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("/ /")))
    ("rainshowersandthunder_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("'" ,(cloud-peeker--lightning) "/")))
    ("rainshowersandthunder_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("'" ,(cloud-peeker--lightning) "/")))
    ("rainshowersandthunder_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("'" ,(cloud-peeker--lightning) "/")))
    ("sleet" . (,(cloud-peeker--cloudy) ("* /")))
    ("sleetandthunder" . (,(cloud-peeker--cloudy) ("*" ,(cloud-peeker--lightning) "/")))
    ("sleetshowers_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("* /")))
    ("sleetshowers_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("* /")))
    ("sleetshowers_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("* /")))
    ("sleetshowersandthunder_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("*" ,(cloud-peeker--lightning) "/")))
    ("sleetshowersandthunder_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("*" ,(cloud-peeker--lightning) "/")))
    ("sleetshowersandthunder_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("*" ,(cloud-peeker--lightning) "/")))
    ("snow" . (,(cloud-peeker--cloudy) ("* *")))
    ("snowandthunder" . (,(cloud-peeker--cloudy) (" " ,(cloud-peeker--lightning) "*")))
    ("snowshowers_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) ("* *")))
    ("snowshowers_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) ("* *")))
    ("snowshowers_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) ("* *")))
    ("snowshowersandthunder_day" . (,(cloud-peeker--cloudy (cloud-peeker-sun)) (" " ,(cloud-peeker--lightning) "*")))
    ("snowshowersandthunder_night" . (,(cloud-peeker--cloudy (cloud-peeker--moon)) (" " ,(cloud-peeker--lightning) "*")))
    ("snowshowersandthunder_polartwilight" . (,(cloud-peeker--cloudy (cloud-peeker--polartwilight)) (" " ,(cloud-peeker--lightning) "*")))))

(provide 'cloud-peeker-icons)
;;; cloud-peeker-icons.el ends here
