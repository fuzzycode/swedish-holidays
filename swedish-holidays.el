;;; swedish-holidays.el --- Provides Swedish holidays for the calendar -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Björn Larsson
;;
;; Author: Björn Larsson <https://github.com/fuzzycode>
;; Maintainer: Björn Larsson <develop@bjornlarsson.net>
;; Created: August 14, 2021
;; Modified: August 14, 2021
;; Version: 0.0.1
;; Keywords: calendar swedish localization
;; Homepage: https://github.com/bjornlarsson/swedish-holidays
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Provides Swedish holidays for Emacs calendar
;;


;;; Code:

(eval-when-compile
  (require 'lunar)
  (require 'solar)
  (require 'calendar)
  (require 'holidays))

;;;###autoload
(defvar swedish-holidays
  '((holiday-fixed 1 1 "Nyårsdagen")
    (holiday-fixed 1 6 "Trettondedag Jul")
    (holiday-fixed 5 1 "Första Maj")

    (holiday-easter-etc -2 "Långfredagen")
    (holiday-easter-etc 0 "Påskdagen")
    (holiday-easter-etc 1 "Annandag Påsk")
    (holiday-easter-etc 39 "Kristi Himmelfärdsdagen")
    (holiday-easter-etc 49 "Pingstdagen")

    (holiday-fixed 6 6 "Sveriges Nationaldag")
    (swedish-holidays-midsummer-nth 0 "Midsommardagen")
    (holiday-fixed 12 24 "Julafton")
    (holiday-fixed 12 25 "Juldagen")
    (holiday-fixed 12 26 "Annandag Jul")
    (holiday-fixed 12 31 "Nyårsafton"))
  "Official holidays in Sweden.")

;;;###autoload
(defvar swedish-holidays-extras
  '((holiday-fixed 1 5 "Trettondagsafton")

    (holiday-easter-etc -1 "Påskafton")
    (holiday-easter-etc 48 "Pingstafton")

    (swedish-holidays-midsummer-nth -1 "Midsommarafton")

    (holiday-fixed 4 30 "Valborgsmässoafton"))
  "Extra holidays in Sweden.")

;;;###autoload
(defvar swedish-holidays-misc
  '((holiday-fixed 1 13 "Tjugondag Knut")
    (holiday-float 5 0 -1 "Mors dag")
    (holiday-float 11 0 2 "Fars dag")
    (holiday-fixed 2 14 "Alla hjärtans dag")
    (holiday-easter-etc -47 "Fettisdagen")
    (holiday-fixed 10 24 "FN-dagen")
    (holiday-float 12 0 -4 "Första advent" 24)
    (holiday-float 12 0 -3 "Andra advent" 24)
    (holiday-float 12 0 -2 "Tredje advent" 24)
    (holiday-float 12 0 -1 "Fjärde advent" 24)
    (holiday-fixed 12 10 "Nobeldagen")
    (holiday-fixed 12 13 "Lucia"))
  "Extra days of interest in the Swedish calendar.")

(defun swedish-holidays-midsummer-nth (n name)
  "Date of Nth day after Swedish midsummer named NAME.
Negative numbers will return days before midsummer."
  (let ((midsommar-d (calendar-dayname-on-or-before
                      6 (calendar-absolute-from-gregorian
                         (list 6 26 displayed-year)))))
    (list (list (calendar-gregorian-from-absolute (+ midsommar-d n)) name))))


(defun swedish-holidays-solar-sunrise-sunset-string-a (date &optional nolocation)
"String of *local* times of sunrise, sunset, and daylight on Gregorian DATE.
Optional NOLOCATION non-nil means do not print the location.

This is a localized re-implementation of 'solar-sunrise-sunset-string`."
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s, %s%s (%s timmar dagsljus)"
     (if (car l)
         (concat "Sol upp " (apply #'solar-time-string (car l)))
       "Ingen soluppgång")
     (if (cadr l)
         (concat "Sol ned " (apply #'solar-time-string (cadr l)))
       "Ingen solnedgång")
     (if nolocation ""
       (format " vid %s" (eval calendar-location-name)))
     (nth 2 l))))

;;;###autoload
(defun swedish-holidays-setup ()
  "Setup Swedish localization for common date names and formats."

  ;; Use localized version of sunrise/sunset name string function
  (advice-add 'solar-sunrise-sunset-string :override #'swedish-holidays-solar-sunrise-sunset-string-a)

  (setq calendar-holidays (append swedish-holidays swedish-holidays-extras swedish-holidays-misc))

  ;; Start the week on Monday
  (setq calendar-week-start-day 1)

  (setq calendar-date-style 'european)

  (setq calendar-date-display-form
        '((if dayname
              (concat dayname ", "))
          day " " monthname " " year))

  (setq calendar-time-display-form
        '(24-hours ":" minutes))

  (setq lunar-phase-names '("Nymåne" "Växande halvmåne" "Fullmåne" "Avtagande halvmåne"))

  (setq solar-n-hemi-seasons '("Vårdagjämningen" "Sommarsolståndet" "Höstdagjämningen" "Vintersolståndet"))

  (setq calendar-day-name-array
        ["söndag" "måndag" "tisdag" "onsdag" "torsdag" "fredag" "lördag"]
        calendar-day-abbrev-array
        ["sön" "mån" "tis" "ons" "tors" "fre" "lör"]
        calendar-day-header-array
        ["sö" "må" "ti" "on" "to" "fr" "lö"])
  (setq calendar-month-name-array
        ["januari" "februari" "mars" "april" "maj" "juni"
         "juli" "augusti" "september" "oktober" "november" "december"]
        calendar-month-abbrev-array
        ["jan" "feb" "mar" "apr" "maj" "jun"
         "jul" "aug" "sep" "okt" "nov" "dec"]))

(provide 'swedish-holidays)
;;; swedish-holidays.el ends here
