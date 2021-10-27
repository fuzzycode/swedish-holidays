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
(defvar swedish-holidays-christian-holidays (mapcar 'purecopy '((holiday-fixed 1 6 "Trettondedag Jul")
                                                                (holiday-easter-etc -2 "Långfredagen")
                                                                (holiday-easter-etc 0 "Påskdagen")
                                                                (holiday-easter-etc 1 "Annandag Påsk")
                                                                (holiday-easter-etc 39 "Kristi Himmelfärdsdagen")
                                                                (holiday-fixed 12 24 "Julafton")
                                                                (holiday-fixed 12 25 "Juldagen")
                                                                (holiday-fixed 12 26 "Annandag Jul")
                                                                (holiday-easter-etc -1 "Påskafton")
                                                                (holiday-easter-etc 48 "Pingstafton")
                                                                (holiday-easter-etc 49 "Pingstdagen")))
  "Swedish christian holidays.")

;;;###autoload
(defvar swedish-holidays-local-holidays (mapcar 'purecopy '((holiday-fixed 1 1 "Nyårsdagen")
                                                            (holiday-fixed 5 1 "Första Maj")
                                                            (holiday-fixed 6 6 "Sveriges Nationaldag")
                                                            (swedish-holidays--midsummer-nth -1 "Midsommarafton")
                                                            (swedish-holidays--midsummer-nth 0 "Midsommardagen")
                                                            (swedish-holidays--alla-helgona-nth 0 "Alla helgons dag")
                                                            (holiday-fixed 12 31 "Nyårsafton")))
  "Swedish local bank holidays.")

;;;###autoload
(defvar swedish-holidays-other-holidays (mapcar 'purecopy '((holiday-fixed 1 5 "Trettondagsafton")
                                                            (holiday-fixed 1 13 "Tjugondag Knut")
                                                            (holiday-float 5 0 -1 "Mors dag")
                                                            (holiday-fixed 2 14 "Alla hjärtans dag")
                                                            (holiday-easter-etc -47 "Fettisdagen")
                                                            (holiday-fixed 4 30 "Valborgsmässoafton")
                                                            (holiday-fixed 10 24 "FN-dagen")
                                                            (holiday-float 11 0 2 "Fars dag")
                                                            (swedish-holidays--alla-helgona-nth -1 "Allhelgonaafton")
                                                            (holiday-float 12 0 -4 "Första advent" 24)
                                                            (holiday-float 12 0 -3 "Andra advent" 24)
                                                            (holiday-float 12 0 -2 "Tredje advent" 24)
                                                            (holiday-float 12 0 -1 "Fjärde advent" 24)
                                                            (holiday-fixed 12 10 "Nobeldagen")
                                                            (holiday-fixed 12 13 "Lucia")))
  "Swedish holidays, not considered bank holidays.")

;;;###autoload
(defvar swedish-holidays-misc-holidays (mapcar 'purecopy '((holiday-fixed 1 28 "Konungens namnsdag")
                                                           (holiday-fixed 2 2 "Kyndelsmässodagen")
                                                           (holiday-fixed 3 8 "Internationella kvinnodagen")
                                                           (holiday-fixed 3 12 "Kronprinsessans namnsdag")
                                                           (holiday-fixed 3 25 "Vårfrudagen")
                                                           (holiday-fixed 4 30 "Konungens födelsedag")
                                                           (holiday-fixed 4 1 "Första april")
                                                           (holiday-fixed 7 14 "Kronprinsessans födelsedag")
                                                           (holiday-fixed 8 8 "Drottningens namnsdag")
                                                           (holiday-fixed 11 6 "Gustaf Adolfsdagen")
                                                           (holiday-fixed 11 19 "Internationella mansdagen")
                                                           (holiday-fixed 12 23 "Drottningens födelsedag")))
  "Swedish extra holidays.")

;;;###autoload
(defvar swedish-holidays-solar-holidays
  (mapcar 'purecopy
          '((solar-equinoxes-solstices)
            (holiday-sexp calendar-daylight-savings-starts
                          (format "Sommartid börjar %s"
                                  (solar-time-string
                                   (/ calendar-daylight-savings-starts-time (float 60))
                                   calendar-standard-time-zone-name)))
            (holiday-sexp calendar-daylight-savings-ends
                          (format "Vintertid börjar %s"
                                  (solar-time-string
                                   (/ calendar-daylight-savings-ends-time (float 60))
                                   calendar-daylight-time-zone-name)))))
  "A localized re-implementation of `holiday-solar-holidays'.")

(defun swedish-holidays--midsummer-nth (n name)
  "Date of Nth day after Swedish midsummer named NAME.
Negative numbers will return days before midsummer."
  (let* ((midsommar-d (calendar-dayname-on-or-before
                       6 (calendar-absolute-from-gregorian
                          (list 6 26 displayed-year))))
         (date (calendar-gregorian-from-absolute (+ n midsommar-d))))
    (holiday-fixed (car date) (cadr date) name)))

(defun swedish-holidays--alla-helgona-nth (n name)
  "Date of Nth day after Swedish 'Alla helgons dag' named NAME.
Negative numbers will return days before 'Alla helgons dag'."
  (let ((date (calendar-gregorian-from-absolute
               (+ n (calendar-dayname-on-or-before 6 (calendar-absolute-from-gregorian (list 11 6 displayed-year)))))))
    (holiday-fixed (car date) (cadr date) name)))


(defun swedish-holidays-solar-sunrise-sunset-string-a (date &optional nolocation)
  "String of *local* times of sunrise, sunset, and daylight on Gregorian DATE.
Optional NOLOCATION non-nil means do not print the location.

This is a localized re-implementation of `solar-sunrise-sunset-string'."
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
(defun swedish-holidays-setup (&optional extras)
  "Setup Swedish localization for common date names and formats.
If EXTRAS is non nil, `swedish-holidays-misc-holidays' will be added to the list of holidays"

  ;; Use localized version of sunrise/sunset name string function
  (advice-add 'solar-sunrise-sunset-string :override #'swedish-holidays-solar-sunrise-sunset-string-a)

  (setq holiday-solar-holidays swedish-holidays-solar-holidays
        holiday-christian-holidays swedish-holidays-christian-holidays
        holiday-local-holidays swedish-holidays-local-holidays
        holiday-other-holidays swedish-holidays-other-holidays
        calendar-holidays (append swedish-holidays-christian-holidays swedish-holidays-local-holidays swedish-holidays-other-holidays))

  (when extras
    (setq calendar-holidays (append calendar-holidays swedish-holidays-misc-holidays))
    (setq holiday-other-holidays (append holiday-other-holidays swedish-holidays-misc-holidays)))

  ;; Clear holidays that do not apply
  (setq holiday-hebrew-holidays nil
        holiday-bahai-holidays nil
        holiday-oriental-holidays nil
        holiday-islamic-holidays nil)

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

;;;###autoload
(defun swedish-holidays-disable ()
  "Remove any advice installed by this package."
  (advice-remove 'solar-sunrise-sunset-string #'swedish-holidays-solar-sunrise-sunset-string-a))

(provide 'swedish-holidays)
;;; swedish-holidays.el ends here
