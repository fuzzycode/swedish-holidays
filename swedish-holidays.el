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

    (holiday-fixed 4 30 "Valborgsmässoafton"))
  "Extra holidays in Sweden.")

;;;###autoload
(defvar swedish-holidays-misc
  '((holiday-float 12 0 -4 "Första advent" 24)
    (holiday-float 12 0 -3 "Andra advent" 24)
    (holiday-float 12 0 -2 "Tredje advent" 24)
    (holiday-float 12 0 -1 "Fjärde advent" 24)
    (holiday-fixed 12 13 "Lucia"))
  "Extra days of interest in the Swedish calendar.")

(provide 'swedish-holidays)
;;; swedish-holidays.el ends here
