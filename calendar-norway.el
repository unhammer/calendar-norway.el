;;; calendar-norway.el --- Norwegian calendar for Emacs

;; Copyright (C) 2012 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.9
;; Keywords: calendar norwegian localization

;; Based on http://bigwalter.net/daniel/elisp/sv-kalender.el v.1.8
;; Copyright (C) 2002,2003,2004,2007,2009 Daniel Jensen
;; Author: Daniel Jensen <daniel@bigwalter.net>

;; This file is not part of GNU Emacs.

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

;;; Commentary:
;;; Kommentarer:

;; Norwegian calendar localization.

;; Example usage:
;; (when (require 'calendar-norway nil 'noerror)
;;   ;; Localise date format, weekdays, months, lunar/solar names:
;;   (calendar-norway-common-settings)
;;   (setq calendar-holidays
;; 	(append
;; 	 ;; Include days where you don't have to work:
;; 	 calendar-norway-raude-dagar
;; 	 ;; Include other days that people celebrate:
;; 	 calendar-norway-andre-merkedagar
;; 	 ;; Include daylight savings time:
;; 	 calendar-norway-dst
;; 	 ;; And then you can add some non-Norwegian holidays etc. if you like:
;; 	 '((holiday-fixed 3 17 "St. Patricksdag")
;; 	   (holiday-fixed 10 31 "Hallowe'en")
;; 	   (holiday-float 11 4 4 "Thanksgiving")
;; 	   (solar-equinoxes-solstices)))))

;;; Code:

(defun calendar-norway-common-settings ()
  "Localise dates, weekdays, months, lunar/solar names, etc.
to Norwegian."
  (setq calendar-week-start-day 1) 	; måndag som første dag i veka
  (unless (fboundp 'calendar-set-date-style)
    (require 'calendar))
  (calendar-set-date-style 'european)	; day/month/year
  (setq calendar-date-display-form	; «ons. 2. mai 2012»
	'((if dayname
	      (concat dayname ", "))
	  day ". " monthname " " year))
  (setq calendar-time-display-form	; 24-timars, ingen tidssone
	'(24-hours ":" minutes))
  (setq calendar-day-name-array
	["søndag" "måndag" "tysdag" "onsdag" "torsdag" "fredag" "laurdag"])
  (setq calendar-month-name-array
	["januar" "februar" "mars"     "april"   "mai"      "juni"
	 "juli"    "august"   "september" "oktober" "november" "desember"])
  
  (eval-after-load "solar"
    '(setq solar-n-hemi-seasons
	   '("Vårjamdøgn" "Sommarsolkverv"
	     "Haustjamdøgn" "Vintersolkverv")))

  (defadvice lunar-phase-name (around sv-lunar-phase-name activate)
    "Månfasernas namn på svenska."
    (setq ad-return-value
	  (let ((phase (ad-get-arg 0)))
	    (cond ((= 0 phase) "Nymåne")
		  ((= 1 phase) "Månen i ny")
		  ((= 2 phase) "Fullmåne")
		  ((= 3 phase) "Månen i ne")))))
  
  (defadvice solar-sunrise-sunset-string (around sv-solar-sunrise-sunset-string
						 activate)
    "Soloppgang og solnedgang på norsk."
    (setq ad-return-value
	  (let ((l (solar-sunrise-sunset date)))
	    (format
	     "%s, %s ved %s (%s timar dagslys)"
	     (if (car l)
		 (concat "Sol opp " (apply 'solar-time-string (car l)))
	       "Ingen soloppgang")
	     (if (car (cdr l))
		 (concat "ned " (apply 'solar-time-string (car (cdr l))))
	       "ingen solnedgang")
	     (eval calendar-location-name)
	     (car (cdr (cdr l))))))))

(defun calendar-norway-calculate-easter (year)
  "Calculate the date for Easter in YEAR."
  (let* ((century (1+ (/ year 100)))
         (shifted-epact (% (+ 14 (* 11 (% year 19))
                              (- (/ (* 3 century) 4))
                              (/ (+ 5 (* 8 century)) 25)
                              (* 30 century))
                           30))
         (adjusted-epact (if (or (= shifted-epact 0)
                                 (and (= shifted-epact 1)
                                      (< 10 (% year 19))))
                             (1+ shifted-epact)
                           shifted-epact))
         (paschal-moon (- (calendar-absolute-from-gregorian
                           (list 4 19 year))
                          adjusted-epact)))
    (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

;; Helgdagar
(defvar calendar-norway-raude-dagar
      '((holiday-fixed 1 1 "Nyttårsdag")

	;; Jul
	(holiday-fixed 12 25 "Førstedag jul")
	(holiday-fixed 12 26 "Annandag jul")

        ;; Påske og pinse
        (filter-visible-calendar-holidays
         (mapcar
          (lambda (dag)
            (list (calendar-gregorian-from-absolute
                   (+ (calendar-norway-calculate-easter displayed-year) (car dag)))
                  (cadr dag)))
          '((  -3 "Skjærtorsdag")
	    (  -2 "Langfredag")
            (  -1 "Påskeaftan")
            (   0 "Påskedagen")
            (  +1 "Annandag påske")
            ( +39 "Kristi himmelferdsdag")
            ( +49 "Førstedag pinse")
            ( +50 "Annandag pinse"))))

	(holiday-fixed 5 1 "Internasjonal arbeidardag")
	(holiday-fixed 5 17 "Grunnlovsdagen")

        (let ((midsommar-d (calendar-dayname-on-or-before
                            6 (calendar-absolute-from-gregorian
                               (list 6 26 displayed-year)))))
          ;; Midsommar
          (filter-visible-calendar-holidays
          (list
           (list
            (calendar-gregorian-from-absolute (1- midsommar-d))
            "Midtsommaraftan")
           (list
            (calendar-gregorian-from-absolute midsommar-d)
            "Midtsommardagen")))))
      "Raude kalenderdagar i Noreg.")

(defvar calendar-norway-andre-merkedagar
      '(;; Meir påske
        (filter-visible-calendar-holidays
         (mapcar
          (lambda (dag)
            (list (calendar-gregorian-from-absolute
                   (+ (calendar-norway-calculate-easter displayed-year) (car dag)))
                  (cadr dag)))
	  '(( -3 "Skjærtorsdag"))))

        (holiday-fixed 12 31 "Nyttårsaftan")
	(holiday-fixed 2 14 "Valentinsdag")
	(holiday-fixed 3 8 "Internasjonale kvinnedagen")
	(holiday-fixed 4 1 "Første april")

	(holiday-float 2 0 2 "Morsdag")
	(holiday-float 11 0 2 "Farsdag")

	(holiday-fixed 5 8 "Frigjeringsdagen")

	(holiday-fixed 6 23 "Sankthansaftan (jonsokaftan)")
	(holiday-fixed 7 29 "Olsok")

        (holiday-fixed 10 24 "FN-dagen")

        (holiday-fixed 12 13 "Luciadagen")
	(holiday-fixed 12 24 "Julaftan")
	)
      "Høgtider som ikkje er raude kalenderdagar i Noreg.")

(defvar calendar-norway-dst
  '((if (progn
	  (require 'cal-dst)
	  t)
	(funcall 'holiday-sexp calendar-daylight-savings-starts
		 '(format "Sommartid byrjar %s"
			  (if
			      (fboundp 'atan)
			      (solar-time-string
			       (/ calendar-daylight-savings-starts-time
				  (float 60))
			       calendar-standard-time-zone-name)
			    ""))))
    (funcall 'holiday-sexp calendar-daylight-savings-ends
	     '(format "Vintertid byrjar %s"
		      (if
			  (fboundp 'atan)
			  (solar-time-string
			   (/ calendar-daylight-savings-ends-time
			      (float 60))
			   calendar-daylight-time-zone-name)
			""))))
  "Solar equinoxes and Daylight Saving Time localised to Norwegian")

(provide 'calendar-norway)
