;;
;; ithkuil-mode.el
;; ---------------

;; Author: Michael Dickens <mdickens93@gmail.com>
;; Created: 2020-11-07

;;; Commentary:

;; Major mode for writing the Ithkuil language.
;; Written for morphology version 0.16.


;;; Code:

;;;
;;;
;;; Helper functions.
;;;
;;;

(defun make-regexp (wordlist)
  "Use the strings in the car slots of an alist to produce a
  regexp that matches any of those strings.

  WORDLIST must be an alist where the car slots are strings. The
  cdr slots can be anything."
  (regexp-opt wordlist 'symbols))

(defun alist-multi-get (nested-alist &rest keys)
  (if (null keys)
      nested-alist
    (apply 'alist-multi-get (cdr (assoc (car keys) nested-alist)) (cdr keys))))

;;;
;;;
;;; Keywords.
;;; Needed for parsing and for syntax highlighting.
;;;
;;;

(defvar ithkuil-stem-keywords '("S1" "S2" "S3" "S0"))
(defvar ithkuil-version-keywords '("PRC" "CPT"))
(defvar ithkuil-context-keywords '("EXS" "FNC" "RPS" "AMG"))
(defvar ithkuil-function-keywords '("STA" "DYN"))
(defvar ithkuil-specification-keywords '("BSC" "CTE" "CSV" "OBJ"))

;; This doc is outdated but it's the newest full explanation of the Ca slot
;; https://ithkuil.place/4/archive/2019-12-10-Ca-expansion.pdf
(defvar ithkuil-membership-keywords '("UPX" "MPS" "MPD" "MPF"))
(defvar ithkuil-structure-keywords '("SEP" "CND" "FSD"))
(defvar ithkuil-extension-keywords '("DEL" "PRX" "ICP" "ATV" "GRA" "DEPL"))
(defvar ithkuil-affiliation-keywords '("CSL" "ASO" "COA" "VAR"))
(defvar ithkuil-perspective-keywords '("M" "P" "N" "A"))
(defvar ithkuil-essence-keywords '("NRM" "RPV"))

;;;
;;;
;;; Lookup tables.
;;;
;;;

(defvar ithkuil-slot-Vv-table
  ;; look up stem / version / context.
  ;; NEG/4 shortcut not yet supported
  '(("S1"
     ("PRC" ("EXS" . "a")
             ("FNC" . "ai")
             ("RPS" . "ia")
             ("AMG" . "ao"))
     ("CPT" (("EXS" . "ä")
             ("FNC" . "au")
             ("RPS" . "iä")
             ("AMG" . "ae"))))
    ("S2"
     ("PRC" ("EXS" . "e")
             ("FNC" . "ei")
             ("RPS" . "ie")
             ("AMG" . "ea"))
     ("CPT" ("EXS" . "i")
             ("FNC" . "eu")
             ("RPS" . "ië")
             ("AMG" . "eo")))
    ("S3"
     ("PRC" ("EXS" . "u")
             ("FNC" . "ui")
             ("RPS" . "ua")
             ("AMG" . "oa"))
     ("CPT" ("EXS" . "ü")
             ("FNC" . "iu")
             ("RPS" . "ue")
             ("AMG" . "öa")))
    ("S0"
     ("PRC" ("EXS" . "o")
             ("FNC" . "oi")
             ("RPS" . "uo")
             ("AMG" . "oe"))
     ("CPT" ("EXS" . "ö")
             ("FNC" . "ou")
             ("RPS" . "uö")
             ("AMG" . "öe")))))


(defvar ithkuil-category-ordering
  '((stem . 0) (version . 1) (context . 2)
    (func . 1) (spec . 2)
    (membership . 0) (structure . 1) (extension . 2) (affiliation . 3) (perspective . 4) (essence . 5)))

;;;
;;;
;;; Internal Ithkuil functions.
;;;
;;;

(defun ithkuil-category (word)
  (cond
   ((member word ithkuil-stem-keywords) 'stem)
   ((member word ithkuil-version-keywords) 'version)
   ((member word ithkuil-context-keywords) 'context)
   ((member word ithkuil-function-keywords) 'function)
   ((member word ithkuil-specification-keywords) 'specification)
   ((member word ithkuil-membership-keywords) 'membership)
   ((member word ithkuil-structure-keywords) 'structure)
   ((member word ithkuil-extension-keywords) 'extension)
   ((member word ithkuil-affiliation-keywords) 'affiliation)
   ((member word ithkuil-perspective-keywords) 'perspective)
   ((member word ithkuil-essence-keywords) 'essence)
   (t nil)))

(defun ithkuil-slot-type (word)
  (cond
   ((member word ithkuil-stem-keywords) '(Vv Vr))
   ((member word ithkuil-version-keywords) 'Vv)
   ((member word ithkuil-context-keywords) 'Vv)
   ((member word ithkuil-function-keywords) 'Vr)
   ((member word ithkuil-specification-keywords) 'Vr)
   ((member word ithkuil-membership-keywords) 'Ca)
   ((member word ithkuil-structure-keywords) 'Ca)
   ((member word ithkuil-extension-keywords) 'Ca)
   ((member word ithkuil-affiliation-keywords) 'Ca)
   ((member word ithkuil-perspective-keywords) 'Ca)
   ((member word ithkuil-essence-keywords) 'Ca)
   (t nil)))

(defun ithkuil-beginning-of-slot ()
  "Scan to beginning of slot."
  (while (string-match-p "[a-zA-Z0-3/]" (thing-at-point 'char t))
    (backward-char))
  (forward-char))

(defun ithkuil-end-of-slot ()
  "Scan to end of slot."
  (while (and (not (null (thing-at-point 'char t)))
              (string-match-p "[a-zA-Z0-3/]" (thing-at-point 'char t)))
    (forward-char)))

(defun ithkuil-read-slot-keywords-at-point ()
  (save-excursion
    (ithkuil-beginning-of-slot)
    (loop
     for keyword = (thing-at-point 'word t)
     do (forward-word)
     collect keyword
     while (eq ?/ (char-after))
     do (forward-char))))

(defun ithkuil-sort-keywords (keywords)
  (sort keywords (lambda (x y) (< (alist-get (ithkuil-category x) ithkuil-category-ordering)
                                  (alist-get (ithkuil-category y) ithkuil-category-ordering)))))

;;;
;;;
;;; Major mode functions
;;;
;;;

(defun ithkuil-encode-slot-Vv (stem version context)
  (alist-multi-get ithkuil-slot-Vv-table stem version context))


(defun ithkuil-encode-slot-Vr (stem func spec)
  (message "nah")
  )


(defun ithkuil-encode-slot-Ca (membership structure extension affiliation perspective essence)
  (message (format "%s/%s/%s/%s/%s/%s" membership structure extension affiliation perspective essence))
  )


(defun ithkuil-encode-slot-at-point ()
  (interactive)
  (ithkuil-beginning-of-slot)
  (setq slot-start (point))
  (let* ((slot-keywords (ithkuil-read-slot-keywords-at-point))
         (identifying-keywords (seq-filter (lambda (x) (not (listp (ithkuil-slot-type x)))) slot-keywords))
         (slot-type (ithkuil-slot-type (car identifying-keywords))))

    (dolist (keyword slot-keywords)
      (setq curr-slot-types (ithkuil-slot-type keyword))
      (when (not (listp curr-slot-types))
        (setq curr-slot-types (list curr-slot-types)))
      (when (every (lambda (curr-slot-type) (not (eq slot-type curr-slot-type))) curr-slot-types)
        (error (format "ithkuil-encode-slot-at-point: Keyword %s does not match expected slot-type %s"
                       keyword (upcase (symbol-name slot-type))))))
    (setq encoded
          (apply
           (alist-get slot-type '((Vv . ithkuil-encode-slot-Vv)
                                  (Vr . ithkuil-encode-slot-Vr)
                                  (Ca . ithkuil-encode-slot-Ca)))
           (ithkuil-sort-keywords slot-keywords)))

    (ithkuil-end-of-slot)
    (delete-and-extract-region slot-start (point))
    (insert encoded)))

(defun ithkuil-encode-slot-in-region ()
  (interactive)
  (setq encoded (ithkuil-encode-slot-at-point))
  (kill-region)
  (insert encoded))


(defun ithkuil-encode-formative-at-point ())

;;;
;;;
;;; Syntax highlighting
;;;
;;;

(setq ithkuil-font-lock-keywords
      `(
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
        (,(make-regexp ithkuil-version-keywords) . font-lock-keyword-face)
        ))



(define-derived-mode ithkuil-mode fundamental-mode "ithkuil"
  "major mode for writing text in the Ithkuil language."
  ;; TODO: maybe use text-mode or something as the base mode?

  (setq font-lock-defaults '((ithkuil-font-lock-keywords)))
  )

(add-to-list 'auto-mode-alist '("\\.ith\\'" . ithkuil-mode))

(provide 'ithkuil-mode)

;;; ithkuil-mode.el ends here
