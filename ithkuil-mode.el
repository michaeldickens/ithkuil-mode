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

(defvar ithkuil-formative-legal-chars "[a-zA-Z0-3/\\-]")
(defvar ithkuil-slot-legal-chars "[a-zA-Z0-3/]")

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
    (function . 0) (specification . 1)
    (membership . 0) (structure . 1) (extension . 2) (affiliation . 3) (perspective . 4) (essence . 5)))

(defvar ithkuil-legal-slot-transitions
  ;; TODO: add support for incorporated roots and reversed affixes
  '((beginning . (Vv))
    (Vv . (Cr))
    (Cr . (Vr))
    (Vr . (Ca))
    (Ca . (end Vx Vn Vc))
    (Vx . (Cs))
    (Cs . (Vx end Vn Vc))
    (Vn . (Cn))
    (Cn . (end Vc))
    (Vc . (Vk))
    (Vk . (end)))
  "A state machine for formatives,
indicating which slot transitions are permissible when reading
left to right.

BEGINNING and END are special states indicating the beginning/end
of the formative.")

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

(defun ithkuil-slot-type-for-category (category)
  (cond
   ((eq category 'stem) '(Vv Vr))
   ((eq category 'version) 'Vv)
   ((eq category 'context) 'Vv)
   ((eq category 'function) 'Vr)
   ((eq category 'specification) 'Vr)
   ((eq category 'membership) 'Ca)
   ((eq category 'structure) 'Ca)
   ((eq category 'extension) 'Ca)
   ((eq category 'affiliation) 'Ca)
   ((eq category 'perspective) 'Ca)
   ((eq category 'essence) 'Ca)
   (t nil)))

(defun ithkuil-beginning-of-slot ()
  "Scan to beginning of slot."
  (while (string-match-p ithkuil-slot-legal-chars (thing-at-point 'char t))
    (backward-char))
  (forward-char))

(defun ithkuil-end-of-slot ()
  "Scan to end of slot."
  (while (and (not (null (thing-at-point 'char t)))
              (string-match-p ithkuil-slot-legal-chars (thing-at-point 'char t)))
    (forward-char)))

(defun ithkuil-beginning-of-formative ()
  (loop do (ithkuil-beginning-of-slot)
        while (not (eq (point) (point-min)))
        do (backward-char)
        while (string= "-" (thing-at-point 'char t))
        do (backward-char))
  (forward-char))

(defun ithkuil-end-of-formative ()
  ;; TODO: this will behave incorrectly if formative reaches end of buffer
  (loop do (ithkuil-end-of-slot)
        while (not (eq (point) (point-max)))
        while (string= "-" (thing-at-point 'char t))
        do (forward-char))
  (backward-char))

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

(defun ithkuil-slot-at-point ()
  "Return an internal representation of the slot at point.

  A slot is represented as the pair (SLOT-TYPE . VALUE)

  VALUE is a sorted list of keywords or a singleton list containing a root/affix."
  (save-excursion
    (ithkuil-beginning-of-slot)
    (let* ((slot-keywords (ithkuil-read-slot-keywords-at-point))
           (identifying-keywords (seq-filter (lambda (x) (not (listp (ithkuil-slot-type x)))) slot-keywords))
           (slot-type (ithkuil-slot-type (car identifying-keywords))))
      (if (null slot-type)
        ;; TODO: This assumes nil slot-type means it's a root, but it could also be an affix
        `(Cr . ,slot-keywords)

        (dolist (keyword slot-keywords)
          (setq curr-slot-types (ithkuil-slot-type keyword))
          (when (not (listp curr-slot-types))
            (setq curr-slot-types (list curr-slot-types)))
          (when (every (lambda (curr-slot-type) (not (eq slot-type curr-slot-type))) curr-slot-types)
            (error (format "ithkuil-encode-slot-at-point: Keyword %s does not match expected slot-type %s"
                           keyword (upcase (symbol-name slot-type))))))

        `(,slot-type . ,(ithkuil-sort-keywords slot-keywords))))))

(defun ithkuil-update-slot (slot category new-value)
  "SLOT: A slot data type containing the slot type and value
  CATEGORY: A symbol representing the category to update (version, context, specification, etc.)
  NEW-VALUE: The new value to insert into the slot.

  Return a new slot with the indicated value replaced."
  (setq category-slot-type (ithkuil-slot-type-for-category category))
  (when (not (if (listp category-slot-type)
                 (member (car slot) category-slot-type)
               (eq (car slot) category-slot-type)))
    (error (format "ithkuil-update-slot: Category %s does not exist in slot type %s (requires %s)"
                   (upcase (symbol-name category)) (car slot) category-slot-type)))

  (let ((new-slot `(,(car slot) . ,(apply 'list (cdr slot))))
        (index (cdr (assoc category ithkuil-category-ordering))))
    (setcar (nthcdr index (cdr new-slot)) new-value)
    new-slot))

(defun ithkuil-delete-slot ()
  "Delete slot at point."
  ;; TODO: if a slot is not at point, idk what this will do
  (ithkuil-beginning-of-slot)
  (setq slot-start (point))
  (ithkuil-end-of-slot)
  (delete-and-extract-region slot-start (point)))

(defun ithkuil-insert-slot (slot)
  "Insert SLOT at point."
  ;; TODO: only supports keyword-based slots, not roots or affixes
  (insert (string-join (cdr slot) "/")))

(defun ithkuil-delete-formative ()
  "Delete formative at point."
  (ithkuil-beginning-of-formative)
  (setq start (point))
  (ithkuil-end-of-formative)
  (forward-char)
  (delete-and-extract-region start (point)))

(defun ithkuil-insert-formative (formative)
  "Insert FORMATIVE at point."
  (dolist (slot formative)
    (ithkuil-insert-slot slot)
    (insert "-"))
  (when (not (null formative))
    (delete-char -1)))

(defun ithkuil-set-keyword-for-slot (slot category candidates)
  "Set the keyword"
  ;; TODO: currently only supports setting keyword if the correct slot is at point
  (setq slot-type (car slot))
  (setq category-slot-type (ithkuil-slot-type-for-category category))
  (when (not (eq slot-type category-slot-type))
    (error (format "ithkuil-set-%s: Slot type at point must be %s, not %s" keyword-symbol category-slot-type (symbol-name slot-type))))
  (setq new-value
        (helm :sources
              (helm-build-sync-source (format "ithkuil-%s" category)
                :candidates candidates
                :fuzzy-match t
                )))
  (ithkuil-delete-slot)
  (ithkuil-insert-slot (ithkuil-update-slot slot category new-value)))

(defun ithkuil-set-keyword-generic (category candidates)
  "Set the keyword"
  (save-excursion
    (setq formative (ithkuil-formative-at-point))
    (setq category-slot-type (ithkuil-slot-type-for-category category))
    (setq slot (assoc category-slot-type formative))
    (setq new-value
          (helm :sources
                (helm-build-sync-source (format "ithkuil-%s" category)
                  :candidates candidates
                  :fuzzy-match t
                  )))
    (when (not (null new-value))  ; new-value is nil if user aborts
      (setq new-slot (ithkuil-update-slot slot category new-value))
      (message (format "new slot: %s" new-slot))
      (setcdr (assoc category-slot-type formative) (cdr new-slot))
      (ithkuil-delete-formative)
      (ithkuil-insert-formative formative))))

(defun ithkuil-formative-at-point ()
  "Read formative at point. Return an alist of slots."
  (save-excursion
    ;; TODO: this breaks if the formative reaches end of buffer
    (ithkuil-beginning-of-formative)
    (setq slots-alist
          (loop for slot = (ithkuil-slot-at-point)
                collect slot
                do (ithkuil-end-of-slot)
                while (string= "-" (thing-at-point 'char t))
                do (forward-char)))

    (setq slot-types (mapcar #'car slots-alist))
    (let ((prev-slot-type 'beginning))
      (dolist (slot-type (mapcar #'car slots-alist))
        (when (not (member slot-type (alist-get prev-slot-type ithkuil-legal-slot-transitions)))
          (error (format "Syntax error: Slot type %s cannot be followed by slot type %s (expected %s)" prev-slot-type slot-type (alist-get slot-type ithkuil-legal-slot-transitions))))
        (setq prev-slot-type slot-type)))
    slots-alist))

;;;
;;;
;;; Major mode functions
;;;
;;;

(defun ithkuil-encode-slot-Vv (stem version context)
  "Encode slot Vv from long form into Ithkuil."
  (alist-multi-get ithkuil-slot-Vv-table stem version context))


(defun ithkuil-encode-slot-Vr (stem func spec)
  (message "TODO")
  )


(defun ithkuil-encode-slot-Ca (membership structure extension affiliation perspective essence)
  (message (format "%s/%s/%s/%s/%s/%s" membership structure extension affiliation perspective essence))
  )

(defun ithkuil-encode-slot-at-point ()
  (interactive)
  (setq slot (ithkuil-slot-at-point))
  (setq slot-type (car slot))
  (setq slot-keywords (cdr slot))
  (setq encoded
      (apply
        (alist-get slot-type '((Vv . ithkuil-encode-slot-Vv)
                               (Vr . ithkuil-encode-slot-Vr)
                               (Ca . ithkuil-encode-slot-Ca)))
        slot-keywords))

  (ithkuil-delete-slot)
  (insert encoded))

(defun ithkuil-encode-slot-in-region ()
  (interactive)
  (setq encoded (ithkuil-encode-slot-at-point))
  (kill-region)
  (insert encoded))

(defun ithkuil-encode-formative-at-point ()
  (interactive)
  )

(defun ithkuil-set-stem ()
  "Set stem for the formative at point."
  (interactive)
  (ithkuil-set-keyword-generic
   'stem
   '(("Stem 1 (S1)" . "S1")
     ("Stem 2 (S2)" . "S2")
     ("Stem 3 (S3)" . "S3")
     ("Stem 4 (S4)" . "S4"))))

(defun ithkuil-set-version ()
  "Set version for the formative at point."
  (interactive)
  (ithkuil-set-keyword-generic
   'version
   '(("Processural (PRC): non-goal-oriented" . "PRC")
     ("Completive (CPT): goal-oriented" . "CPT"))))

(defun ithkuil-set-context ()
  "Set context for the formative at point."
  (interactive)
  (ithkuil-set-keyword-generic
   'context
   '(("Existential (EXS): ontologically objective" . "EXS")
     ("Functional (FNC): defined socially by ideas, attitudes, beliefs, opinions, culture, use" . "FNC")
     ("Representational (RPS): as symbol, metaphor, or metonym" . "RPS")
     ("Amalgamative (AMG): systemic, gestalt-like nature, derived from interrelationship of all parts" . "AMG"))))

(defun ithkuil-set-function ()
  "Set function for the formative at point."
  (interactive)
  (ithkuil-set-keyword-generic
   'function
   '(("Stative (STA): Static, unchanging entity" . "STA")
     ("Dynamic (DYN): Action/change or state involving change" . "DYN"))))

(defun ithkuil-set-specification ()
  "Set specification for the formative at point."
  (interactive)
  (ithkuil-set-keyword-generic
   'specification
   '(("Basic (BSC): Holistic instantiation of a root; an instance/occurrence" . "BSC")
      ("Contential (CTE): Content or essence or idealized/platonic form" . "CTE")
      ("Constitutive (CSV): The form in which an entity/state/act actually expresses itself" . "CSV")
      ("Objective (OBJ): Means by which root occurs, or object/entity associated with an interaction, or tangible outcome, or experiencer of a state/act" . "OBJ"))
     ))

(defun ithkuil-set-membership ()
  "Set membership for the formative at point."
  (interactive)
  (ithkuil-set-keyword-generic
   'membership
   '(("Uniplex (UNI)" . "UNI")
     ("Multiplex - Similar (MPX)" . "MPX")
     ("Multiplex - Dissimilar (MPD)" . "MPD")
     ("Multiplex - Fuzzy (MPF)" . "MPF"))))

(defun ithkuil-set-structure ()
  "Set structure for the formative at point."
  (interactive)
  (ithkuil-set-keyword-generic
   'structure
   '(("Separate (SEP)" . "SEP")
     ("Connected (CND)" . "CND")
     ("Fused (FSD)" . "FSD"))))

(defun ithkuil-set-extension ()
  "Set extension for the formative at point."
  (interactive)
  (ithkuil-set-keyword-generic
   'extension
   '(("Delimitive (DEL)" . "DEL")
     ("Proximal (PRX)" . "PRX")
     ("Incipient (ICP)" . "ICP")
     ("Attenuative (ATV)" . "ATV")
     ("Graduative (GRA)" . "GRA")
     ("Depletive (DPL)" . "DPL"))))

(defun ithkuil-set-affiliation ()
  "Set affiliation for the formative at point."
  (interactive)
  (ithkuil-set-keyword-generic
   'affiliation
   '(("Consolidative (CSL): Function/state is inapplicable or irrelevant" . "CSL")
     ("Associative (ASO): Members of set share a function/state" . "ASO")
     ("Coalescent (COA): Members of set have different but complementary functions/states" . "COA")
     ("Variative (VAR): Members of set have varying functions/states or are at odds" . "VAR"))))

(defun ithkuil-set-perspective ()
  "Set perspective for the formative at point."
  (interactive)
  (ithkuil-set-keyword-generic
   'perspective
   '(("Monadic (M): Bounded embodiment or single contextual entity" . "M")
     ("Polyadic (P)" . "P")
     ("Nomic (N): An archetype, containing all conceivable members of a set" . "N")
     ("Abstract (A): The concept of a thing, or temporal abstraction; like English -hood/-ness" . "A")
     )))

(defun ithkuil-set-essence ()
  "Set essence for the formative at point."
  (interactive)
  (ithkuil-set-keyword-generic
   'essence
   '(("Normal (NRM)" . "NRM")
     ("Representative (RPV)" . "RPV"))))

(defun ithkuil-set-category-at-point ()
  "Set the value of the category at point, prompting with a list of options."
  (interactive)
  (setq word (thing-at-point 'word t))
  (setq category (ithkuil-category word))
  (when (null category)
    (error (format "Unknown category for \"%s\"" word)))
  (funcall (alist-get category
                      '((stem . ithkuil-set-stem)
                        (version . ithkuil-set-version)
                        (context . ithkuil-set-context)
                        (function . ithkuil-set-function)
                        (specification . ithkuil-set-specification)
                        (membership . ithkuil-set-membership)
                        (structure . ithkuil-set-structure)
                        (extension . ithkuil-set-extension)
                        (affiliation . ithkuil-set-affiliation)
                        (perspective . ithkuil-set-perspective)
                        (essence . ithkuil-set-essence)))))



(defun ithkuil-insert-template ()
  (interactive)
  "Insert a template formative at point. This template can then
  be modified using the various setter functions."
  (insert "S1/PRC/EXS-exampleroot-DYN/BSC-UPX/SEP/DEL/CSL/M/NRM")
  )

;;;
;;;
;;; Syntax highlighting
;;;
;;;

(setq ithkuil-font-lock-keywords `(
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
        (,(make-regexp
           (append ithkuil-version-keywords
                   ithkuil-version-keywords
                   ithkuil-context-keywords
                   ithkuil-function-keywords
                   ithkuil-specification-keywords
                   ithkuil-membership-keywords
                   ithkuil-structure-keywords
                   ithkuil-extension-keywords
                   ithkuil-affiliation-keywords
                   ithkuil-perspective-keywords
                   ithkuil-essence-keywords))
           . font-lock-keyword-face)))



(define-derived-mode ithkuil-mode fundamental-mode "ithkuil"
  "major mode for writing text in the Ithkuil language."
  ;; TODO: maybe use text-mode or something as the base mode?

  (setq font-lock-defaults '((ithkuil-font-lock-keywords))))

(add-to-list 'auto-mode-alist '("\\.ith\\'" . ithkuil-mode))

(define-key ithkuil-mode-map (kbd "C-c C-i") 'ithkuil-insert-template)
(define-key ithkuil-mode-map (kbd "C-c C-t") 'ithkuil-set-category-at-point)

(define-key ithkuil-mode-map (kbd "C-c C-s s") 'ithkuil-set-stem)
(define-key ithkuil-mode-map (kbd "C-c C-s v") 'ithkuil-set-version)
(define-key ithkuil-mode-map (kbd "C-c C-s c") 'ithkuil-set-context)
(define-key ithkuil-mode-map (kbd "C-c C-s f") 'ithkuil-set-function)
(define-key ithkuil-mode-map (kbd "C-c C-s p") 'ithkuil-set-specification)
(define-key ithkuil-mode-map (kbd "C-c C-s m") 'ithkuil-set-membership)
(define-key ithkuil-mode-map (kbd "C-c C-s t") 'ithkuil-set-structure)
(define-key ithkuil-mode-map (kbd "C-c C-s e") 'ithkuil-set-extension)
(define-key ithkuil-mode-map (kbd "C-c C-s a") 'ithkuil-set-affiliation)
(define-key ithkuil-mode-map (kbd "C-c C-s p") 'ithkuil-set-perspective)
(define-key ithkuil-mode-map (kbd "C-c C-s n") 'ithkuil-set-essence)

(provide 'ithkuil-mode)

;;; ithkuil-mode.el ends here
