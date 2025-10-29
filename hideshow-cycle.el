;;; hideshow-cycle.el --- Cycle code folding with hideshow -*- lexical-binding: t; -*-
;;
;; Author: Andreas Jonsson <ajdev8@gmail.com>
;; Maintainer: Andreas Jonsson <ajdev8@gmail.com>
;; URL: https://github.com/sonofjon/hideshow-cycle.el
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience
;;
;;; Commentary:
;;
;; Utilities for cycling code folding with hideshow, progressively revealing
;; deeper levels.
;;
;; This package provides commands to cycle through code folding levels both
;; locally (current block) and globally (entire buffer).  Each invocation
;; reveals one more nested level up to a configurable maximum depth.  Once
;; the maximum is reached, the next invocation fully expands the
;; block/buffer.  If fully visible, the next invocation hides it entirely.
;;
;; Inspired by karthink's blog post "Simple folding with Hideshow":
;; https://karthinks.com/software/simple-folding-with-hideshow/
;;
;; Provides:
;;  - hs-cycle
;;  - hs-cycle-global
;;
;; Usage:
;;
;;   (use-package hideshow-cycle
;;     :commands (hs-cycle hs-cycle-global)
;;     :custom
;;     (hs-cycle-max-depth 3)
;;     :bind
;;     (:map hs-minor-mode-map
;;           ("TAB" . hs-cycle)
;;           ("<backtab>" . hs-cycle-global)))
;;
;;; Code:

(require 'hideshow)

;;; Customization

(defgroup hs-cycle nil
  "Cycle code folding with hideshow."
  :group 'hideshow)

(defcustom hs-cycle-max-depth 3
  "The maximum depth level to reveal with `hs-cycle'.
If nil, cycle through all levels."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'hs-cycle)

;;; Internal variables

(defvar hs-cycle--depth nil
  "Current depth level for `hs-cycle'.")

(defvar hs-cycle--global-depth nil
  "Current depth level for `hs-cycle-global'.
Tracks the current level of code folding globally.")

;;; Helper functions

(defun hs-cycle--suppress-messages (orig-fun &rest args)
  "Suppress messages during call to function ORIG-FUN.
ARGS are the arguments to be passed to ORIG-FUN."
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply orig-fun args)))

(defun hs-cycle--add-suppress-messages-advice (functions)
  "Add advice to suppress messages for the provided FUNCTIONS.
FUNCTIONS can be a symbol or a list of symbols representing function
names."
  (let ((func-list (if (listp functions) functions (list functions))))
    (dolist (fn func-list)
      (advice-add fn :around #'hs-cycle--suppress-messages))))

(defun hs-cycle--remove-suppress-messages-advice (functions)
  "Remove advice that suppresses messages for the provided FUNCTIONS.
FUNCTIONS can be a symbol or a list of symbols representing function
names."
  (let ((func-list (if (listp functions) functions (list functions))))
    (dolist (fn func-list)
      (advice-remove fn #'hs-cycle--suppress-messages))))

(defun hs-cycle--count-levels ()
  "Return the number of nested levels within the current block."
  (save-excursion
    (let ((level 0)
          (current-pos (point))
          (end-pos (progn
                     (when (hs-find-block-beginning)
                       (hs-forward-sexp (match-data t) 1))
                     (point))))
      (goto-char current-pos)
      (while (hs-find-next-block hs-block-start-regexp end-pos nil)
        (let ((start (point)))
          (hs-forward-sexp (match-data t) 1)
          (let ((block-end (point)))
            (when (> block-end start)
              (goto-char start)
              (setq level (max level (1+ (hs-cycle--count-levels))))
              (goto-char block-end)))))
      level)))

(defun hs-cycle--already-hidden-any-p ()
  "Return non-nil if any sub-level within the current block is hidden."
  (save-excursion
    (let ((current-pos (point))
          (end-pos (progn
                     (when (hs-find-block-beginning)
                       (hs-forward-sexp (match-data t) 1))
                     (point)))
          hidden-found)
      (goto-char current-pos)
      (while (and (not hidden-found)
                  (hs-find-next-block hs-block-start-regexp end-pos nil))
        (when (hs-already-hidden-p)
          (setq hidden-found t)))
      hidden-found)))

;;; Public functions

;;;###autoload
(defun hs-cycle (&optional arg)
  "Cycle code folding, progressively revealing deeper levels.
Each invocation reveals one more nested level up to
`hs-cycle-max-depth'.  Once the maximum depth is reached, fully expand
the block on the next call.  If the block is fully visible, hide it
entirely.

With prefix argument ARG, reverse the cycling direction.  With double
prefix argument (C-u C-u), toggle between fully hidden and fully shown."
  (interactive "P")
  (let ((hs-functions '(hs-hide-block hs-show-block hs-hide-level))
        (max-depth (or hs-cycle-max-depth (hs-cycle--count-levels)))
        (reverse-p (equal arg '(4)))
        (toggle-p (equal arg '(16))))
    (hs-cycle--add-suppress-messages-advice hs-functions)
    (unwind-protect
        (save-excursion
          (cond
           ;; Double prefix: toggle between fully hidden and fully shown
           (toggle-p
            (if (hs-cycle--already-hidden-any-p)
                (progn
                  (hs-show-block)
                  (setq hs-cycle--depth nil)
                  (message "hs-cycle: fully shown"))
              (hs-hide-block)
              (setq hs-cycle--depth 0)
              (message "hs-cycle: fully hidden")))
           ;; Single prefix: reverse direction
           (reverse-p
            (cond
             ;; Currently hidden level
             ((hs-cycle--already-hidden-any-p)
              (cond
               ;; Not at min depth: decrease depth
               ((and hs-cycle--depth (> hs-cycle--depth 0))
                (setq hs-cycle--depth (1- hs-cycle--depth))
                (if (= hs-cycle--depth 0)
                    (progn
                      (hs-hide-block)
                      (message "hs-cycle depth: 0"))
                  (hs-hide-level hs-cycle--depth)
                  (message "hs-cycle depth: %s" hs-cycle--depth)))
               ;; At min depth: show entire block
               (t
                (hs-show-block)
                (setq hs-cycle--depth nil)
                (message "hs-cycle depth: all"))))
             ;; Currently no hidden level: hide from max depth
             (t
              (setq hs-cycle--depth max-depth)
              (hs-hide-level hs-cycle--depth)
              (message "hs-cycle depth: %s" hs-cycle--depth))))
           ;; No prefix: normal forward cycling
           (t
            (cond
             ;; Currently hidden level
             ((hs-cycle--already-hidden-any-p)
              (cond
               ;; Not at max depth: increase depth
               ((or (not hs-cycle--depth)
                    (< hs-cycle--depth max-depth))
                (setq hs-cycle--depth (if hs-cycle--depth
                                         (1+ hs-cycle--depth)
                                       1))
                (hs-hide-level hs-cycle--depth)
                (message "hs-cycle depth: %s" hs-cycle--depth))
               ;; At max depth: show entire block
               (t
                (hs-show-block)
                (setq this-command nil)
                (setq hs-cycle--depth nil)
                (message "hs-cycle depth: all"))))
             ;; Currently no hidden level: hide entire block
             (t
              (hs-hide-block)
              (setq hs-cycle--depth 0)
              (message "hs-cycle depth: 0"))))))
      (hs-cycle--remove-suppress-messages-advice hs-functions))))

;;;###autoload
(defun hs-cycle-global (&optional arg)
  "Cycle code folding globally, progressively revealing deeper levels.
Each invocation reveals one more nested level up to
`hs-cycle-max-depth'.  Once the maximum depth is reached, fully expand
all blocks on the next call.  If all blocks are fully visible, hide them
entirely.

With prefix argument ARG, reverse the cycling direction.  With double
prefix argument (C-u C-u), toggle between fully hidden and fully shown."
  (interactive "P")
  (let ((hs-functions '(hs-hide-all hs-show-all hs-hide-level))
        (max-depth (or hs-cycle-max-depth 3))
        (reverse-p (equal arg '(4)))
        (toggle-p (equal arg '(16))))
    (hs-cycle--add-suppress-messages-advice hs-functions)
    (unwind-protect
        (save-excursion
          (cond
           ;; Double prefix: toggle between fully hidden and fully shown
           (toggle-p
            (if hs-cycle--global-depth
                (progn
                  (hs-show-all)
                  (setq hs-cycle--global-depth nil)
                  (message "Global hs-cycle: fully shown"))
              (hs-hide-all)
              (setq hs-cycle--global-depth 0)
              (message "Global hs-cycle: fully hidden")))
           ;; Single prefix: reverse direction
           (reverse-p
            (cond
             ;; Currently hidden level
             (hs-cycle--global-depth
              (cond
               ;; Not at min depth: decrease depth
               ((> hs-cycle--global-depth 0)
                (setq hs-cycle--global-depth (1- hs-cycle--global-depth))
                (if (= hs-cycle--global-depth 0)
                    (progn
                      (hs-hide-all)
                      (message "Global hs-cycle depth: 0"))
                  (save-excursion
                    (goto-char (point-min))
                    (hs-hide-level hs-cycle--global-depth))
                  (message "Global hs-cycle depth: %s"
                           hs-cycle--global-depth)))
               ;; At min depth: show all
               (t
                (hs-show-all)
                (setq hs-cycle--global-depth nil)
                (message "Global hs-cycle depth: all"))))
             ;; Currently no hidden level: hide from max depth
             (t
              (setq hs-cycle--global-depth max-depth)
              (save-excursion
                (goto-char (point-min))
                (hs-hide-level hs-cycle--global-depth))
              (message "Global hs-cycle depth: %s"
                       hs-cycle--global-depth))))
           ;; No prefix: normal forward cycling
           (t
            (cond
             ;; Currently hidden level
             (hs-cycle--global-depth
              (cond
               ;; Not at max depth: increase depth
               ((< hs-cycle--global-depth max-depth)
                (setq hs-cycle--global-depth
                      (1+ hs-cycle--global-depth))
                (save-excursion
                  (goto-char (point-min))
                  (hs-hide-level hs-cycle--global-depth))
                (message "Global hs-cycle depth: %s"
                         hs-cycle--global-depth))
               ;; At max depth: show all blocks
               (t
                (hs-show-all)
                (setq this-command nil)
                (setq hs-cycle--global-depth nil)
                (message "Global hs-cycle depth: all"))))
             ;; Currently no hidden level: hide all blocks
             (t
              (hs-hide-all)
              (setq hs-cycle--global-depth 0)
              (message "Global hs-cycle depth: 0"))))))
      (hs-cycle--remove-suppress-messages-advice hs-functions))))

(provide 'hideshow-cycle)
;;; hideshow-cycle.el ends here
