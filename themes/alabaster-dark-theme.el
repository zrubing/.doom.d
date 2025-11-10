;;; alabaster-dark-theme.el -- Alabaster Dark theme for Emacs.
;;; -*- lexical-binding: t -*-

;; Author: Based on Alabaster by Chris Etheridge (theme originally by Nikita Tonsky)
;; URL: https://github.com/tonsky/sublime-scheme-alabaster
;; Package-Version: 20241107.0001
;; Version: 1.0

;;; Commentary:
;;
;; Alabaster Dark is a theme created by Nikita Tonsky.
;; Based on Sublime Text Alabaster Dark scheme: https://github.com/tonsky/sublime-scheme-alabaster
;; Uses minimal highlighting with careful color selection.

(deftheme alabaster-dark
  "Alabaster Dark skin.")

(let ((background-color "#0E1415")      ; Deep dark blue-green (from Sublime)
      (foreground-color "#CECECE")      ; Light grey (from Sublime)
      (selection-color "#293334")       ; Darker blue-green (from Sublime)
      (line-highlight "#1A2526")        ; Line highlight - converted to solid color
      (active-color "#CD974B")          ; Golden yellow (from Sublime)
      (string-color "#95CB82")          ; Soft green (from Sublime)
      (comment-color "#DFDF8E")         ; Yellowish (from Sublime)
      (constant-color "#CC8BC9")        ; Purple (from Sublime)
      (definition-color "#71ADE7")      ; Light blue (from Sublime)
      (punctuation-color "#708B8D")     ; Muted cyan (from Sublime)
      (error-color "#c33")              ; Red (from Sublime)
      (inner-bracket "#8B9B9D"))        ; Semi-transparent punctuation - converted to solid
  
  (custom-theme-set-faces
   'alabaster-dark
   ;; Basics
   `(default ((t (:background ,background-color :foreground ,foreground-color))))
   `(cursor ((t (:background ,active-color))))
   '(bold ((t (:bold t))))
   '(bold-italic ((t (:italic t :bold t))))
   '(italic ((t (:italic t))))
   '(underline ((t (:underline t))))

   ;; Frame
   `(fringe ((t (:background ,background-color))))
   `(mode-line ((t (:background "#1A2526" :foreground ,foreground-color
                                :box (:line-width 1 :color "#2A3536")))))
   `(mode-line-highlight ((t (:box (:line-width 2 :color ,active-color)))))
   `(mode-line-inactive
     ((t (:inherit mode-line :background "#1A2526" :foreground "#708B8D"
                   :box (:line-width 1 :color "#253031") :weight light))))

   ;; Line highlighting
   `(hl-line ((t (:background ,line-highlight))))
   `(highlight ((t (:background ,active-color :foreground "#000"))))
   `(region ((t (:background ,selection-color))))
   `(secondary-selection ((t (:background "#3A4546"))))

   ;; Parentheses
   `(show-paren-match ((t (:foreground ,active-color :underline t))))
   `(show-paren-mismatch ((t (:foreground ,error-color :background "#2B1D1E"))))

   ;; Search
   `(isearch ((t (:background ,active-color :foreground "#000"))))
   `(lazy-highlight ((t (:background "#1F2A2B" :foreground ,active-color))))

   ;; Font Lock - Based on Alabaster Dark scheme
   `(font-lock-comment-face ((t (:foreground ,comment-color))))
   `(font-lock-string-face ((t (:foreground ,string-color))))
   `(font-lock-constant-face ((t (:foreground ,constant-color))))
   `(font-lock-keyword-face ((t (:foreground ,foreground-color)))) ; Keywords unhighlighted
   `(font-lock-function-name-face ((t (:foreground ,definition-color))))
   `(font-lock-variable-name-face ((t (:foreground ,foreground-color)))) ; Variables unhighlighted
   `(font-lock-type-face ((t (:foreground ,foreground-color)))) ; Types unhighlighted
   `(font-lock-builtin-face ((t (:foreground ,foreground-color)))) ; Builtins unhighlighted
   `(font-lock-preprocessor-face ((t (:foreground ,foreground-color))))
   `(font-lock-warning-face ((t (:foreground ,error-color :background "#2B1D1E"))))
   
   ;; Special characters and escapes
   `(font-lock-escape-face ((t (:background "#181F20"))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,active-color))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,active-color))))

   ;; Mode specific
   ;; Minibuffer
   `(minibuffer-prompt ((t (:foreground ,definition-color))))

   ;; Completions
   `(completions-annotations ((t (:foreground ,comment-color))))
   `(completions-first-difference ((t (:foreground ,active-color :bold t))))

   ;; Diffs
   '(diff-added ((t (:foreground "#95CB82"))))
   '(diff-removed ((t (:foreground "#c33"))))
   '(diff-changed ((t (:foreground "#CD974B"))))
   '(diff-file-header ((t (:foreground "#71ADE7" :bold t))))
   '(diff-hunk-header ((t (:foreground "#DFDF8E"))))

   ;; Magit (if available)
   '(magit-diff-add ((t (:foreground "#95CB82"))))
   '(magit-diff-del ((t (:foreground "#c33"))))
   '(magit-diff-hunk-header ((t (:foreground "#DFDF8E"))))
   '(magit-hash ((t (:foreground "#CC8BC9"))))
   '(magit-head ((t (:foreground "#71ADE7"))))
   `(magit-section-highlight ((t (:background ,line-highlight))))

   ;; Error highlighting
   `(error ((t (:foreground ,error-color :background "#2B1D1E"))))
   `(warning ((t (:foreground ,active-color))))
   `(success ((t (:foreground ,string-color))))

   ;; Org mode (basic support)
   `(org-block ((t (:background "#1A2526" :foreground ,foreground-color))))
   `(org-block-begin-line ((t (:foreground ,comment-color))))
   `(org-headline-done ((t (:foreground ,comment-color :strike-through t))))
   `(org-link ((t (:foreground ,definition-color :underline t))))
   `(org-code ((t (:foreground ,string-color))))
   `(org-verbatim ((t (:foreground ,constant-color))))

   ;; Built-in syntax
   `(shadow ((t (:foreground ,comment-color))))
   `(trailing-whitespace ((t (:background ,error-color))))

   ;; Line numbers
   `(line-number ((t (:foreground "#4A5556" :background ,background-color))))
   `(line-number-current-line ((t (:foreground "#6A7576" :background ,line-highlight :bold t))))

   ;; Done
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'alabaster-dark)

;;; alabaster-dark-theme.el ends here
