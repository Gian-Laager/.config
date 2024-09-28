;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gian Laager"
      user-mail-address "gianlaager@outlook.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "FantasqueSansM Nerd Font" :size 15))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(defun nvim-jj-escape ()
  "Escape from evil insert state."
  (interactive)
  (insert "j") ; Insert "j" temporarily because else it looks weird
  (let ((event (read-event nil nil 0.25))) ; Wait for a short time for the next event
    (if (and event (characterp event) (eq event ?j)) ; Check if the next event is "j"
      (progn
        (delete-char -1) ; Delete the temporarily inserted "j"
        (evil-normal-state))
      (progn
        (if event (insert event))))))

(with-eval-after-load 'evil-maps
  (define-key evil-insert-state-map "j" 'nvim-jj-escape)
  (define-key evil-visual-state-map (kbd "s") 'evil-substitute)
  (define-key evil-normal-state-map (kbd "s") 'evil-substitute)
  (define-key evil-normal-state-map (kbd "SPC b a") 'evil-prev-buffer)
  (define-key evil-normal-state-map (kbd "SPC b f") 'evil-next-buffer)
)


;; Make "~" symbol work on german keyboard
(set-input-method 'german-prefix)

(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "M-RET") 'flyspell-correct-word-before-point)
)

(with-eval-after-load 'term
  (define-key term-mode-map (kbd "C-C") nil)
)

(auto-save-mode +1)

(setq doc-view-continuous t)

(require 'ispell)

(add-to-list 'ispell-hunspell-dict-paths-alist '("deutsch-hunspell"
                                              "/usr/share/hunspell/de_CH.aff"))

(add-to-list 'ispell-hunspell-dict-paths-alist '("english-hunspell"
                                              "/usr/share/hunspell/en_US.aff"))

(setq ispell-program-name "hunspell"
      ispell-dictionary "english-hunspell")

(defun switch-dictionary-de-en ()
  "Switch german and english dictionaries."
  (interactive)
  (let* ((dict ispell-current-dictionary)
         (new (if (string= dict "deutsch-hunspell") "english-hunspell"
                "deutsch-hunspell")))
    (ispell-change-dictionary new)
    (message "Switched dictionary from %s to %s" dict new)))

(setq ispell-program-name "hunspell"          ; Use hunspell to correct mistakes
      ispell-dictionary   "english-hunspell") ; Default dictionary to use

(defun switch-dictionary-de-en ()
  "Switch german and english dictionaries."
  (interactive)
  (let* ((dict ispell-current-dictionary)
         (new (if (string= dict "deutsch-hunspell") "english-hunspell"
                   "deutsch-hunspell")))
    (ispell-change-dictionary new)
    (message "Switched dictionary from %s to %s" dict new)))

(setq lsp-rust-analyzer-server-command '("/path/to/rust-analyzer"))
(add-hook 'rustic-mode-hook #'lsp)


(setq-default TeX-command-extra-options "-shell-escape")
;; ;;

;; ;; This elisp code uses use-package, a macro to simplify configuration. It will
;; ;; install it if it's not available, so please edit the following code as
;; ;; appropriate before running it.

;; ;; Note that this file does not define any auto-expanding YaSnippets.

;; ;; Install use-package
;; (package-install 'use-package)

;; ;; AucTeX settings - almost no changes
;; (use-package latex
;;   :ensure auctex
;;   :hook ((LaTeX-mode . prettify-symbols-mode))
;;   :bind (:map LaTeX-mode-map
;;          ("C-S-e" . latex-math-from-calc))
;;   :config
;;   ;; Format math as a Latex string with Calc
;;   (defun latex-math-from-calc ()
;;     "Evaluate `calc' on the contents of line at point."
;;     (interactive)
;;     (cond ((region-active-p)
;;            (let* ((beg (region-beginning))
;;                   (end (region-end))
;;                   (string (buffer-substring-no-properties beg end)))
;;              (kill-region beg end)
;;              (insert (calc-eval `(,string calc-language latex
;;                                           calc-prefer-frac t
;;                                           calc-angle-mode rad)))))
;;           (t (let ((l (thing-at-point 'line)))
;;                (end-of-line 1) (kill-line 0)
;;                (insert (calc-eval `(,l
;;                                     calc-language latex
;;                                     calc-prefer-frac t
;;                                     calc-angle-mode rad))))))))

;; (use-package preview
;;   :after latex
;;   :hook ((LaTeX-mode . preview-larger-previews))
;;   :config
;;   (defun preview-larger-previews ()
;;     (setq preview-scale-function
;;           (lambda () (* 1.25
;;                    (funcall (preview-scale-from-face)))))))

;; ;; CDLatex settings
;; (use-package cdlatex
;;   :ensure t
;;   :hook (LaTeX-mode . turn-on-cdlatex)
;;   :bind (:map cdlatex-mode-map
;;               ("<tab>" . cdlatex-tab)))

;; ;; Yasnippet settings
;; (use-package yasnippet
;;   :ensure t
;;   :hook ((LaTeX-mode . yas-minor-mode)
;;          (post-self-insert . my/yas-try-expanding-auto-snippets))
;;   :config
;;   (use-package warnings
;;     :config
;;     (cl-pushnew '(yasnippet backquote-change)
;;                 warning-suppress-types
;;                 :test 'equal))

;;   (setq yas-triggers-in-field t)

;;   ;; Function that tries to autoexpand YaSnippets
;;   ;; The double quoting is NOT a typo!
;;   (defun my/yas-try-expanding-auto-snippets ()
;;     (when (and (boundp 'yas-minor-mode) yas-minor-mode)
;;       (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
;;         (yas-expand)))))

;; ;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas
;; ;; fields
;; (use-package cdlatex
;;   :hook ((cdlatex-tab . yas-expand)
;;          (cdlatex-tab . cdlatex-in-yas-field))
;;   :config
;;   (use-package yasnippet
;;     :bind (:map yas-keymap
;;            ("<tab>" . yas-next-field-or-cdlatex)
;;            ("TAB" . yas-next-field-or-cdlatex))
;;     :config
;;     (defun cdlatex-in-yas-field ()
;;       ;; Check if we're at the end of the Yas field
;;       (when-let* ((_ (overlayp yas--active-field-overlay))
;;                   (end (overlay-end yas--active-field-overlay)))
;;         (if (>= (point) end)
;;             ;; Call yas-next-field if cdlatex can't expand here
;;             (let ((s (thing-at-point 'sexp)))
;;               (unless (and s (assoc (substring-no-properties s)
;;                                     cdlatex-command-alist-comb))
;;                 (yas-next-field-or-maybe-expand)
;;                 t))
;;           ;; otherwise expand and jump to the correct location
;;           (let (cdlatex-tab-hook minp)
;;             (setq minp
;;                   (min (save-excursion (cdlatex-tab)
;;                                        (point))
;;                        (overlay-end yas--active-field-overlay)))
;;             (goto-char minp) t))))

;;     (defun yas-next-field-or-cdlatex nil
;;       (interactive)
;;       "Jump to the next Yas field correctly with cdlatex active."
;;       (if
;;           (or (bound-and-true-p cdlatex-mode)
;;               (bound-and-true-p org-cdlatex-mode))
;;           (cdlatex-tab)
;;         (yas-next-field-or-maybe-expand)))))

;; ;; Array/tabular input with org-tables and cdlatex
;; (use-package org-table
;;   :after cdlatex
;;   :bind (:map orgtbl-mode-map
;;               ("<tab>" . lazytab-org-table-next-field-maybe)
;;               ("TAB" . lazytab-org-table-next-field-maybe))
;;   :init
;;   (add-hook 'cdlatex-tab-hook 'lazytab-cdlatex-or-orgtbl-next-field 90)
;;   ;; Tabular environments using cdlatex
;;   (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
;;                                        "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil nil t))
;;   (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
;;                                        "\\begin{bmatrix} ? \\end{bmatrix}"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil nil t))
;;   (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
;;                                        "\\begin{pmatrix} ? \\end{pmatrix}"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil nil t))
;;   (add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
;;                                         "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil t nil))
;;   :config
;;   ;; Tab handling in org tables
;;   (defun lazytab-position-cursor-and-edit ()
;;     ;; (if (search-backward "\?" (- (point) 100) t)
;;     ;;     (delete-char 1))
;;     (cdlatex-position-cursor)
;;     (lazytab-orgtbl-edit))

;;   (defun lazytab-orgtbl-edit ()
;;     (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
;;     (orgtbl-mode 1)
;;     (open-line 1)
;;     (insert "\n|"))

;;   (defun lazytab-orgtbl-replace (_)
;;     (interactive "P")
;;     (unless (org-at-table-p) (user-error "Not at a table"))
;;     (let* ((table (org-table-to-lisp))
;;            params
;;            (replacement-table
;;             (if (texmathp)
;;                 (lazytab-orgtbl-to-amsmath table params)
;;               (orgtbl-to-latex table params))))
;;       (kill-region (org-table-begin) (org-table-end))
;;       (open-line 1)
;;       (push-mark)
;;       (insert replacement-table)
;;       (align-regexp (region-beginning) (region-end) "\\([:space:]*\\)& ")
;;       (orgtbl-mode -1)
;;       (advice-remove 'orgtbl-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)))

;;   (defun lazytab-orgtbl-to-amsmath (table params)
;;     (orgtbl-to-generic
;;      table
;;      (org-combine-plists
;;       '(:splice t
;;                 :lstart ""
;;                 :lend " \\\\"
;;                 :sep " & "
;;                 :hline nil
;;                 :llend "")
;;       params)))

;;   (defun lazytab-cdlatex-or-orgtbl-next-field ()
;;     (when (and (bound-and-true-p orgtbl-mode)
;;                (org-table-p)
;;                (looking-at "[[:space:]]*\\(?:|\\|$\\)")
;;                (let ((s (thing-at-point 'sexp)))
;;                  (not (and s (assoc s cdlatex-command-alist-comb)))))
;;       (call-interactively #'org-table-next-field)
;;       t))

;;   (defun lazytab-org-table-next-field-maybe ()
;;     (interactive)
;;     (if (bound-and-true-p cdlatex-mode)
;;         (cdlatex-tab)
;;       (org-table-next-field))))
