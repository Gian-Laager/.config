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
;; (setq doom-font (font-spec :family "Fira Code"))

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


(with-eval-after-load 'evil-maps
  (define-key evil-insert-state-map (kbd "jj") 'evil-normal-state)
  ;; (define-key evil-visual-state-map (kbd "jj") 'evil-normal-state)
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
