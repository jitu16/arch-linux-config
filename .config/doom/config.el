;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; --- IDENTITY ---
(setq user-full-name "Zahidul Islam Jitu"
      user-mail-address "jitumstock@gmail.com")
;; --- FONTS ---
;; We explicitly set this to the Nerd Font we installed on Arch
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))

;; --- THEME ---
(setq doom-theme 'doom-one) ;; Or 'doom-dracula' if you liked the dark purple one

;; --- UI SETTINGS ---
;; --- FORCE LINE NUMBERS EVERYWHERE ---
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(add-hook! 'special-mode-hook #'display-line-numbers-mode)
(remove-hook! '(text-mode-hook
                conf-mode-hook
                special-mode-hook)
              #'doom-disable-line-numbers-h)

(add-to-list 'default-frame-alist '(undecorated . t)) ;; No title bars (Perfect for Sway)
(add-to-list 'default-frame-alist '(alpha . 90))      ;; Transparency (The Physics HUD look)
;; Automatically open .ipynb files in EIN mode
(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . ein:notebook-mode))
;; Make treemacs slightly wider for long LaTeX file names
(setq treemacs-width 35)

;; --- ORG MODE ---
(setq org-directory "/home/code/work/notes/org-files/")


(setq org-agenda-files (directory-files-recursively "/home/code/work/notes/org-files/" "\\.org$"))

;; --- INPUT HABITS ---
(setq evil-escape-key-sequence "jk")
(setq evil-escape-delay 0.2)

;; Remap standard C/C++ modes to their Tree-sitter equivalents
(after! cc-mode
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

;; --- LATEX CONFIGURATION (Zathura Edition) ---
(after! latex
  (setq TeX-fold-unfold-around-mark t)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (map! :map LaTeX-mode-map :n "g d" #'reftex-view-crossref)

  ;; Use Zathura for viewing PDFs
  (setq TeX-view-program-selection '((output-pdf "Zathura"))))

;; Disable annoying chktex warning
(after! flycheck
  (add-to-list 'flycheck-disabled-checkers 'tex-chktex))

;; Babel mode for org file
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)))

(add-hook 'org-mode-hook #'turn-on-org-cdlatex)

;; Force Org-mode to open PDFs specifically with Zathura
(after! org
  ;; Make cdlatex the highest priority for TAB in Org mode
  (add-hook 'org-tab-first-hook #'org-try-cdlatex-tab)
  ;; The '%s' is a placeholder for the file path
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s")))

;; --- HISTORY OPTIMIZATIONS ---
(after! savehist
  (setq savehist-additional-variables
        (remove 'kill-ring savehist-additional-variables))
  ;; Keep your M-x command history safe and plenty!
  (setq history-length 1000)      ;; Keep 1000 commands
  (setq history-delete-duplicates t))

;; elfeed config
(after! elfeed
  (setq elfeed-search-filter "@3-days-ago +unread")
  (elfeed-org)
  (run-at-time nil (* 8 60 60) #'elfeed-update)
  (add-hook 'elfeed-show-mode-hook #'visual-line-mode))

;; Don't spellcheck LaTeX macros
(setq ispell-skip-html t)
(add-hook 'latex-mode-hook 'flyspell-mode)

;; --- CUSTOM FUNCTIONS ---
(defun +jitu/copy-flycheck-errors-with-code ()
  "Copy error messages AND the offending line of code to clipboard."
  (interactive)
  (if flycheck-current-errors
      (let ((errors (mapconcat
                     (lambda (err)
                       (let* ((line-no (flycheck-error-line err))
                              (level (flycheck-error-level err))
                              (msg (flycheck-error-message err))
                              ;; Jump to line and grab text without moving cursor permanently
                              (code-line (save-excursion
                                           (goto-char (point-min))
                                           (forward-line (1- line-no))
                                           (string-trim (thing-at-point 'line t)))))
                         ;; Format: Line X (Error): Message \n > Code
                         (format "Line %d (%s): %s\n> %s"
                                 line-no level msg code-line)))
                     flycheck-current-errors
                     "\n\n"))) ;; Double spacing between errors for readability
        (kill-new errors)      ;; Copy to Emacs kill ring
        (gui-select-text errors) ;; Copy to System Clipboard
        (message "Copied %d errors (with code) to clipboard!" (length flycheck-current-errors)))
    (message "No errors to copy!")))

(defun +jitu/yank-to-other-window-and-switch ()
  "Yank current selection, paste in next window, and switch focus."
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring (region-beginning) (region-end))
                (thing-at-point 'line t))))
    (other-window 1)
    (insert text)
    (message "Zapped text to other window!")))

;; Define a function to reopen the current file as root
(defun +jitu/sudo-this-file ()
  "Open the current file as root using TRAMP."
  (interactive)
  (let ((pos (point)))
    (find-alternate-file (concat "/sudo::" (buffer-file-name)))
    (goto-char pos)))


;; --- KEYBINDINGS ---
;; Note: On Linux, 'Alt' is 'Meta' by default. We don't need the "ns-command-modifier" hacks.

;; Map Space+c+i to cycle string case (camelCase -> snake_case)
(map! :leader
      :prefix ("c" . "code")
      :desc "Cycle String Case" "i" #'string-inflection-all-cycle)

;; Dired Tab Toggle
(map! :after dired
      :map dired-mode-map
      "<tab>" #'dired-subtree-toggle
      "TAB"   #'dired-subtree-toggle)

;;Break Point for GDB
(map! :leader
      :prefix ("d" . "debug")
      :desc "GUD Breakpoint" "b" #'gud-break)

;; --- CUSTOM PROJECT BOOKMARKS ---
(defun +jitu/project-bookmarks ()
  "Filter bookmarks by project and jump to one."
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)

  (let ((root (doom-project-root)))
    (if (not root)
        (message "Not in a project!")

      (let* ((candidates
              (cl-remove-if-not
               (lambda (bm)
                 (let ((file (bookmark-get-filename bm)))
                   (and file (string-prefix-p root (expand-file-name file)))))
               bookmark-alist))

             (names (mapcar #'car candidates)))

        (if names
            (bookmark-jump (completing-read "Project Bookmarks: " names nil t))
          (message "No bookmarks found in this project."))))))

;; Global keybinding: M-t toggles the Vterm popup
(map! :g "M-t" #'+vterm/toggle)

;; Ensure M-t works INSIDE vterm to close it (prevents typing 't')
(after! vterm
  (map! :map vterm-mode-map
        "M-t" #'+vterm/toggle))

;; Drag Lines (Alt+j / Alt+k)
(use-package! drag-stuff
  :config
  (map! :nv "M-j" #'drag-stuff-down
        :nv "M-k" #'drag-stuff-up
       :nv "M-h" #'drag-stuff-left 
       :nv "M-l" #'drag-stuff-right))

;; CDLaTeX Tab fix
(after! cdlatex
  (map! :map cdlatex-mode-map
        :i "TAB" #'cdlatex-tab
        :i "<tab>" #'cdlatex-tab))

;; Bind SPC c g to copying the whole error buffer with the code line
(map! :leader
      :desc "Copy errors + code" "c g" #'+jitu/copy-flycheck-errors-with-code)

(map! :nv "z p" #'+jitu/yank-to-other-window-and-switch)

;; Bind SPC f s to enter sudo mode for a file
(map! :leader
      (:prefix-map ("f" . "file")
       :desc "Sudo reopen file" "s" #'+jitu/sudo-this-file))

;; Local project bookmark
(map! :leader
      (:prefix "b"
       :desc "List Project Bookmarks" "L" #'+jitu/project-bookmarks))
