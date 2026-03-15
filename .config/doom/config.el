;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; --- IDENTITY ---
(setq user-full-name "Zahidul Islam Jitu"
      user-mail-address "jitumstock@gmail.com")

;; --- FONTS ---
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))

;; --- THEME ---
(setq doom-theme 'doom-one)

;; --- UI SETTINGS ---
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(add-hook! 'special-mode-hook #'display-line-numbers-mode)
(remove-hook! '(text-mode-hook
                conf-mode-hook
                special-mode-hook)
              #'doom-disable-line-numbers-h)

(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(alpha . 90))

(add-to-list 'auto-mode-alist '("\\.kbd\\'" . lisp-mode))

(setq treemacs-width 35)


;; --- Platform Specific Code ---
(if (eq system-type 'darwin)
    (progn
      (setq org-directory "/Users/zjitu/work/notes/org-files/")
      (setq org-agenda-files (directory-files-recursively "/Users/zjitu/work/notes/org-files/" "\\.org$"))
      (after! latex (setq TeX-view-program-selection '((output-pdf "open"))))
      (after! org   (add-to-list 'org-file-apps '("\\.pdf\\'" . "open %s"))))
  (progn
    (setq org-directory "/home/code/work/notes/org-files/")
    (setq org-agenda-files (directory-files-recursively "/home/code/work/notes/org-files/" "\\.org$"))
    (after! latex (setq TeX-view-program-selection '((output-pdf "Zathura"))))
    (after! org   (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s")))))

;; --- INPUT HABITS ---
(setq evil-escape-key-sequence "jk")
(setq evil-escape-delay 0.2)
(setq org-latex-compiler "xelatex")

(require 'ox-latex)

;; --- LATEX CONFIGURATION (Zathura Edition) ---
(after! latex
  (setq TeX-fold-unfold-around-mark t)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (map! :map LaTeX-mode-map :n "g d" #'reftex-view-crossref))

;; Config for Beamer
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass[presentation]{beamer}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(with-eval-after-load 'org
  (add-to-list 'org-preview-latex-process-alist
               '(xelatex-dvisvgm
                 :programs ("xelatex" "dvisvgm")
                 :description "xdv > svg"
                 :message "you need xelatex and dvisvgm installed"
                 :image-input-type "xdv"
                 :image-output-type "svg"
                 :image-size-adjust (1.7 . 1.5)
                 :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                 :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O")))
  (setq org-preview-latex-default-process 'xelatex-dvisvgm))

(after! cdlatex
  (map! :map cdlatex-mode-map
        :i "TAB" #'cdlatex-tab
        :i "<tab>" #'cdlatex-tab)

  (add-to-list 'cdlatex-command-alist
               '("tr" "Insert trace operator" "\\operatorname{tr}?" cdlatex-position-cursor nil t))

  (add-to-list 'cdlatex-command-alist
               '("Tr" "Insert trace operator" "\\operatorname{Tr}?" cdlatex-position-cursor nil t))

  (add-to-list 'cdlatex-command-alist
               '("no" "Insert equation break with nonumber" "\\nonumber \\\\\n&\\quad ?" cdlatex-position-cursor nil t)))
;; Disable annoying chktex warning
(after! flycheck
  (add-to-list 'flycheck-disabled-checkers 'tex-chktex))

;; Babel mode for org file
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)))

;; Force Org-mode to open PDFs specifically with Zathura
(after! org
  (add-hook 'org-tab-first-hook #'org-try-cdlatex-tab)
  (setq org-highlight-latex-and-related '(native script)))

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

;; --- CUSTOM FUNCTIONS ---
(defun +jitu/copy-flycheck-errors-with-code ()
"Copy Flycheck error messages and the corresponding lines of code to the clipboard.

Input: None
Output: Returns the copied error string if errors exist, otherwise returns nil."
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
"Yank the currently selected text or the current line, paste it into the next window, and switch focus to that window.

Input: None
Output: Returns nil."
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring (region-beginning) (region-end))
                (thing-at-point 'line t))))
    (other-window 1)
    (insert text)
    (message "Zapped text to other window!")))

;; Define a function to reopen the current file as root
(defun +jitu/sudo-this-file ()
"Reopen the current file with root privileges using TRAMP while preserving the cursor position.

Input: None
Output: Returns nil."
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
