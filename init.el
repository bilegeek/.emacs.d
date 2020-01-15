;; ###########
;; # CREDITS #
;; ###########

;; Unless stated otherwise, most of the configs for SLIME, Paredit, and multiple-cursors are modified from
;; Shinmera's Portacle (https://github.com/portacle/emacsd). Elements of the Company and Magit configs are
;; also from there.

;; ####################
;; # GENERAL SETTINGS #
;; ####################

;;~~~~~

(when (or (eq system-type 'gnu/linux) (eq system-type 'berkeley-unix))
  (modify-frame-parameters nil '((wait-for-wm . nil))))

(setq default-frame-alist
      '((top . 0)(left . 0)
		  (width . 153)(height . 38)))

;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq gc-cons-threshold most-positive-fixnum)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;~~~~~

(prefer-coding-system 'utf-8-unix)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(set-language-environment "utf-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8)

;;~~~~~

;;Text highlighting and crap.
(setq sentence-end-double-space nil)
(delete-selection-mode t)

;;Highlighting, visibility, etc.
(setq search-highlight t            ;; highlight when searching... 
  query-replace-highlight t)        ;; ...and replacing
(show-paren-mode 1)
(setq show-paren-delay nil)
(blink-cursor-mode 0)
(setq visible-bell t)
(customize-set-variable 'show-trailing-whitespace t)
(setq indicate-empty-lines t)
;;(global-linum-mode t)
(global-hl-line-mode)
(global-visual-line-mode 1)
(column-number-mode t)
(transient-mark-mode 1)
(setq word-wrap t)
(setq-default cursor-type '(bar . 3))

;;Scrolling
(setq auto-window-vscroll nil)
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq redisplay-dont-pause t)
(customize-set-variable 'mouse-yank-at-point t)
(global-set-key [mode-line C-mouse-1] 'tear-off-window)

;;Ignore case during completion.
(setq completion-ignore-case t)
(customize-set-variable 'read-file-name-completion-ignore-case t)
(customize-set-variable 'read-buffer-completion-ignore-case t)

;;Set tab widths.
(setq default-tab-width 3)
(setq-default c-basic-offset 3
				  tab-width 3
				  indent-tabs-mode t)

;;Title, modeline time, etc.

(setq inhibit-start-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))
;;(display-time)
;;(setq display-time-24hr-format t)
;;(setq display-time-day-and-date t)

;;Files, autosaving, etc.
;;(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;;From https://jamiecollinson.com/blog/my-emacs-config/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
		backup-by-copying t    ; Don't delink hardlinks
		version-control t      ; Use version numbers on backups
		delete-old-versions t  ; Automatically delete excess backups
		kept-new-versions 20   ; how many of the newest versions to keep
		kept-old-versions 5    ; and how many of the old
		)

(save-place-mode 1)
(setq save-place-file "~/.emacs.d/saveplace")
(setq scroll-preserve-screen-position 'keep)
(setq default-directory "~/")
(setq vc-follow-symlinks t)
(size-indication-mode t)
(add-hook 'find-file-hooks 'goto-address-prog-mode)

(when (or (eq system-type 'darwin) (eq system-type 'windows-nt))
  (setq delete-by-moving-to-trash t))

(when (eq system-type 'darwin)
  (ns-set-resource nil "ApplePressAndHoldEnabled" "NO"))

;;Commands
(setq disabled-command-function nil)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;~~~~~

;; Don't try to use an external TLS program on Windows (it won't work).
(setq my-use-tls (or (not (eq system-type 'windows-nt)) (gnutls-available-p)))

(if (not (eq system-type 'windows-nt)) (if (and (version< emacs-version "26.3") (>= libgnutls-version 30600)) (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))

;; Some combination of GNU TLS and Emacs fail to retrieve archive
;; contents over https.
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341

;; Increase TLS security. To test this, run `test-https-verification' from `conf/utils/https'. See <https://lists.gnu.org/archive/html/emacs-devel/2018-06/msg00718.html>.
(setq network-security-level 'high)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elpa")
(cond
 ((eq system-type 'gnu/linux) (add-to-list 'load-path "~/.emacs.d/manualext/sly"))
 ((eq system-type 'windows-nt) (add-to-list 'load-path "~/sly")))

;;(setq package-check-signature t)
(setq load-prefer-newer t)

(require 'use-package)
(require 'gnu-elpa-keyring-update)

;;~~~~~

;;Misc. elisp functions are put into a separate file because clutter.
(load "~/.emacs.d/elisp/xeu_elisp_util.el")
(load "~/.emacs.d/elisp/custom.el")

;;~~~~~












;; ################################
;; # COMMANDS AND TEXT NAVIGATION #
;; ################################

;;~~~~~

(require 'ido)
(ido-mode 'buffers)
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
									"*Messages*" "Async Shell Command" "*scratch*" "*Flycheck error messages*"))

;;~~~~~

(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right-bottom)

;;~~~~~

(require 'multiple-cursors)
(require 'expand-region)

;; Workaround for https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)

;;From https://old.reddit.com/r/emacs/comments/eeyhdz/weekly_tipstricketc_thread/fch1bkv/
(defun snippins/helm-M-x ()
  (interactive)
  (if (call-interactively 'helm-M-x)
      (let ((cmd (intern (car extended-command-history))))
        (if multiple-cursors-mode
            (if (and cmd
                     (not (memq cmd mc--default-cmds-to-run-once))
                     (not (memq cmd mc/cmds-to-run-once))
                     (or mc/always-run-for-all
                         (memq cmd mc--default-cmds-to-run-for-all)
                         (memq cmd mc/cmds-to-run-for-all)
                         (mc/prompt-for-inclusion-in-whitelist cmd)))
                (mc/execute-command-for-all-fake-cursors cmd))))))

;; Populate default MC lists
(unless (file-exists-p mc/list-file)
  (setq mc/cmds-to-run-for-all
        '(backward-sexp
          downcase-region
          electric-newline-and-maybe-indent
          end-of-buffer
          forward-sexp
          indent-for-tab-command
          kill-region
          paredit-backslash
          paredit-backward
          paredit-close-round
          paredit-close-square
          paredit-comment-dwim
          paredit-convolute-sexp
          paredit-doublequote
          paredit-forward
          paredit-forward-barf-sexp
          paredit-forward-delete
          paredit-forward-down
          paredit-forward-slurp-sexp
          paredit-kill
          paredit-newline
          paredit-open-round
          paredit-open-square
          paredit-reindent-cl-defun
          paredit-semicolon
          paredit-splice-sexp-killing-backward
          paredit-backslash
          reindent-then-newline-and-indent
          scroll-other-window
          switch-to-buffer
          upcase-region
          yank-rectangle))
  (setq mc/cmds-to-run-once
        '(down-list
          ido-list-directory
          mouse-drag-mode-line)))

;;~~~~~

(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)

;;~~~~~

(require 'powerline)
(powerline-default-theme)

;;~~~~~

(require 'avy)

;;~~~~~

(require 'iedit)

;;~~~~~

(require 'ivy)
(require 'swiper)
(require 'counsel)
(require 'hydra)
(ivy-mode 1)
(setq ivy-initial-inputs-alist nil)
(use-package ivy-hydra
  :ensure t)
(setq ivy-use-virtual-buffers t
      ivy-count-format "%d/%d ")
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;;(setq search-default-mode #'char-fold-to-regexp)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;;From https://old.reddit.com/r/emacs/comments/cmnumy/weekly_tipstricketc_thread/ew3jyr5/
(defvar minibuffer-this-command+ nil
  "Command minibuffer started with.")

(add-hook 'minibuffer-setup-hook
			 (defun minibuffer-set-this-command+ ()
				(setq minibuffer-this-command+ real-this-command)))

(define-key minibuffer-local-map (kbd "C-u") 'minibuffer-restart-with-prefix+)
(define-key ivy-minibuffer-map (kbd "C-u") 'minibuffer-restart-with-prefix+)
(defun minibuffer-restart-with-prefix+ ()
  "Restart current minibuffer/ivy command with prefix argument."
  (interactive)
  (let ((input (ivy--input)))
    (cond ((memq  #'ivy--queue-exhibit post-command-hook)
           (ivy-quit-and-run
             (let ((current-prefix-arg '(4))
                   (ivy-initial-inputs-alist `((,(ivy-state-caller ivy-last) . ,input))))
               (call-interactively (ivy-state-caller ivy-last)))))
          (t
           (ivy-quit-and-run
             (let ((current-prefix-arg '(4)))
               (minibuffer-with-setup-hook (lambda ()
                                             (insert input)
                                             (minibuffer-message "C-u"))
                 (call-interactively minibuffer-this-command+))))))))

;;From https://old.reddit.com/r/emacs/comments/cz1xt6/weekly_tipstricketc_thread/f03pkn8/
;;Plus a shrink function derived from it.
(defun my-ivy-minibuffer-grow ()
  (interactive)
  (let ((lines (prefix-numeric-value current-prefix-arg)))
    (while (> lines 0)
      (ivy-minibuffer-grow)
      (setq lines (1- lines)))))

(defun my-ivy-minibuffer-shrink ()
  (interactive)
  (let ((lines (prefix-numeric-value current-prefix-arg)))
	 (while (> lines 0)
		(ivy-minibuffer-shrink)
		(setq lines (1- lines)))))

(define-key ivy-minibuffer-map (kbd "C-u") #'my-ivy-minibuffer-grow)
(define-key ivy-minibuffer-map (kbd "M-u") #'my-ivy-minibuffer-shrink)

;;From https://old.reddit.com/r/emacs/comments/dewzuy/weekly_tipstricketc_thread/f3avs8g/
(defun my-counsel-descbinds (&optional prefix buffer)
  "Show a list of all defined keys and their definitions.
If non-nil, show only bindings that start with PREFIX.
BUFFER defaults to the current one."
  (interactive)
  (ivy-read "Bindings: " (my-counsel--descbinds-cands prefix buffer)
            :action #'counsel-descbinds-action-describe
            :history 'my-counsel-descbinds-history
            :caller 'my-counsel-descbinds))

(defun my-counsel--descbinds-cands (&optional prefix buffer)
  "Get key bindings starting with PREFIX in BUFFER.
See `describe-buffer-bindings' for further information."
  (let ((buffer (or buffer (current-buffer)))
        (re-exclude (regexp-opt
                     '("<vertical-line>" "<bottom-divider>" "<right-divider>"
                       "<mode-line>" "<C-down-mouse-2>" "<left-fringe>"
                       "<right-fringe>" "<header-line>"
                       "<vertical-scroll-bar>" "<horizontal-scroll-bar>"
                       "<remap>")))
        res)
    (with-temp-buffer
      (let ((indent-tabs-mode t))
        (describe-buffer-bindings buffer prefix))
      (goto-char (point-min))
      ;; Skip the "Key translations" section
      (re-search-forward " ")
      (forward-char 1)
      (while (not (eobp))
        (when (looking-at "^\\([^\t\n]+\\)[\t ]*\\(.*\\)$")
          (let ((key (match-string 1))
                (fun (match-string 2))
                cmd)
            (unless (or (member fun '("??" "self-insert-command"))
                        (string-match re-exclude key)
                        (not (and (commandp (setq cmd (intern-soft fun)))
                                  (not (member fun '("ignore"))))))
              (push
               (cons (format
                      "%-35.35s %s"
                      fun
                      (propertize key 'face 'counsel-key-binding))
                     (cons key cmd))
               res))))
        (forward-line 1)))
    (nreverse res)))

;;~~~~~

(require 'all-the-icons)

(require 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(all-the-icons-ivy-setup)

(require 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;;~~~~~

(require 'alert)
(setq alert-default-style 'notifier)

;;~~~~~

;;nlinum instead of linum because it's more efficient
;;yascrollbar so I can have a scrollbar

(use-package linum-off :ensure t)
(require 'nlinum-relative)
(setq nlinum-highlight-current-line t)
(global-nlinum-mode)

(global-yascroll-bar-mode 1)
(setq yascroll:delay-to-hide nil)

(require 'shackle)
(setq shackle-default-alignment 'below)

;;~~~~~





;; ###############################################
;; # VERSION CONTROL, FILESYSTEM, REMOTE EDITING #
;; ###############################################

;;~~~~~

;;Version control porcelain.
(require 'magit)
(setq magit-git-executable "git")
(setq git-commit-fill-column 9999)
(setq git-commit-summary-max-length 9999)
(setq git-commit-finish-query-functions nil)
;; use ido to checkout branchsego
(setq magit-completing-read-function 'magit-ido-completing-read)

;;~~~~~

(require 'git-gutter)
(global-git-gutter-mode t)
;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

;;~~~~~

(require 'git-gutter+)
(global-git-gutter+-mode)

(eval-after-load 'git-gutter+
  '(progn
   ;;; Jump between hunks
     (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
    ;;; Act on hunks
     (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
     ;; Stage hunk at point.
     ;; If region is active, stage all hunk lines within the region.
     (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
     (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)))

(require 'git-gutter-fringe+)

;; Optional: Activate minimal skin
(git-gutter-fr+-minimal)

;;~~~~~

(require 'mo-git-blame)

(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;;~~~~~

(require 'git-timemachine)
;;I've also installed git-timemachine; use M-x to access it.

;;~~~~~

(use-package git-link :ensure t :commands (git-link git-link-homepage))

;;~~~~~

(require 'projectile)
(projectile-mode +1)
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package counsel-projectile
  :ensure t
  :config
  (add-hook 'after-init-hook 'counsel-projectile-mode))

;;From https://old.reddit.com/r/emacs/comments/cz1xt6/weekly_tipstricketc_thread/eyvsycv/
;;Switch to buffer in current project.
(defun my/switch-to-project-buffer () "Switch to project buffer." (interactive) (let ((project-dir (cdr (project-current)))) (if project-dir (let ((buf-cur (current-buffer)) entries) (dolist (buf (buffer-list)) (let ((buf-name (buffer-name buf))) (when (and (not (eq buf buf-cur)) (not (string-prefix-p " " buf-name)) (file-in-directory-p (or (buffer-file-name buf) (with-current-buffer buf default-directory)) project-dir)) (push buf-name entries)))) (let* ((def (buffer-name (other-buffer buf-cur))) (def (when (member def entries) def))) (switch-to-buffer (completing-read (concat "Switch to project buffer" (when def (format " (default %s)" def)) ": ") entries nil nil nil 'buffer-name-history def)))) (call-interactively 'switch-to-buffer))))

;;~~~~~

(require 'neotree)
(setq neo-smart-open nil)
(setq neo-autorefresh nil)
(setq neo-theme 'icons)
(setq neo-window-width 40)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq-default neo-show-hidden-files t)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (treemacs--find-python3))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

;;~~~~~

(require 'tramp)
(setq tramp-default-method "ssh"
		tramp-use-ssh-controlmaster-options nil
		backup-enable-predicate
		(lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (let ((method (file-remote-p name 'method)))
                    (when (stringp method)
                      (member method '("su" "sudo"))))))))

(require 'tramp-sh)

;;~~~~~

(require 'sane-term)
(setq sane-term-kill-on-exit t)

;;From https://oremacs.com/2019/03/24/shell-apt/

(advice-add 'ansi-color-apply-on-region :before 'ora-ansi-color-apply-on-region)

(defun ora-ansi-color-apply-on-region (begin end)
  "Fix progress bars for e.g. apt(8).
Display progress in the mode line instead."
  (let ((end-marker (copy-marker end))
        mb)
    (save-excursion
      (goto-char (copy-marker begin))
      (while (re-search-forward "\0337" end-marker t)
        (setq mb (match-beginning 0))
        (when (re-search-forward "\0338" end-marker t)
          (let ((progress (buffer-substring-no-properties
                           (+ mb 2) (- (point) 2))))
            (delete-region mb (point))
            (ora-apt-progress-message progress)))))))

(defun ora-apt-progress-message (progress)
  (message
   (replace-regexp-in-string
    "%" "%%"
    (ansi-color-apply progress))))

;;From https://old.reddit.com/r/emacs/comments/di4pqz/weekly_tipstricketc_thread/f3u1ph7/
(use-package ansi-color
  :ensure nil
  :config
  (add-hook 'comint-preoutput-filter-functions 'ansi-color-filter-apply)
  :hook
  (compilation-filter . colorize-compilation-buffer)
  :preface
  (autoload 'ansi-color-apply-on-region "ansi-color")
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package xterm-color
  :functions (xterm-color-filter)
  :config
  ;; Configuration for shell-mode
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (defun akirak/xterm-color-shell-mode-hook ()
    ;; Disable font-locking in this buffer to improve performance
    (font-lock-mode -1)
    ;; Prevent font-locking from being re-enabled in this buffer
    (make-local-variable 'font-lock-function)
    (setq font-lock-function (lambda (_) nil))
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))
  ;; Configuration for compilation-mode
  (add-to-list 'compilation-environment "TERM=xterm-256color")
  (defun akirak/xterm-color-compilation-start (proc)
    ;; We need to differentiate between compilation-mode buffers
    ;; and running as part of comint (which at this point we assume
    ;; has been configured separately for xterm-color)
    (when (eq (process-filter proc) 'compilation-filter)
      ;; This is a process associated with a compilation-mode buffer.
      ;; We may call `xterm-color-filter' before its own filter function.
      (set-process-filter
       proc
       (lambda (proc string)
         (funcall 'compilation-filter proc
                  (xterm-color-filter string))))))
  :hook
  (shell-mode . akirak/xterm-color-shell-mode-hook)
  (compilation-start . akirak/xterm-color-compilation-start))

;; Render unicode characters properly.
;;
;; https://stackoverflow.com/questions/6820051/unicode-characters-in-emacs-term-mode
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(provide 'setup-terminal-colors)

;;~~~~~





;; ##############
;; # TEXT STUFF #
;; ##############

;;~~~~~

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-startup-indented 'f)
(setq org-directory "~/org")
(setq org-special-ctrl-a/e 't)
;;(setq org-default-notes-file (concat org-directory "/notes.org"))
;;(define-key global-map "\C-cc" 'org-capture)
;;(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-src-fontify-natively 't)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)

(defun ap/org-tree-to-indirect-buffer (&optional arg)
  "Create indirect buffer and narrow it to current subtree.
The buffer is named after the subtree heading, with the filename
appended.  If a buffer by that name already exists, it is
selected instead of creating a new buffer."
  (interactive "P")
  (let* ((new-buffer-p)
         (buffer-name (let ((heading (nth 4 (org-heading-components))))
                        ;; FIXME: I think there's an Org function that does this, but I can't find it.
                        (concat (if (string-match org-bracket-link-regexp heading)
                                    ;; Heading is an org link; use link name
                                    ;; TODO: but what if only part of the heading is?
                                    (match-string 3 heading)
                                  ;; Not a link; use whole heading
                                  heading)
                                "::" (buffer-name))))
         (new-buffer (or (get-buffer buffer-name)
                         (prog1 (condition-case nil
                                    (make-indirect-buffer (current-buffer) buffer-name 'clone)
                                  (error (make-indirect-buffer (current-buffer) buffer-name)))
                           (setq new-buffer-p t)))))
    (switch-to-buffer new-buffer)
    (when new-buffer-p
      (rename-buffer buffer-name)
      (org-narrow-to-subtree)))
  )
(advice-add 'org-tree-to-indirect-buffer :override 'ap/org-tree-to-indirect-buffer)

;;From https://old.reddit.com/r/emacs/comments/dewzuy/weekly_tipstricketc_thread/f30qx5e/
(defun org-repair-property-drawers ()
  "Fix properties drawers in current buffer.
     Ignore non Org buffers."
  (when (eq major-mode 'org-mode)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((case-fold-search t)
           (inline-re (and (featurep 'org-inlinetask)
                           (concat (org-inlinetask-outline-regexp)
                                   "END[ \t]*$"))))
       (org-map-entries
        (lambda ()
          (unless (and inline-re (org-looking-at-p inline-re))
            (save-excursion
              (let ((end (save-excursion (outline-next-heading) (point))))
                (forward-line)
                (when (org-looking-at-p org-planning-line-re) (forward-line))
                (when (and (< (point) end)
                           (not (org-looking-at-p org-property-drawer-re))
                           (save-excursion
                             (and (re-search-forward org-property-drawer-re end t)
                                  (eq (org-element-type
                                       (save-match-data (org-element-at-point)))
                                      'drawer))))
                  (insert (delete-and-extract-region
                           (match-beginning 0)
                           (min (1+ (match-end 0)) end)))
                  (unless (bolp) (insert "\n"))))))))))))

(defun fix-org-property-drawers ()
  (interactive)
  (cl-loop for filename in (f-glob  "*.org" org-directory)
           ;; [2017-04-10 Mon 05:34] Worked fine.
           unless (s-matches? "#" filename)
           do (with-current-buffer (find-file-noselect filename)
					 (message "Processing %s" filename)
					 (org-remove-readonly)
					 (ignore-errors (org-repair-property-drawers))
					 (org-mark-readonly)
					 (save-buffer))))

;;Follow functions from https://old.reddit.com/r/emacs/comments/cw1eky/weekly_tipstricketc_thread/ey7y8x2/
(defun follow-toggle+ (&optional arg)
  "Derived from `follow-delete-other-windows-and-split'.

With prefix argument use ARG number of windows, defaults to two."
  (interactive "P")
  (cond (follow-mode
         (delete-other-windows)
         (follow-mode -1))
        (t
         (let ((splits (if arg
                           (1- (abs (prefix-numeric-value arg)))
                         1))
               (w (selected-window))
               (start (window-start)))
           (delete-other-windows)
           (let ((window-combination-resize t))
             (while (> splits 0)
               (setq splits (1- splits))
               (set-window-start
                (split-window-right) start)))
           (setq follow-internal-force-redisplay t)
           (follow-mode 1)))))

(defun follow-defun+ ()
  "Create enough windows to follow defun."
  (interactive)
  (let* ((dbeg (save-excursion
                 (beginning-of-defun 1)
                 (point)))
         (dend (save-excursion
                 (end-of-defun 1)
                 (point)))
         (dlines (1+ (count-lines dbeg dend)))
         (wlines (1- (window-height))))
    (set-window-start  nil dbeg)
    (setf (window-point) dbeg)
    (forward-line (or scroll-margin 0))
    (when (> dlines wlines)
      (follow-toggle+ (1+ (/ dlines wlines))))))

(require 'org-mime)

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("∙"))
  (add-hook 'org-mode-hook 'org-bullets-mode))

(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Setting up org-projectile
(require 'org-projectile)
(setq org-projectile-projects-file
      "~/.emacs.d/org/TODO.org")
(push (org-projectile-project-todo-entry) org-capture-templates)
(setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))

;;~~~~~

(require 'xah-math-input)
(global-xah-math-input-mode 1)

;;~~~~~

(require 'emojify)
(add-hook 'after-init-hook #'global-emojify-mode)

;;~~~~~

(use-package doc-view :ensure t
  :mode ("\\.pdf" . doc-view-mode))

;;~~~~~

;;(require 'auctex)
(latex-preview-pane-enable)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;~~~~~

(require 'flyspell)

(cond ((eq system-type 'windows-nt) (setq ispell-program-name "c:/hunspell/bin/hunspell.exe"))
		((eq system-type 'gnu/linux) (setq ispell-program-name "hunspell")))

;;From https://passingcuriosity.com/2017/emacs-hunspell-and-dictionaries/
(setenv
 "DICPATH"
 (concat (getenv "HOME") "~/.emacs.d/dictionary"))

(setq ispell-local-dictionary "en_US")

(setq ispell-local-dictionary-alist
      ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
      ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
        ))

;;From https://gist.github.com/vibrog/752198
(setq ispell-dictionary-alist
		'((nil "[A-Za-z]" "[^A-Za-z]" "[']" t
				 ("-d" "en_US" "-i" "utf-8") nil utf-8)
		  ("american"
			"[A-Za-z]" "[^A-Za-z]" "[']" nil
			("-d" "en_US") nil utf-8)
		  ("english"
			"[A-Za-z]" "[^A-Za-z]" "[']" nil
			("-d" "en_GB") nil utf-8)
		  ("british"
			"[A-Za-z]" "[^A-Za-z]" "[']" nil
			("-d" "en_GB") nil utf-8)))

;;From https://emacs.stackexchange.com/questions/47344/ispell-not-working-with-hunspell
;;WTF did it take SO LONG for me to find this?
;; Work around for Hunspell 1.7.0 
(cond ((not (eq system-type 'windows-nt)) (progn
														  (defun manage-hunspell-1.7 (old-function-ispell &rest arguments)
															 "Add null-device when calling \"hunspell -D\"."
															 (if  (equal "-D"  (nth 4 arguments))
																  (funcall old-function-ispell "hunspell" null-device t nil "-D" null-device)
																(apply old-function-ispell  arguments)))
														  (advice-add 'ispell-call-process :around #'manage-hunspell-1.7))))



(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;;Enables Flyspell for comments and org-mode.
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook 'turn-on-flyspell)

;;Stop ispell from criticizing org-mode stuff.
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

;; NO spell check for embedded snippets
(defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  (let* ((rlt ad-return-value)
         (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\|quote\\)")
         (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\|quote\\)")
         (case-fold-search t)
			b e)
    (when ad-return-value
      (save-excursion
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t))))
      (if (and b e (< (point) e)) (setq rlt nil)))
    (setq ad-return-value rlt)))

;;~~~~~





;; ##############
;; # LANG STUFF #
;; ##############

;;~~~~~

(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/yasnippets")

;;~~~~~

(require 'company)
(require 'company-quickhelp)

(defun company-elisp-finder-keyword-backend (command &optional arg &rest ign)
  "`company-backend' for finder-keywords."
  (case command
    (prefix
     (and (require 'finder nil t)
          (or (company-grab ":group '\\(\\(\\sw\\|\\s_\\)*\\)" 1)
              (company-grab "Keywords:.*[ \t]+\\(\\(\\sw\\|\\s_\\)*\\)" 1))))
    (candidates (all-completions arg finder-known-keywords))
    (meta (cdr (assoc (intern arg) finder-known-keywords)))))

(global-company-mode)
(company-quickhelp-mode 1)
(setq company-quickhelp-delay 0.7
      company-tooltip-align-annotations t)
(setq company-minimum-prefix-length 2)

  ;;; From https://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode
  ;;; Prevent suggestions from being triggered automatically. In particular,
  ;;; this makes it so that:
  ;;; - TAB will always complete the current selection.
  ;;; - RET will only complete the current selection if the user has explicitly
  ;;;   interacted with Company.
  ;;; - SPC will never complete the current selection.
  ;;;
  ;;; Based on:
  ;;; - https://github.com/company-mode/company-mode/issues/530#issuecomment-226566961
  ;;; - https://emacs.stackexchange.com/a/13290/12534
  ;;; - http://stackoverflow.com/a/22863701/3538165
  ;;;
  ;;; See also:
  ;;; - https://emacs.stackexchange.com/a/24800/12534
  ;;; - https://emacs.stackexchange.com/q/27459/12534

;; <return> is for windowed Emacs; RET is for terminal Emacs
(dolist (key '("<return>" "RET"))
  ;; Here we are using an advanced feature of define-key that lets
  ;; us pass an "extended menu item" instead of an interactive
  ;; function. Doing this allows RET to regain its usual
  ;; functionality when the user has not explicitly interacted with
  ;; Company.
  (define-key company-active-map (kbd key)
    `(menu-item nil company-complete
                :filter ,(lambda (cmd)
                           (when (company-explicit-action-p)
                             cmd)))))
(define-key company-active-map (kbd "TAB") #'company-complete-selection)
(define-key company-active-map (kbd "SPC") nil)

;; Company appears to override the above keymap based on company-auto-complete-chars.
;; Turning it off ensures we have full control.
(setq company-auto-complete-chars nil)

(add-to-list 'company-backends '(company-lua company-math-symbols-latex company-math-symbols-unicode))

(define-key company-active-map (kbd "<up>") 'company-select-previous)
(define-key company-active-map (kbd "<down>") 'company-select-next)
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

;;~~~~~

;; global activation of the unicode symbol completion 
(add-to-list 'company-backends 'company-math-symbols-unicode)

;;~~~~~

(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

;;~~~~~

(require 'cedet)

(setq semantic-default-submodes
      '(;; Perform semantic actions during idle time
        global-semantic-idle-scheduler-mode
        ;; Use a database of parsed tags
        global-semanticdb-minor-mode
        ;; Decorate buffers with additional semantic information
        global-semantic-decoration-mode
        ;; Highlight the name of the function you're currently in
        global-semantic-highlight-func-mode
        ;; show the name of the function at the top in a sticky
        global-semantic-stickyfunc-mode
        ;; Generate a summary of the current tag when idle
        global-semantic-idle-summary-mode
		  global-semantic-idle-completions-mode
        ;; Show a breadcrumb of location during idle time
        global-semantic-idle-breadcrumbs-mode
        ;; Switch to recently changed tags with `semantic-mrub-switch-tags',
        ;; or `C-x B'
        global-semantic-mru-bookmark-mode))

(defvar semantic-modes
  '(emacs-lisp-mode python-mode c-mode objc-mode c++-mode js2-mode fortran-mode cobol-mode basic-mode ada-mode octave-mode haskell-mode opascal-mode lisp-mode prog-mode css-mode lua-mode prolog-mode inferior-prolog-mode gnuplot-mode comint-mode geiser-repl sly-mrepl))

(hook-modes semantic-modes
  (semantic-mode))

(global-ede-mode t)

;;~~~~~

;;Copied from some website, for JavaScript syntactic sugar.
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2
        js-indent-level 2))

;;~~~~~

;;(use-package flycheck
;;  :ensure t
;;  :init (global-flycheck-mode))

(require 'flycheck)
(global-flycheck-mode)

;;~~~~~

(add-hook 'pascal-mode-hook 'opascal-mode)
;;(add-hook 'css-mode-hook 'xah-css-mode)
(add-to-list 'auto-mode-alist '("\\.bas\\'" . basic-mode))
(add-to-list 'auto-mode-alist '("\\.cbl\\'" . cobol-mode))
(add-to-list 'auto-mode-alist '("\\.adb\\'" . ada-mode))

(defvar electric-pair-modes
  '(python-mode inferior-python-mode c-mode objc-mode c++-mode js2-mode fortran-mode cobol-mode basic-mode ada-mode inferior-octave-mode octave-mode haskell-mode opascal-mode prog-mode css-mode lua-mode prolog-mode inferior-prolog-mode gnuplot-mode comint-mode term-mode latex-mode LaTeX-mode))

(hook-modes electric-pair-modes
  (electric-pair-local-mode))

(require 'aggressive-indent)

(defvar aggressive-indent-modes
  '(python-mode inferior-python-mode c-mode objc-mode c++-mode js2-mode fortran-mode cobol-mode basic-mode ada-mode inferior-octave-mode octave-mode haskell-mode opascal-mode prog-mode css-mode lua-mode prolog-mode inferior-prolog-mode gnuplot-mode comint-mode geiser-repl sly-mrepl))

(hook-modes aggressive-indent-modes
  (aggressive-indent-mode))

(which-function-mode)

;;~~~~~

(require 'gnuplot)
(setq gnuplot-program "/usr/bin/gnuplot")

;;;; ====  Configure Gnuplot-mode  ====
;; these lines enable the use of gnuplot mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
(setq auto-mode-alist
		(append
       (list
        '("\\.gp$" . gnuplot-mode)
        '("\\.plt$" . gnuplot-mode)
        )
		 auto-mode-alist))

;;~~~~~




;; ##############
;; # LISP STUFF #
;; ##############

(require 'slime)

(setq slime-contribs '(slime-fancy slime-asdf slime-sprof slime-mdot-fu
											  slime-compiler-notes-tree slime-hyperdoc
											  slime-indentation slime-repl))
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-startup-animation nil)
(setq slime-auto-select-connection 'always)
(setq slime-kill-without-query-p t)
(setq slime-description-autofocus t) 
(setq slime-fuzzy-explanation "")
(setq slime-asdf-collect-notes t)
(setq slime-inhibit-pipelining nil)
(setq slime-load-failed-fasl 'always)
(setq slime-when-complete-filename-expand t)
(setq slime-repl-history-remove-duplicates t)
(setq slime-repl-history-trim-whitespaces t)
(setq slime-export-symbol-representation-auto t)
(setq lisp-indent-function 'common-lisp-indent-function)
(setq lisp-loop-indent-subclauses nil)
(setq lisp-loop-indent-forms-like-keywords t)
(setq lisp-lambda-list-keyword-parameter-alignment t)
(setq slime-buffer-connection t)
(setq slime-buffer-package t)

(setq slime-lisp-implementations
		'((sbcl ("sbcl") :coding-system utf-8-unix)
		  (sbcl-latest ("~/clozure/sbcl-2.0.0-x86-64-linux/run-sbcl.sh") :coding-system utf-8-unix)
		  (ccl64 ("ccl64") :coding-system utf-8-unix)
		  (ccl64-latest ("~/clozure/linuxx86/lx86cl64") :coding-system utf-8-unix)
		  (cmucl ("cmucl") :coding-system utf-8-unix)
		  (clisp ("clisp") :coding-system utf-8-unix)
		  (ecl ("ecl") :coding-system utf-8-unix)))
(setq slime-default-lisp 'sbcl-latest)

(add-hook 'slime-repl-mode-hook 'set-slime-repl-return)

(defun set-slime-repl-return ()
  (define-key slime-repl-mode-map (kbd "RET") 'slime-repl-return-at-end)
  (define-key slime-repl-mode-map (kbd "<return>") 'slime-repl-return-at-end))

(defun slime-repl-return-at-end ()
  (interactive)
  (if (<= (point-max) (point))
      (slime-repl-return)
		(slime-repl-newline-and-indent)))

(advice-add 'slime :around #'my/with-buffers-in-same-window)

;;~~~~~

(require 'geiser)
(setq geiser-active-implementations '(racket))

(cond ((eq system-type 'gnu/linux) (setq geiser-racket-binary "racket"))
		((eq system-type 'windows-nt) (setq geiser-racket-binary "c:/Program Files/Racket/Racket.exe")))

(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;;~~~~~

(require 'paredit)
(eval-when-compile (require 'cl))
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'racket-mode-hook           'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook 'enable-paredit-mode)
(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eshell-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; Fix the spacing for macro characters such as #p, etc.
(defvar known-macro-characters (make-hash-table))

(defun determine-cl-macro-character (macro-char)
  (when (slime-connected-p)
    (lexical-let ((macro-char macro-char))
      (slime-eval-async
       `(cl:ignore-errors
         (cl:not (cl:null (cl:get-macro-character
                           (cl:code-char ,macro-char)))))
       (lambda (result)
         (puthash macro-char result known-macro-characters))))))

(defun cl-macro-character-p (macro-char)
  (pcase (gethash macro-char known-macro-characters :not-found)
    (`t t)
    (`nil nil)
    (:not-found
     (determine-cl-macro-character macro-char)
     (or ;; Don't know the result (yet), determine statically.
      (cl-find macro-char '(?# ?,))))))

(defun paredit-detect-cl-macro-character (endp delimiter)
  (when (cl-find major-mode '(slime-repl-mode lisp-mode))
    (if (not endp)
        (save-excursion
          (let ((1-back (char-before (point)))
                (2-back (char-before (- (point) 1))))
				(null (or (cl-macro-character-p (char-before (point)))
                      (cl-macro-character-p (char-before (1- (point))))))))
      t)))

(with-eval-after-load 'paredit
  (add-to-list 'paredit-space-for-delimiter-predicates
               'paredit-detect-cl-macro-character))







;; #######
;; # WEB #
;; #######

;;~~~~~

(require 'w3m-load)
;;So after a bunch of headaches figuring out WTF with MIME, I had to install SEMI. Confused? Good.
(require 'mime-w3m)
(setq browse-url-browser-function  'w3m-goto-url-new-session)
(setq w3m-default-display-inline-images t)
(setq w3m-default-directory "~/.emacs.d/w3m/")
(setq w3m-default-save-directory "~/.emacs.d/w3m/saves/")
(setq w3m-add-tab-number t)
;;From http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html

;;~~~~~

(require 'hackernews)

;;~~~~~

;;(cond ((eq system-type 'gnu/linux) (load "~/.emacs.d/elisp/mu4esetup.el")))

;;~~~~~

;; (require 'circe)

;; (add-hook 'circe-chat-mode-hook 'my-circe-prompt)
;; (defun my-circe-prompt ()
;;   (lui-set-prompt
;;    (concat (propertize (concat (buffer-name) ">")
;;                        'face 'circe-prompt-face)
;;            " ")))

;; (setq circe-format-server-topic "*** Topic change by {userhost}: {topic-diff}")
;; (setq circe-format-say "{nick:-16s} {body}")
;; (setq circe-format-self-say "<{nick}> {body}")

;; (setq
;;  lui-time-stamp-position 'right-margin
;;  lui-time-stamp-format "%H:%M")

;; (add-hook 'lui-mode-hook 'my-circe-set-margin)
;; (defun my-circe-set-margin ()
;;   (setq right-margin-width 5))

;; (setq
;;  lui-time-stamp-position 'right-margin
;;  lui-fill-type nil)

;; (add-hook 'lui-mode-hook 'my-lui-setup)
;; (defun my-lui-setup ()
;;   (setq
;;    fringes-outside-margins t
;;    right-margin-width 5
;;    word-wrap t
;;    wrap-prefix "    "))

;; (load "lui-logging" nil t)
;; (enable-lui-logging-globally)
;; (setq lui-logging-directory "~/.emacs.d/irclog/")

;; (load "~/.emacs.d/elisp/.circeprivate.el")

;;~~~~~

;; (require 'jabber)

;; ;;Lotsa goodies at https://www.emacswiki.org/emacs/JabberEl
;; (setq
;;  jabber-history-enabled t
;;  jabber-use-global-history nil
;;  jabber-backlog-number 40
;;  jabber-backlog-days 30
;;  )
;; (setq jabber-history-dir "~/.emacs.d/jabberhistory")
;; (setq jabber-alert-presence-message-function (lambda (who oldstatus newstatus statustext) nil))
;; (define-key jabber-chat-mode-map (kbd "RET") 'newline)
;; (define-key jabber-chat-mode-map (kbd "C-j") 'jabber-chat-buffer-send)

;; (setq my-chat-prompt "[%t] %n>\n")
;; (when (featurep 'jabber)
;;   (setq
;;    jabber-chat-foreign-prompt-format my-chat-prompt
;;    jabber-chat-local-prompt-format my-chat-prompt
;;    jabber-groupchat-prompt-format my-chat-prompt
;;    jabber-muc-private-foreign-prompt-format "[%t] %g/%n>\n"
;;    )
;;   )

;; ;; Message alert hooks
;; (define-jabber-alert echo "Show a message in the echo area"
;;   (lambda (msg)
;;     (unless (minibuffer-prompt)
;;       (message "%s" msg))))

;;~~~~~

(require 'elfeed)
(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-db-directory "~/.emacs.d/elfeed/")

;;~~~~~





;; #########
;; # GAMES #
;; #########

(setq tetris-score-file 
		"~/.emacs.d/tetris-scores")

;;~~~~~





;; ######################
;; # GLOBAL KEYBINDINGS #
;; ######################

;;~~~~~

(setq set-mark-command-repeat-pop t)
(windmove-default-keybindings 'shift)

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

;;Next buffer and previous buffer key combos are loaded after Ivy.
;; ^ Disregard this statement, the bindings are commented out.
;;See the zooming wizardry for why I disabled these.
;;(global-set-key (kbd "C--") 'text-scale-decrease)
;;(global-set-key (kbd "C-=") 'text-scale-increase)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key [(meta backspace)] 'goto-last-edit)
(global-set-key [(ctrl backspace)] 'backward-delete-word)
(global-set-key (kbd "C-9") 'xah-toggle-letter-case)
(global-set-key (kbd "M-9") 'xah-select-text-in-quote)
(global-set-key (kbd "M-8") 'xah-extend-selection)
(global-set-key (kbd "M-7") 'xah-select-line)
(global-set-key (kbd "M-6") 'xah-select-block)

(global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
(define-key global-map (kbd "C-;") 'iedit-mode)

;; change font size, interactively
(global-set-key (kbd "C->") 'zoomin10)
(global-set-key (kbd "C-<") 'zoomout10)

;;Put this after loading ivy.
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(eval-after-load "term"
  '(progn
	  (define-key term-raw-map (kbd "M-x") nil)
     (define-key term-raw-map (kbd "M-[") nil)
     (define-key term-raw-map (kbd "M-]") nil)))

(eval-after-load "w3m"
  '(progn
	  (define-key w3m-mode-map (kbd "M-[") nil)
	  (define-key w3m-mode-map (kbd "M-]") nil)))

;;~~~~~

;;From commands.el
(global-set-key (kbd "C-q") 'er/expand-region)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-<next>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<prior>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-m <down>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-m <up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-m <right>") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-M-m <left>") 'mc-unmark-previous-like-this)
(global-set-key (kbd "C-M-m a") 'mc/mark-all-like-this)

(global-set-key (kbd "C-c j") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-s") 'avy-goto-word-1)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)


;;~~~~~

;;From text.el
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x C-g") 'git-gutter)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

;; Mark current hunk
(global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)

(global-set-key (kbd "C-x g") 'git-gutter+-mode) ; Turn on/off in the current buffer
(global-set-key (kbd "C-x G") 'global-git-gutter+-mode) ; Turn on/off globally

;;(global-set-key [?\C-c ?g ?c] 'mo-git-blame-current)
;;(global-set-key [?\C-c ?g ?f] 'mo-git-blame-file)

(global-set-key [f8] 'neotree-toggle)

;;~~~~~





;; ###############
;; # THEME STUFF #
;; ###############

;;~~~~~

(require 'rainbow-delimiters)

(defvar rainbow-delimiters-modes '(latex-mode LaTeX-mode prog-mode inferior-python-mode inferior-octave-mode inferior-prolog-mode comint-mode geiser-repl slime-repl-mode sly-mrepl eshell-mode term-mode))

(hook-modes aggressive-indent-modes
  (rainbow-delimiters-mode))

;;From https://yoo2080.wordpress.com/2013/12/21/small-rainbow-delimiters-tutorial/
;;and https://kaashif.co.uk/2015/04/11/rainbow-brackets-in-emacs/
;;modified by me to crank up the saturation further.
(require 'cl-lib)
(require 'color)

(defun rainbow-colors-intense ()
  (interactive)
  (cl-loop
	for index from 1 to rainbow-delimiters-max-face-count
	do
	(let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 100))))

(rainbow-colors-intense)

(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error
                    :strike-through t)






;;~~~~~
(setq custom-safe-themes t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (ir-black)))
 '(fci-rule-color "#f8fce8")
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(mouse-yank-at-point t)
 '(package-selected-packages
	(quote
	 (shackle cycle-themes plan9-theme elfeed-goodies elfeed jabber circe ada-mode oauth2 oauth realgud hackernews w3m company-lua ir-black-theme)))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(show-trailing-whitespace t)
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(let-faces-be)
(default-ir-black-crap)
(decrease-modeline-font-size 20)
(my/zoom-out 10)


;; Put this at the very end of the init file.
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
