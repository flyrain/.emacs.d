;; -------------------------------------------
;;
;; dotemacs for GNU Emacs
;; Time-stamp: <2014-02-06 01:57:45 yufei>
;;
;; -------------------------------------------

;(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))

(setq inhibit-startup-message t) 

(show-paren-mode t) 

;;========================== ECB =====================================
(setq stack-trace-on-error t)

;(setq stack-trace-on-error nil) ;;don’t popup Backtrace window
(setq ecb-tip-of-the-day nil)
(setq ecb-auto-activate t)
(setq ecb-layout-name "left6")
;(setq ecb-options-version "2.40")
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1–mouse-2))
(setq ecb-source-path (quote ("~/")))

;;======================= ADD PACKAGE SOURCES ========================
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;;======================= SHUTDOWN EMACS SERVER INSTANCE =============
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;;========================== IDO =====================================
(require 'init-ido)

;;========================== AUTOCOMPLETE ============================
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'org-mode)
;; set user-dic
(setq ac-user-dictionary '("Hello" "world"))

;;========================== ORG MODE  ===============================
; Some initial langauges we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   ))

(setq org-agenda-files (list "~/Dropbox" "~/Dropbox/diary"))
(global-set-key (kbd "<f12>") 'org-agenda)

(setq org-default-notes-file "~/Dropbox/refile.org")
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-log-done 'time)

;; ======================== Google ===================================
(load-file "~/.emacs.d/plugins/google.el")
(require 'google-search)
(define-key global-map (kbd "C-c s") 'google-search-selection)

;; ======================== Auctex ===================================
;(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)

(mapc (lambda (mode)
      (add-hook 'LaTeX-mode-hook mode))
      (list 'auto-fill-mode
            'LaTeX-math-mode
            'turn-on-reftex
            'linum-mode))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; remove all tabs before saving
                 ; TeX-engine 'xetex       ; use xelatex default
                  TeX-show-compilation t) ; display compilation windows
            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)))
(put 'narrow-to-region 'disabled nil)

;; ======================== ETC ======================================
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; ======================= C-MODE ====================================
(setq c-default-style "linux"
          c-basic-offset 4)

;(require 'google-c-style)
;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;transparent
;(modify-frame-parameters (selected-frame) `((alpha . 95)))

;; ======================== KILL CURRENT BUFFER ======================
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'kill-this-buffer)

;; ======================== MOVE BETWEEN WINDOWS =====================
;; move between windows with S-arrow, this is commented because I use
;; new windows management key binding as following.
;(windmove-default-keybindings) 
;(setq windmove-wrap-around t)

;; ======================== Windows Management =======================
;; map the window manipulation keys to meta 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap
(global-set-key (kbd "M-O") 'rotate-windows)

;; Replace dired's M-o
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "M-o") 'other-window))) ; was dired-omit-mode
;; Replace ibuffer's M-o
(add-hook 'ibuffer-mode-hook (lambda () (define-key ibuffer-mode-map (kbd "M-o") 'other-window))) ; was ibuffer-visit-buffer-1-window
;; To help Unlearn C-x 0, 1, 2, o
(global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
(global-unset-key (kbd "C-x 2")) ; was split-window-vertically
(global-unset-key (kbd "C-x 1")) ; was delete-other-windows
(global-unset-key (kbd "C-x 0")) ; was delete-window
(global-unset-key (kbd "C-x o")) ; was other-window

;; Borrowed from https://gist.github.com/1415844
;; Also see http://emacsworld.blogspot.com/2011/12/moving-buffers-between-windows.html
(defun rotate-left (l)
  (append  (cdr l) (list (car l))))
(defun rotate-windows ()
  (interactive)
  (let ((start-positions (rotate-left (mapcar 'window-start (window-list))))
        (buffers (rotate-left (mapcar 'window-buffer (window-list)))))
    (mapcar* (lambda (window  buffer pos)
               (set-window-buffer window buffer)
               (set-window-start window pos))
             (window-list)
             buffers
             start-positions)))

;; ======================= MAKE ======================================
(global-set-key [(control return)] 'compile)

;; ======================= IEDIT =====================================
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

;; ==================== OPEN A LINE ==================================
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] 'smart-open-line)

;; ==================== EDIT REMOTE FILE =============================
(require 'tramp)
(setq tramp-default-method "scp")

;; ==================== Haskell ======================================
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; ==================== TIME-STAMP ===================================
;; when there is a "Time-stamp: <>" in the first 10 lines of the file,
;; emacs will write time-stamp information there when saving the file.
;; -------------------------------------------
(setq time-stamp-active t) ; do enable time-stamps
(setq time-stamp-line-limit 10) ; check first 10 buffer lines for
                                ; Time-stamp: <>
;something wrong here
;(setq time-stamp-format "Last modified yyyy-mm-dd HH:MM:SS by %L") ; date format
(add-hook 'before-save-hook 'time-stamp) ; update when saving

;; ==================== ETAGS ========================================
; ctag -e -R . 

;; ==================== FILL-COLUMN LENGTH ===========================
(setq fill-column 89) 

;; ========================== SEND MAIL  =============================
(setq user-mail-address "yufei.gu@utdallas.edu")

;; =========================== ORG-CRYPT =============================
(require 'org-crypt)
; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; GPG key to use for encryption
(setq org-crypt-key "ff") ;this doesn't work, I should enter password
                          ;every time when I was saving modified
                          ;entries
(setq org-crypt-disable-auto-save nil)

;; =========================== GNU GLOBAL =============================
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; ==================== SEARCH-BACK-CURRENT-WORD =====================
;; functions for kernel source code review
(defun search-back-current-word ()
  "search backward by current word"
  (interactive)
  (save-restriction
    (let (start end)
      (skip-chars-backward "-_A-Za-z0-9") (setq start (point))
      (skip-chars-forward "-_A-Za-z0-9") (setq end (point))
      (setq current-word  (buffer-substring start end))

      ;TODO put some strings handle here
      (set-mark start) ;mark the whole area, optional
      (goto-char start) 
      (search-backward  current-word nil t)
      )))

(global-set-key (kbd "C-q") 'search-back-current-word)


(defun open-file-line(name line)
   "open file and goto specified line"
  (switch-to-buffer-other-window name)
  (find-file name)
  (goto-line line)
  (hl-line-highlight))

;(open-file-line "test.c" 3)

(defun goto-src-line()
  (interactive)
  (setq my-str (buffer-substring (line-beginning-position) (line-end-position)))
  (setq my-list (delete "" (split-string my-str "\t")))
  (if (> (length my-list) 4)
   (progn 
     (open-file-line 
      ;(concat "../linux-2.6.32-rc8" (nth 4 my-list)) 
      (concat "./linux-3.2.52" (nth 4 my-list)) 
      (string-to-number (nth 3 my-list)))
     (other-window 1)
     (goto-line (+ 1 (line-number-at-pos)))))
)

(global-set-key (kbd "C-z") 'goto-src-line)

;; ==================== julia mode  ==================================
(require 'julia-mode)

;; ==================== eim  ==================================
(add-to-list 'load-path "~/.emacs.d/plugins/emacs-eim")
(autoload 'eim-use-package "eim" "Another emacs input method")
;; Tooltip 暂时还不好用, it may have problem in Windows or Mac, but it is OK for Linux, so I keep it.
;(setq eim-use-tooltip nil)

(register-input-method
 "eim-wb" "euc-cn" 'eim-use-package
 "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")

;; 用 ; 暂时输入英文
(require 'eim-extra)
(global-set-key ";" 'eim-insert-ascii)

(custom-set-variables '(default-input-method "eim-py"))

;; ==================== ESHELL  ==================================
(setq eshell-aliases-file "~/.emacs.d/eshell-alias")
;set completion in eshell case insensitive
(setq eshell-cmpl-ignore-case t)

;; clear the buffer in eshell
(defun eshell-clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(add-hook 'eshell-mode-hook
    (lambda ()
   (local-set-key "\C-l" 'eshell-clear))
)

;; ==================== DICT  ==================================
(defun bing-dict ()
  "clear the eshell buffer."
  (interactive)
  (save-restriction
    (let (start end)
      (skip-chars-backward "-_A-Za-z0-9") (setq start (point))
      (skip-chars-forward "-_A-Za-z0-9") (setq end (point))
      (setq current-word  (buffer-substring start end))
      (switch-to-buffer-other-window "dict") 
      (eww (concat "http://cn.bing.com/dict/search?q=" current-word))
      )))

(global-set-key (kbd "C-c q") 'bing-dict)
    
;; ==================== custom set  ==================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(custom-safe-themes (quote ("71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default)))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-2--C-mouse-2) t)
 '(fci-rule-color "#383838")
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtpauth.utdallas.edu")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
