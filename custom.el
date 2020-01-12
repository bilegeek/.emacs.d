;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom splitting functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))
(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

;;From https://old.reddit.com/r/lisp/comments/eng1yr/is_there_any_way_to_force_the_slime_repl_to_load/fe1dcob/
(defun my/with-buffers-in-same-window (f &rest args)
  (let ((display-buffer-function #'display-buffer-same-window))
    (apply f args)))

;;~~~~~

;;From https://emacs.stackexchange.com/questions/501/how-do-i-group-hooks

(defmacro hook-modes (modes &rest body)
  (declare (indent 1))
  `(--each ,modes
     (add-hook (intern (format "%s-hook" it))
               (lambda () ,@body))))

;;From https://old.reddit.com/r/emacs/comments/csut1x/weekly_tipstricketc_thread/exn5tvc/
(defun edit-file-with-sudo ()
  "Take the file currently being edited, and open it as root with `sudo'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

;;From https://old.reddit.com/r/emacs/comments/csut1x/weekly_tipstricketc_thread/exl8x76/
(defmacro list! (&rest args)
  "A convenience macro for creating a quoted list."
  `'(,@args))

;;~~~~~

;;From https://old.reddit.com/r/emacs/comments/cpq3ru/weekly_tipstricketc_thread/ewtvxhf/
(defun eval-region-if-active (arg)
  "Advice for evaluation commands, to have them call
`eval-region' when the region is activate."
  (when (use-region-p)
    (eval-region (region-beginning) (region-end) arg)
    (deactivate-mark)
    t))

(dolist (fn '(eval-print-last-sexp eval-last-sexp eval-defun))
  (advice-add fn :before-until #'eval-region-if-active))

;;~~~~~

(defun visual-line-line-range ()
  (save-excursion 
    (cons (progn (vertical-motion 0) (point)) 
          (progn (vertical-motion 1) (point)))))

(defun toggle-hl-line-range ()
  (interactive)
  (if (eq hl-line-range-function 'visual-line-line-range)
      (setq hl-line-range-function nil)
    (setq hl-line-range-function 'visual-line-line-range)))

;;~~~~~

;;From https://emacs.stackexchange.com/questions/5428/restart-emacs-from-within-emacs

(when (eq system-type 'gnu/linux) (progn
												(defun launch-separate-emacs-in-terminal ()
												  (suspend-emacs "fg ; emacs -nw"))
												(defun launch-separate-emacs-under-x ()
												  (call-process "sh" nil nil nil "-c" "emacs &"))
												(defun restart-emacs ()
												  (interactive)
												  ;; We need the new emacs to be spawned after all kill-emacs-hooks
												  ;; have been processed and there is nothing interesting left
												  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
																															  #'launch-separate-emacs-under-x
																															#'launch-separate-emacs-in-terminal)))))
													 (save-buffers-kill-emacs)))))

(defun edit-config()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;From https://www.blogger.com/comment.g?blogID=3992530807750384868&postID=7380396645565037070&bpli=1
(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list)) (delete-other-windows))

;;~~~~~

;;From https://emacs-fu.blogspot.com/search?updated-max=2009-01-31T09:39:00%2B02:00&max-results=7&start=91&by-date=false
(defun djcb-count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
        (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

;;From https://old.reddit.com/r/emacs/comments/eeyhdz/weekly_tipstricketc_thread/fcb5kw6/
(defun dm/stringified-mode-line () (interactive) (with-help-window "*Stringified Mode Line*" (with-current-buffer "*Stringified Mode Line*" (font-lock-mode -1) (insert (format-mode-line mode-line-format))))) 

;;From https://www.emacswiki.org/emacs/UnicodeEncoding
(defun unicode-insert (char)
  "Read a unicode code point and insert said character.
    Input uses `read-quoted-char-radix'.  If you want to copy
    the values from the Unicode charts, you should set it to 16."
  (interactive (list (read-quoted-char "Char: ")))
  (ucs-insert char))

;;Sets it so you can use hexadecimal Unicode codes.
(setq read-quoted-char-radix 16)

(defun what-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor.
Samples of valid input:

  ffff
  0xffff
  #xffff
  FFFF
  0xFFFF
  #xFFFF

Test cases
  64*0xc8+#x12c 190*0x1f4+#x258
  100 200 300   400 500 600"
  (interactive )

  (let (inputStr tempStr p1 p2 )
    (save-excursion
      (re-search-backward "[^0-9A-Fa-fx#]" nil t)
      (forward-char)
      (setq p1 (point) )
      (re-search-forward "[^0-9A-Fa-fx#]" nil t)
      (backward-char)
      (setq p2 (point) ) )

    (setq inputStr (buffer-substring-no-properties p1 p2) )

    (let ((case-fold-search nil) )
      (setq tempStr (replace-regexp-in-string "^0x" "" inputStr )) ; C, Perl, …
      (setq tempStr (replace-regexp-in-string "^#x" "" tempStr )) ; elisp …
      (setq tempStr (replace-regexp-in-string "^#" "" tempStr ))  ; CSS …
      )

    (message "Hex %s is %d" tempStr (string-to-number tempStr 16 ) )
    ))

(defun backward-delete-word ()
  "Delete a word backwards. Delete text from previous line only when
current line is empty. This behaviour is similar to the one used by
SublimeText/Atom/VSCode/etc."
  (interactive)
  (if (= 0 (current-column))
      (call-interactively #'backward-delete-char-untabify)
    (let ((point-after-bw (save-excursion (backward-word) (point))))
      (if (< (count-lines 1 point-after-bw) (count-lines 1 (point)))
          (delete-region (line-beginning-position) (point))
        (delete-region (point) point-after-bw)))))

(defun goto-last-edit ()
  "Go to the last edit made in the current buffer."
  (interactive)
  (unless (or (consp buffer-undo-list)
              (not buffer-undo-list))
    (user-error "Can't go to last edit: invalid undo list"))
  (let ((pos (catch 'loop
               (dolist (item buffer-undo-list)
                 (when (and (consp item)
                            (or (integerp (car item))
                                (stringp (car item))))
                   (throw 'loop (abs (cdr item))))))))
    (unless (or (null pos)
                (= (point) pos))
      (push-mark)
      (goto-char pos))))

;;From https://old.reddit.com/r/emacs/comments/dewzuy/weekly_tipstricketc_thread/f3be8kq/
(defun backward-up-sexp (a)
  "backwards up multiple sexps.
   prefix command interpretation:
     0    → to beginning of all nested sexps
     -    → to end of all nested sexps
     x|+x → x-times go back out of sexps to beginning
     -x   → x-times go out of sexps to end
     universal-command interpreted as +x"
  (interactive "P")
  (condition-case err
      (let ((arg)
            (loop))
        (cond
         ((null a) ;; back-up once
          (setq arg -1
                loop nil))
         ((eq a '-) ;; up to end of all sexps
          (setq arg 1
                loop t))
         ((numberp a)
          (cond 
           ((= a 0) ;; back-up to begin of all sexps
            (setq arg -1
                  loop t))
           (t (setq arg (- a) ;; do it a times
                    loop nil))))
         (t (setq arg (- (car a)) ;; interpret `universal-command'
                  loop nil)))
        (while (progn  ;; do-while loop
                 (up-list arg t t)
                 loop)))
    (scan-error ;; stay quiet
     nil)))

;;~~~~~

;;From https://old.reddit.com/r/emacs/comments/dg4941/how_to_get_line_at_point_with_character_point_is/f3a8osu/
(defun highlight-positions (buf &optional lst) (interactive (cl-loop with buf* = (read-buffer-to-switch "buffer: ") with lst* = nil until (>= (length lst*) 10) do (push (1+ (random (buffer-size))) lst*) finally return (list buf* lst*))) (pop-to-buffer "*highlight positions*") (erase-buffer) (mapc (lambda (pos) (with-current-buffer buf (save-excursion (goto-char pos) (let ((line (buffer-substring (line-beginning-position) (1+ (line-end-position)))) (linepos (- pos (line-beginning-position)))) (put-text-property linepos (1+ linepos) 'face 'bold-italic line) (with-current-buffer "*highlight positions*" (insert line)))))) lst))


;;~~~~~

;;From https://old.reddit.com/r/emacs/comments/dewzuy/weekly_tipstricketc_thread/f39y5is/
(cl-defmacro lambda* (&optional args &body body)
  "A `lambda' that allows for the closures created with it to
\"be byte-compiled\" (don't think it does in Emacs Lisp). ARGS can be 
any amount of symbol in a list (which it *must* be
a list of symbols), and BODY is everything else.

One must make sure to use the symbols in the list ARGS inside of the BODY.

This only works in these sorts of contexts:

    (setq plus-one (lambda* (x) (+ x 1)))

    (defvar *plus-three* (lambda* (x) (+ x 3)))

which can be used like so

    (funcall plus-one 4)
    (funcall *plus-three* 3)"
  (declare (indent defun) (debug t))
  (cl-declare (warn 0)
              (optimize ;; These honestly do nothing in Emacs Lisp, lol
               (speed 3)
               (space 1)
               (safety 1)))
  (cl-coerce `(lambda ,args ,@body) 'function))

;;;###autoload
(cl-defmacro λ! (&body body)
  "Expands to (lambda () (interactive) ,@body)."
  (declare (doc-string 1))
  (cl-coerce `(lambda () (interactive) ,@body) 'function))

;;;###autoload
(defalias 'lambda! 'λ!)

;;;###autoload
(defun λ!! (command &optional arg)
  "Expands to a command that interactively calls COMMAND with prefix ARG."
  (declare (doc-string 1))
  (lambda () (interactive)
    (let ((current-prefix-arg arg))
      (call-interactively command))))

;;;###autoload
(defalias 'lambda!! 'λ!!)

;;~~~~~

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2017-04-19"
  (interactive)
  (let (
        (deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning)
              $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]-_")
        (setq $p1 (point))
        (skip-chars-forward "[:alnum:]-_")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command select between any bracket chars, not the inner text of a bracket. For example, if text is

 (a(b)c▮)

 the selected char is “c”, not “a(b)c”.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-12-18"
  (interactive)
  (let (
        ($skipChars
         (if (boundp 'xah-brackets)
             (concat "^\"" xah-brackets)
           "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
        $pos
        )
    (skip-chars-backward $skipChars)
    (setq $pos (point))
    (skip-chars-forward $skipChars)
    (set-mark $pos)))

(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

when no selection,
• if cursor is on a bracket, select whole bracketed thing including bracket
• if cursor is on a quote, select whole quoted thing including quoted
• if cursor is on the beginning of line, select the line.
• else, select current word.

when there's a selection, the selection extension behavior is still experimental.
Roughly:
• if 1 line is selected, extend to next line.
• if multiple lines is selected, extend to next line.
• if a bracketed text is selected, extend to include the outer bracket. If there's no outer, select current line.

 to line, or bracket/quoted text,
or text block, whichever is the smallest.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-01-15"
  (interactive)
  (if (region-active-p)
      (progn
        (let (($rb (region-beginning)) ($re (region-end)))
          (goto-char $rb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (set-mark (line-beginning-position)))
              (progn
                (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq $rb (line-beginning-position))
            (progn
              (goto-char $rb)
              (let (($firstLineEndPos (line-end-position)))
                (cond
                 ((eq $re $firstLineEndPos)
                  (progn
                    (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< $re $firstLineEndPos)
                  (progn
                    (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> $re $firstLineEndPos)
                  (progn
                    (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char $re)
                    (if (eq (point) (line-end-position))
                        (progn
                          (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        (message "multiple lines but end is not eol. make it so" )
                        (goto-char $re)
                        (end-of-line)))))
                 (t (error "logic error 42946" ))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              (message "less than 1 line" )
              (end-of-line) ; select current line
              (set-mark (line-beginning-position))))
           (t (message "last resort" ) nil))))
    (progn
      (cond
       ((looking-at "\\s(")
        (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-at "\\s)")
        (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        (message "string quote")
        (mark-sexp)) ; string quote
       ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
        (message "beginning of line and not empty")
        (end-of-line)
        (set-mark (line-beginning-position)))
       ((or (looking-back "\\s_" 1) (looking-back "\\sw" 1))
        (message "left is word or symbol")
        (skip-syntax-backward "_w" )
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (mark-sexp))
       ((and (looking-at "\\s ") (looking-back "\\s " 1))
        (message "left and right both space" )
        (skip-chars-backward "\\s " ) (set-mark (point))
        (skip-chars-forward "\\s "))
       ((and (looking-at "\n") (looking-back "\n" 1))
        (message "left and right both newline")
        (skip-chars-forward "\n")
        (set-mark (point))
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next text block
       (t (message "just mark sexp" )
          (mark-sexp))
       ;;
       ))))

(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-11-01"
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))))

(defun xah-select-block ()
  "Select the current/next block of text between blank lines.
If region is active, extend selection downward by block.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-11-01"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move"))))

(defun xah-delete-current-text-block ()
  "Delete the current text block or selection, and copy to `kill-ring'.
A “block” is text between blank lines.

URL `http://ergoemacs.org/emacs/emacs_delete_block.html'
Version 2017-07-09"
  (interactive)
  (let ($p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (progn
        (if (re-search-backward "\n[ \t]*\n+" nil "move")
            (progn (re-search-forward "\n[ \t]*\n+")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (setq $p2 (point))))
    (kill-region $p1 $p2)))

(defun xah-space-to-newline ()
  "Replace space sequence to a newline char.
Works on current block or selection.

URL `http://ergoemacs.org/emacs/emacs_space_to_newline.html'
Version 2017-08-19"
  (interactive)
  (let* ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil t)
          (replace-match "\n" ))))))

(defun xah-change-bracket-pairs ( @from-chars @to-chars)
  "Change bracket pairs from one type to another.

For example, change all parenthesis () to square brackets [].

Works on selected text, or current text block.

When called in lisp program, @from-chars or @to-chars is a string of bracket pair. eg \"(paren)\",  \"[bracket]\", etc.
The first and last characters are used.
If the string contains “,2”, then the first 2 chars and last 2 chars are used, for example  \"[[bracket,2]]\".
If @to-chars is equal to string “none”, the brackets are deleted.

 If the string has length greater than 2, the rest are ignored.

URL `http://ergoemacs.org/emacs/elisp_change_brackets.html'
Version 2018-03-31"
  (interactive
   (let (($bracketsList
          '("(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "\"ascii quote\""
            "[[double square,2]]"
            "“curly quote”"
            "‘single quote’"
            "‹angle quote›"
            "«double angle quote»"
            "「corner」"
            "『white corner』"
            "【LENTICULAR】"
            "〖white LENTICULAR〗"
            "〈angle bracket〉"
            "《double angle bracket》"
            "〔TORTOISE〕"
            "⦅white paren⦆"
            "〚white square〛"
            "⦃white curly bracket⦄"
            "〈angle bracket〉"
            "⦑ANGLE BRACKET WITH DOT⦒"
            "⧼CURVED ANGLE BRACKET⧽"
            "⟦math square⟧"
            "⟨math angle⟩"
            "⟪math DOUBLE ANGLE BRACKET⟫"
            "⟮math FLATTENED PARENTHESIS⟯"
            "⟬math WHITE TORTOISE SHELL BRACKET⟭"
            "❛HEAVY SINGLE QUOTATION MARK ORNAMENT❜"
            "❝HEAVY DOUBLE TURNED COMMA QUOTATION MARK ORNAMENT❞"
            "❨MEDIUM LEFT PARENTHESIS ORNAMENT❩"
            "❪MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT❫"
            "❴MEDIUM LEFT CURLY BRACKET ORNAMENT❵"
            "❬MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT❭"
            "❮HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT❯"
            "❰HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT❱"
            "none"
            )))
     (list
      (ido-completing-read "Replace this:" $bracketsList )
      (ido-completing-read "To:" $bracketsList ))))
  (let ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil "move")
            (progn (re-search-backward "\n[ \t]*\n")
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ( (case-fold-search nil)
               $fromLeft
               $fromRight
               $toLeft
               $toRight)
          (cond
           ((string-match ",2" @from-chars  )
            (progn
              (setq $fromLeft (substring @from-chars 0 2))
              (setq $fromRight (substring @from-chars -2))))
           (t
            (progn
              (setq $fromLeft (substring @from-chars 0 1))
              (setq $fromRight (substring @from-chars -1)))))
          (cond
           ((string-match ",2" @to-chars)
            (progn
              (setq $toLeft (substring @to-chars 0 2))
              (setq $toRight (substring @to-chars -2))))
           ((string-match "none" @to-chars)
            (progn
              (setq $toLeft "")
              (setq $toRight "")))
           (t
            (progn
              (setq $toLeft (substring @to-chars 0 1))
              (setq $toRight (substring @to-chars -1)))))
          (cond
           ((string-match "markdown" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "`\\([^`]+?\\)`" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "tilde" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "~\\([^~]+?\\)~" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "ascii quote" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "\"\\([^\"]+?\\)\"" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "equal" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "=\\([^=]+?\\)=" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           (t (progn
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromLeft nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toLeft "FIXEDCASE" "LITERAL")))
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromRight nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toRight "FIXEDCASE" "LITERAL")))))))))))

(defun xah-insert-date ()
  "Insert current date time.
Insert date in this format: yyyy-mm-dd.
When called with `universal-argument', prompt for a format to use.
If there's text selection, delete it first.

URL `http://ergoemacs.org/emacs/elisp_insert-date-time.html'
version 2018-07-03"
  (interactive)
  (let (($style
         (if current-prefix-arg
             (string-to-number
              (substring
               (ido-completing-read
                "Style:"
                '(
                  "1 → 2018-04-12 Thursday"
                  "2 → 20180412224611"
                  "3 → 2018-04-12T22:46:11-07:00"
                  "4 → 2018-04-12 22:46:11-07:00"
                  "5 → Thursday, April 12, 2018"
                  "6 → Thu, Apr 12, 2018"
                  "7 → April 12, 2018"
                  "8 → Apr 12, 2018"
                  )) 0 1))
           0
           )))
    (when (use-region-p) (delete-region (region-beginning) (region-end)))
    (insert
     (cond
      ((= $style 0)
       ;; "2016-10-10"
       (format-time-string "%Y-%m-%d"))
      ((= $style 1)
       ;; "2018-04-12 Thursday"

       (format-time-string "%Y-%m-%d %A"))
      ((= $style 2)
       ;; "20180412224015"
       (replace-regexp-in-string ":" "" (format-time-string "%Y%m%d%T")))
      ((= $style 3)
       (concat
        (format-time-string "%Y-%m-%dT%T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z")))
       ;; "2018-04-12T22:45:26-07:00"
       )
      ((= $style 4)
       (concat
        (format-time-string "%Y-%m-%d %T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z")))
       ;; "2018-04-12 22:46:11-07:00"
       )
      ((= $style 5)
       (format-time-string "%A, %B %d, %Y")
       ;; "Thursday, April 12, 2018"
       )
      ((= $style 6)
       (format-time-string "%a, %b %d, %Y")
       ;; "Thu, Apr 12, 2018"
       )
      ((= $style 7)
       (format-time-string "%B %d, %Y")
       ;; "April 12, 2018"
       )
      ((= $style 8)
       (format-time-string "%b %d, %Y")
       ;; "Apr 12, 2018"
       )
      (t
       (format-time-string "%Y-%m-%d"))))))

;;~~~~~

;;From https://old.reddit.com/r/emacs/comments/cmnumy/weekly_tipstricketc_thread/
(defun yank-rectangle-as-newlines+ (&optional pos)
  (interactive)
  (let ((pos (or pos (point))))
    (save-restriction
      (narrow-to-region pos pos)
      (yank-rectangle))))

;;~~~~~

(defun my-eww-open-in-new-buffer (url)
  "Fetch URL in a new EWW buffer."
  (interactiev
   (let* ((uris (eww-suggested-uris))
			 (prompt (concat "Enter URL or keywords"
								  (if uris (format " (default %s)" (car uris)) "")
								  ": ")))
     (list (read-string prompt nil nil uris))))
  (setq url (eww--dwim-expand-url url))
  (with-current-buffer
      (if (eq major-mode 'eww-mode) (clone-buffer)
        (generate-new-buffer "*eww*"))
    (unless (equal url (eww-current-url))
      (eww-mode)
      (eww (if (consp url) (car url) url)))))

;;~~~~~

;;From https://old.reddit.com/r/emacs/comments/cgptj7/weekly_tipstricketc_thread/
(defun describe-keymap (keymap)
  "Describe keys bound in KEYMAP."
  (interactive
   (list
    (completing-read "Keymap: "
							(cl-loop for x being the symbols
										if (and (boundp x) (keymapp (symbol-value x)))
										collect (symbol-name x))
							nil t nil 'variable-name-history)))
  (help-setup-xref (list #'describe-keymap keymap)
                   (called-interactively-p 'interactive))
  (with-output-to-temp-buffer (help-buffer)
    (princ keymap) (terpri) (terpri)
    (let ((doc (documentation-property
                (intern keymap)
                'variable-documentation)))
      (when doc (princ doc) (terpri) (terpri)))
    (princ (substitute-command-keys (format "\\{%s}" keymap)))))

;; Adapted from https://github.com/xuchunyang/emacs.d/blob/5b51f2783d9b22d284617ce9e7d1d7b518461599/lisp/chunyang-simple.el#L197-L212
(defun display-keys+ ()
  "Make keymap display readable."
  (interactive)
  (if (catch 'toggle-key-ov
        (dolist (o (overlays-in (point-min) (point-max)))
          (when (overlay-get o 'toggle-key-ov)
            (throw 'toggle-key-ov t))))
      (remove-overlays (point-min) (point-max) 'toggle-key-ov t)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "([0-9]+" nil :no-error)
        (let ((ov (make-overlay (1+ (match-beginning 0)) (match-end 0))))
          (overlay-put ov 'display (single-key-description
                                    (string-to-number (substring (match-string 0) 1))))
          (overlay-put ov 'toggle-key-ov t))))))

;;~~~~~

(defun let-faces-be ()
  (interactive)
  (let ((faces '(mode-line
					  mode-line-buffer-id
					  mode-line-buffer-id-inactive
					  mode-line-emphasis
					  mode-line-highlight
					  mode-line-inactive)))
	 (mapc
	  (lambda (face) (set-face-attribute face nil :font "DejaVu Sans Mono"))
	  faces)))

(defun increase-modeline-font-size (size)
  (interactive)
  (let ((faces '(mode-line
					  mode-line-buffer-id
					  mode-line-buffer-id-inactive
					  mode-line-emphasis
					  mode-line-highlight
					  mode-line-inactive)))
	 (mapc
	  (lambda (face) (set-face-attribute face nil
													 :height
													 (+ (face-attribute face :height)
														 size)))
	  faces)))

(defun decrease-modeline-font-size (size)
  (interactive)
  (let ((faces '(mode-line
					  mode-line-buffer-id
					  mode-line-buffer-id-inactive
					  mode-line-emphasis
					  mode-line-highlight
					  mode-line-inactive)))
	 (mapc
	  (lambda (face) (set-face-attribute face nil
													 :height
													 (- (face-attribute face :height)
														 size)))
	  faces)))

;;http://blog.vivekhaldar.com/post/4809065853/dotemacs-extract-interactively-change-font-size
;;https://emacs.stackexchange.com/questions/10438/how-to-set-the-default-font-size
;;Plus a few modifications of my own from up above.

(defun my/zoom-in (size)
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         size)))

(defun my/zoom-out (size)
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (- (face-attribute 'default :height)
                         size)))

(defun zoomin10 ()
  (interactive)
  (increase-modeline-font-size 10)
  (my/zoom-in 10))
(defun zoomout10 ()
  (interactive)
  (decrease-modeline-font-size 10)
  (my/zoom-out 10))

(defun default-ir-black-crap ()
  (interactive)
  (set-cursor-color "#ffc000")
  (set-background-color "#000000")
  (set-face-attribute 'fringe nil :background "#303030")
  (set-face-background 'linum "#000000")
  (set-face-foreground 'linum "#ffffff")
  (set-face-background 'header-line "#303030")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  ;;(set-face-background 'show-paren-match "#0000e0")
  (set-face-background 'show-paren-match "#ff1493")
  (set-face-foreground 'show-paren-match "#ffffff")
  (set-face-attribute 'region nil :background "#006400"))
