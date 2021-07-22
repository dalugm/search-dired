;;; search-dired.el --- Enhance find-dired -*- lexical-binding: t -*-

;; Author: dalu <mou.tong@qq.com>
;; Maintainer: dalu <mou.tong@qq.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))
;; Homepage: https://www.github.com/dalugm/search-dired
;; Keywords: search, file, dired

;; This file is NOT part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Enhance `find-dired'.
;;

;;; Code:

(require 'dired)

(defgroup search-dired nil
  "Run a `search-dired-program' command and Dired the output."
  :group 'search-dired)

(defcustom search-dired-program
  (if (executable-find "fd") "fd" "find")
  "The default search-dired program."
  :group 'search-dired
  :type 'string)

(defcustom search-dired-exec-terminator
  (if (eq 0
          (ignore-errors
           (process-file search-dired-program nil nil nil
                         null-device "-exec" "echo" "{}" "+")))
      "+"
    (shell-quote-argument ";"))
  "String that terminates \"find -exec COMMAND {} \".
The value should include any needed quoting for the shell.
Common values are \"+\" and \"\\\\;\", with the former more
efficient than the latter.  Because the \"+\" variant of the
\"exec\" command executes \"ls\" on the whole file-list while
the default \";\" variant executes \"ls\" for each found file."
  :group 'search-dired
  :type 'string)

(defcustom search-dired-ls-option
  (if (string= search-dired-program "fd")
      (cons (concat "| xargs -0 "
                    insert-directory-program
                    " -ld --quoting-style=literal")
            "-ld")
    (cons "-ls" "-alhuG1v"))
  "A pair of options to produce and parse an `ls -l'-type list from `search-dired-program'."
  :type '(cons (string :tag "Search Option")
               (string :tag "Ls Switches"))
  :group 'search-dired)

(defcustom search-dired-fd-args "-0 --color never"
  "Pre args when using `fd'."
  :group 'search-dired
  :type 'string)

(defvar search-dired-args nil
  "Last arguments given to `search-dired-program'.")

(defvar search-dired-history nil
  "Search history.  Use `M-n' and `M-p' to switch between.")

;;;###autoload
(defun search-dired-dwim (file)
  "Search FILE under current working directory."
  (interactive "sSearch file (with args): ")
  (search-dired default-directory
    (if (string= search-dired-program "fd")
        (format "--type f %s" file)
      (format "-type f -name '*%s*'" file))))

(defun search-dired (dir args)
  "Run `search-dired-program' and Dired the output on a buffer.

The command run (after changing into DIR) is essentially

    `search-dired-program . ARGS -ls'

except that the car of the variable `search-dired-ls-option'
specifies what to use in place of \"-ls\" as the final argument."
  (interactive (list (read-directory-name "Search in directory: " nil "" t)
                     (read-string "Search file (with args): "
                                  search-dired-args
                                  '(search-dired-history . 1))))
  (let ((dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq dir (file-name-as-directory (expand-file-name dir)))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
        (error "Search-Dired needs a directory: %s" dir))
    (switch-to-buffer (get-buffer-create "*Search Dired*"))

    ;; See if there's already a `search-dired-program' running
    ;; If true, kill it first
    (let ((proc (get-buffer-process (current-buffer))))
      (when proc
        (if (or (not (eq (process-status proc) 'run))
                (yes-or-no-p
                 (format-message
                   "`search-dired-program' process is running; kill it? ")))
            (condition-case nil
                (progn
                  (interrupt-process proc)
                  (sit-for 1)
                  (delete-process proc))
              (error nil))
          (error "Cannot have two processes in `%s' at once"
                 (buffer-name)))))

    ;; prepare buffer
    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)

    ;; start process
    (setq default-directory dir
          search-dired-args args        ; save for next interactive call
          args (concat search-dired-program
                 (if (string= search-dired-program "fd")
                     (concat " " search-dired-fd-args)
                   " . ")
                 (if (string= args "")
                     ""
                   (concat
                     " " args " "))
                 (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|+\\)\\'"
                                   (car search-dired-ls-option))
                     (format
                       "%s %s %s"
                       (match-string 1 (car search-dired-ls-option))
                       (shell-quote-argument "{}")
                       search-dired-exec-terminator)
                   (car search-dired-ls-option))))
    ;; Start the `search-dired-program' process.
    (shell-command (concat args "&") (current-buffer))

    ;; enable Dired mode
    ;; The next statement will bomb in classic dired
    ;; (no optional arg allowed)
    (dired-mode dir (cdr search-dired-ls-option))
    ;; provide a keybinding to kill the search-dired-program
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map "\C-c\C-k" #'search-dired-kill-find)
      (use-local-map map))
    ;; disable Dired sort
    (make-local-variable 'dired-sort-inhibit)
    (setq dired-sort-inhibit t)
    (set (make-local-variable 'revert-buffer-function)
         `(lambda (ignore-auto noconfirm)
            (search-dired ,dir ,search-dired-args)))
    (set (make-local-variable 'dired-subdir-alist)
         (list (cons default-directory (point-min-marker))))
    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " dir ":\n")

    ;; Make second line a `find' line in analogy to the `total' or
    ;; `wildcard' line.
    (let ((point (point)))
      (insert "  " args "\n")
      (dired-insert-set-properties point (point)))
    (setq buffer-read-only t)
    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-filter proc (function search-dired-filter))
      (set-process-sentinel proc (function search-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) (point) (current-buffer)))))

(defun search-dired-kill-find ()
  "Kill the `search-dired-program' process running in the current buffer."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc (eq (process-status proc) 'run)
      (eq (process-filter proc) (function search-dired-filter))
      (condition-case nil
        (delete-process proc)
        (error nil)))))

(defun search-dired-filter (proc string)
  "Use PROC to filter STRING for `search-dired' processes."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (let ((buffer-read-only nil)
                    (beg (point-max))
                    (l-opt
                     (and (consp search-dired-ls-option)
                          (string-match "l"
                                        (cdr search-dired-ls-option))))
                    (ls-regexp
                     (concat
                      "^ +[^ \t\r\n]+\\( +[^ \t\r\n]+\\) +"
                      "[^ \t\r\n]+ +[^ \t\r\n]+\\( +[^[:space:]]+\\)")))
                (goto-char beg)
                (insert string)
                (goto-char beg)
                (or (looking-at "^")
                    (forward-line 1))
                (while (looking-at "^")
                  (insert "  ")
                  (forward-line 1))
                ;; Convert ` ./FILE' to ` FILE'
                ;; This would lose if the current chunk of output
                ;; starts or ends within the ` ./', so back up a bit:
                (goto-char (- beg 3))   ; no error if < 0
                (while (search-forward " ./" nil t)
                  (delete-region (point) (- (point) 2)))
                ;; Pad the number of links and file size.  This is a
                ;; quick and dirty way of getting the columns to line up
                ;; most of the time, but it's not foolproof.
                (when l-opt
                  (goto-char beg)
                  (goto-char (line-beginning-position))
                  (while (re-search-forward ls-regexp nil t)
                    (replace-match (format "%4s" (match-string 1))
                                   nil nil nil 1)
                    (replace-match (format "%9s" (match-string 2))
                                   nil nil nil 2)
                    (forward-line 1)))
                ;; Find all the complete lines in the unprocessed
                ;; output and process it to add text properties.
                (goto-char (point-max))
                (when (search-backward "\n" (process-mark proc) t)
                  (progn
                    (dired-insert-set-properties (process-mark proc)
                                                 (1+ (point)))
                    (move-marker (process-mark proc) (1+ (point)))))))))
      ;; The buffer has been killed.
      (delete-process proc))))

(defun search-dired-sentinel (proc state)
  "Use PROC to sentinel STATE for `search-dired' processes."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (when (buffer-name buf)
      (with-current-buffer buf
        (let ((buffer-read-only nil))
          (save-excursion
            (goto-char (point-max))
            (let ((point (point)))
              (insert "\n"
                (substring state 0 -1)  ; omit '\n' at end of STATE.
                " at " (substring (current-time-string) 0 19))
              (dired-insert-set-properties point (point)))
            (delete-process proc)))
        (message "search-dired finished.")))))

(provide 'search-dired)

;;; search-dired.el ends here
