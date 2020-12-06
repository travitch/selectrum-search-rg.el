;;; selectrum-search-rg.el -- Interface for searching a project (or the current directory) via selectrum -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an interface to ripgrep results using completing-read.
;; It is adapted from the selectrum wiki
;; (https://github.com/raxod502/selectrum/wiki/Useful-Commands#search-with-ripgrep-like-counsel-rg).

;;;; Setup

;; (use-package selectrum-search-rg
;;   :commands (selectrum-search-rg))

(require 'selectrum)
(require 'rg)
(require 'compile)

(defvar selectrum-search-rg-history nil)

(defun selectrum-search-rg-make-action (input dir)
  (lambda (c)
    ;; jump to current candidate in the *rg* buffer.
    ;; rg implemented with `compile', so I make it work like below.
    ;; let-bound method not working, unkown reason.
    (let ((old-compilation-finish-functions compilation-finish-functions))
      (setq compilation-finish-functions
            (list
             (lambda (_a _b)
               (unwind-protect
                    (progn
                      (pop-to-buffer (current-buffer))
                      (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" c)
                        (let ((file-name (match-string-no-properties 1 c))
                              (line-number (match-string-no-properties 2 c)))
                          (if rg-group-result
                              (progn
                                (re-search-forward (format "^File: %s" file-name) nil t)
                                (re-search-forward (format "^ *%s" line-number) nil t)
                                (re-search-forward input (point-at-eol) t))
                            (re-search-forward (format "%s:%s:" file-name line-number) nil t)
                            (re-search-forward input (point-at-eol) t)))))
                 (setq compilation-finish-functions old-compilation-finish-functions)))))
      ;; dispatch to rg.el search.
      (rg input "*" dir))))


;;;###autoload
(defun selectrum-search-rg ()
  "Search like 'counsel-rg'.

Searches the current project (as reported by projectile) if any;
otherwise uses the current working directory.

'C-c C-o' to pop the rg.el's Occur view, make sure package `rg' is installed."
  (interactive)
  (unless (executable-find "rg")
    (user-error "ripgrep must be installed and in PATH."))
  (let* (type
         input
         (project-dir (ignore-errors (projectile-project-root)))
         (dir (if project-dir project-dir default-directory))
         (word (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 nil))
         (command (if (memq system-type '(ms-dos windows-nt))
                      "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R> ."
                    "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R>"))
         (cands (lambda (in)
                  (let ((msg nil)
                        (prop (lambda (cs)
                                (mapcar (lambda (c)
                                          (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):" c)
                                            (add-face-text-property (match-beginning 1) (match-end 1) 'compilation-info nil c)
                                            (add-face-text-property (match-beginning 2) (match-end 2) '(:underline t :inherit compilation-line-number) nil c))
                                          c)
                                        cs))))
                    (setq type nil)
                    (when (< (length in) 3)
                      (setq msg "Search requires at least three characters"))
                    ;; take space in INPUT as .*?
                    ;; take m-space as [[:blank:]]
                    (setq input
                          (replace-regexp-in-string
                           " +" "[[:blank:]]"
                           (replace-regexp-in-string
                            "\\([^ ]\\) \\([^ ]\\)" "\\1.+?\\2"
                            (string-trim in))))
                    (if msg
                        (prog1 nil
                          (setq-local selectrum-refine-candidates-function
                                      (lambda (_ __) (list msg))))
                      (kill-local-variable 'selectrum-refine-candidates-function)
                      (let* ((default-directory dir)
                             (cs (split-string
                                  (shell-command-to-string (grep-expand-template command input)) "\n")))
                        `((candidates . ,(funcall prop cs))
                          (input . ,input)))))))
         (cand (let ((selectrum-should-sort-p nil)
                     (selectrum-minibuffer-bindings (make-sparse-keymap)))
                 (selectrum-read "rg: " cands
                                 :initial-input word
                                 :may-modify-candidates t
                                 :history 'selectrum-search-rg-history
                                 :require-match t))))
    (if (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
        (let ((file-name (match-string-no-properties 1 cand))
              (line-number (match-string-no-properties 2 cand)))
          (xref-push-marker-stack) ; use M-, to go back!
          (find-file (expand-file-name file-name dir))
          (goto-char (point-min))
          (forward-line (1- (string-to-number line-number)))
          (re-search-forward input (point-at-eol) t)
          (recenter))
      (message "Bad candidate?"))))

(provide 'selectrum-search-rg)
;;; selectrum-search-rg.el ends here
