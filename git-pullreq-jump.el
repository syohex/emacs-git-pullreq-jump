;;; git-pullreq-jump.el --- Jump github pull request page -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-git-pullreq-jump
;; Version: 0.01
;; Package-Requires: ((emacs "26.3"))

;; This program is free software; you can redistribute it and/or modify
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

;; Jump to pull request page where current line is changed. This is useful when you want to
;; know why this line is changed.
;;
;; inspired from https://gist.github.com/kazuho/eab551e5527cb465847d6b0796d64a39

;;; Code:

(require 'cl-lib)
(require 'browse-url)

(defun git-pullreq-jump--committed-line-p (commit-id)
  ;; 'git blame' shows that un-committed line is '000000000'
  (not (string-match-p "\\`\\(?:0+\\|-\\)\\'" commit-id)))

(defun git-pullreq-jump--first-parent (file line)
  (let ((args (list "--no-pager" "blame" "-w" "-L" (format "%d,+1" line)
                    "--first-parent" (file-name-nondirectory file))))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "Failed to execute git blame --first-parent"))
      (goto-char (point-min))
      (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
             (parent-id (cl-first (split-string line))))
        (unless (git-pullreq-jump--committed-line-p parent-id)
          (error "This line is not committed yet"))
        parent-id))))

(defun git-pullreq-jump--find-pr-number (commit-id)
  (let ((args (list "--no-pager" "show" "--oneline" commit-id)))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "Failed to execute git show --oneline"))
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward "Merge\\s-+\\(?:pull\\s-+request\\|pr\\)\\s-+#\\([0-9]+\\)")
          (string-to-number (match-string-no-properties 1)))))))

(defun git-pullreq-jump--convert-to-https-url (url)
  (replace-regexp-in-string
   "\\.git\\'" ""
   (replace-regexp-in-string
    "github\\.com:" "github.com/"
    (replace-regexp-in-string "\\`git@" "https://" url))))

(defun git-pullreq-jump--remote-url ()
  (with-temp-buffer
    (unless (zerop (process-file "git" nil t nil "remote" "get-url" "origin"))
      (error "Failed to get origin url"))
    (goto-char (point-min))
    (let ((url (buffer-substring-no-properties (point) (line-end-position))))
      (if (not (string-match-p "\\`git@" url))
          url
        (git-pullreq-jump--convert-to-https-url url)))))

(defun git-pullreq-jump--pullreq-url (pr-number)
  (let ((remote-url (git-pullreq-jump--remote-url)))
    (format "%s/pull/%d" remote-url pr-number)))

;;;###autoload
(defun git-pullreq-jump ()
  (interactive)
  (let* ((file (buffer-file-name (buffer-base-buffer)))
         (line (line-number-at-pos))
         (parent-id (git-pullreq-jump--first-parent file line))
         (pr-number (git-pullreq-jump--find-pr-number parent-id))
         (pr-url (git-pullreq-jump--pullreq-url pr-number)))
    (browse-url pr-url)))

(provide 'git-pullreq-jump)

;;; git-pullreq-jump.el ends here
