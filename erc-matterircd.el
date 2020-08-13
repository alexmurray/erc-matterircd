;;; erc-matterircd.el --- matterircd integration for ERC         -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Alex Murray

;; Author: Alex Murray <murray.alex@gmail.com>
;; Maintainer: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/alexmurray/erc-matterircd
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

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

;; Helpers to cleanup /gif and easy display via erc-image (although this is
;; not required, it is recommended), also will automatically connect to
;; mattermost when connecting to a matterircd server, and finally ensures @
;; is prepended when completing nicknames in when connected to matterircd.

;; (require 'erc-matterircd)
;; (setq erc-matterircd-server "mattermost.server")
;; (setq erc-matterircd-team "mytest")
;; (setq erc-matterircd-password "password")
;; (add-to-list 'erc-modules 'matterircd)
;; (erc-update-modules)
;;
;; Then connect to matterircd as a normal erc server:
;; (erc :server "localhost" :port "6667" :nick "mynick")

;;; Code:
(require 'erc)
(require 'erc-button)
(require 'erc-networks)
(require 'erc-pcomplete)

(defgroup erc-matterircd nil
  "Integrate ERC with matterircd"
  :group 'erc)

(defcustom erc-matterircd-server nil
  "The mattermost server to connect to via matterircd."
  :group 'erc-matterircd
  :type 'string)

(defcustom erc-matterircd-team nil
  "The mattermost team to connect to via matterircd."
  :group 'erc-matterircd
  :type 'string)

(defcustom erc-matterircd-password nil
  "The password to use for mattermost to connect via matterircd."
  :group 'erc-matterircd
  :type 'string)

(defun erc-matterircd-connect-to-mattermost (server nick)
  "Try login to mattermost on SERVER with NICK."
  ;; server should contain matterircd somewhere in it
  (when (string-match-p "matterircd" server)
    (erc-message "PRIVMSG"
                 (format "mattermost login %s %s %s %s"
                         erc-matterircd-server erc-matterircd-team
                         nick erc-matterircd-password))))

(defvar erc-matterircd-view-gif-url-function #'browse-url
  "Function used to view /gif URLs.")

(defun erc-matterircd-cleanup-gifs ()
  "Cleanup gifs sent via matterircd.
For each /gif we see two messages:

 */gif [name](URL)*
![GIF for 'name'](URL)

In mattermost this is shown as:
/gif name
[image]

So we want to rewrite the first to show similarly (and make the
[name] bit be a button to link to the image, and then for the
second, just leave the URL and let erc-image do the hard work."
  (when (eq' 'matterircd (erc-network))
    (goto-char (point-min))
    (while (re-search-forward "\\*\\/gif \\[\\(.*\\)\\](\\(.*\\))\\*" nil t)
      (let ((name (match-string-no-properties 1))
            (url (match-string-no-properties 2)))
        (let (start end)
          (delete-region (match-beginning 0) (match-end 0))
          (insert "/gif ")
          (setq start (point))
          (insert name)
          (setq end (point))
          (erc-button-add-button start end
                                 erc-matterircd-view-gif-url-function
                                 nil
                                 (list url))))))
  (goto-char (point-min))
  (while (re-search-forward "!\\[GIF for '.*'\\](\\(.*\\))" nil t)
    (let ((url (match-string-no-properties 1)))
      (let (start end)
        (delete-region (match-beginning 0) (match-end 0))
        (insert url)))))

(defun erc-matterircd-format-actions ()
  "Format /me actions correctly.
For each /me we see the message as
*message*

In mattermost this is shown as italic, so rewrite it to use
italics instead."
  (when (eq 'matterircd (erc-network))
    (goto-char (point-min))
    (while (re-search-forward "\\*\\(.*\\))\\*" nil t)
      (let ((message (match-string-no-properties 1)))
        (delete-region (match-beginning 0) (match-end 0))
        ;; erc-italic-face is only in very recent emacs 28 so use italic
        ;; for now
        (insert (propertize message 'face 'italic))))))

(defun erc-matterircd-pcomplete-erc-nicks (orig-fun &rest args)
  "Advice for `pcomplete-erc-nicks' to prepend an @ via ORIG-FUN and ARGS."
  (let ((nicks (apply orig-fun args)))
    (if (eq 'matterircd (erc-network))
        (mapcar #'(lambda (nick) (concat "@" nick)) nicks)
      nicks)))

(define-erc-module matterircd nil
  "Integrate ERC with matterircd"
  ((add-to-list 'erc-networks-alist '(matterircd "matterircd.*"))
   (advice-add #'pcomplete-erc-nicks :around #'erc-matterircd-pcomplete-erc-nicks)
   (add-hook 'erc-after-connect #'erc-matterircd-connect-to-mattermost)
   ;; use a depth of -99 so we get added first and -98 so gif cleanup
   ;; occurs before actions
   (add-hook 'erc-insert-modify-hook 'erc-matterircd-cleanup-gifs -99)
   (add-hook 'erc-insert-modify-hook 'erc-matterircd-format-actions -98)
   ;; we want to make sure we come before erc-image-show-url in
   ;; erc-insert-modify-hook
   (when (member 'erc-image-show-url erc-insert-modify-hook)
     ;; remove and re-add to get appended
     (remove-hook 'erc-insert-modify-hook 'erc-image-show-url)
     (add-hook 'erc-insert-modify-hook 'erc-image-show-url t)))
  ((remove-hook 'erc-after-connect 'erc-matterircd-connect-to-mattermost)
   (remove-hook 'erc-insert-modify-hook 'erc-matterircd-format-actions)
   (remove-hook 'erc-insert-modify-hook 'erc-matterircd-cleanup-gifs)
   (advice-remove 'pcomplete-erc-nicks 'erc-matterircd-pcomplete-erc-nicks))
  t)

(provide 'erc-matterircd)
;;; erc-matterircd.el ends here


