;;; dev-init.el --- Interactive dev init for erc-matterircd  -*- lexical-binding: t; -*-
;; Used by test/integration/dev.sh.  Reads connection details from the
;; environment so this file can be checked in without generated content.
;;
;; Environment variables read (all have sensible defaults):
;;   REPO_ROOT       path to the repository root (for load-path)
;;   MM_URL          Mattermost base URL
;;   IRC_HOST        matterircd host
;;   IRC_PORT        matterircd IRC port
;;   MM_TEAM         Mattermost team name
;;   MM_NICK         IRC nick / Mattermost username
;;   MM_PASS         Mattermost password
;;   MM_CHANNEL      IRC channel name (without #)
;;   MATTERIRCD_LOG  path to the matterircd log file (for the startup message)

;;; Code:

;; ── basics ────────────────────────────────────────────────────────────────────
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(column-number-mode 1)

;; ── load erc-matterircd from the working tree ─────────────────────────────────
(add-to-list 'load-path
             (or (getenv "REPO_ROOT")
                 (expand-file-name "../.." (file-name-directory load-file-name))))
(require 'erc-matterircd)

;; ── configure the matterircd ERC module ──────────────────────────────────────
(add-to-list 'erc-modules 'matterircd)
(erc-update-modules)

(let* ((mm-url     (or (getenv "MM_URL")     "http://localhost:8065"))
       (irc-host   (or (getenv "IRC_HOST")   "127.0.0.1"))
       (irc-port   (string-to-number (or (getenv "IRC_PORT") "6667")))
       (mm-team    (or (getenv "MM_TEAM")    "testteam"))
       (mm-nick    (or (getenv "MM_NICK")    "testuser"))
       (mm-pass    (or (getenv "MM_PASS")    "TestUser1!"))
       (mm-channel (or (getenv "MM_CHANNEL") "testchannel")))

  (setq erc-matterircd-server   (replace-regexp-in-string "^https?://" "" mm-url)
        erc-matterircd-team     mm-team
        erc-matterircd-password mm-pass
        erc-prompt-for-password nil
        erc-matterircd-suppress-mattermost-responses nil
        erc-fill-column 160)

  (defun dev/connect ()
    "Connect ERC to the local test matterircd instance."
    (interactive)
    (erc :server irc-host :port irc-port :nick mm-nick :full-name mm-nick))

  (add-hook 'emacs-startup-hook #'dev/connect)

  (message "")
  (message "erc-matterircd dev environment")
  (message "  Mattermost: %s  (team: %s)" mm-url mm-team)
  (message "  IRC:        %s:%d" irc-host irc-port)
  (message "  Nick:       %s  password: %s" mm-nick mm-pass)
  (message "  Channel:    #%s" mm-channel)
  (message "")
  (message "Run M-x dev/connect to reconnect if needed.")
  (when-let ((log (getenv "MATTERIRCD_LOG")))
    (message "Tail matterircd logs: %s" log)))

;;; dev-init.el ends here
