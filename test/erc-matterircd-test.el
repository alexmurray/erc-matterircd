;;; erc-matterircd-test.el --- tests for matterircd integration for ERC         -*- lexical-binding: t; -*-

;;; Commentary:

(require 'ert)
(require 'erc-matterircd)

;;; Code:
(ert-deftest erc-matterircd-test-italics ()
  "Test that *italic* / _italic_ gets handled appropriately."
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert " *italic asterisk* _italic underscore_ ")
      (erc-matterircd-format-italics)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat " " (propertize "*italic asterisk*"
                                       'display
                                       (propertize "italic asterisk" 'face erc-matterircd-italic-face)
                                       'rear-nonsticky t)
                       " " (propertize "_italic underscore_"
                                       'display
                                       (propertize "italic underscore" 'face erc-matterircd-italic-face)
                                       'rear-nonsticky t)
                       " ")))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert "*italic asterisk* _italic underscore_")
      (erc-matterircd-format-italics)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat (propertize "*italic asterisk*"
                                   'display
                                   (propertize "italic asterisk" 'face erc-matterircd-italic-face)
                                   'rear-nonsticky t)
                       " "
                       (propertize "_italic underscore_"
                                   'display
                                   (propertize "italic underscore" 'face erc-matterircd-italic-face)
                                   'rear-nonsticky t))))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert "foo_bar_baz")
      (erc-matterircd-format-italics)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               "foo_bar_baz"))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert "foo_bar baz_ ")
      (erc-matterircd-format-italics)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               "foo_bar baz_ ")))))

(ert-deftest erc-matterircd-test-bolds ()
  "Test that **bold** / __bold__ gets handled appropriately."
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert " **bold asterisk** __bold underscore__ ")
      (erc-matterircd-format-bolds)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat " " (propertize "**bold asterisk**"
                                       'display
                                       (propertize "bold asterisk" 'face 'erc-bold-face)
                                       'rear-nonsticky t)
                       " " (propertize "__bold underscore__"
                                       'display
                                       (propertize "bold underscore" 'face 'erc-bold-face)
                                       'rear-nonsticky t)
                       " ")))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert "**bold asterisk** __bold underscore__")
      (erc-matterircd-format-bolds)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat (propertize "**bold asterisk**"
                                   'display
                                   (propertize "bold asterisk" 'face 'erc-bold-face)
                                   'rear-nonsticky t)
                       " "
                       (propertize "__bold underscore__"
                                   'display
                                   (propertize "bold underscore" 'face 'erc-bold-face)
                                   'rear-nonsticky t))))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert "foo__bar__baz")
      (erc-matterircd-format-bolds)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               "foo__bar__baz"))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert "foo__bar baz__ ")
      (erc-matterircd-format-bolds)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               "foo__bar baz__ ")))))

(ert-deftest erc-matterircd-test-strikethroughs ()
  "Test that ~~strikethrough~~_ gets handled appropriately."
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert " ~~strikethrough~~ ")
      (erc-matterircd-format-strikethroughs)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat " " (propertize "~~strikethrough~~"
                                       'display
                                       (propertize "strikethrough" 'face 'erc-matterircd-strikethrough-face)
                                       'rear-nonsticky t)
                       " "))))))

(ert-deftest erc-matterircd-test-monospace ()
  "Test that `monospace` gets handled appropriately."
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert " `monospace` ")
      (erc-matterircd-format-monospace)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat " " (propertize "`monospace`"
                                       'display
                                       (propertize "monospace" 'face 'erc-matterircd-monospace-face)
                                       'rear-nonsticky t) " ")))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert " `` ")
      (erc-matterircd-format-monospace)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               " `` "))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert " ``` ")
      (erc-matterircd-format-monospace)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               " ``` ")))))

(ert-deftest erc-matterircd-test-links ()
  "Test that [link](url) gets handled appropriately."
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert " [text](url) ")
      (erc-matterircd-format-links)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat " " (propertize "[text](url)"
                                       'display "text"
                                       'erc-matterircd-link-url "url"
                                       'help-echo "url"
                                       'rear-nonsticky t)
                       " ")))
      (erc-matterircd-buttonize-from-text-properties)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat " " (propertize "[text](url)" 'display "text"
                                       'font-lock-face 'erc-button
                                       'mouse-face 'highlight
                                       'erc-callback 'browse-url-button-open-url
                                       'erc-matterircd-link-url "url"
                                       'help-echo "url"
                                       'keymap erc-button-keymap
                                       'rear-nonsticky t
                                       'erc-data '("url"))
                       " "))))))

(ert-deftest erc-matterircd-test-context-ids ()
  "Test that [001] gets handled appropriately."
  (dolist (replace '(nil "-"))
    (with-temp-buffer
      (cl-letf (((symbol-function 'erc-network)
                 (lambda () 'matterircd)))
        (insert " foo [001]")
        (let ((erc-matterircd-replace-context-id replace)
              (props (list 'erc-matterircd-context-id "001"
                           'erc-matterircd-source " foo [001]"
                           'help-echo "[001]"
                           'rear-nonsticky t))
              (suffix "[001]"))
          (erc-matterircd-format-contexts)
          (if replace
              (setq props (append props (list 'display replace
                                              'emojify-inhibit t))))
          (set-text-properties 0 (length suffix)
                               props suffix)
          (should (ert-equal-including-properties
                   (buffer-substring (point-min) (point-max))
                   (concat " foo " suffix)))
          (erc-matterircd-buttonize-from-text-properties)
          (setq props (append props (list 'font-lock-face 'erc-button
                                          'mouse-face 'highlight
                                          'erc-callback 'erc-matterircd-reply-to-context-id
                                          'keymap erc-button-keymap
                                          'rear-nonsticky t
                                          'erc-data '("001"))))
          (set-text-properties 0 (length suffix)
                               props suffix)
          (should (ert-equal-including-properties
                   (buffer-substring (point-min) (point-max))
                   (concat " foo " suffix))))))))

(ert-deftest erc-matterircd-test-cleanup-gifs ()
  "Test that GIF lines are reduced to just the URL."
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert "![GIF for 'foo'](http://example.com/foo.gif)")
      (erc-matterircd-cleanup-gifs)
      (should (equal (buffer-string) "http://example.com/foo.gif"))))
  ;; non-matterircd is a no-op
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'other)))
      (insert "![GIF for 'foo'](http://example.com/foo.gif)")
      (erc-matterircd-cleanup-gifs)
      (should (equal (buffer-string) "![GIF for 'foo'](http://example.com/foo.gif)")))))

(ert-deftest erc-matterircd-test-reactions ()
  "Test that reaction names are wrapped in colons."
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert "added reaction: thumbsup")
      (erc-matterircd-format-reactions)
      (should (equal (buffer-string) "added reaction: :thumbsup:"))))
  ;; multiple reactions on separate lines
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert "added reaction: +1\nadded reaction: heart")
      (erc-matterircd-format-reactions)
      (should (equal (buffer-string) "added reaction: :+1:\nadded reaction: :heart:"))))
  ;; non-matterircd is a no-op
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'other)))
      (insert "added reaction: thumbsup")
      (erc-matterircd-format-reactions)
      (should (equal (buffer-string) "added reaction: thumbsup")))))

(ert-deftest erc-matterircd-test-scrollback ()
  "Test that /SCROLLBACK sends the correct PRIVMSG."
  (let (sent-type sent-msg)
    (cl-letf (((symbol-function 'erc-network) (lambda () 'matterircd))
              ((symbol-function 'erc-default-target) (lambda () "#channel"))
              ((symbol-function 'erc-message)
               (lambda (type msg &rest _) (setq sent-type type sent-msg msg))))
      ;; defaults to 10 lines when no argument given
      (erc-matterircd-scrollback nil)
      (should (equal sent-type "PRIVMSG"))
      (should (equal sent-msg "mattermost scrollback #channel 10"))
      ;; ERC passes command arguments as strings, not integers
      (erc-matterircd-scrollback "5")
      (should (equal sent-msg "mattermost scrollback #channel 5"))
      ;; zero and non-numeric strings fall back to the default 10 lines
      (erc-matterircd-scrollback "0")
      (should (equal sent-msg "mattermost scrollback #channel 10"))
      (erc-matterircd-scrollback "abc")
      (should (equal sent-msg "mattermost scrollback #channel 10"))
      ;; float strings are truncated to an integer (not passed raw to %d)
      (erc-matterircd-scrollback "5.2")
      (should (equal sent-msg "mattermost scrollback #channel 5"))))
  ;; non-matterircd shows an error
  (let (display-args)
    (cl-letf (((symbol-function 'erc-network) (lambda () 'other))
              ((symbol-function 'erc-display-message)
               (lambda (&rest args) (setq display-args args))))
      (with-temp-buffer
        (erc-matterircd-scrollback nil)
        (should (equal display-args `(nil error ,(current-buffer) not-matterircd)))))))

(ert-deftest erc-matterircd-test-PRIVMSG ()
  "Test that PRIVMSG from the mattermost user is intercepted correctly."
  ;; non-mattermost sender is not handled
  (should (null (erc-matterircd-PRIVMSG
                 nil (make-erc-response :sender "other!other@localhost"
                                        :contents "login OK"))))
  ;; "login OK" respects the suppress setting
  (let ((erc-matterircd-suppress-mattermost-responses t))
    (should (eq t (erc-matterircd-PRIVMSG
                   nil (make-erc-response :sender "mattermost!mattermost@localhost"
                                          :contents "login OK")))))
  (let ((erc-matterircd-suppress-mattermost-responses nil))
    (should (null (erc-matterircd-PRIVMSG
                   nil (make-erc-response :sender "mattermost!mattermost@localhost"
                                          :contents "login OK")))))
  ;; unrecognised content from mattermost is not handled
  (should (null (erc-matterircd-PRIVMSG
                 nil (make-erc-response :sender "mattermost!mattermost@localhost"
                                        :contents "some unknown message"))))
  ;; "set viewed for channel" removes that channel from pending requests
  ;; and returns the suppress value
  (let ((erc-matterircd-suppress-mattermost-responses t)
        (erc-matterircd--pending-requests (list (cons "#test" 0)))
        (erc-matterircd--pending-requests-timer nil))
    (should (eq t (erc-matterircd-PRIVMSG
                   nil (make-erc-response :sender "mattermost!mattermost@localhost"
                                          :contents "set viewed for test"))))
    (should (null erc-matterircd--pending-requests)))
  ;; "set viewed for channel" cancels a running timer
  (let* ((timer (run-with-timer 100 nil #'ignore))
         (erc-matterircd--pending-requests (list (cons "#test" 0)))
         (erc-matterircd--pending-requests-timer timer))
    (unwind-protect
        (progn
          (erc-matterircd-PRIVMSG nil (make-erc-response
                                       :sender "mattermost!mattermost@localhost"
                                       :contents "set viewed for test"))
          (should (null erc-matterircd--pending-requests-timer)))
      (when (timerp timer) (cancel-timer timer))))
  ;; "set viewed for channel" with more pending triggers updatelastviewed for next
  (let ((next-channel nil)
        (erc-matterircd--pending-requests (list (cons "#test" 0) (cons "#next" 1)))
        (erc-matterircd--pending-requests-timer nil))
    (cl-letf (((symbol-function 'erc-get-buffer) (lambda (_) (current-buffer)))
              ((symbol-function 'erc-matterircd-updatelastviewed)
               (lambda (ch _force) (setq next-channel ch))))
      (erc-matterircd-PRIVMSG nil (make-erc-response
                                   :sender "mattermost!mattermost@localhost"
                                   :contents "set viewed for test"))
      (should (equal next-channel "#next"))
      (should (equal erc-matterircd--pending-requests (list (cons "#next" 1)))))))

(ert-deftest erc-matterircd-test-context-ids-scan ()
  "Test that context IDs are correctly scanned from buffer text properties."
  ;; no properties returns nil
  (with-temp-buffer
    (insert "no context ids here")
    (should (null (erc-matterircd--context-ids))))
  ;; single ID is found with correct value
  (with-temp-buffer
    ;;  " foo [001] "
    ;;   1234567890
    (insert " foo [001] ")
    (put-text-property 6 11 'erc-matterircd-context-id "001")
    (let ((ids (erc-matterircd--context-ids)))
      (should (= 1 (length ids)))
      (should (equal "001" (caar ids)))))
  ;; multiple IDs: last one scanned is at the head (push order)
  (with-temp-buffer
    ;;  " [001] [002] "
    ;;   1234567890123
    (insert " [001] [002] ")
    (put-text-property 2 7 'erc-matterircd-context-id "001")
    (put-text-property 8 13 'erc-matterircd-context-id "002")
    (let ((ids (erc-matterircd--context-ids)))
      (should (= 2 (length ids)))
      (should (equal "002" (caar ids)))
      (should (equal "001" (caar (cdr ids))))))
  ;; duplicate IDs are deduplicated; first occurrence position is kept
  (with-temp-buffer
    ;;  " [001] text [001] "
    ;;   123456789012345678
    (insert " [001] text [001] ")
    (put-text-property 2 7 'erc-matterircd-context-id "001")
    (put-text-property 13 18 'erc-matterircd-context-id "001")
    (let ((ids (erc-matterircd--context-ids)))
      (should (= 1 (length ids)))
      ;; position 6 = one before point after scanning first [001] (positions 2-6)
      (should (= 6 (cdr (cdar ids)))))))

(ert-deftest erc-matterircd-test-edit-indicators ()
  "Test that (edited) and (deleted) markers are dimmed."
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network) (lambda () 'matterircd)))
      (insert "hello world (edited)")
      (erc-matterircd-format-edit-indicators)
      (should (equal (get-text-property 14 'display)
                     (propertize "(edited)" 'face 'erc-matterircd-edit-indicator-face)))
      (should (get-text-property 14 'rear-nonsticky))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network) (lambda () 'matterircd)))
      (insert "hello world (deleted)")
      (erc-matterircd-format-edit-indicators)
      (should (equal (get-text-property 14 'display)
                     (propertize "(deleted)" 'face 'erc-matterircd-edit-indicator-face)))))
  ;; non-matterircd is a no-op
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network) (lambda () 'other)))
      (insert "hello world (edited)")
      (erc-matterircd-format-edit-indicators)
      (should (null (get-text-property 14 'display))))))

(ert-deftest erc-matterircd-test-search ()
  "Test that /SEARCH sends the correct PRIVMSG to mattermost."
  (let (sent-type sent-msg)
    (cl-letf (((symbol-function 'erc-network) (lambda () 'matterircd))
              ((symbol-function 'erc-message)
               (lambda (type msg &rest _) (setq sent-type type sent-msg msg))))
      ;; simple single-word query
      (erc-matterircd-search " foo")
      (should (equal sent-type "PRIVMSG"))
      (should (equal sent-msg "mattermost search foo"))
      ;; multi-word query preserved intact
      (erc-matterircd-search " foo bar baz")
      (should (equal sent-msg "mattermost search foo bar baz"))
      ;; search modifiers passed through as-is
      (erc-matterircd-search " 10 foo from:alice in:#general")
      (should (equal sent-msg "mattermost search 10 foo from:alice in:#general"))))
  ;; empty query shows error
  (let (display-args)
    (cl-letf (((symbol-function 'erc-network) (lambda () 'matterircd))
              ((symbol-function 'erc-display-message)
               (lambda (&rest args) (setq display-args args))))
      (with-temp-buffer
        (erc-matterircd-search "   ")
        (should (equal (nth 3 display-args) 'erc-matterircd-search-no-query)))))
  ;; non-matterircd shows error
  (let (display-args)
    (cl-letf (((symbol-function 'erc-network) (lambda () 'other))
              ((symbol-function 'erc-display-message)
               (lambda (&rest args) (setq display-args args))))
      (with-temp-buffer
        (erc-matterircd-search " foo")
        (should (equal (nth 3 display-args) 'not-matterircd))))))

(provide 'erc-matterircd-test)
;;; erc-matterircd-test.el ends here


