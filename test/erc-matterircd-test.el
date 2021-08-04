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

(provide 'erc-matterircd-test)
;;; erc-matterircd-test.el ends here


