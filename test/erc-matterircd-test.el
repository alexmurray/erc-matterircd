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
               (concat " " (propertize "italic asterisk" 'face erc-matterircd-italic-face)
                       " " (propertize "italic underscore" 'face erc-matterircd-italic-face) " ")))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert "*italic asterisk* _italic underscore_")
      (erc-matterircd-format-italics)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat (propertize "italic asterisk" 'face erc-matterircd-italic-face) " "
                       (propertize "italic underscore" 'face erc-matterircd-italic-face))))))
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
               (concat " " (propertize "bold asterisk" 'face 'erc-bold-face)
                       " " (propertize "bold underscore" 'face 'erc-bold-face) " ")))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert "**bold asterisk** __bold underscore__")
      (erc-matterircd-format-bolds)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat (propertize "bold asterisk" 'face 'erc-bold-face) " "
                       (propertize "bold underscore" 'face 'erc-bold-face))))))
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
               (concat " " (propertize "strikethrough" 'face 'erc-matterircd-strikethrough-face) " "))))))

(ert-deftest erc-matterircd-test-links ()
  "Test that [link](url) gets handled appropriately."
  (with-temp-buffer
    (cl-letf (((symbol-function 'erc-network)
               (lambda () 'matterircd)))
      (insert " [text](url) ")
      (erc-matterircd-format-links)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat " " (propertize "text" 'erc-matterircd-link-url "url") " ")))
      (erc-matterircd-buttonize-links)
      (should (ert-equal-including-properties
               (buffer-substring (point-min) (point-max))
               (concat " " (propertize "text"
                                       'font-lock-face 'erc-button
                                       'mouse-face 'highlight
                                       'erc-callback 'browse-url-button-open-url
                                       'keymap erc-button-keymap
                                       'rear-nonsticky t
                                       'erc-data '("url"))
                       " "))))))

(provide 'erc-matterircd-test)
;;; erc-matterircd-test.el ends here


