;;; erc-matterircd-integration-test.el --- Integration tests against real matterircd  -*- lexical-binding: t; -*-

;; Requires a running matterircd (pointing at a live Mattermost) and the
;; following environment variables:
;;
;;   IRC_HOST        matterircd host            (default: 127.0.0.1)
;;   IRC_PORT        matterircd IRC port         (default: 6667)
;;   MM_URL          Mattermost base URL         (default: http://localhost:8065)
;;   MM_TEAM         Mattermost team name        (default: testteam)
;;   MM_NICK         IRC nick / Mattermost user  (default: testuser)
;;   MM_PASSWORD     Mattermost password         (default: TestUser1!)
;;   MM_CHANNEL      IRC channel to test in      (default: #testchannel)
;;   MM_ADMIN_TOKEN  Mattermost admin API token  (required for inbound-message tests)
;;   MM_CHANNEL_ID   Mattermost channel GUID     (required for inbound-message tests)
;;
;; Run via:
;;   emacs --batch -L /path/to/repo -l this-file.el --eval "(emt-run)"
;;
;; ── matterircd configuration rationale ───────────────────────────────────────
;;
;; The tests run against a single matterircd instance configured in
;; test/integration/matterircd.toml.  The two non-default settings chosen are:
;;
;;   PrefixContext = true
;;     Adds a "[hex]" context ID to every received message.  This is the more
;;     demanding case for erc-matterircd: formatters must tolerate the ID
;;     prefix that appears between the ERC nick display and the message body.
;;     PrefixContext = false (the matterircd default) is simpler — the ID is
;;     absent — and is fully covered by the unit tests.
;;
;;   DisableMarkdown = true
;;     Passes Mattermost markdown through as raw text so erc-matterircd can
;;     format it (bold, italic, code blocks, block quotes, etc.).  This is the
;;     only configuration in which formatting on the receive path is exercised
;;     end-to-end; it is therefore the more interesting case to integration-test.
;;     With the matterircd default (DisableMarkdown = false) matterircd converts
;;     markdown to IRC control codes, which ERC renders natively without any
;;     erc-matterircd involvement.  That path requires no integration testing
;;     beyond confirming message delivery (T5), and the formatting behaviour of
;;     each individual formatter under that configuration is covered by unit tests.
;;
;; No other matterircd settings materially affect erc-matterircd behaviour, so
;; a single integration run with this configuration is sufficient.

(require 'erc)
(require 'erc-matterircd)

;;; ── configuration ────────────────────────────────────────────────

(defvar emt-irc-host     (or (getenv "IRC_HOST")       "127.0.0.1"))
(defvar emt-irc-port     (string-to-number (or (getenv "IRC_PORT") "6667")))
(defvar emt-mm-url       (or (getenv "MM_URL")         "http://localhost:8065"))
(defvar emt-mm-team      (or (getenv "MM_TEAM")        "testteam"))
(defvar emt-mm-nick      (or (getenv "MM_NICK")        "testuser"))
(defvar emt-mm-password  (or (getenv "MM_PASSWORD")    "TestUser1!"))
(defvar emt-mm-channel   (or (getenv "MM_CHANNEL")     "#testchannel"))
(defvar emt-admin-token  (or (getenv "MM_ADMIN_TOKEN") ""))
(defvar emt-channel-id   (or (getenv "MM_CHANNEL_ID")  ""))

;;; ── test framework ───────────────────────────────────────────────

(defvar emt--pass 0)
(defvar emt--fail 0)
(defvar emt--failures nil)

(defmacro emt-assert (name form)
  "Assert FORM is non-nil and record the result under NAME."
  `(if ,form
       (progn (cl-incf emt--pass)
              (message "  PASS  %s" ,name))
     (cl-incf emt--fail)
     (push ,name emt--failures)
     (message "  FAIL  %s" ,name)))

(defun emt-wait-for (pred &optional timeout-secs)
  "Spin-wait up to TIMEOUT-SECS for PRED to return non-nil.
Processes network events while waiting.  Returns the predicate value."
  (let ((deadline (+ (float-time) (or timeout-secs 30))))
    (while (and (not (funcall pred))
                (< (float-time) deadline))
      (accept-process-output nil 0.3)))
  (funcall pred))

(defun emt-buffer-contains (buf-name text)
  "Return non-nil if buffer BUF-NAME contains TEXT (literal string)."
  (when-let ((buf (get-buffer buf-name)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (search-forward text nil t)))))

(defun emt-post-via-api (message)
  "Post MESSAGE to the test channel via the Mattermost REST API.
Uses MM_ADMIN_TOKEN and MM_CHANNEL_ID from the environment.
Returns t on success, nil on failure.  MESSAGE may contain newlines."
  (when (and (not (string-empty-p emt-admin-token))
             (not (string-empty-p emt-channel-id)))
    (let* ((escaped (replace-regexp-in-string
                     "\n" "\\n"
                     (replace-regexp-in-string "\"" "\\\\\"" message)
                     nil t)))
      (= 0 (call-process "curl" nil nil nil
                         "-sf" "-X" "POST"
                         (concat emt-mm-url "/api/v4/posts")
                         "-H" "Content-Type: application/json"
                         "-H" (concat "Authorization: Bearer " emt-admin-token)
                         "-d" (format "{\"channel_id\":\"%s\",\"message\":\"%s\"}"
                                      emt-channel-id escaped))))))

;;; ── test helpers ─────────────────────────────────────────────────

(defun emt-find-text-prop-in-buffer (buf text prop)
  "Search BUF for any span whose display property equals TEXT with PROP.
Returns the start position of the match, or nil."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (let (result)
        (while (and (not result) (< (point) (point-max)))
          (let* ((end (or (next-single-property-change (point) 'display) (point-max)))
                 (val (get-text-property (point) 'display)))
            (when (and val (equal (if (stringp val) val "") text))
              (setq result (point)))
            (goto-char end)))
        result))))

(defun emt-wait-for-text-in-buffer (buf-name text &optional timeout)
  "Wait for TEXT to appear literally in buffer BUF-NAME."
  (emt-wait-for (lambda () (emt-buffer-contains buf-name text)) timeout))

(defun emt-buffer-has-code-block-display (buf-name)
  "Return non-nil if BUF-NAME contains a code-block display property.
Looks for a display text property whose value is a string carrying
`erc-matterircd-monospace-face', which is the face applied by
`erc-matterircd-format-code-blocks'."
  (when-let ((buf (get-buffer buf-name)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let (found)
          (while (and (not found) (< (point) (point-max)))
            (let* ((end (or (next-single-property-change (point) 'display)
                            (point-max)))
                   (val (get-text-property (point) 'display)))
              (when (and val (stringp val) (> (length val) 0))
                (let ((face (get-text-property 0 'face val)))
                  (when (or (eq face 'erc-matterircd-monospace-face)
                            (and (listp face)
                                 (memq 'erc-matterircd-monospace-face face)))
                    (setq found t))))
              (goto-char end)))
          found)))))

(defun emt-buffer-has-blockquote-display (buf-name)
  "Return non-nil if BUF-NAME contains a block-quote display property.
Looks for a display text property whose value is a string carrying
`erc-matterircd-blockquote-face', which is the face applied by
`erc-matterircd-format-blockquotes'."
  (when-let ((buf (get-buffer buf-name)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let (found)
          (while (and (not found) (< (point) (point-max)))
            (let* ((end (or (next-single-property-change (point) 'display)
                            (point-max)))
                   (val (get-text-property (point) 'display)))
              (when (and val (stringp val) (> (length val) 0))
                (let ((face (get-text-property 0 'face val)))
                  (when (or (eq face 'erc-matterircd-blockquote-face)
                            (and (listp face)
                                 (memq 'erc-matterircd-blockquote-face face)))
                    (setq found t))))
              (goto-char end)))
          found)))))

;;; ── test suite ───────────────────────────────────────────────────

(defun emt-run ()
  "Run all integration tests and exit with appropriate code."
  ;; ── ERC / matterircd module setup ─────────────────────────────
  (add-to-list 'erc-modules 'matterircd)
  (erc-update-modules)
  (setq erc-matterircd-server   (replace-regexp-in-string "^https?://" "" emt-mm-url)
        erc-matterircd-team     emt-mm-team
        erc-matterircd-password emt-mm-password
        ;; Show mattermost bot replies so we can assert on them
        erc-matterircd-suppress-mattermost-responses nil
        erc-prompt-for-password nil
        ;; Wide fill so long lines don't wrap and confuse search
        erc-fill-column 500
        erc-fill-function nil)

  ;; ── T0: connect to matterircd ─────────────────────────────────
  (message "\nConnecting to matterircd at %s:%d..." emt-irc-host emt-irc-port)
  (erc :server emt-irc-host :port emt-irc-port
       :nick emt-mm-nick :full-name emt-mm-nick)

  ;; ── T1: login succeeds ────────────────────────────────────────
  (message "\nT1: login")
  (emt-assert "login OK received within 30s"
              (emt-wait-for
               (lambda () (emt-buffer-contains "mattermost" "login OK"))
               30))

  (unless (emt-buffer-contains "mattermost" "login OK")
    (message "FATAL: could not log in — aborting")
    (kill-emacs 1))

  ;; ── T2: channel auto-joined ────────────────────────────────────
  (message "\nT2: channel join")
  ;; matterircd auto-joins the team's channels; town-square is always present
  (emt-assert "town-square buffer appears within 15s"
              (emt-wait-for (lambda () (get-buffer "#town-square")) 15))

  ;; Also explicitly join our test channel
  (erc-cmd-JOIN emt-mm-channel)
  (emt-assert (format "%s buffer appears within 15s" emt-mm-channel)
              (emt-wait-for (lambda () (get-buffer emt-mm-channel)) 15))

  ;; ── T3: bold formatting (send path) ───────────────────────────
  (message "\nT3: bold formatting")
  (when-let ((ch-buf (get-buffer emt-mm-channel)))
    (with-current-buffer ch-buf
      (erc-send-message "**boldword** normal"))
    ;; The send hook should have applied the display property immediately.
    ;; Give ERC up to 5 s to echo the sent line back into the buffer.
    (emt-wait-for (lambda () (emt-buffer-contains emt-mm-channel "**boldword**")) 5)
    (emt-assert "bold display property set on sent message"
                (with-current-buffer ch-buf
                  (save-excursion
                    (goto-char (point-min))
                    (when (search-forward "**boldword**" nil t)
                      (let ((disp (get-text-property (match-beginning 0) 'display)))
                        (and disp
                             (equal (propertize "boldword" 'face 'erc-bold-face)
                                    disp))))))))

  ;; ── T4: italic formatting (send path) ─────────────────────────
  (message "\nT4: italic formatting")
  (when-let ((ch-buf (get-buffer emt-mm-channel)))
    (with-current-buffer ch-buf
      (erc-send-message "*italicword* normal"))
    (emt-wait-for (lambda () (emt-buffer-contains emt-mm-channel "*italicword*")) 5)
    (emt-assert "italic display property set on sent message"
                (with-current-buffer ch-buf
                  (save-excursion
                    (goto-char (point-min))
                    (when (search-forward "*italicword*" nil t)
                      (let ((disp (get-text-property (match-beginning 0) 'display)))
                        (and disp
                             (get-text-property 0 'face disp)
                             (eq erc-matterircd-italic-face
                                 (get-text-property 0 'face disp)))))))))

  ;; ── T5: inbound bold message (receive path) ───────────────────
  ;; Only runs when the admin token + channel ID are available.
  ;; T5: matterircd converts Mattermost **markdown** to IRC bold codes
  ;; (\x02...\x02) which ERC processes natively — the ** markers are gone
  ;; by the time the buffer is populated.  We therefore test end-to-end
  ;; delivery: API post → matterircd WebSocket → ERC buffer.
  (message "\nT5: inbound message delivery via Mattermost API")
  (if (string-empty-p emt-admin-token)
      (message "  SKIP  (MM_ADMIN_TOKEN not set)")
    (let* ((marker (format "inbound-msg-%d" (random 9999)))
           (posted (emt-post-via-api marker)))
      (unless posted
        (message "  DEBUG: API post failed (token-len=%d channel-id=%S)"
                 (length emt-admin-token) emt-channel-id))
      (emt-assert "inbound message appears in channel buffer within 20s"
                  (emt-wait-for
                   (lambda () (emt-buffer-contains emt-mm-channel marker))
                   20))))

  ;; ── T6: context ID attached to inbound message ─────────────────
  (message "\nT6: context ID")
  (if (string-empty-p emt-admin-token)
      (message "  SKIP  (MM_ADMIN_TOKEN not set)")
    (let* ((marker (format "ctx-test-%d" (random 9999)))
           (posted (emt-post-via-api marker)))
      (unless posted
        (message "  DEBUG: API post failed"))
      (emt-wait-for (lambda () (emt-buffer-contains emt-mm-channel marker)) 20)
      (emt-assert "erc-matterircd-context-id property present on inbound message"
                  (with-current-buffer (get-buffer emt-mm-channel)
                    (save-excursion
                      (goto-char (point-min))
                      (let (found)
                        (while (and (not found) (search-forward marker nil t))
                          ;; Scan ±50 chars around the marker for the context-id property
                          ;; (ID may be a prefix "[001] marker" or suffix "marker [001]")
                          (let ((scan (max (point-min) (- (point) 50))))
                            (while (< scan (min (point-max) (+ (point) 50)))
                              (when (get-text-property scan 'erc-matterircd-context-id)
                                (setq found t))
                              (cl-incf scan))))
                        found))))))

  ;; ── T7: /SCROLLBACK sends correct PRIVMSG ─────────────────────
  (message "\nT7: /SCROLLBACK")
  (when-let ((ch-buf (get-buffer emt-mm-channel)))
    (with-current-buffer ch-buf
      (erc-matterircd-scrollback "5"))
    ;; matterircd doesn't reply with a visible message on scrollback,
    ;; but the command must not produce a not-matterircd error notice.
    (accept-process-output nil 2)
    (emt-assert "/SCROLLBACK does not show not-matterircd error"
                (not (emt-buffer-contains emt-mm-channel "specific to matterircd"))))

  ;; ── T8: /SEARCH sends without error ───────────────────────────
  (message "\nT8: /SEARCH")
  (when-let ((ch-buf (get-buffer emt-mm-channel)))
    (with-current-buffer ch-buf
      (erc-matterircd-search " test"))
    (accept-process-output nil 2)
    (emt-assert "/SEARCH does not show not-matterircd error"
                (not (emt-buffer-contains emt-mm-channel "specific to matterircd"))))

  ;; ── T9: updatelastviewed queues and sends ─────────────────────
  (message "\nT9: updatelastviewed")
  (when-let ((ch-buf (get-buffer emt-mm-channel)))
    (with-current-buffer ch-buf
      (erc-matterircd-updatelastviewed))
    (emt-assert "updatelastviewed response received within 10s"
                (emt-wait-for
                 (lambda () (emt-buffer-contains "mattermost" "set viewed for"))
                 10)))

  ;; ── T10: inbound code-block formatting ───────────────────────────
  ;; With DisableMarkdown = true in matterircd.toml, matterircd passes
  ;; ``` fences through as-is.  erc-matterircd-format-code-blocks scans
  ;; the full ERC buffer on every insertion, so by the time the trailing
  ;; marker line arrives the closing ``` has been processed and the
  ;; display property is set.
  (message "\nT10: inbound code-block formatting")
  (if (string-empty-p emt-admin-token)
      (message "  SKIP  (MM_ADMIN_TOKEN not set)")
    (let* ((marker (format "codeblock-test-%d" (random 9999)))
           ;; Marker comes AFTER the closing ``` so that when it appears
           ;; in the buffer the formatter has already processed the block.
           (posted (emt-post-via-api (format "```python\nx = 1\n```\n%s" marker))))
      (unless posted
        (message "  DEBUG: API post failed"))
      (emt-wait-for (lambda () (emt-buffer-contains emt-mm-channel marker)) 20)
      (emt-assert "code-block display property with monospace face set on inbound message"
                  (emt-buffer-has-code-block-display emt-mm-channel))))

  ;; ── T11: inbound block-quote formatting ──────────────────────────
  ;; With DisableMarkdown = true, matterircd passes "> text" through raw
  ;; rather than converting to "| text".  With PrefixContext = true the
  ;; line arrives as "[xxx] > text"; the formatter skips the context ID
  ;; prefix and still applies erc-matterircd-blockquote-face.
  ;; The marker is posted on its own line so that by the time it arrives
  ;; in the buffer the quote line has already been formatted.
  (message "\nT11: inbound block-quote formatting")
  (if (string-empty-p emt-admin-token)
      (message "  SKIP  (MM_ADMIN_TOKEN not set)")
    (let* ((marker (format "blockquote-test-%d" (random 9999)))
           (posted (emt-post-via-api (format "> This is a block quote\n%s" marker))))
      (unless posted
        (message "  DEBUG: API post failed"))
      (emt-wait-for (lambda () (emt-buffer-contains emt-mm-channel marker)) 20)
      (emt-assert "block-quote display property with blockquote face set on inbound message"
                  (emt-buffer-has-blockquote-display emt-mm-channel))))

  ;; ── summary ───────────────────────────────────────────────────
  (message "\n%s" (make-string 60 ?─))
  (message "Integration test results: %d passed, %d failed" emt--pass emt--fail)
  (when emt--failures
    (message "Failed tests:")
    (dolist (f (nreverse emt--failures))
      (message "  - %s" f)))
  (message "%s\n" (make-string 60 ?─))
  (kill-emacs (if (> emt--fail 0) 1 0)))
