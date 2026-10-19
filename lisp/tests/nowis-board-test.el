;;; nowis-board-test.el --- ERT tests for nowis-board -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'denote)
(require 'nowis-board)
(defvar denote-directory)

(ert-deftest nowis-board-resolve-and-render-test ()
  "Verify Denote note and Org heading card extraction and HTML rendering."
  (let* ((tmp-dir (make-temp-file "nowis-board-test-" t))
         (denote-directory tmp-dir)
         (note-file (expand-file-name "20261019T120000--test-note__tag.org" tmp-dir)))
    (unwind-protect
        (progn
          (with-temp-file note-file
            (insert "#+title:      Test Note\n"
                    "#+date:       [2026-10-19 Mon 12:00]\n"
                    "#+identifier: 20261019T120000\n\n"
                    "Intro paragraph with [[denote:20261019T120000][self link]].\n\n"
                    "* First Section\n"
                    ":PROPERTIES:\n"
                    ":CUSTOM_ID: h:sec-1\n"
                    ":END:\n"
                    "Section body with *bold* text.\n"))
          (let ((whole (nowis-board--card-payload "denote:20261019T120000"))
                (sub (nowis-board--card-payload "denote:20261019T120000::#h:sec-1")))
            (should (equal (alist-get 'title whole) "Test Note"))
            (should (string-match-p "data-emacs-link=\"denote:20261019T120000\""
                                    (alist-get 'html whole)))
            (should (equal (alist-get 'title sub) "First Section"))
            (should (string-match-p "<b>bold</b>" (alist-get 'html sub)))
            (should-not (string-match-p "CUSTOM_ID" (alist-get 'html sub)))))
      (when-let* ((buf (find-buffer-visiting note-file)))
        (kill-buffer buf))
      (delete-directory tmp-dir t))))

(ert-deftest nowis-board-http-roundtrip-test ()
  "Verify local HTTP server serves board JSON and saves scene + companion SVG."
  (let* ((tmp-dir (make-temp-file "nowis-board-http-" t))
         (board-file (expand-file-name "20261019T120500--arch__board.excalidraw" tmp-dir))
         (port (nowis-board--ensure-server)))
    (unwind-protect
        (progn
          (nowis-board--write-scene board-file (nowis-board--default-scene))
          (let* ((url-request-method "POST")
                 (url-request-extra-headers '(("Content-Type" . "application/json")))
                 (url-request-data
                  (encode-coding-string
                   (json-encode
                    `((scene . ((type . "excalidraw")
                                (version . 2)
                                (elements . [((id . "rect-1")
                                              (type . "rectangle")
                                              (isDeleted . :json-false)
                                              (originalText . "Draft idea"))])))
                      (selectedIds . ["rect-1"])
                      (svg . "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>")))
                   'utf-8))
                 (save-url (format "http://127.0.0.1:%d/api/save?file=%s"
                                   port (url-hexify-string board-file)))
                 (resp-buf (url-retrieve-synchronously save-url t nil 5)))
            (when resp-buf (kill-buffer resp-buf))
            (should (file-exists-p (concat board-file ".svg")))
            (should (equal (gethash board-file nowis-board--selected-ids) '("rect-1")))
            (let* ((scene (nowis-board--read-scene board-file))
                   (cands (nowis-board--draft-candidates scene)))
              (should (= (length cands) 1))
              (should (equal (cdar cands) '("rect-1" . "Draft idea"))))))
      (delete-directory tmp-dir t))))

(provide 'nowis-board-test)
;;; nowis-board-test.el ends here
