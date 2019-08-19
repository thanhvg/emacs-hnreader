;;; hnreader.el --- A HN reader -*- lexical-binding: t; -*-
(require 'promise)
(require 'request)
(require 'shr)
(require 'dom)
(require 'cl-lib)

(defvar hn--buffer "*HN*"
  "Buffer for HN pages.")

(defvar hn--comment-buffer "*HNComments*")

(defun hnreader--get-hn-buffer ()
  "Get hn buffer."
  (get-buffer-create hn--buffer))

(defun hnreader--get-hn-comment-buffer ()
  "Get hn commnet buffer."
  (get-buffer-create hn--comment-buffer))

(defvar hnreader--indent 40
  "Tab value which is the width of an indent.
top level commnet is 0 indent
second one is 40
third one is 80.")

(defun hnreader--promise-dom (url)
  "Promise (url . dom) from URL with curl."
  (promise-new
   (lambda (resolve reject)
     (request url
              :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
              :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                    (funcall reject  error-thrown)))
              :success (cl-function (lambda (&key data &allow-other-keys)
                                      (funcall resolve data)))))))

(defun hnreader--prepare-buffer (buf &optional msg)
  "Print MSG message and prepare window for BUF buffer."
  (when (not (equal (window-buffer) buf))
    ;; (switch-to-buffer-other-window howdoi-buffer))
    (display-buffer buf '(display-buffer-use-some-window (inhibit-same-window . t))))
  (with-current-buffer buf
    (read-only-mode -1)
    (erase-buffer)
    (insert (if msg
                msg
              "Loading...")))
  buf)

(defun hnreader--print-frontpage-item (thing subtext)
  "Print THING dom and SUBTEXT dom."
  (insert (format "\n* %s %s (%s) [%s]\n"
                  ;; rank
                  (dom-text (dom-by-class thing "^rank$"))
                  ;; title
                  (dom-text (dom-by-class thing "^storylink$"))
                  ;; points
                  (dom-text (dom-by-class subtext "^score$"))
                  ;; comments
                  (dom-text (last (dom-by-tag subtext 'a)))))
  ;; (setq thanh subtext)
  ;; link
  (insert (dom-attr (dom-by-class thing "^storylink$") 'href) "\n" )
  ;; comment link
  (insert (format "[[elisp:(hnreader-comment %s)][https://news.ycombinator.com/item?id=%s]]"
                  (dom-attr thing 'id)
                  (dom-attr thing 'id))))

(defun hnreader--print-frontpage (dom buf)
  "Print raw DOM on BUF."
  (let ((things (dom-by-class dom "^athing$"))
        (subtexts (dom-by-class dom "^subtext$")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "#+STARTUP: overview indent\n")
      (cl-mapcar #'hnreader--print-frontpage-item things subtexts)
      (org-mode))))

(defun hnreader--print-comments (dom)
  "Print DOM comment page to buffer."
  (let ((comments (dom-by-class  dom "^athing comtr $")))
    (with-current-buffer (hnreader--get-hn-comment-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (insert "#+STARTUP: overview indent\n")
      (dolist (comment comments)
        ;; (setq thanh comment)
        (insert (format "%s %s\n"
                        (hnreader--get-indent
                         (hnreader--get-img-tag-width comment))
                        (hnreader--get-comment-owner comment)))
        (let ((shr-width 80))
          (shr-insert-document (hnreader--get-comment comment))))
      (org-mode))))

(defun hnreader--get-img-tag-width (comment-dom)
  "Get width attribute of img tag in COMMENT-DOM."
  (string-to-number
   (dom-attr (dom-by-tag (dom-by-class comment-dom "^ind$") 'img)
             'width)))

(defun hnreader--get-indent (width)
  "Return headline star string from WIDTH of img tag."
  (let ((stars "\n*")) (dotimes (_ (round (/ width hnreader--indent)) stars)
                        (setq stars (concat stars "*")))))

(defun hnreader--get-comment-owner (comment-dom)
  "Return user who wrote this COMMENT-DOM."
  (dom-text (dom-by-class comment-dom "^hnuser$")))

(defun hnreader--get-comment (comment-dom)
  "Get comment dom from COMMENT-DOM."
  (dom-by-class comment-dom "^commtext"))

(defun hnreader-frontpage ()
  "Testing."
  (interactive)
  (hnreader--prepare-buffer (hnreader--get-hn-buffer))
  (promise-chain (hnreader--promise-dom "https://news.ycombinator.com/news")
    (then (lambda (result)
            (hnreader--print-frontpage result (hnreader--get-hn-buffer))))
    (promise-catch (lambda (reason)
                     (message "catch error in promise prontpage: %s" reason)))))

(defun hnreader-comment (comment-id)
  "Print hn COMMENT-ID page to buffer."
  (interactive "sQuery: ")
  (hnreader--prepare-buffer (hnreader--get-hn-comment-buffer))
  (promise-chain (hnreader--promise-dom (format "https://news.ycombinator.com/item?id=%s" comment-id))
    (then #'hnreader--print-comments)
    (promise-catch (lambda (reason)
                     (message "catch error in promise comments: %s" reason)))))

(provide 'hnreader)
