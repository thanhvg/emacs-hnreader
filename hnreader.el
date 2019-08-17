;;; hnreader.el --- A HN reader -*- lexical-binding: t; -*-
(require 'promise)
(require 'request)
(require 'shr)
(require 'dom)
(require 'cl-lib)

(defvar hn--buffer "*HN*"
  "Buffer for HN pages.")

(defun hnreader--get-hn-buffer ()
  "Get hn buffer."
  (get-buffer-create hn--buffer))

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
                  (dom-text (last (dom-children subtext)))))
  ;; link
  (insert (dom-attr (dom-by-class thing "^storylink$") 'href) "\n" )
  ;; comment link
  (insert "https://news.ycombinator.com/item?id=" (dom-attr thing 'id)))

(defun hnreader--print-frontpage (dom buf)
  "Print raw DOM on BUF."
  (let ((things (dom-by-class dom "^athing$"))
        (subtexts (dom-by-class dom "^subtext$")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "#+STARTUP: overview indent\n#")
      (cl-mapcar #'hnreader--print-frontpage-item things subtexts)
      (org-mode))))

(defun hnreader-frontpage ()
  "Testing."
  (hnreader--prepare-buffer (hnreader--get-hn-buffer))
  (promise-chain (hnreader--promise-dom "https://news.ycombinator.com/news")
    (then (lambda (result)
            (hnreader--print-frontpage result (hnreader--get-hn-buffer))))
    (promise-catch (lambda (reason)
                     (message "catch error in promise prontpage: %s" reason)))))

(provide 'hnreader)
