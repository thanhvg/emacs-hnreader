;;; hnreader.el --- A HN reader -*- lexical-binding: t; -*-
(require 'promise)
(require 'request)
(require 'shr)
(require 'dom)
(require 'cl-lib)

(defvar hnreader--buffer "*HN*"
  "Buffer for HN pages.")

(defvar hnreader--comment-buffer "*HNComments*")

(defun hnreader--get-hn-buffer ()
  "Get hn buffer."
  (get-buffer-create hnreader--buffer))

(defun hnreader--get-hn-comment-buffer ()
  "Get hn commnet buffer."
  (get-buffer-create hnreader--comment-buffer))

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
              :parser (lambda ()
                        (goto-char (point-min))
                        (while (re-search-forward ">\\*" nil t)
                          (replace-match ">-"))
                        (libxml-parse-html-region (point-min) (point-max)))
              :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                    (funcall reject  error-thrown)))
              :success (cl-function (lambda (&key data &allow-other-keys)
                                      (funcall resolve data)))))))

(defun hnreader--prepare-buffer (buf &optional msg)
  "Print MSG message and prepare window for BUF buffer."
  (when (not (equal (window-buffer) buf))
    (switch-to-buffer-other-window buf))
  ;; (display-buffer buf '(display-buffer-use-some-window (inhibit-same-window . t))))
  (with-current-buffer buf
    (read-only-mode -1)
    (erase-buffer)
    (insert (if msg
                msg
              "Loading...")))
  buf)

(defun hnreader--print-frontpage-item (thing subtext)
  "Print THING dom and SUBTEXT dom."
  (let ((id (dom-attr thing 'id))
        (story-link (dom-attr (dom-by-class thing "^storylink$") 'href)))
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
    (insert (format "%s\n[[eww:%s][view story in eww]]\n" story-link story-link))
    ;; comment link
    (insert (format "[[elisp:(hnreader-comment %s)][https://news.ycombinator.com/item?id=%s]]"
                    id
                    id)) ))

(defun hnreader--get-morelink (dom)
  "Get more link from DOM."
  (let ((more-link (dom-by-class dom "morelink")))
    ;; (setq thanh more-link)
    (format "\n* [[elisp:(hnreader-read-page \"https://news.ycombinator.com/%s\")][More]]"
            (dom-attr more-link 'href))))

(defun hnreader--print-frontpage (dom buf)
  "Print raw DOM on BUF."
  (let ((things (dom-by-class dom "^athing$"))
        (subtexts (dom-by-class dom "^subtext$")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "#+STARTUP: overview indent\n")
      (cl-mapcar #'hnreader--print-frontpage-item things subtexts)
      ;; (setq-local org-confirm-elisp-link-function nil)
      (insert (hnreader--get-morelink dom))
      (org-mode)
      (goto-char (point-min))
      (forward-line 2))))

(defun hnreader--get-title (dom)
  "Get title and link from DOM comment page."
  (let ((a-link (dom-by-class dom "^storylink$")))
    (cons (dom-text a-link) (dom-attr a-link 'href))))

(defun hnreader--get-post-info (dom)
  "Get top info about the DOM comment page."
  (let* ((fat-item (dom-by-class dom "^fatitem$"))
         (tr-tag (dom-by-tag fat-item 'tr)))
    (setq thanh fat-item)
    (setq thanh-dom dom)
    (seq-drop (seq-take tr-tag (- (length tr-tag) 1)) 1)))


(defun hnreader--print-node (node)
  "Print the NODE with extra options."
  (let ((shr-width 80)
        (shr-use-fonts nil))
    (shr-insert-document node)))

(defun hnreader--print-comments (dom)
  "Print DOM comment page to buffer."
  (let ((comments (dom-by-class  dom "^athing comtr $"))
        (title (hnreader--get-title dom)))
    (with-current-buffer (hnreader--get-hn-comment-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (insert "#+STARTUP: indent\n")
      (insert "#+TITLE: " (car title))
      (insert (format "\n%s\n[[eww:%s][view story in eww]]\n" (cdr title) (cdr title)))
      (mapc #'hnreader--print-node (hnreader--get-post-info dom))
      ;; (hnreader--print-node (hnreader--get-post-info dom))
      ;; (insert "\n" (cdr title))
      (dolist (comment comments)
        ;; (setq thanh comment)
        (insert (format "%s %s\n"
                        (hnreader--get-indent
                         (hnreader--get-img-tag-width comment))
                        (hnreader--get-comment-owner comment)))
          (hnreader--print-node (hnreader--get-comment comment)))
      (org-mode)
      (org-shifttab 3)
      (goto-char (point-min))
      (forward-line 2))))

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

(defun hnreader-readpage-promise (url)
  "Promise HN URL."
  (hnreader--prepare-buffer (hnreader--get-hn-buffer))
  (promise-chain (hnreader--promise-dom url)
    (then (lambda (result)
            (hnreader--print-frontpage result (hnreader--get-hn-buffer))))
    (promise-catch (lambda (reason)
                     (message "catch error in promise prontpage: %s" reason)))))

(defun hnreader-read-page (url)
  "Print HN URL page."
  (interactive "sLink: ")
  (message url)
  (hnreader-readpage-promise url)
  nil)

(defun hnreader-front()
  "Read front page."
  (interactive)
  (hnreader-read-page "https://news.ycombinator.com/news"))

(defun hnreader-ask ()
  "Read ask page."
  (interactive)
  (hnreader-read-page "https://news.ycombinator.com/ask"))

(defun hnreader-show ()
  "Read show page."
  (interactive)
  (hnreader-read-page "https://news.ycombinator.com/show"))

(defun hnreader-jobs ()
  "Read jobs page."
  (interactive)
  (hnreader-read-page "https://news.ycombinator.com/jobs"))

(defun hnreader-promise-comment (comment-id)
  "Promise to print hn COMMENT-ID page to buffer."
  (hnreader--prepare-buffer (hnreader--get-hn-comment-buffer))
  (promise-chain (hnreader--promise-dom (format "https://news.ycombinator.com/item?id=%s" comment-id))
    (then #'hnreader--print-comments)
    (promise-catch (lambda (reason)
                     (message "catch error in promise comments: %s" reason)))))

(defun hnreader-comment (comment-id)
  "Print hn COMMENT-ID page to buffer."
  (interactive "sComment ID: ")
  (hnreader-promise-comment comment-id)
  nil)

(provide 'hnreader)
