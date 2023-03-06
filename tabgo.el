(require 'map)
(require 'tab-line)
(require 'tab-bar)

(defgroup tabgo nil
  "TODO"
  :group 'convenience)

(defface tabgo-face
  '((t (:weight bold :background "blue" :foreground "white")))
  "TODO Face used for the leading chars.")

(defcustom tabgo-tab-bar-keys "123456789"
  "TODO"
  :group 'tabgo
  ;; TODO list<char> or string
  :type 'list)

(defcustom tabgo-tab-line-keys "qwertyuiop"
  "TODO"
  :group 'tabgo
  ;; TODO list<char> or string
  :type 'list)

(defun tabgo--nth (n xs)
  (if (type-of xs)
      (aref xs n)
    (nth n xs)))

(defun tabgo--get-key (type index)
  (tabgo--nth
   index
   (pcase type
     ('line tabgo-tab-line-keys)
     ('bar tabgo-tab-bar-keys))))

(defmacro tabgo--with-tab-line-highlighted (&rest forms)
  `(let* ((tabgo-tab-line-map (make-hash-table))
          (old-tab-line-fn tab-line-tab-name-format-function)
          (tab-line-tab-name-format-function
           #'(lambda (tab tabs)
               (let ((char (tabgo--get-key 'line (seq-position tabs 'dummy (lambda (it _) (equal tab it)))))
                     (old-format (funcall old-tab-line-fn tab tabs)))
                 (map-put! tabgo-tab-line-map char tab)
                 (format "%s%s"
                         (propertize (format "%c" char) 'face 'tabgo-face)
                         (substring old-format 1))))))
     (set-window-parameter nil 'tab-line-cache nil)
     (force-mode-line-update t)
     ,@forms
     (set-window-parameter nil 'tab-line-cache nil)
     (force-mode-line-update t)))

(defmacro tabgo--with-tab-bar-highlighted (&rest forms)
  `(let* ((tabgo-tab-bar-map (make-hash-table))
          (old-tab-bar-fn tab-bar-tab-name-format-function)
          (tab-bar-tab-name-format-function
           #'(lambda (tab i)
               (let* ((char (tabgo--get-key 'bar (1- i)))
                      (old-format (funcall old-tab-bar-fn tab i))
                      (new-format (format "%s%s"
                                          (propertize (format "%c" char) 'face 'tabgo-face)
                                          (substring old-format 1))))
                 (map-put! tabgo-tab-bar-map char tab)
                 (if (string= old-format new-format)
                     ;; HACK If new-format and old-format equal to
                     ;; each other string-wise, even if the text
                     ;; properties are different, tab-bar is not
                     ;; updated with new text properties. So we do
                     ;; some changes the original text.
                     (let ((placeholder "~"))
                       ;; If the old-format has the X button at the
                       ;; end, the resulting new-format will look
                       ;; exactly the same (except for the leading
                       ;; char's face). Otherwise they'll see a "~" at
                       ;; the end of their tab name, instead of what
                       ;; it's supposed to be.
                       (add-text-properties 0 1 (text-properties-at (1- (length new-format)) new-format) placeholder)
                       (format "%s%s"
                               (substring new-format 0 (1- (length new-format)))
                               placeholder))
                   new-format)))))
     (force-mode-line-update t)
     ,@forms
     (force-mode-line-update t)))

(defun tabgo-line ()
  "TODO"
  (interactive)
  (tabgo--with-tab-line-highlighted
   (let ((result (read-key "Which?")))
     (when-let (selected (map-elt tabgo-tab-line-map result))
       (switch-to-buffer selected)))))

(defun tabgo-bar ()
  "TODO"
  (interactive)
  (tabgo--with-tab-bar-highlighted
   (let ((result (read-key "Which?")))
     (when-let (selected (map-elt tabgo-tab-bar-map result))
       (tab-switch (alist-get 'name selected))))))

(defun tabgo ()
  "TODO"
  (interactive)
  (tabgo--with-tab-bar-highlighted
   (tabgo--with-tab-line-highlighted
    (let ((result (read-key "Which?")))
      (set-window-parameter nil 'tab-line-cache nil)
      (when-let (selected (map-elt tabgo-tab-line-map result))
        (switch-to-buffer selected))
      (when-let (selected (map-elt tabgo-tab-bar-map result))
        (tab-switch (alist-get 'name selected)))))))

(provide 'tabgo)
;;; tabgo.el ends here
