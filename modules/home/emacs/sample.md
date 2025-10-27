```org
(use-package jupyter
  :ensure t
  :after org)

(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

(defun my/jupyter-refesh-langs ()
  "Refresh Jupyter languages"
  (interactive)
  (org-babel-jupyter-aliases-from-kernelspecs t))

(defun my/org-load-jupyter ()
  (interactive)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((jupyter . t)))
  (my/jupyter-refesh-langs))

(with-eval-after-load 'ob-jupyter
  (org-babel-jupyter-override-src-block "python")
  (org-babel-jupyter-override-src-block "hy"))

(use-package ob-async
  :ensure t
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("python" "jupyter-python")))


(setq my/jupyter-runtime-folder (expand-file-name "~/.local/share/jupyter/runtime"))

(defun my/get-open-ports ()
  (mapcar
   #'string-to-number
   (split-string (shell-command-to-string "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'") "\n")))

(defun my/list-jupyter-kernel-files ()
  (mapcar
   (lambda (file) (cons (car file) (cdr (assq 'shell_port (json-read-file (car file))))))
   (sort
    (directory-files-and-attributes my/jupyter-runtime-folder t ".*kernel.*json$")
    (lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))

(defun my/select-jupyter-kernel ()
  (let ((ports (my/get-open-ports))
        (files (my/list-jupyter-kernel-files)))
    (completing-read
     "Jupyter kernels: "
     (seq-filter
      (lambda (file)
        (member (cdr file) ports))
      files))))

(defun my/insert-jupyter-kernel ()
  "Insert a path to an active Jupyter kernel into the buffer"
  (interactive)
  (insert (my/select-jupyter-kernel)))

(defun my/jupyter-connect-repl ()
  "Open an emacs-jupyter REPL, connected to a Jupyter kernel"
  (interactive)
  (jupyter-connect-repl (my/select-jupyter-kernel) nil nil nil t))

(defun my/jupyter-qtconsole ()
  "Open Jupyter QtConsole, connected to a Jupyter kernel"
  (interactive)
  (start-process "jupyter-qtconsole" nil "setsid" "jupyter" "qtconsole" "--existing"
                 (file-name-nondirectory (my/select-jupyter-kernel))))


(defun my/jupyter-cleanup-kernels ()
  (interactive)
  (let* ((ports (my/get-open-ports))
         (files (my/list-jupyter-kernel-files))
         (to-delete (seq-filter
                     (lambda (file)
                       (not (member (cdr file) ports)))
                     files)))
    (when (and (length> to-delete 0)
               (y-or-n-p (format "Delete %d files?" (length to-delete))))
      (dolist (file to-delete)
        (delete-file (car file))))))

(defun my/jupyter-org-scalar (value)
  (cond
   ((stringp value) value)
   (t (jupyter-org-scalar value))))

(define-minor-mode my/emacs-jupyter-raw-output
  "Make emacs-jupyter do raw output")

(defun my/jupyter-org-scalar-around (fun value)
  (if my/emacs-jupyter-raw-output
      (my/jupyter-org-scalar value)
    (funcall fun value)))

(with-eval-after-load 'jupyter
  (advice-add 'jupyter-org-scalar :around #'my/jupyter-org-scalar-around))


(defun my/org-strip-results (data)
  (replace-regexp-in-string ":\\(RESULTS\\|END\\):\n" "" data))

(defun my/org-caption-wrap (data &optional name caption attrs strip-drawer src-wrap)
  (let* ((data-s (if (and strip-drawer (not (string-empty-p strip-drawer)))
                     (my/org-strip-results data)
                   data))
         (drawer-start (if (string-match-p "^:RESULTS:.*" data-s) 10 0)))
    (concat
     (substring data-s 0 drawer-start)
     (and name (not (string-empty-p name)) (concat "#+NAME:" name "\n"))
     (and caption (not (string-empty-p caption)) (concat "#+CAPTION:" caption "\n"))
     (and attrs (not (string-empty-p attrs)) (concat "#+ATTR_LATEX:" attrs "\n"))
     (if (and src-wrap (not (string-empty-p src-wrap)))
         (concat "#+begin_src " src-wrap "\n"
                 (substring data-s drawer-start)
                 (when (not (string-match-p ".*\n" data-s)) "\n")
                 "#+end_src")
       (substring data-s drawer-start)))))


(defun my/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))

(define-minor-mode org-babel-ansi-colors-mode
  "Apply ANSI color codes to Org Babel results."
  :global t
  :after-hook
  (if org-babel-ansi-colors-mode
      (add-hook 'org-babel-after-execute-hook #'my/babel-ansi)
    (remove-hook 'org-babel-after-execute-hook #'my/babel-ansi)))


(defun my/org-babel-execute-buffer-below (&optional arg)
  (interactive "P")
  (org-babel-eval-wipe-error-buffer)
  (let ((point (point)))
    (org-save-outline-visibility t
      (org-babel-map-executables nil
        (when (>= (point) point)
          (if (memq (org-element-type (org-element-context))
                    '(babel-call inline-babel-call))
              (org-babel-lob-execute-maybe)
            (org-babel-execute-src-block arg)))))))

(defun my/org-babel-execute-buffer-above (&optional arg)
  (interactive "P")
  (org-babel-eval-wipe-error-buffer)
  (let ((point (point)))
    (org-save-outline-visibility t
      (org-babel-map-executables nil
        (when (<= (point) point)
          (if (memq (org-element-type (org-element-context))
                    '(babel-call inline-babel-call))
              (org-babel-lob-execute-maybe)
            (org-babel-execute-src-block arg)))))))


(defun my/org-babel-execute-marked (&optional arg)
  (interactive "P")
  (let (markers)
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (elem)
        (let ((params (org-element-property :parameters elem)))
          (when (and params
                     (string-match-p (rx "startup t") params))
            (let ((m (make-marker)))
              (set-marker m (org-element-property :begin elem))
              (set-marker-insertion-type m t)
              (push m markers))))))
    (setq markers (nreverse markers))
    (when arg
      (setq markers
            (seq-filter
             (lambda (m) (> (marker-position m) (point)))
             markers)))
    (dolist (m markers)
      (goto-char m)
      (ignore-errors
        (org-babel-execute-src-block)))))

(with-eval-after-load 'org
  (general-define-key
   :keymaps 'org-babel-map
   "B" #'my/org-babel-execute-buffer-below
   "A" #'my/org-babel-execute-buffer-above)

  (my-leader-def
    :keymaps 'org-mode-map
    "SPC b" '(:wk "org-babel")
    "SPC b" org-babel-map))


(defun my/org-prj-dir (path)
  (expand-file-name path (org-entry-get nil "PRJ-DIR" t)))
```
