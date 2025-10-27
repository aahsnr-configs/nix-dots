(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs."
  (interactive)
  (jupyter-available-kernelspecs t))

(defun my/jupyter-refesh-langs ()
  "Refresh Jupyter languages."
  (interactive)
  (org-babel-jupyter-aliases-from-kernelspecs t))

(defun my/org-load-jupyter ()
  "Load Jupyter support for Org Babel."
  (interactive)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((jupyter . t)))
  (my/jupyter-refesh-langs))

(setq my/jupyter-runtime-folder (expand-file-name "~/.local/share/jupyter/runtime"))

(defun my/get-open-ports ()
  "Get a list of open network ports."
  (mapcar
   #'string-to-number
   (split-string (shell-command-to-string "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'") "\n")))

(defun my/list-jupyter-kernel-files ()
  "List Jupyter kernel files, sorted by modification time."
  (mapcar
   (lambda (file) (cons (car file) (cdr (assq 'shell_port (json-read-file (car file))))))
   (sort
    (directory-files-and-attributes my/jupyter-runtime-folder t ".*kernel.*json$")
    (lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))

(defun my/select-jupyter-kernel ()
  "Select an active Jupyter kernel from a list."
  (let ((ports (my/get-open-ports))
        (files (my/list-jupyter-kernel-files)))
    (completing-read
     "Jupyter kernels: "
     (seq-filter
      (lambda (file)
        (member (cdr file) ports))
      files))))

(defun my/jupyter-connect-repl ()
  "Open an emacs-jupyter REPL, connected to a Jupyter kernel."
  (interactive)
  (jupyter-connect-repl (my/select-jupyter-kernel) nil nil nil t))

(defun my/jupyter-qtconsole ()
  "Open Jupyter QtConsole, connected to a Jupyter kernel."
  (interactive)
  (start-process "jupyter-qtconsole" nil "setsid" "jupyter" "qtconsole" "--existing"
                 (file-name-nondirectory (my/select-jupyter-kernel))))

(defun my/jupyter-cleanup-kernels ()
  "Clean up stale Jupyter kernel connection files."
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

(defun my/org-babel-execute-buffer-below (&optional arg)
  "Execute all Org Babel blocks from the current position to the end of the buffer."
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
  "Execute all Org Babel blocks from the start of the buffer to the current position."
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
