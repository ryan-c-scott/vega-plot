;;; -*- lexical-binding: t; -*-
(require 'cl)

(defvar vega-plot-error-buffer-name "*vega errors*"
  "Buffer name to use when reporting error output from vega")

(defvar vega-plot-show-source-buffer nil
  "Retain the source generated source code, regardless of success/failure.")

(cl-defun vega-plot--guess-filename (&optional ext)
  (when (boundp 'out)
    out)
  (when (equal major-mode 'org-mode)
    (concat
     (replace-regexp-in-string
      "[/\\:]" "-"
      (s-join "-" (org-get-outline-path t)))
     (concat "." (or ext "svg")))))

(cl-defun vega-plot-vega-command (inpath outpath)
  ;; TODO: Variable for vega binary path
  (format "npx -p vega -p vega-lite vl2svg \"%s\" \"%s\"" inpath outpath))

(cl-defun vega-plot-process-errors (output)
  ;; TODO:
  )

(cl-defun vega-plot--mark-errors (errors)
  ;; TODO:
  (cl-loop
   for (line . msg) in errors do
   (save-excursion
     (goto-char (point-min))
     (forward-line (1- line))
     (setq beg (point))
     (forward-line 1)

     (let* ((ovr (or (car (overlays-at beg))
                     (make-overlay beg (point))))
            (existing (overlay-get ovr 'after-string))
            (ovr-msg (concat (when existing (concat existing "\n"))
                             (propertize msg 'face 'font-lock-warning-face)
                             "\n")))
       (overlay-put ovr 'after-string ovr-msg)))))

(cl-defun vega-plot-report-errors (code errors)
  (with-current-buffer (get-buffer-create vega-plot-error-buffer-name)
    (remove-overlays)
    (erase-buffer)
    (insert code)
    (json-pretty-print-buffer)
    (json-mode)
    (vega-plot--mark-errors errors)))

(cl-defun vega-plot--render (form &key filename as-code debug-data)
  "Generate vega file from `FORM' and render image from it."
  (let ((path (f-full filename)))
    (with-temp-buffer
      (insert
       (json-encode
        form))

      (cond
       (as-code
        (buffer-string))
       (t
        (let* ((temp-path (make-temp-file "vega" nil ".json")))
          (write-file temp-path)

          (let* ((output (shell-command-to-string
                          (vega-plot-vega-command temp-path path)))
                 (errors (vega-plot-process-errors output))
                 (preserve-code (or errors vega-plot-show-source-buffer)))

            (when errors
              (warn "Error in plotting. See %s" vega-plot-error-buffer-name))

            (when preserve-code
              (vega-plot-report-errors (buffer-string) errors)))

          (delete-file temp-path))
        filename)))))

(cl-defun vega-plot--process (form &key type dimensions fontscale background)
  "Expand all special commands and inject default forms for the conversion to vega."
  (append `(:$schema "https://vega.github.io/schema/vega-lite/v5.json"
                     :background ,(or background "#ffffffff")
                     :width ,(or (car dimensions) 800)
                     :height ,(or (cadr dimensions) 700))
          (cl-loop
           for (key val) on form by 'cddr append
           (pcase val
             (_ `(,key ,val))))))

;;;###autoload
(cl-defun vega-plot (form &key filename as-code debug-data type dimensions fontscale background)
  (vega-plot--render
   (vega-plot--process form :type type :dimensions dimensions :fontscale fontscale :background background)
   :filename (or filename (vega-plot--guess-filename (and type (format "%s" type))))
   :as-code as-code
   :debug-data debug-data))

;;
(provide 'vega-plot)
