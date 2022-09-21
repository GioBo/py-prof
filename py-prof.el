;;; py-prof.el --- Easily observe python cProfile output  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Gionata Bocci

;; Author: G. Bocci <boccigionata@gmail.com>
;; Version: 0.2
;; Package-Requires: ((dash) (s) (ctable) (f))
;; Keywords: python, cProfile, profiling
;; URL: https://github.com/GioBo/py-prof/

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; The package is meant to be a simple wrapper around cProfile and pstat, so that
;;; users can write down the command they want to profile and get a table
;;; of the results which is shown in a temp buffer.
;;;
;;; py-prof takes advantage of the great ctable package; the table containing
;;; the profiling data can be easily sorted simply clicking on the header of the
;;; columns.

;; Easily observe python cProfile output

;;; Change Log:

;;; Code:


(require 'dash)
(require 's)
(require 'ctable)
(require 'f)
(require 'python)
(defun py-prof-split-lines-prun (line)
  "Split the LINE of the pstat table into its single elements.
Cell that should be numeric (i.e. ncalls, tottime, percall and
cumtime) are converted to numbers."
  (interactive)
  (let*
      (
       (no-space (replace-regexp-in-string "^\\s-+" "" line))
       (string-element (replace-regexp-in-string "\\([0-9]+\\)\\s-+" "\\1|" no-space))
       (string-splitted (split-string string-element "|"))
       (elements
  (if (> (length string-splitted) 1)
      (progn
        (list
         (nth 0 string-splitted)
         (string-to-number (nth 1 string-splitted))
         (string-to-number (nth 2 string-splitted))
         (string-to-number (nth 3 string-splitted))
         (string-to-number (nth 4 string-splitted))
         (nth 5 string-splitted)
         )
        )
    )
  ))
    elements
    )
  )

(defun py-prof-create-table (py-prof--table)
  "Create the ctable and render it.
PY-PROF--TABLE contains the data provided by the profiler."
  (interactive)

  (let*
      ((column-model ; column model
  (list (make-ctbl:cmodel
         :title "ncalls" :min-width 10 :align 'right)
        (make-ctbl:cmodel
         :title "Tottime" :align 'center :sorter 'ctbl:sort-number-lessp
         :min-width 8 :align 'right)
        (make-ctbl:cmodel
         :title "percall" :align 'center :sorter 'ctbl:sort-number-lessp
         :min-width 8 :align 'right)
        (make-ctbl:cmodel
         :title "cumtime" :align 'center :sorter 'ctbl:sort-number-lessp
         :min-width 8 :align 'right)
        (make-ctbl:cmodel
         :title "percall" :align 'center :sorter 'ctbl:sort-number-lessp
         :min-width 8 :align 'right)
        (make-ctbl:cmodel
         :title "filename:lineno(function)" :align 'left
         :min-width 30 :align 'right)
        ))

       (data py-prof--table)

       (model ; data model
  (make-ctbl:model
   :column-model column-model :data data))
       component)
    
    (setq component ; building a ctable component
    (ctbl:create-table-component-buffer
     :model model))
    
    ;; Click event handler
    (ctbl:cp-add-click-hook
     component (lambda ()
     (let ((row  (py-prof-get-file (ctbl:cp-get-selected-data-row component))))
       ;;(message "CTable : Click Hook []")
       (if row
           (progn
       (message (plist-get row :file))
       (find-file (plist-get row :file))
       (forward-line (plist-get row :line))
       )
         )
       (message "Not a file link")
       )
     )
     ) ; update table

    ;; Selection change event handler
    (ctbl:cp-add-selection-change-hook
     component (lambda () (message "CTable : Select Hook %S"
           (ctbl:cp-get-selected component))))

    ;; Update event handler
    (ctbl:cp-add-update-hook
     component (lambda () (message "CTable : Update Hook")))
    
    (pop-to-buffer (ctbl:cp-get-buffer component)))  ; <- C-x C-e here to evaluate
  )


;; go=sys.modules.keys()
;; print os.path.abspath(cf.__file__)
;; os.getcwd()
;; sys.path

(defun py-prof-execute-command-cProfile (COMMAND)
  "Run cProfile and get the results.
COMMAND is a switcher: if equals 'run_function', then the function
asks for a command for which cProfile should be run, otherwise the name
of the file to profile is asked; then the profiler is run,
the output saved to a temp file, reload with pstats an the
output is saved to another human-readable file which; return the name of this
last temp file."
  ;;(interactive)
  (let*
      (
       (temp-file (make-temp-file nil nil ".prof"))
       (temp-file-csv (make-temp-file nil nil ".csv"))

       (comando
  (if (equal COMMAND "run_function")
      (s-replace  "\"" "'"  (read-from-minibuffer "Command to execute : " ""))
    (read-file-name "Enter name of the file to profile:")))
       (comandi (list
     "import cProfile"
     "import pstats"
     (if (equal COMMAND "run_function")
         (format "cProfile.run(\"%s\", filename='%s')" comando temp-file)
       (format "cProfile.run(open('%s', 'rb'), filename='%s')" comando temp-file)
       )
     (format "f = open('%s','w')" temp-file-csv)
     (format "ps = pstats.Stats('%s', stream=f)" temp-file)
     "ps.print_stats()"

     )
    )
       )
    (message temp-file)
    (mapc (lambda(x)
        (python-shell-send-string-no-output x (python-shell-get-process-or-error))
        )
      comandi
      )
    (python-shell-send-string-no-output "f.close()" (python-shell-get-process-or-error))

    temp-file-csv
    )
  )

(defun py-prof-get-file (CONTENT)
"Get the file containing the profiled function.
When the user clicks on a row of the table, the function attempts to
open the file containing the function analyised whithin that row.
CONTENT is the content of the clicked row of the ctable."
  (interactive)
  (let*
      ((py-prof-cell-file (nth 5 CONTENT))
       )
    (if (s-match "^[\{ \<]" py-prof-cell-file)
  nil
      (list :file (nth 0 (split-string py-prof-cell-file ":"))
      :line (string-to-number
       (replace-regexp-in-string ".*:\\([0-9]+\\).*" "\\1"
               py-prof-cell-file)
       )
      )
      )
    )
  )



(defun py-prof-ex(CALLED)
  "Create a table of cProfile output using the ctable package."
  (interactive)
  (let*
      (
       (temp-file-csv (py-prof-execute-command-cProfile CALLED))
       (temp-list (s-split "\n" (f-read temp-file-csv) t))
       ;; find the beginning of the cProfile table
       (temp-header (-find-index (lambda(x) (string-match-p "ncalls\\s-+tottime" x)) temp-list))
  ;; filter out the first rows
  (data-table (-drop (+ 1 temp-header) temp-list))
  (data-filtered (mapcar (lambda(x) (py-prof-split-lines-prun x)) data-table))
  )
       (py-prof-create-table data-filtered)
       )
    )


(defun py-prof-run()
  "docstring"
  (interactive)
  (py-prof-ex "run_function")
  )

(defun py-prof-file()
  "docstring"
  (interactive)
  (py-prof-ex "run_script")
  )


(define-minor-mode py-prof-mode
  "Observe cProfile output."
  :lighter "py-prof"
  :keymap (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c r") 'py-prof-run)
      (define-key map (kbd "C-c f") 'py-prof-file)
      map))


(provide 'py-prof)

;;; py-prof.el ends here
