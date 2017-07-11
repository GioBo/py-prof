;;; py-prof.el --- Easily observe python cProfile output

;; Copyright (C) 2017 gionata

;; Author: gionata <gionata@gionata-dell>
;; Version: 0.1

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

;;; Code:


(require 'dash)
(require 's)
(require 'ctable)

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
  "Build the ctable structure containing the cProfile data and render them.
Data - passed in the  PY-PROF--TABLE -  can be sorted clicking on the columns' headers."
  ;;(interactive)
  (let* ((column-model ; column model
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
	 (component ; ctable component
	  (ctbl:create-table-component-buffer
	   :model model)))
    (pop-to-buffer (ctbl:cp-get-buffer component)))


  )


(defun py-prof-execute-command-cProfile ()
  "Run cProfile on a command and get the results.
Ask for a command for which cProfile should be run, then run it,
save the output to a temp file, reload it with pstats and save the
output to another human-readable file which; return the name of this
last temp file."
  ;;(interactive)
  (let*
      (
       (temp-file (make-temp-file nil nil ".prof"))
       (temp-file-csv (make-temp-file nil nil ".csv"))

       (comando (read-from-minibuffer "Command to execute : " ""))
       (comando (s-replace "\"" "'" comando))
       (comandi (list
		 "import cProfile"
		 "import pstats"
		 (format "cProfile.run(\"%s\", filename='%s')" comando temp-file)
		 (format "f = open('%s','w')" temp-file-csv)
		 (format "ps = pstats.Stats('%s', stream=f)" temp-file)
		 "ps.print_stats()"

		 )
		)
       )
    (message temp-file)
    (mapcar (lambda(x)
	      (python-shell-send-string-no-output x (python-shell-get-process-or-error))
	      )
	    comandi
	    )
    (python-shell-send-string-no-output "f.close()" (python-shell-get-process-or-error))

    temp-file-csv
    )
  )


(defun py-prof ()
  "Create a table of cProfile output using the ctable package."
  (interactive)
  (let*
      (
       (temp-file-csv (py-prof-execute-command-cProfile))
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



(provide 'py-prof)

;;; py-prof.el ends here
