;;; snsync.el --- synchronize ServiceNow files -*- lexical-binding: t -*-

;; Author: Julian Hoch
;; Maintainer: Julian Hoch
;; Version: 0.1.0
;; Package-Requires: (servicenow)
;; Homepage: https://github.com/julian-hoch/snsync
;; Keywords: servicenow


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to synchronize ServiceNow artifacts with Emacs buffers.

;; Code:

(require 'servicenow)



;;; Customization

(defgroup snsync ()
  "Synchronize ServiceNow files with Emacs."
  :group 'servicenow)

;;;; Metadata

;; TODO buffer naming convention should be set here too
;; TODO potentially allow saved filters instead of fixed queries
(defcustom snsync-fields
  '((("sys_script_include" . "script") 
     . ( :label "Script Include"
         :name-field "name"
         :extension "js"
         :scope "sys_scope.scope"
         :query "sys_policy!=protected^ORsys_policy=^sys_scope=global"
         :mode js2-mode))
    (("sys_ui_macro" . "xml")
     . ( :label "UI Macro"
         :name-field ("name" "scoped_name")
         :extension "xml"
         :scope "sys_scope.scope"
         :query "sys_policy!=protected^ORsys_policy=^sys_scope=global"
         :mode nxml-mode))
    (("sys_ui_script" . "script")
     . ( :label "UI Script"
         :name-field ("script_name" "name")
         :extension "js"
         :scope "sys_scope.scope"
         :query "sys_policy!=protected^ORsys_policy=^sys_scope=global"
         :mode js2-mode)))
  "Metadata for ServiceNow fields.  Name-field is the field that should be
used to display the name of the record.  Can be a field name, a list of
field names, or a function that returns the content given the result
hashtable."
  :group 'snsync
  :type '(alist :key-type (cons string string)
		:value-type (plist
                             :options
                                   ((:label string)
                                    (:name-field (choice (string :tag "Primary name field")
                                                         (repeat string)))
                                    (:extension string)
                                    (:scope string)
                                    (:query string)
                                    (:mode symbol)))))

(defun snsync--get-field-metadata (table field)
  "Get metadata for TABLE and FIELD."
  (alist-get (cons table field) snsync-fields nil nil #'equal))

(defun snsync--get-field-mode (table field)
  "Get the mode for TABLE and FIELD."
  (let ((metadata (snsync--get-field-metadata table field)))
    (if metadata
        (plist-get metadata :mode)
      'fundamental-mode)))

(defun snsync--get-name-field (table field)
  "Get the name field for TABLE and FIELD."
  (let ((metadata (snsync--get-field-metadata table field)))
    (if metadata
        (plist-get metadata :name-field)
      nil)))

(defun snsync--get-field-label (table field)
  "Get the label for TABLE and FIELD."
  (let ((metadata (snsync--get-field-metadata table field)))
    (if metadata
        (plist-get metadata :label)
      nil)))

(defun snsync--get-field-query (table field)
  "Get the query for TABLE and FIELD."
  (let ((metadata (snsync--get-field-metadata table field)))
    (plist-get metadata :query)))

(defun snsync--get-field-extension (table field)
  "Get the file extension for TABLE and FIELD."
  (let ((metadata (snsync--get-field-metadata table field)))
    (if metadata
        (plist-get metadata :extension)
      "txt")))

(defun snsync--get-field-scope (table field)
  "Get the scope field for the given TABLE and FIELD."
  (let ((metadata (snsync--get-field-metadata table field)))
    (plist-get metadata :scope)))

(defun snsync--get-main-fields (table field)
  "Returns all the relevant fields for a certain TABLE and FIELD combination."
  (let ((name-field (snsync--get-name-field table field))
        (scope (snsync--get-field-scope table field)))
    (-flatten
     (if scope
         (list field name-field scope)
       (list field name-field)))))


;;; Loading from the Instance

(defun snsync--construct-record-data (table field sys-id data)
  "Construct a record data alist for TABLE, FIELD, SYS-ID and DATA."
  (let* ((scope-field (snsync--get-field-scope table field))
         (name-field (snsync--get-name-field table field))
         (scope-value (sn-get-result-field data scope-field))
         (display-value (sn-get-result-field data name-field))
         (content (sn-get-result-field data field)))
    (list (cons 'display-value display-value)
          (cons 'scope scope-value)
          (cons 'sys-id sys-id)
          (cons 'content content))))

(defun snsync--load-data (table field sys-id)
  "Load a record from TABLE with SYS-ID and main field FIELD, and return
the relevant data as an alist, with keys 'display-value and 'content."
  (let* ((fields (snsync--get-main-fields table field))
         (data (sn-get-record-json table sys-id fields)))
    (snsync--construct-record-data table field sys-id data)))

(defun snsync--fix-line-endings ()
  "Fix line endings in the current buffer, replacing CRLF with LF."
  (goto-char (point-min))
  (while (re-search-forward "\r\n" nil t)
    (replace-match "\n" nil t)))
         
(defun snsync--load-data-as-buffer (table field sys-id &optional data buffer)
  "Load a record from TABLE with SYS-ID and save the value of FIELD as a
new buffer.  If the buffer already exists, it content will be
overwritten.  If BUFFER is provided, use that buffer instead of creating
a new one.  Return the buffer containing the data."
  (unless data
    (setq data (snsync--load-data table field sys-id)))
  (let* ((content (string-trim (alist-get 'content data)))
         (scope (alist-get 'scope data))
         (display-value (alist-get 'display-value data))
         (extension (snsync--get-field-extension table field))
         (buffer-name (snsync--propose-buffer-name table field sys-id display-value))
         (buffer (or buffer (get-buffer-create buffer-name)))
         (use-file-locals nil))
    (unless content
      (error "No content found for %s.%s:%s" table field sys-id))
    (setq use-file-locals snsync-file-locals)
    (with-current-buffer buffer
      (save-excursion
        (erase-buffer)
        (insert content)
        (snsync--fix-line-endings)
        (funcall (snsync--get-field-mode table field))
        (setq-local snsync-current-table table
                    snsync-current-scope scope
                    snsync-current-field field
                    snsync-current-extension extension
                    snsync-current-display-value display-value
                    snsync-current-sys-id sys-id)
        (snsync--set-content-hash)
        (when use-file-locals
          (snsync--set-file-local-variables))
        (snsync-mode 1)
        (when snsync-auto-narrow-to-content
          (snsync-narrow-to-content))))
    buffer))

;;; User Interface Helpers

(defcustom snsync-auto-narrow-to-content nil
  "If non-nil, automatically narrow the buffer to the content of the ServiceNow record."
  :type 'boolean
  :group 'snsync)

(defun snsync-narrow-to-content ()
  "Narrow the current buffer to the content of the ServiceNow record,
excluding file-local variables."
  (interactive)
  (unless (snsync--buffer-connected-p)
    (error "This buffer is not associated with a ServiceNow record."))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward snsync-local-var-regex nil t)
    (narrow-to-region (point-min) (match-beginning 0)))))
   

(defun snsync--prompt-for-table-field ()
  "Prompt the user to select a table and field.  Returns a cons cell
of (TABLE . FIELD)."
  (let* ((label (completing-read "Select Table/Field: "
                                 (mapcar (lambda (pair)
                                           (plist-get (cdr pair) :label))
                                         snsync-fields)))
         (entry (seq-find (lambda (pair)
                            (string= (plist-get (cdr pair) :label) label))
                          snsync-fields))
         (table-field (car entry)))
    table-field))

(defun snsync--prompt-for-record (&optional table field)
  "Prompt the user to select a record from a TABLE and FIELD.  Returns the
TABLE, FIELD and sys_id of the selected record as an alist.

If TABLE and FIELD are not provided, prompt the user to select them."
  (unless (and table field)
    (let ((table-field (snsync--prompt-for-table-field)))
      (setq table (car table-field)
            field (cdr table-field))))
  (let* ((name-field (snsync--get-name-field table field))
         (label (or (snsync--get-field-label table field)
                    table))
         (query (snsync--get-field-query table field)))
    (sn--complete-reference table
                            query
                            (-flatten `("sys_id" "sys_scope.scope" ,name-field)) 
                            ;; formatter
                            (lambda (record)
                              (format "%s [%s]"
                                      (sn-get-result-field record name-field)
                                      (gethash "sys_scope.scope" record)))
                            ;; valueparser
                            (lambda (record)
                              (list :table table
                                    :field field
                                    :sys-id (gethash "sys_id" record)))
                            (format "Select %s:" label))))

;;; Managing Buffers

;;;; Buffer Local Variables
;; These variables maintain state for buffers corresponding to ServiceNow artifacts.

(defvar-local snsync-current-table nil
  "The current ServiceNow table being accessed in this buffer.")

(defvar-local snsync-current-field nil
  "The current ServiceNow field being accessed in this buffer.")

(defvar-local snsync-current-sys-id nil
  "The current ServiceNow sys_id being accessed in this buffer.")

(defvar-local snsync-current-scope nil
  "The current ServiceNow scope being accessed in this buffer.")

(defvar-local snsync-current-display-value nil
  "The current ServiceNow display value being accessed in this buffer.")

(defvar-local snsync-current-extension nil
  "The file extension to use for the current buffer.")

(defvar-local snsync-file-locals nil
  "Whether the current buffer uses file local variables")

;;;; Buffer Helpers

(defun snsync--propose-buffer-name (table field sys-id display-value)
  "Generate a buffer name for TABLE, FIELD, SYS-ID and DISPLAY-VALUE."
  (format "%s (%s.%s:%s)" display-value table field sys-id))

(defun snsync--buffer-connected-p ()
  "Check if the current buffer is connected to a ServiceNow record."
  (and snsync-current-table
       snsync-current-field
       snsync-current-sys-id
       snsync-current-display-value
       snsync-current-extension
       t))

;;;; Downloading To Buffer

;;;###autoload
(defun snsync-download-buffer (&optional table field sys-id)
  "Open a record in a buffer.  If TABLE, FIELD and SYS-ID are not provided,
prompt the user to select them."
  (interactive)
  (unless (and table field sys-id)
    (let ((record (snsync--prompt-for-record table field)))
      (setq table (plist-get record :table)
            field (plist-get record :field)
            sys-id (plist-get record :sys-id))))
  (switch-to-buffer (snsync--load-data-as-buffer table field sys-id)))

;;;###autoload
(defun snsync-reload-buffer (&optional buffer)
  "Reload the BUFFER from the ServiceNow instance.  If BUFFER is not
specified, use the current buffer."
  (interactive)
  (when buffer
    (switch-to-buffer buffer))
  (unless (snsync--buffer-connected-p)
    (error "This buffer is not associated with a ServiceNow record."))
  (let ((table snsync-current-table)
        (field snsync-current-field)
        (sys-id snsync-current-sys-id))
    (snsync--load-data-as-buffer table field sys-id nil (current-buffer))))
  
;;; Saving to the Instance

(defcustom snsync-strip-file-vars t
  "If non-nil, strip file-local variables before uploading."
  :type 'boolean
  :group 'snsync)

(defvar snsync-local-var-regex
  "[[:space:]\n]*\\(<!--\\|//\\) Local Variables:[.\n]+"
  "Regular expression to match file-local variables in the buffer.")

;;;###autoload
(defun snsync-upload-buffer (&optional buffer)
  "Upload data in BUFFER to the ServiceNow instance.  If no BUFFER is
specified, use the current buffer."
  (interactive)
  (when buffer
    (switch-to-buffer buffer))
  (save-restriction
    (widen)
    (when snsync-strip-file-vars
      (snsync-narrow-to-content))
    (let* ((sys-id snsync-current-sys-id)
           (table snsync-current-table)
           (field snsync-current-field)
           (content (buffer-substring-no-properties (point-min) (point-max)))
           (fields (list (cons field content))))
      (sn-update-record table sys-id fields)
      (snsync--set-content-hash)
      (message "Uploaded %s.%s:%s" table field sys-id))))

;;;; Buffer Misc Commands

;;;###autoload
(defun snsync-buffer-status ()
  "Display the current status of the buffer."
  (interactive)
  (if (snsync--buffer-connected-p)
      (let* ((locally-changed (snsync--is-locally-modified-p))
             (remote-changed (snsync--is-remote-modified-p))
             (status (cond
                      ((and locally-changed remote-changed) "Locally and remotely modified")
                      (locally-changed "Locally modified")
                      (remote-changed "Remotely modified")
                      (t "Up to date"))))
        (message "Table: %s, Field: %s, Sys-ID: %s, Scope: %s.  Status: %s"
                 snsync-current-table
                 snsync-current-field
                 snsync-current-sys-id
                 snsync-current-scope
                 status))
    (message "This buffer is not associated with a ServiceNow record.")))

;;;###autoload
(defun snsync-open-in-browser ()
  "Open the current ServiceNow record in a web browser."
  (interactive)
  (if (snsync--buffer-connected-p)
      (sn--open-in-browser snsync-current-table snsync-current-sys-id)
    (error "This buffer is not associated with a ServiceNow record.")))

;;; Managing Files

(defcustom snsync-base-dir
  (expand-file-name "snsync" user-emacs-directory)
  "Base directory for ServiceNow synchronization."
  :type 'directory
  :group 'snsync)

(defcustom snsync-add-file-vars t
  "If non-nil, add file-local variables to the files created by snsync."
  :type 'boolean
  :group 'snsync)

(put 'snsync-current-scope 'safe-local-variable
     (lambda (x) (or (null x) (stringp x))))
(put 'snsync-current-table 'safe-local-variable
     (lambda (x) (or (null x) (stringp x))))
(put 'snsync-current-field 'safe-local-variable
     (lambda (x) (or (null x) (stringp x))))
(put 'snsync-current-sys-id 'safe-local-variable
     (lambda (x) (or (null x) (stringp x))))
(put 'snsync-current-display-value 'safe-local-variable
     (lambda (x) (or (null x) (stringp x))))
(put 'snsync-current-extension 'safe-local-variable
        (lambda (x) (or (null x) (stringp x))))
(put 'snsync-file-locals 'safe-local-variable
     (lambda (x) (or (null x) (booleanp x))))

(defun snsync--construct-file-name (scope table field sys-id display-value extension)
  "Generate a file name for the file representing the data in SCOPE / TABLE / FIELD, for the given FIELD
and SYS-ID, with DISPLAY-VALUE and EXTENSION.  Can contain slashes to
indicate subdirectories."
  (format "%s/%s/%s.%s.%s" scope table display-value field extension))

(defun snsync--construct-file-path (scope table field sys-id display-value extension)
  "Generate a file path for the file representing the data in SCOPE / TABLE / FIELD, for the given FIELD
and SYS-ID, with DISPLAY-VALUE and EXTENSION.  Can contain slashes to
indicate subdirectories."
  (expand-file-name (snsync--construct-file-name scope table field sys-id display-value extension)
                    snsync-base-dir))

(defun snsync--get-buffer-path ()
  "Get the file path for the current buffer."
  (when (snsync--buffer-connected-p)
    (snsync--construct-file-path snsync-current-scope
                                 snsync-current-table
                                 snsync-current-field
                                 snsync-current-sys-id
                                 snsync-current-display-value
                                 snsync-current-extension)))
                           
;;;###autoload
(defun snsync-save-buffer-to-file ()
  "Save the current buffer to a file in the ServiceNow synchronization directory."
  (interactive)
  (if (snsync--buffer-connected-p)
      (let ((file-path (snsync--get-buffer-path)))
        (unless file-path
          (error "Cannot determine file path for current buffer."))
        (write-file file-path)
        (message "Saved buffer to %s" file-path))
    (error "This buffer is not associated with a ServiceNow record.")))

(defun snsync--set-file-local-variables ()
  "Append buffer-local variables as file-local variables to the current buffer."
  (interactive)
  (unless (snsync--buffer-connected-p)
    (error "This buffer is not associated with a ServiceNow record."))
  (setq-local snsync-file-locals t)
  (add-file-local-variable 'snsync-mode 1)
  (add-file-local-variable 'snsync-file-locals t)
  (add-file-local-variable 'snsync-current-table snsync-current-table)
  (add-file-local-variable 'snsync-current-field snsync-current-field)
  (add-file-local-variable 'snsync-current-sys-id snsync-current-sys-id)
  (add-file-local-variable 'snsync-current-scope snsync-current-scope)
  (add-file-local-variable 'snsync-current-display-value snsync-current-display-value)
  (add-file-local-variable 'snsync-current-extension snsync-current-extension)
  (add-file-local-variable 'snsync-content-hash snsync-content-hash))

;;;###autoload
(defun snsync-get-file (&optional table field sys-id)
  "Download and save a ServiceNow record.  If TABLE,
FIELD and SYS-ID are not provided, prompt the user to select them."
  (interactive)
  (snsync-download-buffer table field sys-id)
  (when snsync-add-file-vars
    (snsync--set-file-local-variables))
  (when snsync-auto-narrow-to-content
    (snsync-narrow-to-content))
  (snsync-save-buffer-to-file))

;;;###autoload
(defun snsync-get-all-files-of-table-field (&optional table field query)
  "Download and save all records of TABLE and FIELD matching QUERY.  If TABLE and
FIELD are not provided, prompt the user to select them.  If QUERY is not
provided, use the default query for the field."
  (interactive)
  (unless (and table field)
    (let ((table-field (snsync--prompt-for-table-field)))
      (setq table (car table-field)
            field (cdr table-field))))
  (unless query
    (setq query (snsync--get-field-query table field)))
  (message "Synchronizing all records of %s.%s with query: %s"
                  table field query)
  (let* ((fields (cons "sys_id" (snsync--get-main-fields table field)))
         (records-flat (sn-get-records-json table query fields))
         (record-list (mapcar (lambda (record)
                                (snsync--construct-record-data table field
                                                               (gethash "sys_id" record)
                                                               record))
                              records-flat)))
    (dolist (record record-list)
      (snsync--load-data-as-buffer table
                                   field
                                   (alist-get 'sys-id record)
                                   record)
      (when snsync-add-file-vars
        (snsync--set-file-local-variables))
      (snsync-save-buffer-to-file))))


;;;###autoload
(defun snsync-get-all-files ()
  ;; TODO potentially only load changed files
  "Synchronize all records of all tables and fields defined in `snsync-fields`."
  (interactive)
  (dolist (pair snsync-fields)
    (let ((table (caar pair))
          (field (cdar pair)))
      (message "Synchronizing all records of %s.%s" table field)
      (snsync-get-all-files-of-table-field table field))))

;; TODO sync all files (download / upload?)

;;;###autoload
(defun snsync-dwim ()
  "Synchronize the current buffer with the ServiceNow instance.  If there are no remote changes, upload the buffer.  If there are no local changes, download the buffer.  If there are both local and remote changes, prompt the user to resolve the conflict."
  (interactive)
  (unless (snsync--buffer-connected-p)
    (error "This buffer is not associated with a ServiceNow record."))
  (let ((locally-changed (snsync--is-locally-modified-p))
        (remote-changed (snsync--is-remote-modified-p)))
    (cond
        ((and locally-changed remote-changed)
         (snsync-resolve-conflicts))
        (locally-changed
         (snsync-upload-buffer)
         (message "Local changes uploaded to ServiceNow."))
        (remote-changed
         (snsync-reload-buffer)
         (message "Remote changes downloaded from ServiceNow."))
        (t
         (message "No changes detected. Buffer is up to date.")))))

;;; Conflict Detection

(defvar-local snsync-content-hash nil
  "Hash of the buffer's data as of last synchronization.  Used to detect local modifications.")

(put 'snsync-content-hash 'safe-local-variable
     (lambda (x) (or (null x) (stringp x))))

(defun snsync--get-buffer-hash ()
  "Get a hash of the current buffer's content, excluding file-local variables."
  (unless (snsync--buffer-connected-p)
    (error "This buffer is not associated with a ServiceNow record."))
  (save-restriction
    (widen)
    (snsync-narrow-to-content)
        (secure-hash 'md5 (current-buffer))))

(defun snsync--set-content-hash ()
  "Set the content hash of the current buffer to the current content hash."
  (setq-local snsync-content-hash (snsync--get-buffer-hash)))

(defun snsync--is-locally-modified-p ()
  "Check if the content of the current buffer has been modified since the last synchronization."
  (unless (snsync--buffer-connected-p)
    (error "This buffer is not associated with a ServiceNow record."))
  (not (string= snsync-content-hash (snsync--get-buffer-hash))))

(defvar snsync--temp-buffer-name "*snsync-temp*"
  "Temporary buffer name used for loading data from ServiceNow.")

(defun snsync--get-remote-hash ()
  "Get the hash of the remote record's content."
  (unless (snsync--buffer-connected-p)
    (error "This buffer is not associated with a ServiceNow record."))
  (let ((table snsync-current-table)
        (field snsync-current-field)
        (sys-id snsync-current-sys-id))
    (with-current-buffer (get-buffer-create snsync--temp-buffer-name)
      (snsync--load-data-as-buffer table field sys-id nil (current-buffer))
      (snsync--get-buffer-hash))))

(defun snsync--temp-remote-buffer-name ()
  "Create a temporary buffer name for the remote record's content."
        (format "*%s:REMOTE*" (buffer-name)))

(defun snsync--is-remote-modified-p ()
  "Check if the remote record has been modified since the last synchronization."
  (unless (snsync--buffer-connected-p)
    (error "This buffer is not associated with a ServiceNow record."))
  (let ((snsync--temp-buffer-name (snsync--temp-remote-buffer-name)))
    (not (string= snsync-content-hash (snsync--get-remote-hash)))))

;;; Conflict Resolution

(defcustom snsync-save-merge-window-config t
  "If non-nil, save the window configuration before merging buffers."
  :type 'boolean
  :group 'snsync)

(defcustom snsync-auto-upload-after-merge t
  "If non-nil, automatically upload the buffer after merging."
  :type 'boolean
  :group 'snsync)

(defvar-local snsync-pre-merge-window-config nil
  "Window configuration before merging buffers.  Used to restore the window layout after merging.")

(defun snsync--merge-buffers ()
  "Merge the current buffer with the temporary buffer containing the remote record's content."
  (unless (snsync--buffer-connected-p)
    (error "This buffer is not associated with a ServiceNow record."))
  (let* ((snsync--temp-buffer-name (snsync--temp-remote-buffer-name))
        (diff-buffer (get-buffer snsync--temp-buffer-name)))
    (unless diff-buffer
      (error "Temporary buffer for diffing does not exist."))
    (when snsync-save-merge-window-config
      (setq-local snsync-pre-merge-window-config (current-window-configuration)))
    (snsync-narrow-to-content)
    (ediff-merge-buffers (current-buffer) diff-buffer)))

(defun snsync--apply-merge ()
  "Take the output of `ediff-merge-buffers' and apply it to the local
buffer, then send to the instance.
Runs when ediff merge finishes (if hook is set up)." 
  (let ((local-buffer ediff-buffer-A)
        (remote-buffer ediff-buffer-B)
        (merged-buffer ediff-buffer-C)
        (is-connected
         (with-current-buffer ediff-buffer-A (snsync--buffer-connected-p)))
        (previous-window-config
         (with-current-buffer ediff-buffer-A snsync-pre-merge-window-config))
        (result))
    (set-buffer remote-buffer)
    (setq buffer-read-only nil)
    (set-buffer local-buffer)
    (setq buffer-read-only nil)
    (when is-connected
      (if (not (yes-or-no-p "Apply merges?"))
          (progn
            (when snsync-save-merge-window-config
              (set-window-configuration snsync-pre-merge-window-config))
            (message "Merges not applied."))
        ;; TODO ideally we would check if the result buffer still has conflict markers
        (set-buffer merged-buffer)
        (setq result (buffer-substring-no-properties (point-min) (point-max)))
        (set-buffer local-buffer)
        (erase-buffer)
        (insert result)
        (when snsync-file-locals
          (snsync--set-file-local-variables))
        (when snsync-auto-narrow-to-content
          (snsync-narrow-to-content))
        (when snsync-save-merge-window-config
          (set-window-configuration snsync-pre-merge-window-config))
        (when snsync-auto-upload-after-merge
          (snsync-upload-buffer))
        (message "Merges applied.")))))

(defcustom snsync-diff-function 'diff-buffers
  "Function to use for diffing buffers.  Default is `diff-buffers'."
  :type 'function
  :group 'snsync)

(defun snsync--diff-buffers ()
  "Diff the current buffer with the temporary buffer containing the remote record's content."
  (unless (snsync--buffer-connected-p)
    (error "This buffer is not associated with a ServiceNow record."))
  (let* ((snsync--temp-buffer-name (snsync--temp-remote-buffer-name))
         (diff-buffer (get-buffer snsync--temp-buffer-name)))
    (unless diff-buffer
      (error "Temporary buffer for diffing does not exist."))
    (funcall snsync-diff-function (current-buffer) diff-buffer)))

(defun snsync--prompt-conflict-resolution ()
  "Prompt the user to resolve a conflict between local and remote changes."
 (let ((read-answer-short t)) 
   (read-answer "Local changes detected.  Remote changes detected.  How do you want to
resolve the conflict? "
                '(("upload" ?\C-u "Upload local changes")
                  ("download" ?\C-d "Download remote changes")
                  ("diff" ?d "Diff local and remote changes")
                  ("merge" ?m "Merge local and remote changes")
                  ("open" ?o "Open remote record in browser")
                  ("quit" ?q "Quit without resolving")))))

(defun snsync-resolve-conflicts ()
  "Resolve a conflict between local and remote changes in the current buffer."
  (interactive)
  (unless (snsync--buffer-connected-p)
    (error "This buffer is not associated with a ServiceNow record."))
  (let ((resolution (snsync--prompt-conflict-resolution)))
    (cond
     ((string= resolution "upload")
      (snsync-upload-buffer))
     ((string= resolution "download")
      (snsync-reload-buffer))
     ((string= resolution "diff")
      (snsync--diff-buffers))
     ((string= resolution "merge")
      (snsync--merge-buffers))
     ((string= resolution "open")
      (snsync-open-in-browser))
     ((string= resolution "quit")
      (message "Conflict resolution aborted."))
     (t
      (error "Unknown resolution option: %s" resolution)))))


;;; Minor Mode

;;;###autoload
(define-minor-mode snsync-mode
  "Enable ServiceNow synchronization mode."
  :global nil
  :group 'snsync
  :lighter "snSYNC"
  :after-hook
  (when (and snsync-mode snsync-auto-narrow-to-content)
    (snsync-narrow-to-content))
  :keymap (make-sparse-keymap))

(put 'snsync-mode 'safe-local-variable
     (lambda (x) (or (null x) (eq x 1))))


(provide 'snsync)

;;; snsync.el ends here
