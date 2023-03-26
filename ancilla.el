;;; ancilla.el --- Emacs package that uses AI to assist with code generation, editing, refactoring, and answering questions. ;; -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27"))

;;; Commentary:

;; This package provides AI-powered assistance for code generation,
;; editing, refactoring, and answering questions.  It uses
;; state-of-the-art machine learning algorithms to analyze code and
;; provide suggestions for improvements.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)
(require 'diff)

(defvar url-http-end-of-headers)

(defgroup ancilla nil
  "Emacs package that uses AI to assist with code generation, editing, refactoring, and answering questions."
  :group 'tools)

(defcustom ancilla-adaptors
  '(chatgpt codex)
  "List of adaptors for AI-powered assistance."
  :type '(repeat symbol)
  :group 'ancilla)

(defcustom ancilla-adaptor
  'chatgpt
  "Current adaptor for AI-powered assistance."
  :type 'symbol
  :group 'ancilla)

(defcustom ancilla-adaptor-chatgpt-openai-api-key
  nil
  "API key for the OpenAI GPT-3 API used by the chatgpt adaptor."
  :type 'string
  :group 'ancilla)

(defun ancilla-ask ()
  "Ask a coding-related question and get an AI-powered answer."
  (interactive)
  (message "Ask a question function not yet implemented."))

(defun ancilla-generate ()
  "Generate code using AI-powered suggestions."
  (interactive)
  (message "Code generation function not yet implemented."))

(defun ancilla-rewrite ()
  "Refactor code using AI-powered suggestions."
  (interactive)
  (let* ((instruction (read-string "Instruction: "))
         (buffer-context (ancilla--get-buffer-context))
         (refactored-code (funcall (function-get ancilla-adaptor 'ancilla-rewrite)
                                   :instruction instruction
                                   :buffer-context buffer-context))
         (old-code (plist-get buffer-context :selection)))
    (ancilla--show-diff-changes old-code refactored-code)
    (when (y-or-n-p "Accept the change? ")
      (save-excursion
        (delete-region (region-beginning) (region-end))
        (insert refactored-code)))
    (ancilla--hide-diff-changes)))


(defun ancilla--show-diff-changes (old new)

  (with-current-buffer
      (get-buffer-create " *ancilla-diff-old*")
    (delete-region (point-min) (point-max))
    (insert old))
  (with-current-buffer
      (get-buffer-create " *ancilla-diff-new*")
    (delete-region (point-min) (point-max))
    (insert new))
  (save-excursion
    (let ((diff-buffer (get-buffer-create "*ancilla-diff*")))
      (diff-no-select (get-buffer " *ancilla-diff-old*")
                      (get-buffer " *ancilla-diff-new*")
                      nil nil
                      diff-buffer)
      (display-buffer diff-buffer)
      diff-buffer)))


(defun ancilla--hide-diff-changes ()
  "Delete the window of the buffer with the name \"*ancilla-diff*\"."
  (delete-window (get-buffer-window "*ancilla-diff*")))

(defun ancilla--get-buffer-context ()
  "Get context, which includes file-name, buffer-mode, selection,
text before selection, and text after selection."
  (interactive)
  (let* ((get-region (lambda (from to) (buffer-substring-no-properties from to)))
         (region-start (if (not (region-active-p)) (point) (region-beginning)))
         (region-end (if (not (region-active-p)) (point) (region-end))))
    (list :file-name (buffer-file-name)
          :buffer-mode (symbol-name major-mode)
          :selection (funcall get-region region-start region-end)
          :before-selection (funcall get-region
                                     (max (point-min) (window-start))
                                     region-start)
          :after-selection (funcall get-region
                                    region-end
                                    (min (point-max) (window-end))))))

(cl-defun ancilla--request-and-extract-json (&key url extract)
  "Call 'url-retrieve-synchronously' and parse the json, call extract
on that json, and return whatever extract returns.

URL: The URL to retrieve.

EXTRACT: A function that takes the JSON response and extracts the
desired information."
  (let ((json-object-type 'plist)
        (json-key-type 'symbol)
        (json-array-type 'list))
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char url-http-end-of-headers)
      (funcall extract (json-read)))
    ))

(defun ancilla--adaptor-chatgpt-parse-response (json start end)
  (let* ((message (plist-get (car (plist-get json 'choices)) 'message))
         (content (plist-get message 'content)))
         (with-temp-buffer
           (insert content)
           (goto-char (point-min))
           (message (buffer-string))
           (search-forward start)
           ;; (forward-char)
           (set-mark (point))
           (search-forward end)
           (backward-char (length end))
           ;; (backward-char (1+ (length end)))
           (buffer-substring-no-properties (region-beginning) (region-end)))))

(cl-defun ancilla--adaptor-chatgpt-rewrite (&key instruction buffer-context)
  "Rewrite code using the chatgpt adaptor.

INSTRUCTION: The instruction to follow when rewriting the code.
BUFFER-CONTEXT: The context about what needs to be rewritten."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " ancilla-adaptor-chatgpt-openai-api-key))))
         (prompt
          (concat "User selected the region marked by <|BEGIN SELECTION|>/<|END SELECTION|> and asked:\n"
                  instruction
                  "\n\nReply with the replacement for SELECTION. "
                  "Your response must begin with <|BEGIN REPLACEMENT|> "
                  "and stop at <|END REPLACEMENT|>. "
                  "Do not include updated code."))
         (context-prompt
          (concat "Here is the context that may or may not be useful:"
                  "\n- filename: " (plist-get buffer-context :file-name)
                  "\n- editor mode: " (plist-get buffer-context :buffer-mode)
                  ))
         (input (concat (plist-get buffer-context :before-selection)
                         "<|BEGIN SELECTION|>"
                         (plist-get buffer-context :selection)
                         "<|END SELECTION|>"
                         (plist-get buffer-context :after-selection)
                         ))
         (messages `((("role" . "user") ("content" . ,context-prompt))
                     (("role" . "user") ("content" . ,input))
                     (("role" . "user") ("content" . ,prompt))))
         (url-request-data (json-encode `(:model "gpt-3.5-turbo" :messages ,messages))))
    (with-current-buffer (get-buffer-create "*ancilla-chatgpt-debug*")
      (delete-region (point-min) (point-max))
      (insert prompt)
      (insert "\n\f\n")
      (insert context-prompt)
      (insert "\n\f\n")
      (insert input))
    (ancilla--request-and-extract-json
     :url "https://api.openai.com/v1/chat/completions"
     :extract
     (lambda (json)
       (ancilla--adaptor-chatgpt-parse-response
        json "<|BEGIN REWRITE|>" "<|END REWRITE|>")))))

(cl-defun ancilla--adaptor-dummy-rewrite (&key instruction selection)
  "(provide 'bugzilla)")

(function-put 'chatgpt 'ancilla-rewrite 'ancilla--adaptor-chatgpt-rewrite)

(provide 'ancilla)

;;; ancilla.el ends here
