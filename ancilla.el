;;; ancilla.el --- Emacs AI assistance for code generation, editing & refactoring. -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27") cl-lib dash)

;; Commentary:

;; This package provides AI-powered assistance for code generation,
;; editing, refactoring, and answering questions.
;;
;; (use-package ancilla
;;  :straight (:host github :repo "shouya/ancilla.el")
;;  :custom (ancilla-adaptor-chatgpt-openai-api-key "sk-XXXXXXXXXX")
;;  :bind ("C-x C-r" . ancilla-rewrite))
;;

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
      (funcall extract (json-read)))))

(defun ancilla--adaptor-chatgpt-extract-content (json)
  (plist-get (plist-get (car (plist-get json 'choices)) 'message) 'content))

(defun ancilla--adaptor-chatgpt-get-text-between (content start end)
  "Get the part of CONTENT between START and END"
  (let* ((start-pos (and (string-match start content) (match-end 0)))
         (end-pos (string-match end content start-pos)))
    (when (and start-pos end-pos)
      (substring content start-pos end-pos))))

(defun ancilla--adaptor-chatgpt-request-buffer-parse ()
  (with-current-buffer (get-buffer-create "*ancilla-chatgpt*")
    (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
           (messages (split-string content "\n\f\n" t)))
      (mapcar (lambda (message)
                (cond
                 ((string-prefix-p "USER> " message)
                  `("user" . ,(string-remove-prefix "USER> " message)))
                 ((string-prefix-p "SYSTEM> " message)
                  `("system" . ,(string-remove-prefix "SYSTEM> " message)))
                 ((string-prefix-p "ASSISTANT> " message)
                  `("assistant" . ,(string-remove-prefix "ASSISTANT> " message)))
                 ))
              messages))))

(defun ancilla--adaptor-chatgpt-request-buffer-append (role text)
  (with-current-buffer (get-buffer-create "*ancilla-chatgpt*")
    (save-excursion
      (goto-char (point-max))
      (insert (format "%s> " (upcase role)))
      (insert (format "%s\n\f\n" text)))))

(defun ancilla--adaptor-chatgpt-request-buffer-reset ()
  (with-current-buffer (get-buffer-create "*ancilla-chatgpt*")
    (delete-region (point-min) (point-max))))

(defun ancilla--adaptor-chatgpt-request-buffer ()
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" .
             ,(concat "Bearer " ancilla-adaptor-chatgpt-openai-api-key))))
         (messages (mapcar (lambda (x) `(("role" . ,(car x))
                                         ("content" . ,(cdr x))))
                           (ancilla--adaptor-chatgpt-request-buffer-parse)))
         (url-request-data
          (json-encode
           `(:model "gpt-3.5-turbo"
                    :messages ,messages
                    :temperature 0.1))))

    (ancilla--request-and-extract-json
     :url "https://api.openai.com/v1/chat/completions"
     :extract
     (lambda (json)
       (ancilla--adaptor-chatgpt-request-buffer-append
        "assistant"
        (ancilla--adaptor-chatgpt-extract-content json))))

    (pcase (last (ancilla--adaptor-chatgpt-request-buffer-parse))
      (`(("assistant" . ,message))
       (ancilla--adaptor-chatgpt-get-text-between
        message
        "<|begin replacement|>" "<|end replacement|>")))))

(cl-defun ancilla--adaptor-chatgpt-rewrite (&key instruction buffer-context)
  "Rewrite code using the chatgpt adaptor.

INSTRUCTION: The instruction to follow when rewriting the code.
BUFFER-CONTEXT: The context about what needs to be rewritten."
  (ancilla--adaptor-chatgpt-request-buffer-reset)

  ;; context prompt
  (ancilla--adaptor-chatgpt-request-buffer-append
   "user"
   (concat "Here is the context that may or may not be useful:"
           "\n- filename: " (plist-get buffer-context :file-name)
           "\n- editor mode: " (plist-get buffer-context :buffer-mode)
           ))

  ;; input
  (ancilla--adaptor-chatgpt-request-buffer-append
   "user"
   (concat (plist-get buffer-context :before-selection)
           "<|begin selection|>"
           (plist-get buffer-context :selection)
           "<|end selection|>"
           (plist-get buffer-context :after-selection)
           ))

  ;; instruction
  (ancilla--adaptor-chatgpt-request-buffer-append
   "user"
   (concat "User selected the region marked by <|begin selection|>/<|end selection|> and asked:\n"
           instruction
           "\n\nReply with the replacement for SELECTION. "
           "Your response must begin with <|begin replacement|> "
           "and stop at <|end replacement|>. "
           "Do not include updated code. Preserve original whitespace.")
   )
  (ancilla--adaptor-chatgpt-request-buffer))

(cl-defun ancilla--adaptor-dummy-rewrite (&key instruction selection)
  "(provide 'bugzilla)")

(function-put 'chatgpt 'ancilla-rewrite 'ancilla--adaptor-chatgpt-rewrite)

(provide 'ancilla)

;;; ancilla.el ends here
