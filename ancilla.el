;;; ancilla.el --- Your AI coding assistant -*- lexical-binding: t; -*-

;; Author: Shou Ya <shouya@users.noreply.github.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27"))
;; Homepage: https://github.com/shouya/ancilla.el

;;; Commentary:

;; This package provides AI-powered assistance for code generation,
;; editing, refactoring, and answering questions.
;;
;; (use-package ancilla
;;  :straight (:host github :repo "shouya/ancilla.el")
;;  :custom (ancilla-adaptor-chat-openai-api-key "sk-XXXXXXXXXX")
;;  :bind ("C-x C-r" . ancilla-generate-or-rewrite))
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)
(require 'diff)
(require 'files)
(require 'let-alist)

(defvar url-http-end-of-headers)

(defgroup ancilla nil
  "Emacs package that uses AI to assist with code generation, editing, refactoring, and answering questions."
  :group 'tools)

(defcustom ancilla-adaptor
  'chat
  "Current adaptor for AI-powered assistance."
  :type 'symbol
  :group 'ancilla
  :options '(chat))

(defcustom ancilla-adaptor-chat-model
  "gpt-3.5-turbo"
  "The model to use."
  :type 'string
  :group 'ancilla
  :options '("gpt-3.5-turbo" "gpt-4"))

(defcustom ancilla-adaptor-chat-api-endpoint
  "https://api.openai.com/v1/chat/completions"
  "API endpoint for the OpenAI chat completions."
  :type 'string
  :group 'ancilla)

(defcustom ancilla-adaptor-chat-openai-api-key
  nil
  "API key for the OpenAI GPT-3 API used by the chat adaptor."
  :type 'string
  :group 'ancilla)

(defcustom ancilla-async
  t
  "Whether or not to use async mode."
  :type 'boolean
  :group 'ancilla)

(defcustom ancilla-show-confirmation 'rewrite-only
  "Specify when to show confirmation.

Possible values are t (always), 'rewrite-only, 'generate-only, or nil (never)."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Only for rewrite" rewrite-only)
                 (const :tag "Only for generate" generate-only)
                 (const :tag "Never" nil))
  :group 'ancilla)

;; ------------- PUBLIC COMMANDS ---------------
;;;###autoload
(defun ancilla-ask ()
  "Ask a coding-related question and get an AI-powered answer."
  (interactive)
  (message "Ask a question function not yet implemented."))

;;;###autoload
(defun ancilla-generate-or-rewrite ()
  "Generate or rewrite code using AI-powered suggestions.

Calls 'ancilla-generate' of there is no active selection, and
call 'ancilla-rewrite' otherwise."
  (interactive)
  (if (region-active-p)
      (ancilla-rewrite)
    (ancilla-generate)))

;;;###autoload
(defun ancilla-generate ()
  "Generate code using AI-powered suggestions."
  (interactive)
  (ancilla--call-adaptor-with-instruction 'generate))

;;;###autoload
(defun ancilla-rewrite ()
  "Refactor code using AI-powered suggestions."
  (interactive)
  (ancilla--call-adaptor-with-instruction 'rewrite))

;; ------------ PRIVATE FUNCTIONS --------------

(defun ancilla--call-adaptor-with-instruction (mode)
  (let* ((instruction (read-string "Instruction: "))
         (buffer-context (ancilla--get-buffer-context))
         (adaptor-request-fn-name
          (cond
           ((eq mode 'rewrite) 'ancilla-rewrite)
           ((eq mode 'generate) 'ancilla-generate)))
         (adaptor-request-fn (get ancilla-adaptor adaptor-request-fn-name)))
    (when-let ((hooks (get ancilla-adaptor 'ancilla-hooks)))
      (dolist (hook hooks) (funcall hook
                                    :instruction instruction
                                    :buffer-context buffer-context
                                    :mode mode)))
    (funcall adaptor-request-fn
             :instruction instruction
             :buffer-context buffer-context
             :callback
             (apply-partially 'ancilla--diff-replace-selection
                              mode
                              (plist-get buffer-context :excursion)
                              (plist-get buffer-context :selection)))))

(defun ancilla--show-confirmation-p (mode)
  "Determine whether to show confirmation based on the current mode.

MODE should be either 'rewrite' or 'generate'.  The function
checks the value of `ancilla-show-confirmation` and returns t if
a confirmation message should be shown, or nil otherwise."
  (pcase ancilla-show-confirmation
    ('t t)
    ('nil nil)
    ('rewrite-only (eq mode 'rewrite))
    ('generate-only (eq mode 'generate))
    (_ nil)))

(defmacro ancilla--with-excursion (excursion &rest forms)
  "Execute FORMS with the excursion saved in EXCURSION.

EXCURSION should be a triple generated with:

  `(,(current-buffer) ,(point-marker) ,(mark-marker))"

  `(let ((buffer (nth 0 ,excursion))
         (point (nth 1 ,excursion))
         (mark (nth 2 ,excursion)))
     (save-excursion
       (with-current-buffer buffer
         (goto-char point)
         (push-mark mark t nil)
         ,@forms
         (pop-mark)))))

(defun ancilla--replace-selection (excursion new-text)
  "Replace the current selection with NEW-TEXT in the buffer saved in EXCURSION.

NEW-TEXT is the text to replace the current selection with.  If the
region is not active, NEW-TEXT will be inserted at the current point
in the buffer saved in EXCURSION."
  (ancilla--with-excursion
   excursion
   (when (region-active-p)
     (delete-region (region-beginning) (region-end)))
   (insert new-text)))

(defun ancilla--diff-replace-selection (mode excursion old-text new-text)
  "Replace the current selection with NEW-TEXT.

Based on the MODE and the 'ancilla-show-confirmation' config,
show a diff of the changes between OLD-TEXT and NEW-TEXT.  If the
user accepts the change, the selection is replaced with NEW-TEXT.
Otherwise, the selection is left unchanged.

One must pass the EXCURSION before the change is made.  See
'ancilla--with-excursion'."
  (if (not (ancilla--show-confirmation-p mode))
      ;; accept without asking
      (ancilla--replace-selection excursion new-text)
    ;; show diff and confirmation
    (ancilla--show-diff-changes old-text new-text)

    ;; scroll the diff window without leaving y-or-n-p prompt.
    (let ((query-replace-map (copy-keymap query-replace-map)))
      (define-key query-replace-map [remap next-line]
        (lambda nil (interactive)
          (with-selected-window (get-buffer-window "*ancilla-diff*")
            (scroll-up 1))))
      (define-key query-replace-map [remap previous-line]
        (lambda nil (interactive)
          (with-selected-window (get-buffer-window "*ancilla-diff*")
            (scroll-down 1))))

      (when (y-or-n-p "Accept the change? ")
        (ancilla--replace-selection excursion new-text)))
    (ancilla--hide-diff-changes)))

(defun ancilla--show-diff-changes (old new)
  "Display the diff between OLD and NEW text in a buffer named \"*ancilla-diff*\".

OLD and NEW are strings representing the old and new text,
respectively.  The function creates two temporary buffers, \"
*ancilla-diff-old*\" and \" *ancilla-diff-new*\", to store the
old and new text.  It then generates a diff between these two
buffers and displays the result in a buffer named
\"*ancilla-diff*\".  The function returns the diff buffer."
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

(defun ancilla--get-buffer-file-name ()
  "Get the current file name.

Return a relative path when a project is detected."
  (if-let* ((file-name (buffer-file-name))
            (root (cond
                   ((and (fboundp 'projectile-project-root)
                         (projectile-project-root)))
                   ((and (fboundp 'ffip-project-root)
                         (ffip-project-root)))
                   ((and (fboundp 'project-current)
                         (fboundp 'project-root)
                         (project-root (project-current))))
                   (t 'no-project)))
            (local-file-name
             (if (not (eq root 'no-project))
                 (f-relative file-name (expand-file-name root))
               (file-name-nondirectory file-name))))
      local-file-name
    "(unnamed file)"))

(defun ancilla--get-buffer-context ()
  "Get the buffer context (like current selection) needed to complete the task."
  (interactive)
  (let* ((get-region (lambda (from to) (buffer-substring-no-properties from to)))
         (region-start (if (not (region-active-p)) (point) (region-beginning)))
         (region-end (if (not (region-active-p)) (point) (region-end))))
    (list
     ;; used to provide metadata information to AI
     :file-name (ancilla--get-buffer-file-name)
     :buffer-mode (symbol-name major-mode)

     ;; used to generate the prompt
     :selection (funcall get-region region-start region-end)
     :before-selection (funcall get-region
                                (max (point-min) (window-start))
                                region-start)
     :after-selection (funcall get-region
                               region-end
                               (min (point-max) (window-end)))

     ;; used to replace result accurately
     :excursion `(,(current-buffer) ,(point-marker) ,(mark-marker)))))

(cl-defun ancilla--request-and-extract-json (&key url callback)
  "Request the URL and execute CALLBACK.

URL: The URL to retrieve.

EXTRACT: A function that takes the JSON response and extracts the
desired information.

You can make this function synchronous by setting 'ancilla-async' to nil."
  (let ((url-callback (lambda (_status)
                        (let ((json-object-type 'alist)
                              (json-key-type 'symbol)
                              (json-array-type 'vector))
                          (goto-char url-http-end-of-headers)
                          (funcall callback (json-read))))))
    (if ancilla-async
        (url-retrieve url url-callback '() t t)
      (with-current-buffer (url-retrieve-synchronously url t t)
        (funcall url-callback nil)))))

;; ---- PRIVATE FUNCTIONS FOR CHAT ADAPTOR -----

(defun ancilla--adaptor-chat-extract-content (json)
  "Get the assistant's message content from JSON."
  (let-alist json (let-alist (aref .choices 0) .message.content)))

(defun ancilla--adaptor-chat-get-text-between (content start end)
  "Get the part of CONTENT between START and END."
  (let* ((start-pos (and (string-match start content) (match-end 0)))
         (end-pos (string-match end content start-pos)))
    (if (and start-pos end-pos)
        (substring content start-pos end-pos)
      (switch-to-buffer "*ancilla-chat*")
      (error "Failed to parse OpenAI response, showing chat log buffer"))))

(defun ancilla--adaptor-chat-request-buffer-parse ()
  "Parse the *ancilla-chat* buffer into a conversation.

Returns a list of (ROLE . TEXT) pairs, where ROLE is one of
\"user\", \"system\", or \"assistant\". TEXT is a string."
  (with-current-buffer (get-buffer-create "*ancilla-chat*")
    (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
           (messages (split-string content "\n\f\n" t)))
      (mapcar (lambda (message)
                (cond
                 ((string-prefix-p "USER> " message)
                  `("user" . ,(string-remove-prefix "USER> " message)))
                 ((string-prefix-p "SYSTEM> " message)
                  `("system" . ,(string-remove-prefix "SYSTEM> " message)))
                 ((string-prefix-p "ASSISTANT> " message)
                  `("assistant" . ,(string-remove-prefix "ASSISTANT> " message)))))
              messages))))

(defun ancilla--adaptor-chat-request-buffer-append (role text)
  "Append a message to *ancilla-chat* buffer.

See 'ancilla--adaptor-chat-request-buffer-parse' for the use of
ROLE and TEXT."
  (with-current-buffer (get-buffer-create "*ancilla-chat*")
    (save-excursion
      (goto-char (point-max))
      (insert (format "%s> " (upcase role)))
      (insert (format "%s\n\f\n" text)))))

(defun ancilla--adaptor-chat-request-buffer-reset ()
  "Reset the conversation in *ancilla-chat* buffer."
  (with-current-buffer (get-buffer-create "*ancilla-chat*")
    (ancilla-chat-mode)
    (delete-region (point-min) (point-max))))

(defun ancilla--adaptor-chat-request-buffer-send (callback)
  "Send the conversation in *ancilla-chat* to 'ancilla-adaptor-chat-api-endpoint'.

CALLBACK should take the assistant's reply (string) as the only
argument."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" .
             ,(concat "Bearer " ancilla-adaptor-chat-openai-api-key))))
         (messages (mapcar (lambda (x) `(("role" . ,(car x))
                                         ("content" . ,(cdr x))))
                           (ancilla--adaptor-chat-request-buffer-parse)))
         (url-request-data
          (json-encode
           `(:model ,ancilla-adaptor-chat-model
                    :messages ,messages
                    :temperature 0.1))))

    (ancilla--request-and-extract-json
     :url ancilla-adaptor-chat-api-endpoint
     :callback
     (lambda (json)
       ;; insert the response
       (ancilla--adaptor-chat-request-buffer-append
        "assistant"
        (ancilla--adaptor-chat-extract-content json))

       ;; parse the response
       (pcase (last (ancilla--adaptor-chat-request-buffer-parse))
         (`(("assistant" . ,message))
          (funcall callback message)))))))

(cl-defun ancilla--adaptor-chat-rewrite
    (&key instruction buffer-context callback)
  "Rewrite code using the chat adaptor.

INSTRUCTION: The instruction to follow when rewriting the code.
BUFFER-CONTEXT: The context about what needs to be rewritten.
CALLBACK: The function to call when AI returns.  It accepts the
replacement text as argument."
  (ancilla--adaptor-chat-request-buffer-reset)

  ;; context prompt
  (ancilla--adaptor-chat-request-buffer-append
   "user"
   (concat "Here is the context that may or may not be useful:"
           "\n- filename: " (plist-get buffer-context :file-name)
           "\n- editor mode: " (plist-get buffer-context :buffer-mode)))

  ;; input
  (ancilla--adaptor-chat-request-buffer-append
   "user"
   (concat (plist-get buffer-context :before-selection)
           "<|begin selection|>"
           (plist-get buffer-context :selection)
           "<|end selection|>"
           (plist-get buffer-context :after-selection)))

  ;; instruction
  (ancilla--adaptor-chat-request-buffer-append
   "user"
   (concat "User selected the region marked by <|begin selection|>/<|end selection|> and asked:\n"
           instruction
           "\n\nReply with the replacement for SELECTION. "
           "Your response must begin with <|begin replacement|> "
           "and stop at <|end replacement|>. "
           "Do not include updated code. Preserve original whitespace."))

  (ancilla--adaptor-chat-request-buffer-send
   (lambda (message)
     (let ((replacement (ancilla--adaptor-chat-get-text-between
                         message
                         "<|begin replacement|>" "<|end replacement|>")))
       (funcall callback replacement)))))

(cl-defun ancilla--adaptor-chat-generate
    (&key instruction buffer-context callback)
  "Generate code using the chat adaptor.

INSTRUCTION: The instruction to follow when rewriting the code.
BUFFER-CONTEXT: The context about what needs to be rewritten.
CALLBACK: The function to call when AI returns.  It accepts the
generated text as argument."
  (ancilla--adaptor-chat-request-buffer-reset)

  ;; context prompt
  (ancilla--adaptor-chat-request-buffer-append
   "user"
   (concat "Here is the context that may or may not be useful:"
           "\n- filename: " (plist-get buffer-context :file-name)
           "\n- editor mode: " (plist-get buffer-context :buffer-mode)))

  ;; input
  (ancilla--adaptor-chat-request-buffer-append
   "user"
   (concat (plist-get buffer-context :before-selection)
           "<|cursor|>"
           (plist-get buffer-context :after-selection)))

  ;; instruction
  (ancilla--adaptor-chat-request-buffer-append
   "user"
   (concat "User placed their cursor at <|cursor|> and asked:\n"
           instruction
           "\n\n"
           "Reply with the inserted content at cursor. "
           "Begin your reply with <|begin insertion|> and "
           "stop at <|end insertion|>. "
           "Preserve original whitespace. "))

  (ancilla--adaptor-chat-request-buffer-send
   (lambda (message)
     (let ((insertion (ancilla--adaptor-chat-get-text-between
                       message
                       "<|begin insertion|>" "<|end insertion|>")))
       (funcall callback insertion)))))

(defun ancilla--adaptor-chat-show-request-message (&rest _)
  "Show a message indicating the request the model used."
  (message "Requesting (%s)..." ancilla-adaptor-chat-model))

(put 'chat 'ancilla-rewrite 'ancilla--adaptor-chat-rewrite)
(put 'chat 'ancilla-generate 'ancilla--adaptor-chat-generate)
(put 'chat 'ancilla-hooks '(ancilla--adaptor-chat-show-request-message))

(defvar ancilla-chat-mode-keywords
  `((,(rx bol (or "USER" "SYSTEM" "ASSISTANT") ">") . font-lock-keyword-face)
    (,(rx (seq "<|begin " (group (+ word)) "|>")
          (group (*? anychar))
          (seq "<|end " (backref 1) "|>"))
     . (2 font-lock-string-face t))
    (,(rx (or (seq "<|begin " (+ word) "|>")
              (seq "<|end " (+ word) "|>")
              (seq "<|cursor|>")))
     . font-lock-variable-name-face)))

;; (defvar ancilla-chat-mode-hook nil)
;; (defvar ancilla-chat-mode-map (make-sparse-keymap))

(define-derived-mode ancilla-chat-mode fundamental-mode "A/Chat"
  "Major mode for ancilla chat log."
  ;; enable custom font-lock
  (setq font-lock-defaults '(ancilla-chat-mode-keywords))
  (setq font-lock-multiline t)

  ;; render page break as a horizontal ruler
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\f
        (make-vector 10 (make-glyph-code ?- 'page-break-lines))))

(provide 'ancilla)

;;; ancilla.el ends here
