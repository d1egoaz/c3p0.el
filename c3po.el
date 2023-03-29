;;; c3po.el --- C3PO.el is an Emacs package that enables communication with the ChatGPT API. -*- lexical-binding: t -*-

;; Author: Diego Alvarez <c3po@diegoa.ca>
;; Keywords: c3po, chatgpt
;; Package-Requires: ((emacs "26.1"))
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; C3PO.el is an Emacs package for interacting with the ChatGPT API.  This package is named after the
;; famous protocol droid from Star Wars, who was known for his ability to communicate and translate
;; languages.  Similarly, C3PO.el provides an AI-powered chatbot interface that allows users to
;; interact with the ChatGPT API through Emacs.
;;
;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.
;;
;;; Code:

(require 'url)
(require 'json)
(require 'markdown-mode)
(require 'diff-mode)

(defvar url-http-end-of-headers) ;; later redefined by url-http

(defvar c3po-buffer-name "*🤖C3PO🤖*" "The name of the C-3PO buffer.")

(defvar c3po-api-key nil "The API key for the OpenAI API.")

(defvar c3po-model "gpt-3.5-turbo" "The model for the OpenAI Conversation API.")

(defvar c3po-developer-role "You are a large language model living inside Emacs, and the perfect programmer.
Use a role of a Software Developer and Software Architect.
Response MUST be concise.
Response MUST use full and well written markdown, code blocks must use the right language tag."
  "Message for system setup of developer role.")

(defvar c3po-writter-role "You are a large language model living inside Emacs, and the perfect writing assistance.
Your background is a Software Developer and Software Architect.
Response MUST be concise."
  "Message for system setup of writter role.")

(defvar c3po-grammar-prompt "Please correct this to standard English.
The initial and end double quotes MUST be removed from the response.
Contractions are permited.
Please respond only with the corrected sentences."
  "Message for grammar prompt.")

(defvar c3po--last-role "Store the last used role. Used for session replies.")

(defvar c3po--session-messages '()
  "List of messages with roles user and assistant for the current session/conversation.")

(defun c3po--request-open-api (role callback &rest args)
  "Send session messages request to OpenAI API with ROLE, get result via CALLBACK.
Pass additional ARGS to the CALLBACK function."
  (interactive)
  (setq c3po--last-role role)

  (if (not c3po-api-key)
      (message "Please provide an OpenAI API key first.")
    (let* ((api-key c3po-api-key)
           (url "https://api.openai.com/v1/chat/completions")
           (model c3po-model)
           (url-request-method "POST")
           (url-request-extra-headers `(("Content-Type" . "application/json")
                                        ("Authorization" . ,(encode-coding-string(format "Bearer %s" api-key) 'utf-8))))
           (url-request-data (encode-coding-string
                              (json-encode `(:model ,model :messages ,c3po--session-messages))
                              'utf-8)))
      (url-retrieve url
                    #'c3po--extract-content-response
                    (list callback args)))))

(defun c3po--extract-content-response (_status callback &rest args)
  "Extract the last lines of a JSON string from a buffer.
Call user's CALLBACK with the result and passes the aditional ARGS."
  ;; url-http sets a marker named url-http-end-of-headers after retrieving the web content, allowing
  ;; us to skip the HTTP headers directly using this marker:
  (let* ((data (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max)))
         (json-string (decode-coding-string data 'utf-8))
         (json-object (json-read-from-string json-string))
         (message-content (aref (cdr (assoc 'choices json-object)) 0))
         (content (cdr (assoc 'content (cdr (assoc 'message message-content))))))
    (message "🤖: %s" content) ;; debug
    (apply callback content args)))

(defun c3po-append-result (str)
  "Insert STR at the end of the c3po buffer."
  (save-window-excursion
    (let ((buf (get-buffer-create c3po-buffer-name)))
      (with-current-buffer buf
        (gfm-mode)
        (goto-char (point-max))
        (insert (concat "\n" str))
        (goto-char (point-max))))))

(defun c3po--replace-region-with (prompt beg end)
  "Replace the region BEG END and replace the selection with the result of the PROMPT."
  (let ((prompt (concat prompt "\n" (buffer-substring-no-properties beg end))))
    (c3po-new-session)
    (c3po--add-message "system" c3po-writter-role)
    (c3po--add-message "user" prompt)
    (c3po--request-open-api 'writter
                            (lambda (result &rest args)
                              (let* ((arguments (car args))
                                     (buf (nth 0 arguments)) ; gets buffer name
                                     (beg (nth 1 arguments)) ; this is the beg passed as additional arg
                                     (end (nth 2 arguments)) ; end
                                     )
                                (with-current-buffer buf
                                  (save-excursion
                                    (delete-region beg end)
                                    (goto-char beg)
                                    (insert result "\n")))))
                            (buffer-name) ; here is where the additional args are passed
                            beg
                            end)))

(defun c3po-rewrite-and-replace (&optional beg end)
  "Rewrite the region BEG END and replace the selection with the result."
  (interactive "r")
  (if (use-region-p)
      (c3po--replace-region-with "Please rewrite the following text:" beg end)
    (message "No region selected or region is empty")))

(defun c3po-chat (prompt role)
  "Interact with the ChatGPT API with the PROMPT using the role ROLE.
Uses by default the writter role."
  (interactive
   (list (read-string "Enter your prompt: ")
         'writter))
  (c3po-new-session)
  (c3po-append-result (format "\n# New Session - %s\n## 🙋‍♂️ Prompt\n%s\n" (format-time-string "%A, %e %B %Y %T %Z") prompt))
  (c3po--add-message "system" (if (eq role 'dev) c3po-developer-role c3po-writter-role))
  (c3po--add-message "user" prompt)
  (c3po--request-open-api role
                          (lambda (result &rest _args)
                            (c3po--add-message "assistant" result)
                            (c3po-append-result (format "### 🤖 Response\n%s\n" result))
                            (pop-to-buffer c3po-buffer-name))))

(defun c3po-dev-chat (prompt)
  "Interact PROMPT with the ChatGPT API and display the response.  Using dev role."
  (interactive "sEnter your prompt (dev role): ")
  (c3po-chat prompt 'dev))

(defun c3po-summarize ()
  "Summarize the selected text or prompt for prompt and summarize."
  (interactive)
  (c3po--action-on-text "tl;dr" "Enter text to summarize: " 'writter))

(defun c3po-rewrite ()
  "Rewrite the selected text or prompt for prompt and rewrite."
  (interactive)
  (c3po--action-on-text "Rewrite the following text" "Enter text to rewrite: " 'writter))

(defun c3po-gen-test ()
  "Generate test for the passed text."
  (interactive)
  (c3po--action-on-text "Generate unit tests for the following code"  "Enter code to generate tests: " 'dev))

(defun c3po-correct-grammar (text)
  "Corrects TEXT into standard English."
  (interactive "sEnter text to correct: ")
  (let ((prompt (concat c3po-grammar-prompt ":\n" text)))
    (c3po-new-session)
    (c3po-append-result (format "\n# New Session - %s\n## 🙋‍♂️ Prompt\n%s\n" (format-time-string "%A, %e %B %Y %T %Z") prompt))
    (c3po--add-message "system" c3po-writter-role)
    (c3po--add-message "user" prompt)
    (c3po--request-open-api 'writter
                            (lambda (result &rest _args)
                              (c3po--add-message "assistant" result)
                              (c3po-append-result (format "### 🤖 Response\n%s\n" result))
                              (c3po--diff-strings text result)
                              (pop-to-buffer c3po-buffer-name)
                              (pop-to-buffer "*Diff*")))))

(defun c3po--diff-strings (str1 str2)
  "Compare two strings (STR1 and STR2) and return the result."
  (let ((buf1 (generate-new-buffer " *c3po-diff-str1*"))
        (buf2 (generate-new-buffer " *c3po-diff-str2*"))
        (diff-output nil))
    (with-current-buffer buf1
      (insert str1))
    (with-current-buffer buf2
      (insert str2))
    ;; Perform the diff and store it in a new buffer
    (with-temp-buffer
      (diff buf1 buf2 "-U0") ; -U0: Unidiff with 0 lines of context
      (diff-mode)
      (diff-refine-hunk)) ; to highlight single character changes
    ;; not interested for now, as I can use the diff buffer to show diffs
    ;; additional (buffer-string) is only returning the first line :/
    ;; (with-current-buffer "*Diff*"
    ;;   (setq diff-output (buffer-string)))
    (kill-buffer buf1)
    (kill-buffer buf2)
    diff-output))

(defun c3po-correct-grammar-and-replace (&optional beg end)
  "Correct sentences into standard English.  Replace current region BEG END."
  (interactive "r")
  (if (use-region-p)
      (c3po--replace-region-with (concat c3po-grammar-prompt ":") beg end)
    (message "No region selected or region is empty")))

(defun c3po--action-on-text (action action-prompt role)
  "Act on the selected text via the ACTION and ROLE.
If an action is not passed it will ask the user using ACTION-PROMPT"
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (read-string action-prompt))))
    (c3po-chat (format "%s:\n%s" action text) role)))

(defun c3po-explain-code ()
  "Explain the code for the selected text or prompt for prompt and explain."
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (read-string "Enter code to explain: "))))
    (c3po-chat (format "Explain the following code, be concise:\n```%s\n%s```" (c3po--get-buffer-role-as-tag) text) 'dev)))

(defun c3po--get-buffer-role-as-tag ()
  "Get buffer mode as a string to be used as a tag for a markdown code block."
  (let ((str (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)) ))
    (if (string-suffix-p "-ts" str) ;; for the new *-ts-modes
        (substring str 0 (- (length str) 3))
      str)))

;;; Session support

(defun c3po--add-message (role content)
  "Add a message with given ROLE and CONTENT to the session message alist."
  (setq c3po--session-messages (append c3po--session-messages `((("role" . ,role) ("content" . ,content))))))

(defun c3po-new-session ()
  "Reset the session message list to an empty list."
  (setq c3po--session-messages '()))

(defun c3po-reply ()
  "Reply with a message and submit the information."
  (interactive)
  (let ((prompt (read-string "Enter your prompt: ")))
    (c3po--add-message "user" prompt)
    (c3po-append-result (format "#### 🙋‍♂️ Reply\n%s\n" prompt))
    (c3po--request-open-api c3po--last-role
                            (lambda (result &rest _args)
                              (c3po--add-message "assistant" result)
                              (c3po-append-result (format "##### 🤖 Response\n%s\n" result))
                              (pop-to-buffer c3po-buffer-name)))))

(provide 'c3po)
;;; c3po.el ends here
