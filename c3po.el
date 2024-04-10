;;; c3po.el --- C3PO.el is an Emacs package that enables communication with the ChatGPT API. -*- lexical-binding: t -*-

;; Author: Diego Alvarez <c3po@diegoa.ca>
;; Keywords: c3po, chatgpt, openai
;; Package-Requires: ((emacs "27.1") (compat "29.1.0.0"))
;; Version: 0.20231120
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; 🤖 Meet c3po.el, the Emacs droid you’ve been looking for!
;; This package will take your workflow to a galaxy far, far away.  🌟
;; C3PO.el is an Emacs package for interacting with the ChatGPT API.
;; May the source be with you!
;;
;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.
;;
;;; Code:
(require 'compat) ;; for emacs < 29, for keymap-local-set

(require 'diff)
(require 'json)
(if (fboundp 'markdown-mode)
    (require 'markdown-mode) ; https://github.com/jrblevin/markdown-mode
  (message "`markdown-mode' package not found, some functionality may be limited."))
(require 'seq)
(require 'url)

;; It will be later redefined by url-http to avoid a warning during compilation.
(defvar url-http-end-of-headers)

(defvar c3po-api-key nil "The API key for the OpenAI API.")

(defvar c3po-buffer-name "*🤖C3PO🤖*" "The name of the C-3PO buffer.")

(defvar c3po-results-file-path nil "The name of the C-3PO results file.")

(defvar c3po-diff-buffer-name "*🤖C3PO Diff🤖*" "The name of the C-3PO Diff buffer.")

(defvar c3po-input-buffer-name "*🤖C3PO input buffer🤖*" "The name of the C-3PO input buffer.")

(defvar c3po-base-path-local-models "http://localhost:11434/v1/chat/completions" "The base URL path for local models.")

(defun c3po--openai-headers ()
  "Composes the HTTP request headers for the OpenAI API call."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(encode-coding-string (format "Bearer %s" c3po-api-key) 'utf-8))))

(defun c3po--anthropic-headers ()
  "Composes the HTTP request headers for the Anthropic API call."
  `(("Content-Type" . "application/json")
    ("anthropic-version" . "2023-06-01")
    ("x-api-key" . ,(encode-coding-string c3po-api-key 'utf-8))))

(defvar c3po-model-alist
  `(("codellama"                 :url ,c3po-base-path-local-models :openaiformat t :req-headers ,'c3po--openai-headers)
    ("codellama:13b"             :url ,c3po-base-path-local-models :openaiformat t :req-headers ,'c3po--openai-headers)
    ("dolphincoder"              :url ,c3po-base-path-local-models :openaiformat t :req-headers ,'c3po--openai-headers)
    ("phi"                       :url ,c3po-base-path-local-models :openaiformat t :req-headers ,'c3po--openai-headers)
    ("gpt-3.5-turbo"             :url "https://api.openai.com/v1/chat/completions"      :openaiformat t :req-headers ,'c3po--openai-headers)
    ("gpt-4-turbo"       :url "https://api.openai.com/v1/chat/completions"      :openaiformat t :req-headers ,'c3po--openai-headers)
    ("llama2:7b"                 :url ,c3po-base-path-local-models :openaiformat t :req-headers ,'c3po--openai-headers)
    ("llama2:13b"                :url ,c3po-base-path-local-models :openaiformat t :req-headers ,'c3po--openai-headers)
    ("mistral:7b"                :url ,c3po-base-path-local-models :openaiformat t :req-headers ,'c3po--openai-headers)
    ("openchat"                  :url ,c3po-base-path-local-models :openaiformat t :req-headers ,'c3po--openai-headers)
    ("claude-3-haiku-20240307"   :url "https://api.anthropic.com/v1/messages" :openaiformat nil :req-headers ,'c3po--anthropic-headers)
    ("claude-3-sonnet-20240229"  :url "https://api.anthropic.com/v1/messages" :openaiformat nil :req-headers ,'c3po--anthropic-headers))
  "This alist maps model names to their corresponding API endpoints and whether they use OpenAI API messages.")

(defun c3po-get-model-property (model prop)
  "Get property PROP for MODEL."
  (plist-get (cdr (assoc model c3po-model-alist)) prop))

(defvar c3po-model (caar c3po-model-alist) "The model for the OpenAI Chat API.")

(defvar c3po-temperature 1.0 "The temperature for the OpenAI Chat API.")

(defvar c3po--last-used-droid nil "Last used droid to be used for replies.")

(defvar c3po-default-pre-processors '(c3po-add-to-buffer-pre-processor) "List of default pre-processors applied to all droids.")

(defvar c3po-default-post-processors '(c3po-add-to-buffer-post-processor) "List of default post-processors applied to all droids.")

;; Only reply with corrected or original text, without extra details.
;; and focus on correct spelling and punctuation.
;; I want you to only reply with the correction and nothing else, do not provide additional information, only enhanced text or the original text.
(defvar c3po-droids-alist
  '(
    (assistant . (:system-prompt "You are a helpful assistant.
Answer the following question only if you know the answer or can make a well-informed guess; otherwise tell me you don't know it.
"))
    (grammar-checker . (
                        :additional-pre-processors (c3po-show-diff-pre-processor)
                        :additional-post-processors (c3po-show-diff-post-processor)
                        :system-prompt "You're the best grammar assistant in the world!
I will communicate with you in any language and you will correct spelling, correctess, correct sentences, correct punctuation, correct errors, fix verbs forms, fix verbs, and enhance the grammar in my text.
You may use contractions and avoid passive voice.

Do not surround the response with any text.

Make sure to follow these rules:
<common-english-grammar-mistakes>
  <mistake>
    <category>Verb Tense Errors</category>
    <description>Incorrect use of verb tenses, such as using present tense when past tense is required and vice versa.</description>
    <description>Confusion between continuous and simple tenses.</description>
  </mistake>
  <mistake>
    <category>Subject-Verb Agreement</category>
    <description>Lack of agreement between the subject and verb in number, e.g., using a singular verb with a plural subject or vice versa.</description>
  </mistake>
  <mistake>
    <category>Articles (a, an, the)</category>
    <description>Incorrect use or omission of articles, such as using "a" instead of "an" or vice versa.</description>
    <description>Overuse or omission of the definite article "the."</description>
  </mistake>
  <mistake>
    <category>Prepositions</category>
    <description>Misuse of prepositions, such as using "in" instead of "on" or "at," or omitting prepositions where they are needed.</description>
  </mistake>
  <mistake>
    <category>Word Order</category>
    <description>Incorrect word order in sentences, especially in questions and negative sentences.</description>
    <description>Misplacement of adverbs or adjectives.</description>
  </mistake>
  <mistake>
    <category>Pluralization</category>
    <description>Incorrect plural forms of nouns, such as failing to add "-s" or "-es" when necessary.</description>
  </mistake>
  <mistake>
    <category>Pronoun Errors</category>
    <description>Confusion between subject and object pronouns.</description>
    <description>Incorrect use of possessive pronouns.</description>
  </mistake>
  <mistake>
    <category>Double Negatives</category>
    <description>Using double negatives, which is grammatically incorrect in standard English.</description>
  </mistake>
  <mistake>
    <category>Modal Verbs</category>
    <description>Misuse of modal verbs like can, could, will, would, should, etc.</description>
  </mistake>
  <mistake>
    <category>Confusing Similar Words</category>
    <description>Confusing words that sound similar but have different meanings and spellings (e.g., "their," "there," and "they're").</description>
  </mistake>
  <mistake>
    <category>Lack of Plural/Singular Agreement</category>
    <description>Mistakes in matching singular and plural nouns and verbs in a sentence.</description>
  </mistake>
</common-english-grammar-mistakes>

<example>
test to connect to old DB after the v15 migration. to validate app can't connect to it

<ok_result>
Test to connect to old DB after v15 migration. To validate app can't connect to it.
</ok_result>

<invalid_result>
The corrected text is:
\"Test to connect to old DB after v15 migration. To validate app can't connect to it.\"
</invalid_result>
<issues>
- Is adding additional information like 'The corrected text is:' which it was mentioned not to do it.
- It's surrounding the result between double quotes. Result shouldn't be surrounded
</issues>
</example>
"
                        :prefix-prompt-with "Edit the following text for spelling and grammar mistakes: <text>"
                        :sufix-prompt-with "</text>"))
    (grammar-checker2 . (
                        :additional-pre-processors (c3po-show-diff-pre-processor)
                        :additional-post-processors (c3po-show-diff-post-processor)
                        :system-prompt "
Your task is to take the text provided by the user and rewrite it into a clear,
grammatically correct version while preserving the original meaning as closely as possible.
Correct any spelling mistakes, punctuation errors, verb tense issues, word choice problems, and other grammatical mistakes.
I want you to only reply with the raw text for the correction and nothing else, do not provide additional information.
Do not surround the response with any text.

<example>
test to connect to old DB after the v15 migration. to validate app can't connect to it

<ok_result>
Test to connect to old DB after v15 migration. To validate app can't connect to it.
</ok_result>

<invalid_result>
The corrected text is:
\"Test to connect to old DB after v15 migration. To validate app can't connect to it.\"

</invalid_result>
<issues>
Errors:
- Is adding additional information like 'The corrected text is:' which it was mentioned not to do it.
- It's surrounding the result between quotes. Result shouldn't be surrounded
- It's adding a line break after the result.
</issues>
<example>
"
                        :prefix-prompt-with "Correct the text delimited by triple quotes:\n\"\"\""
                        :sufix-prompt-with "\"\"\""))

    (developer . (:system-prompt "
I want you to act as a programming expert who can provide guidance, tips, and best practices for various programming languages.
You can review and analyze existing code, identify areas for optimization, and suggest changes to enhance performance, readability, and maintainability.
Please share insights on refactoring techniques, code organization, and how to follow established coding standards to ensure a clean and consistent codebase.
Please offer guidance on how to improve error handling, optimize resource usage, and implement best practices to minimize potential bugs and security vulnerabilities.
Lastly, offer advice on selecting the appropriate tools, libraries, and frameworks for specific projects, and assist with understanding key programming concepts, such as algorithms, data structures, and design patterns.
You are chatting with the user via an Emacs buffer. This means most of the time your lines should be short and concise, unless the user's request requires reasoning or long-form outputs.
Your answers must be written in full and well-structured markdown. Code blocks must use the appropriate language tag.
"))

    (rewriter . (
                 :additional-post-processors (c3po-show-diff-post-processor)
                 :system-prompt "
I want you to acct as my writing assistant with strong programming skills.
I'll converse with you in any language, and you can refine my writing.
Use contractions, avoid too much passive voice, and preserve the meaning.
Only provide the revised text.
All of my future messages aim to be improved."
                 :prefix-prompt-with "Rewrite this:\n"))
    )
  "Alist of droids with a Plist of properties.
Call `c3po-make-droid-helper-functions' to have the helper functions created if you modify this variable manually.")

(defvar c3po-chat-conversation '()
  "List of messages with droids user and assistant for the current chat.")

(defun c3po-add-new-droid(droid)
  "Basic function to add a DROID to the droids list."
  (add-to-list 'c3po-droids-alist droid)
  ;; recreate functions using the macros
  (c3po-make-droid-helper-functions))

(defun c3po-get-droid-property (droid prop)
  "Get property PROP for DROID."
  (plist-get (cdr (assoc droid c3po-droids-alist)) prop))

(defun c3po--apply-pre-processors (droid prompt)
  "Get the DROID processors and invoke the function, passing the PROMPT."
  (when-let ((processors (append c3po-default-pre-processors (c3po-get-droid-property droid :additional-pre-processors))))
    (seq-do (lambda (f) (funcall f droid prompt)) processors)))

(defun c3po--apply-post-processors (droid prompt result &rest args)
  "Get the DROID post-processors and invoke the function.
It pass to the function the DROID, PROMPT, RESULT, and ARGS."
  (when-let ((processors (append c3po-default-post-processors (c3po-get-droid-property droid :additional-post-processors))))
    (seq-do (lambda (f) (funcall f droid prompt result args)) processors)))

(defun c3po--apply-post-processors-and-replace-region (droid prompt result &rest args)
  "Get the DROID post-processors and invoke the function.
It adds an additional processor to kill the current active region.
It pass to the function the DROID, PROMPT, RESULT, and ARGS."
  (save-window-excursion
    (when-let ((processors (append c3po-default-post-processors
                            (c3po-get-droid-property droid :additional-post-processors)
                            '(c3po--replace-region-post-processor))))
      (seq-do (lambda (f) (funcall f droid prompt result args)) processors))))

(defun c3po-is-initial-system-message-p ()
  "Return t if the chat has only received an initial system message."
  (length= c3po-chat-conversation 1))

(defun c3po-add-to-buffer-pre-processor (droid prompt)
  "Pre-processor to add the DROID and PROMPT to the `c3po-buffer-name'."
  (c3po-append-result
   (if (c3po-is-initial-system-message-p)
       (format "\n# Chat (%s)[%s] - %s\n## 🙋‍♂️ Prompt\n%s\n" droid c3po-model (format-time-string "%A, %e %B %Y %T %Z") prompt)
     (format "## 🙋‍♂️ Prompt\n%s\n" prompt)))
  (when-let ((buf (get-buffer c3po-buffer-name)))
    (with-selected-window (get-buffer-window buf)
      (recenter))))

(defun c3po-add-to-buffer-post-processor (_droid _prompt result &rest _args)
  "Post-processor to add the RESULT to the `c3po-buffer-name'."
  (save-excursion
    (c3po-append-result (format "### 🤖 Answer\n%s\n" result))))

(defun c3po--encode-request-body (model sys-prompt conversation temperature)
  "Encodes the JSON body for the OpenAI API request."
  (encode-coding-string
   (json-encode (append `(:model ,model
                                 :messages ,conversation
                                 :temperature ,temperature
                                 :max_tokens 2048
                                 :stream :json-false)
                        (when sys-prompt `(:system ,sys-prompt))))
   'utf-8))

(defun c3po--perform-api-call (endpoint headers body on-success extra-args)
  "Perform the API call to ENDPOINT, sending HEADERS BODY."
  (message ">>>Sending API call with body: %S %S %S" endpoint headers body) ;; This could be removed or hidden based on log level
  (let ((url-request-method "POST")
        (url-request-extra-headers headers)
        (url-request-data body))
  (url-retrieve endpoint
                #'c3po--process-http-response
                (list on-success extra-args)
                nil ;; Prevent buffer display
                t))) ;; Prevent buffer auto-kill

(defun c3po--initiate-openai-conversation (on-success &rest extra-args)
  "Initiate a conversation with model by sending a request and processing the response asynchronously."
  (interactive)
  (unless c3po-api-key
    (error "API key is missing. Please provide it first"))
  (let ((openaiformat (c3po-get-model-property c3po-model :openaiformat))
        (sys-prompt (c3po-get-droid-property c3po--last-used-droid :system-prompt))
        (url (c3po-get-model-property c3po-model :url))
        (req-headers (funcall (c3po-get-model-property c3po-model :req-headers))))
    (when openaiformat
      (when (length= c3po-chat-conversation 1)
        (setq c3po-chat-conversation (append `((("role" . "system") ("content" . ,sys-prompt))) c3po-chat-conversation)))
      (setq sys-prompt nil))
    (let ((body (c3po--encode-request-body c3po-model sys-prompt c3po-chat-conversation c3po-temperature)))
      (when c3po-chat-conversation ;;debug
        (message ">>>: %S"c3po-chat-conversation))
      (c3po--perform-api-call url req-headers body on-success extra-args))))

  ;; debug:
  ;; (goto-char (point-min))
  ;;  (let* ((data (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max))))
  ;;    (message "\nresponse>>> %S" data))
  ;;  (message "\n>>> end")
  ;; (goto-char (point-min))

(defun c3po--extract-http-response ()
  "Extracts the HTTP response body from the current buffer."
  ;; url-http sets a marker named url-http-end-of-headers after retrieving the web content, allowing
  ;; us to skip the HTTP headers directly using this marker.
 ;;  (goto-char (point-min))
  ;;  (let* ((data (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max))))
  ;;    (message "\nresponse>>> %S" data))
  ;;  (message "\n>>> end")
  ;; (goto-char (point-min))
  (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max)))

(defun c3po--safe-parse-json (response-body)
  "Safely parse a JSON string from the HTTP RESPONSE-BODY."
  (condition-case err
      (let ((decoded-json (decode-coding-string response-body 'utf-8)))
        (json-read-from-string decoded-json))
    (error
     (message "Failed to parse JSON: err: %s, body: %s" err response-body)
     nil)))

(defun c3po--extract-content-from-json2 (parsed-json)
  "Extract the specific content from a PARSED-JSON."
  (let ((choice-message (aref (cdr (assoc 'choices parsed-json)) 0)))
    (cdr (assoc 'content (cdr (assoc 'message choice-message))))))

(defun c3po--extract-content-from-json (parsed-json)
  "Extract the specific content from a PARSED-JSON."
  (let ((choice-message (aref (cdr (assoc 'content parsed-json)) 0)))
    (cdr (assoc 'text choice-message))))

(defun c3po--execute-callback (content callback args)
  "Execute the CALLBACK function with the CONTENT and provided additional ARGS."
  (let ((target-droid (cdr (assoc 'droid args)))
        (query-prompt (cdr (assoc 'prompt args)))
        (callback-args (cdr (assoc 'args args))))
    (apply callback target-droid query-prompt content callback-args)))

(defun c3po--process-http-response (_ callback &rest callback-args)
  "Process response, extract content, then invokes CALLBACK with CALLBACK-ARGS."
  (let* ((response-body (c3po--extract-http-response))
         (parsed-json (c3po--safe-parse-json response-body))
         (content (when parsed-json
                    (if (c3po-get-model-property c3po-model :openaiformat)
                        (c3po--extract-content-from-json2 parsed-json)
                      (c3po--extract-content-from-json parsed-json)
                        )
                    )))
    (when content
      (c3po--add-message "assistant" content)
      (c3po--execute-callback content callback (car callback-args)))))

(defun c3po-send-conversation (droid prompt post-processors-fn &rest args)
  "Prepare the PROMPT for the DROID.
If POST-PROCESSORS-FN is nil it'll use `c3po--apply-post-processors'.
Pass ARGS to the `url-retrieve' function."
  (interactive)
  (c3po--hide-diff-window)
  (catch 'my-tag
    (let* ((prompt (or
                    prompt ;; A prompt was passed.
                    (if current-prefix-arg ;; add prefix to current region if any
                        (if (use-region-p)
                            (c3po--make-input-buffer (format "(%s)> Enter the prompt to act on the active region" (symbol-name droid))
                                                     (buffer-substring-no-properties (region-beginning) (region-end))
                                                     (c3po--get-buffer-mode-as-tag))
                          (progn
                            (message "When using universal-argument a region should be active.")
                            (throw 'my-tag nil)))
                      (if (use-region-p) ;; use existing region or ask user for prompt
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (c3po--make-input-buffer (format "(%s)> Enter the prompt" (symbol-name droid)) nil nil))
                      )))
           (post-fn (or
                     post-processors-fn
                     #'c3po--apply-post-processors))
           ;; in order to keep the user prompt without the configured droid prefix
           (prompt-with-prefix (if (c3po-is-initial-system-message-p)
                                   (concat
                                    (c3po-get-droid-property droid :prefix-prompt-with)
                                    prompt
                                    (c3po-get-droid-property droid :sufix-prompt-with))
                                 prompt)))
      (c3po-pop-results-buffer)

      (c3po--apply-pre-processors droid prompt-with-prefix)
      (c3po--add-message "user" prompt-with-prefix)
      (apply #'c3po--initiate-openai-conversation
             post-fn `((droid . ,droid) (prompt . ,prompt) (args . ,args))))))

(defun c3po-append-result (str)
  "Insert STR at the end of the c3po buffer."
  (let ((buf (if c3po-results-file-path
                 (find-file-noselect c3po-results-file-path)
               (get-buffer-create c3po-buffer-name))))
  ;; (let ((buf (get-buffer-create c3po-buffer-name)))
    (with-current-buffer buf
      (if (featurep 'markdown-mode)
          (gfm-mode)
        (text-mode))
      (read-only-mode -1)
      (setq-local header-line-format
                  (concat "Droid 🤖: " (propertize (symbol-name c3po--last-used-droid) 'face '(:foreground "DarkGoldenrod3"))
                          " Model ✨: " (propertize c3po-model 'face '(:foreground "aquamarine3"))))
      (goto-char (point-max))
      (insert (concat "\n" str))
      (read-only-mode t)
      (save-buffer))))

(defun c3po--replace-region-post-processor (_droid prompt result &rest args)
  "Callback used to kill region with RESULT using ARGS.
Check PROMPT to validate if needs to add back the new line."
  ;; Adds back the final new line if the prompt had it.
  (when (string-suffix-p "\n" prompt)
    (setq result (concat result "\n")))

  (let* ((arguments (car args))
         (buf (nth 0 arguments)) ; gets buffer name
         (beg (nth 1 arguments)) ; region beg
         (end (nth 2 arguments))) ; region end
    (with-current-buffer buf
      (save-excursion
        (kill-region beg end)
        ;; (delete-region beg end)
        (goto-char beg)
        (insert result))
      (keyboard-escape-quit))))

(defun c3po-send-conversation-and-replace-region (droid)
  "Setup c3po to start a `c3po-send-conversation' with DROID.
And result will be used by `c3po--callback-replace-region'."
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (c3po-send-conversation droid
                                nil
                                #'c3po--apply-post-processors-and-replace-region
                                (buffer-name) ; here is where the additional args are passed
                                beg
                                end))
    (message "No region selected or region is empty")))

(defun c3po-show-diff-pre-processor (_droid _prompt)
  "Empty the buffer `c3po-buffer-name'."
  (with-current-buffer (get-buffer-create c3po-diff-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)))

(defun c3po-show-diff-post-processor (_droid prompt result &rest _args)
  "Callback to show diff from PROMPT vs RESULT (tail of ARGS)."
  ;; Adds back the final new line if the prompt had it.
  (when (string-suffix-p "\n" prompt)
    (setq result (concat result "\n")))
  (c3po--diff-strings prompt result)
  (c3po--pop-helper-buffer c3po-diff-buffer-name))

(defmacro !c3po--make-chat (droid)
  "Macro to create functions chats for each DROID."
  `(defun ,(intern (concat "c3po-" (symbol-name droid) "-new-chat")) ()
     ,(format "Interact with the Chat API using the droid %s." (symbol-name droid))
     (interactive)
     (c3po-new-chat ',droid)
     (c3po-send-conversation ',droid nil nil)))

(defmacro !c3po--make-replace-region-chat (droid)
  "Macro to create functions to kill regions chats for each DROID."
  `(defun ,(intern (concat "c3po-" (symbol-name droid) "-new-chat-replace-region")) ()
     ,(format "Interact with the Chat API using the droid %s. Also kill the region with the result." (symbol-name droid))
     (interactive)
     (c3po-new-chat ',droid)
     (c3po-send-conversation-and-replace-region ',droid)))

(defun c3po-make-droid-helper-functions ()
  "Create all the droid chats and replace-region chats.
Example: c3po-corrector-chat, c3po-corrector-chat-replace-region, etc."
  (dolist (element c3po-droids-alist)
    (let ((droid (car element)))
      (eval `(!c3po--make-chat ,droid))
      (eval `(!c3po--make-replace-region-chat ,droid)))))

(c3po-make-droid-helper-functions)

(defvar c3po-diff-copy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'c3po--diff-copy)
    map)
  "Keymap for `c3po--diff-copy', a minor mode.")

(define-minor-mode c3po-diff-copy-mode
  "C3PO diff copy mode.
Use `\\[c3po--diff-copy]' to close the diff buffer and copy the result to the kill ring."
  :lighter "C3PO-DIFF"
  (setq-local header-line-format (substitute-command-keys
                                  "C3PO Diff buffer.  Copy to clipboard: `\\[c3po--diff-copy]'.")))

(defun c3po--make-input-buffer (prompt context lang)
  "Display the input buffer, capture input, and return the content when closed.
Uses PROMPT as header line format."
  (let* ((buffer (get-buffer-create c3po-input-buffer-name))
         (input ""))
    (with-current-buffer buffer
      (if (featurep 'markdown-mode)
          (gfm-mode)
        (text-mode))
      (erase-buffer)
      (when (and (featurep 'evil) (bound-and-true-p evil-local-mode))
        (evil-insert-state))
      (when (and context lang)
        (if (equal current-prefix-arg 8)
          (insert (format "\n\n>>> Context:\n%s" context))
          (insert (format "\n\n>>> Context:\n```%s\n%s```" lang context))
      ))
     (goto-char 0)
      (setq-local header-line-format (format "%s. Finish ‘C-c C-c’, abort ‘C-c C-k’." prompt))
      (keymap-local-set "C-c C-c"
                        (lambda ()
                          (interactive)
                          (setq input (buffer-string))
                          (kill-buffer)
                          (exit-recursive-edit))))
    ;; TODO: find out why it doesn't work
    (keymap-local-set "C-c C-k"
                      (lambda ()
                        (interactive)
                        (kill-buffer)
                        (abort-recursive-edit)))
    (c3po-pop-results-buffer)
    (c3po-append-result "")
    (c3po--pop-helper-buffer buffer)
    (recursive-edit)
    input))

(defun c3po-pop-results-buffer ()
  "Display `c3po-buffer-name' in a right side window."
  (interactive)
  (let ((buf (if c3po-results-file-path
                 (find-file-noselect c3po-results-file-path)
               (get-buffer-create c3po-buffer-name))))
  ;; (let ((buffer (get-buffer-create c3po-buffer-name)))
    (display-buffer-in-side-window buf
                                   '((side . right)
                                     (window-width . 0.5)))))

(defun c3po--pop-helper-buffer (buffer)
  "Show BUFFER on the bottom as a side window."
  (let ((window (display-buffer-in-side-window
                 (get-buffer  buffer)
                 '((side . bottom)
                   (window-height . 0.5)))))
    (select-window window)))

(defun c3po--diff-copy ()
  "Copy `c3po--diff-result' to kill ring and kill `c3po-diff-buffer-name' buffer."
  (interactive)
  (let ((name c3po-diff-buffer-name))
    (kill-new
     (buffer-local-value 'c3po--diff-result (get-buffer name)))
  (c3po--hide-diff-window)))

(defun c3po--diff-strings (str1 str2)
  "Compare two strings (STR1 and STR2) and return the result."
  (let ((buf1 (generate-new-buffer " *c3po-diff-str1*"))
        (buf2 (generate-new-buffer " *c3po-diff-str2*"))
        (diff-output nil))
    (with-current-buffer buf1
      (insert str1))
    (with-current-buffer buf2
      (insert str2))
    (with-current-buffer (get-buffer-create c3po-diff-buffer-name)
      ;; Perform the diff and store it in a new buffer
      (diff-no-select buf1 buf2 "-U0" nil (current-buffer)) ; -U0: Unidiff with 0 lines of context
      (toggle-truncate-lines 1)
      (diff-refine-hunk) ; to highlight single character changes
      (setq-local c3po--diff-result str2) ; Temporarily store the result locally.
      (c3po-diff-copy-mode 1))
    (kill-buffer buf1)
    (kill-buffer buf2)
    diff-output))

(defun c3po--hide-diff-window()
  "Hide the diff window."
  (let ((window (get-buffer-window c3po-diff-buffer-name)))
    (when window
      (delete-window window))))

(defun c3po-explain-code ()
  "Explain the code for the selected region, or prompt the user for input."
  (interactive)
  (c3po-new-chat 'developer)
  (let ((prompt (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (c3po--make-input-buffer (format "(%s)> Enter the code " (symbol-name 'developer)) nil nil))))
    (c3po-send-conversation
     'developer
     (format "Explain the following code, be concise:\n```%s\n%s```" (c3po--get-buffer-mode-as-tag) prompt)
     nil)))

(defun c3po--get-buffer-mode-as-tag ()
  "Get buffer mode as a string to be used as a tag for a markdown code block."
  (let ((str (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)) ))
    (if (string-suffix-p "-ts" str) ;; for the new *-ts-modes
        (substring str 0 (- (length str) 3))
      str)))

(defun c3po--add-message (role content)
  "Add a message with given ROLE and CONTENT to the chat message alist."
  (setq c3po-chat-conversation (append c3po-chat-conversation `((("role" . ,role) ("content" . ,content))))))

(defun c3po-new-chat (droid)
  "Reset the chat conversation and set a new chat using DROID."
  (setq c3po-chat-conversation '())
  ;; (c3po--add-message "system" (c3po-get-droid-property droid :system-prompt))
  (setq c3po--last-used-droid droid))

(defun c3po-reply ()
  "Reply with a message and submit the new conversation."
  (interactive)
  (c3po-send-conversation c3po--last-used-droid nil nil))

;; Make sure to show the diff buffer when calling the functions:
;; `c3po-grammar-checker-new-chat-replace-region' and `c3po-rewriter-new-chat-replace-region'.
(advice-add 'c3po-grammar-checker-new-chat-replace-region :after (lambda () (c3po--pop-helper-buffer c3po-diff-buffer-name)))
(advice-add 'c3po-rewriter-new-chat-replace-region :after (lambda () (c3po--pop-helper-buffer c3po-diff-buffer-name)))

(defun c3po-select-c3po-model ()
  "Select a c3po model from the ones defined in c3po-model-alist using a completion interface."
  (interactive)
  (let* ((model-names (mapcar 'car c3po-model-alist))
         (selected-model (completing-read "Select GPT model: " model-names)))
    (setq c3po-model selected-model)
    (message "C3PO model changed to: %S" c3po-model)))

(defun c3po-toggle-c3po-model ()
  "Toggle the model between GPT 3.5 and 4."
  (interactive)
  (setq c3po-model
        (if (string-equal c3po-model "gpt-3.5-turbo")
            "gpt-4-turbo"
          "gpt-3.5-turbo"))
  (message "ChatGPT model changed to: %S" c3po-model))

;; Deprecations:
(define-obsolete-function-alias 'c3po-chat 'c3po-assistant-new-chat "0.202308")
(define-obsolete-function-alias 'c3po-correct-grammar 'c3po-grammar-checker-new-chat "0.202308")
(define-obsolete-function-alias 'c3po-correct-grammar-and-replace 'c3po-grammar-checker-new-chat-replace-region "0.202308")
(define-obsolete-function-alias 'c3po-dev-chat 'c3po-developer-new-chat "0.202308")
(define-obsolete-function-alias 'c3po-rewrite 'c3po-rewriter-new-chat "0.202308")

(provide 'c3po)
;;; c3po.el ends here
