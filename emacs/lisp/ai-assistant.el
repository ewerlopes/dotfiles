(straight-use-package 'gptel)
(setq
 gptel-model   'test
 gptel-backend (gptel-make-openai "llama-cpp"
                 :stream t
                 :protocol "http"
                 :host "localhost:8080"
                 :models '(test)))

(use-package gptel-prompts
  :straight (:type git :host github :repo "jwiegley/gptel-prompts")
  :after (gptel)
  :demand t
  :config
  (gptel-prompts-update)
  ;; Ensure prompts are updated if prompt files change
  (gptel-prompts-add-update-watchers)
  (setq gptel-prompts-directory
       (expand-file-name "~/.config/emacs/prompts/"))
)
