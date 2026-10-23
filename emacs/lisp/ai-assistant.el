;; -*- lexical-binding: t; -*-
(use-package gptel
  :straight (:build t)
  :config
  (setq
     gptel-model   'gemma-4-12B
     gptel-backend (gptel-make-openai "llama-cpp"
                 :stream t
                 :protocol "http"
                 :host "localhost:8080"
                 :models '(gemma-4-12B))))
