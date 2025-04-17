(use-package semext
  :vc (
       :url "https://github.com/ahyatt/semext"
            :rev :newest
            :branch "main"
            )
  :ensure t
  :init
  (require 'llm-openai) ;; <- provides "make-llm-openai-compatible"
  ;; Replace provider with whatever you want, see https://github.com/ahyatt/llm
  (setopt semext-provider (make-llm-openai-compatible
                           :url "http://localhost:8686/v1/"
                           :chat-model "llamacpp-gemma-3-1b-it"
                           :key ""
                           ))
  (cl-defmethod llm-capabilities ((provider llm-openai-compatible))
    '(streaming embeddings tool-use streaming-tool-use json-response model-list image-input))
  (setq llm-warn-on-nonfree nil)
)
