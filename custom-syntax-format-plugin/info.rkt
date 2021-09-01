#lang info
(define collection "custom-syntax-format-plugin")

(define deps
  '("base"
    "custom-syntax-format"))

(define drracket-tool-names (list "DrRacket Auto-Formatting"))
(define drracket-tools (list (list "formatting-tool.rkt")))

(define build-deps
  '())
(define pkg-desc "An auto-formatting plugin for DrRacket")
(define version "0.1")
(define pkg-authors '(aowens-21 shhyou))
