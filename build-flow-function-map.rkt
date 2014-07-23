#lang racket

(provide build-flow-function-map)

(require "../semantics/abstract.rkt"
         (only-in "../pda-to-pda-risc/risc-enhanced/data.rkt"
                  pda-risc-enh-initial-term
                  pda-term-succs)
         )

(define (build-flow-function-map pda-risc-enh)
  (define flow-function-map (make-hash))

  ;; compute-and-set-ff! : Term -> Void
  (define (compute-and-set-ff! t)
    (hash-set! flow-function-map t (compute-flow-function t)))

  ;; build-flow-function-map : [SetEqOf Term]
  ;;                           [SetEqOf Term]
  ;;                           ->
  ;;                           Void
  (define (build-flow-function-map W Seen)
    (cond [(set-empty? W) (void)]
          [else (compute-and-set-ff! (set-first W))
                (let-values (((new-W new-Seen)
                              (for/fold
                                  ([W2 (set-rest W)]
                                   [Seen Seen])
                                  ([new (pda-term-succs (set-first W))])
                                (if (set-member? Seen new)
                                    (values W2 Seen)
                                    (values (set-add W2 new)
                                            (set-add Seen new))))))
                  (build-flow-function-map new-W new-Seen))]))

  (build-flow-function-map (seteq (pda-risc-enh-initial-term pda-risc-enh))
                           (seteq (pda-risc-enh-initial-term pda-risc-enh)))

  flow-function-map)

