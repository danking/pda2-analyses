#lang racket

(require "../pda2/pda2.rkt"
         "build-flow-function-map.rkt"
         "../semantics/abstract.rkt"
         "../semantics/monadic-configuration.rkt"
         "../semantics/context.rkt"
         (only-in "../pda-to-pda-risc/risc-enhanced/data.rkt"
                  pdarisc-reg-uid
                  pda-risc-enh-initial-term
                  )
         )
(provide forward-analysis
         )

(define (forward-analysis flow-value-bounded-lattice
                          initial-flow-value
                          fv-next
                          pda-risc-enh)

  (define register-count (pdarisc-reg-uid pda-risc-enh))
  (define init-term (pda-risc-enh-initial-term pda-risc-enh))

  (define ff-map (build-flow-function-map pda-risc-enh))

  (define (flow t)
    (lambda (ctx sigma st)
      ;; (printf "-------------------------------------------------------------------------------\n")
      ;; (printf "  ctx: ~a\n" ctx)
      ;; (printf "  t: ~a\n" t)
      ;; (printf "  sigma: ~a\n" sigma)
      ((hash-ref ff-map t) ctx sigma st)))
  (define init-config (init-configuration register-count))

  (FlowAnalysis flow
                flow-ctx
                flow-across
                (set (list init-ctx init-astate init-term))
                init-config
                initial-ctx-state))
