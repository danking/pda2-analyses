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

  (define initial-basic-block (create-basic-blocks pda-risc-enh))

  (define-values (ff-map ff-ctx-map)
    (build-flow-function-map initial-basic-block))

  (define (flow bb)
    (hash-ref ff-map bb)
    ;; (lambda (ctx sigma st)
    ;;   (printf "-------------------------------------------------------------------------------\n")
    ;;   (printf "  ctx: ~a\n" ctx)
    ;;   (printf "  t: ~a\n" t)
    ;;   (printf "  sigma: ~a\n" sigma)
    ;;   ((hash-ref ff-map t) ctx sigma st))
    )
  (define init-config (init-configuration register-count))
  (define init-ctx-state (initial-ctx-state))

  (FlowAnalysis flow
                flow-ctx
                flow-across
                ctx-gte?
                abstract-state-gte?
                (set (list init-ctx init-astate init-term))
                init-config
                init-ctx-state))
