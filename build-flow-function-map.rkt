#lang racket

(provide build-flow-function-map)

(require "../semantics/abstract.rkt"
         "../semantics/context.rkt"
         "../pda-to-pda-risc/risc-enhanced/basic-block-data.rkt"
         )

(define (build-flow-function-map initial-basic-block)
  (define flow-function-map (make-hash))
  (define flow-context-function-map (make-hash))

  ;; compute-and-set-ff! : BBlock -> Void
  (define (compute-and-set-ff! bb)
    (define f (compute-flow-function (first (block-nodes bb))))
    (define fctx (compute-flow-function (first (block-nodes bb))))
    (define-values (composed-f composed-fctx)
      (for/fold
          ([composed-f f]
           [composed-fctx fctx])
          ([term (in-list (rest (block-nodes bb)))]))
      ;; note we right-compose these functions
      (values (compose-ff (compute-flow-function term) composed-f)
              (compose-fctx (flow-ctx term) composed-fctx)))
    (hash-set! flow-function-map
               bb
               composed-f)
    (hash-set! flow-context-function-map
               bb
               composed-fctx))

  ;; build-flow-function-map : [SetOf BBlock]
  ;;                           [SetOf BBlock]
  ;;                           ->
  ;;                           Void
  (define (build-flow-function-map W Seen)
    (cond [(set-empty? W) (void)]
          [else (compute-and-set-ff! (set-first W))
                (let-values (((new-W new-Seen)
                              (for/fold
                                  ([W2 (set-rest W)]
                                   [Seen Seen])
                                  ([new (block-succs (set-first W))])
                                (if (set-member? Seen new)
                                    (values W2 Seen)
                                    (values (set-add W2 new)
                                            (set-add Seen new))))))
                  (build-flow-function-map new-W new-Seen))]))

  (build-flow-function-map (set initial-basic-block)
                           (set initial-basic-block))

  (values flow-function-map flow-context-function-map))

(define (compose-ff f g)
  (lambda (ctx new-ctx sigma confg)
    (define-values (succs config) (f ctx new-ctx sigma confg))
    (assert (= 1 (set-count succs)))
    (match-define (list ctx* sigma* config*) (set-first succs))
    (g ctx* (none) sigma* config*)))

(define (compose-fctx f g)
  (lambda (ctx sigma ctxstate config)
    (define-values (new-ctx ctxstate* config*) (f ctx sigma ctxstate config))
    (assert (none) new-ctx)
    (g ctx sigma ctxstate* config*)))

(define-syntax assert
  (syntax-rules ()
    [(assert x y)
     (unless (equal? x y)
       (error 'assertion-failure "~a doesn't equal ~a" x y))]
    [(assert x)
     (unless x
       (error 'assertion-failure "~a isn't true" 'x))]))
