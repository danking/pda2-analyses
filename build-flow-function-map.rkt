#lang racket

(provide build-flow-function-map)

(require "../semantics/abstract.rkt"
         "../semantics/context.rkt"
         "../pda-to-pda-risc/risc-enhanced/basic-block-data.rkt"
         )
(require racket/contract/region)

(define (build-flow-function-map initial-basic-block)
  (define flow-function-map (make-hash))
  (define flow-context-function-map (make-hash))

  ;; compute-and-set-ff! : BBlock -> Void
  (define (compute-and-set-ff! bb)
    (define-values (f fctx)
      (match bb
        [(basic-block _ nodes #f)
         (values flow-function-unit flow-ctx-unit)]
        [(basic-block _ nodes block-succ)
         (assert-true block-succ "block-succ is false?! ~a" nodes)
         (define f (compute-flow-function (first (block-nodes bb))))
         (define fctx (flow-ctx (first (block-nodes bb))))
         (define-values (composed-f composed-fctx)
           (for/fold
               ([composed-f f]
                [composed-fctx fctx])
               ([term (in-list (rest (block-nodes bb)))])
             (values (compose-ff (compute-flow-function term) composed-f)
                     (compose-fctx (flow-ctx term) composed-fctx))))
         (define/contract (final-f ctx new-ctx sigma config)
           (-> any/c any/c any/c any/c
               (values (set/c (list/c any/c any/c block/c))
                       any/c))
           (define-values (items config*) (composed-f ctx new-ctx sigma config))
           (values (replace-all-with-block items block-succ)
                   config*))
         (values final-f composed-fctx)]
        [(branch-block _ node block-succs)
         (define f (compute-flow-function node))
         (define/contract (final-f ctx new-ctx sigma config)
           (-> any/c any/c any/c any/c
               (values (set/c (list/c any/c any/c block/c))
                       any/c))
           (define-values (items config*) (f ctx new-ctx sigma config))
           (values (replace-with-block-succs items block-succs) config*))
         (values final-f (flow-ctx node))]))
    (hash-set! flow-function-map
               bb
               f)
    (hash-set! flow-context-function-map
               bb
               fctx))

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

(define (compose-ff g f)
  (lambda (ctx new-ctx sigma config)
    (define-values (succs config*) (f ctx new-ctx sigma config))
    (assert (set-count succs) 1
            "succs ~a" succs)
    (match-define (list ctx* sigma* code) (set-first succs))
    (g ctx* (none) sigma* config*)))

(define (compose-fctx g f)
  (lambda (ctx sigma ctxstate config)
    (define-values (new-ctx ctxstate* config*) (f ctx sigma ctxstate config))
    (assert (none) new-ctx)
    (g ctx sigma ctxstate* config*)))

(define (replace-all-with-block items block)
  (for/set ([item items])
    (match-define (list ctx* sigma* term) item)
    (list ctx* sigma* block)))

(define (replace-with-block-succs items blocks)
  (for*/set ([item items]
             [block blocks]
             #:when
             (equal? (first (block-nodes block)) (third item)))
    (match-define (list ctx sigma term) item)
    (list ctx sigma block)))


(define-syntax assert
  (syntax-rules ()
    [(_ x y msg vals ...)
     (unless (equal? x y)
       (error 'assertion-failure
              (string-append "~a [~a] doesn't equal ~a [~a]\n" msg)
              x 'x y 'y vals ...))]
    [(_ x y)
     (assert x y "")]))

(define-syntax assert-true
  (syntax-rules ()
    [(_ x)
     (assert-true x "~a isn't true" 'x)]
    [(_ x msg vals ...)
     (unless x
       (error 'assertion-failure msg vals ...))]))
