#lang racket

(require "node.rkt")

(provide Dijkstra)

; Dijktsra
(define (Dijkstra start all-nodes)
  
  ;; parent
  (define parent (make-hash))
  ;; distance
  (define d (make-hash))
  ;; priority queue
  (define queue (mutable-set))

  ;; add start to queue
  (hash-set! d start 0)
  (hash-set! parent start start)
  (set-add! queue start)

  ;; set default values
  (map (λ (i)
         (cond
           [(not (equal? (node-id start) (node-id i))) (let () (hash-set! d i +inf.0)
                                                         (hash-set! parent i #f)
                                                         )]
           )
         )
       all-nodes)

  (let loop()
    (cond
      ;; check if the queue is empty => no path
      [(set-empty? queue) (get-path all-nodes parent d)]
      [else ((λ (node)
               ;; remove the node that will be procces from the prio queue
               (set-remove! queue node)
               ;; skip if there are no paths
               (cond
                 [(eq? (node-paths node) #f) (loop)]
                 )
               ((λ ()
                  (map (λ (i)
                         ((λ (neighbor dist)
                            (cond
                              [(and (set-member? queue neighbor) (> (hash-ref d neighbor) dist))
                               ((λ ()
                                  (hash-set! d neighbor dist)
                                  (hash-set! parent neighbor node)
                                  ))]
                              [(and (boolean? (hash-ref parent neighbor))  (not (hash-ref parent neighbor)))
                               ((λ ()
                                  (hash-set! d neighbor dist)
                                  (hash-set! parent neighbor node)
                                  (set-add! queue neighbor)
                                  ))]
                              )
                            )
                          (path-target i) (+ (hash-ref d node) (path-cost i)))
                         ) (node-paths node))
                  (loop)
                  )
                )
               )
             (lowest-d-node (set->list queue) d))]
      )
    )
  )

;; get the node with the lowest d value from the prio queue
(define (lowest-d-node nodes d)
  (let rec ([lst (rest nodes)] [lowest (first nodes)])
    (cond
      [(empty? lst) lowest]
      [(< (hash-ref d (first lst)) (hash-ref d lowest))
       (rec (rest lst) (first lst))]
      [else (rec (rest lst) lowest)]
      )
    )
  )

;; get the path found by the algorithm
(define (get-path all-nodes parent d)
  (map (λ (node)
         (cons (node-id node) (hash-ref d node))
         ) all-nodes)
  )

;;;;;;; TEST ;;;;;;;;;;;;;;
(define A (node "A" 100 250))
(define B (node "B" 450 70))
(define C (node "C" 450 450))
(define D (node "D" 600 80))
(define E (node "E" 600 300))
(define F (node "F" 800 260))

(set-node-paths! A (list (path C 20)))
(set-node-paths! A (cons (path B 10) (node-paths A)))

(set-node-paths! B (list (path D 50)))
(set-node-paths! B (cons (path E 10) (node-paths B)))

(set-node-paths! C (list (path D 20)))
(set-node-paths! C (cons (path E 33) (node-paths C)))

(set-node-paths! D (list (path E 20)))
(set-node-paths! D (cons (path F 2) (node-paths D)))

(set-node-paths! E (list (path F 1)))

(define (test) (Dijkstra A (list A B C D E F)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;