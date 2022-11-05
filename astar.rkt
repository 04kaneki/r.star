#lang racket

(require "node.rkt")

(provide A*)

; A* algorithm
(define (A* start target all-nodes)

  ;; parent
  (define parent (make-hash))
  ;; distance
  (define d (make-hash))
  ;; heuristic
  (define h (make-hash))
  ;; priority queue
  (define queue (mutable-set))

  ;; add start to queue
  (hash-set! d start 0)
  (hash-set! parent start start)
  (hash-set! h start (euclidean-distance start target))
  (set-add! queue start)

  ;; set default values
  (map (λ (i)
         (cond
           [(not (equal? (node-id start) (node-id i))) (let () (hash-set! d i +inf.0)
                                                         (hash-set! parent i #f)
                                                         (hash-set! h i (euclidean-distance i target)))]
           )
         )
       all-nodes)

  ;; algorithm loop
  (let loop()
    (cond
      ;; check if the queue is empty => no path
      [(set-empty? queue) (displayln "There is no path from the start node to the target node.")]
      [else ((λ (node)
               ;; remove the node that will be procces from the prio queue
               (set-remove! queue node)
               ;; check if the node is the target
               (cond
                 [(eq? node target) (get-path start target parent)]
                 [else ((λ ()
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
                        )]
                 )
               )
             (lowest-f-node (set->list queue) d h))]
      )
    )
  )


;; euclidean distance for heuristics
(define (euclidean-distance A B)
  (sqrt ( + (expt (- (node-x A) (node-x B)) 2) (expt (- (node-y A) (node-y B)) 2)))
  )

;; get the node with the lowest f value from the list
(define (lowest-f-node nodes d h)  
  (let rec ([lst (rest nodes)] [lowest (first nodes)])
    (cond
      [(empty? lst) lowest]
      [(< (+
           (hash-ref d (first lst))
           (hash-ref h (first lst)))
          (+
           (hash-ref d lowest)
           (hash-ref h lowest))
          ) (rec (rest lst) (first lst))]
      [else (rec (rest lst) lowest)]
      )
    )
  )

;; get the path found by the algorithm
(define (get-path start target parent)
  (let rec ([node target] [path '()])
    (cond
      [(eq? node start) (reverse (cons (node-id start) path))]
      [else (rec (hash-ref parent node) (cons (node-id node) path))]
      )
    )
  )


;;;;;;; TEST ;;;;;;;;;;;;;;
(define A (node "A" 100 250))
(define B (node "B" 450 70))
(define C (node "C" 450 450))
(define D (node "D" 600 80))
(define E (node "E" 600 300))
(define F (node "F" 800 260))

(set-node-paths! A (list (path C 316)))
(set-node-paths! A (cons (path B 316) (node-paths A)))

(set-node-paths! B (list (path D 167)))
(set-node-paths! B (cons (path E 367) (node-paths B)))

(set-node-paths! C (list (path D 467)))
(set-node-paths! C (cons (path E 267) (node-paths C)))

(set-node-paths! D (list (path E 200)))
(set-node-paths! D (cons (path F 317) (node-paths D)))

(set-node-paths! E (list (path F 217)))

(define (test) (A* A F (list A B C D E F)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;