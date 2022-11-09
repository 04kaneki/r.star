#lang racket/gui

(require math/base)
(require "node.rkt")

(provide draw-map-A* draw-map-Dijkstra random-map)

;; create a map with n random nodes with 2 paths each
(define (random-map n [x-max 900] [y-max 500] [max-cost 500])

  (let ([nodes (for/list ([i n]) (node (list->string (for/list ([i 6]) (integer->char (random-integer 65 90)))) (random-integer 0 x-max ) (random-integer 0 y-max)))])
    (for ([node nodes])
      (set-node-paths! node (list (path (list-ref nodes (random (length nodes))) (random-integer 0 max-cost))))
      (set-node-paths! node (cons (path (list-ref nodes (random (length nodes))) (random-integer 0 max-cost)) (node-paths node)))
      )
    nodes
    )
  )

;; draws graph for the nodes
(define (draw-map-A* nodes start target path-nodes [x-max 900] [y-max 500])

  ;; bitmap
  (define map (make-bitmap x-max y-max))
  (define dc (new bitmap-dc% [bitmap map]))
  
  ;; draw the nodes + paths into the map
  (let nodes-loop([nodes nodes])
    (cond
      [(empty? nodes) ]
      [else ((λ (node)
               ;; green if start or target
               (cond
                 [(eq? node start) (send dc set-brush "green" 'solid)]
                 [(eq? node target) (send dc set-brush "green" 'solid)]
                 )
               
               ;; draw the node
               (send dc set-pen "black" 2 'solid)
               (send dc draw-ellipse (node-x node) (node-y node) 30 30)
               (send dc draw-text (node-id node) (+ (node-x node) 10) (+ (node-y node) 10))
               (send dc set-brush "white" 'solid)
               
               ;; draw its paths
               (let path-loop([paths (node-paths node)])
                 (cond
                   [(not (empty? paths)) ((λ (path)
                                            (send dc set-pen "black" 2 'solid)
                                            (cond [(and (set-member? path-nodes node) (set-member? path-nodes (path-target path))) (send dc set-pen "green" 2 'solid)])
                                            (send dc draw-line (+ (node-x node) 30) (+ (node-y node) 15) (node-x (path-target path)) (+ (node-y (path-target path)) 15))
                                            (send dc draw-text (number->string (path-cost path)) (/ (+ (node-x (path-target path)) (node-x node)) 2) (/ (+ (node-y (path-target path)) (node-y node)) 2))                                 
                                            (path-loop (rest paths))
                                            ) (first paths))]
                   )                                                                                                                         
                 )
               (nodes-loop (rest nodes))
               ) (first nodes))]
      )
    )


  ;; save as image
  (send map save-file "map-astar.png" 'png)
  ;; display the image
  (define logo (read-bitmap "map-astar.png" 'png))
  (define f (new frame% [label "The map for the A* graph"]))
  (send f show #t)
  (void (new message% [parent f] [label logo]))
  )


;; draws graph for the nodes
(define (draw-map-Dijkstra pairs start [x-max 900] [y-max 500])

  ;; bitmap
  (define map (make-bitmap x-max y-max))
  (define dc (new bitmap-dc% [bitmap map]))
  (send dc set-pen "black" 2 'solid)
  
  ;; draw the nodes + paths into the map
  (let pair-loop([pairs pairs])
    (cond
      [(empty? pairs) ]
      [else ((λ (pair)
               
               ;; green if start or target
               (cond
                 [(eq? (car pair) start) (send dc set-brush "green" 'solid)]
                 )
               
               ;; draw the node
               (send dc draw-ellipse (node-x (car pair)) (node-y (car pair)) 30 30)
               (send dc draw-text (node-id (car pair)) (+ (node-x (car pair)) 10) (+ (node-y (car pair)) 10))
               (send dc draw-text (number->string (cdr pair)) (+ (node-x (car pair)) 7) (node-y (car pair)))
               (send dc set-brush "white" 'solid)
               
               ;; draw its paths
               (let path-loop([paths (node-paths (car pair))])
                 (cond
                   [(not (empty? paths)) ((λ (path)                                           
                                            (send dc draw-line (+ (node-x (car pair)) 30) (+ (node-y (car pair)) 15) (node-x (path-target path)) (+ (node-y (path-target path)) 15))                                            
                                            (send dc draw-text (number->string (path-cost path)) (/ (+ (node-x (path-target path)) (node-x (car pair))) 2) (/ (+ (node-y (path-target path)) (node-y (car pair))) 2))
                                            (path-loop (rest paths))
                                            ) (first paths))]
                   )                                                                                                                         
                 )
               (pair-loop (rest pairs))
               ) (first pairs))]
      )
    )


  ;; save as image
  (send map save-file "map-dijkstra.png" 'png)
  ;; display the image
  (define logo (read-bitmap "map-dijkstra.png" 'png))
  (define f (new frame% [label "The map for the Dijkstra graph"]))
  (send f show #t)
  (void (new message% [parent f] [label logo]))
  )


