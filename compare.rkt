#lang racket

(require "node.rkt")
(require "map.rkt")
(require "astar.rkt")
(require "dijkstra.rkt")

;; prints the real & cpu & dc time to
;; execute each algorithm with n nodes
(define (compare-speed [n 1000])
  (displayln (string-join (list "Calculating path for" (number->string n) "nodes.")))
  (displayln "--------------------")
  ((λ (nodes)
     ((λ (start target)
        (displayln "A*: ")
        (time (A* start target nodes #f))
        (displayln "--------------------")
        (displayln "Dijkstra: ")
        (time (Dijkstra start nodes #f))
        )
      (list-ref nodes (random (length nodes))) (list-ref nodes (random (length nodes))))
     ) (random-map n))
  )