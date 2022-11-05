#lang racket

(provide path node node-id node-x node-y node-paths path-target path-cost set-node-paths!)

(struct path (target cost) #:transparent)
(struct node (id x y [paths #:mutable #:auto]) #:auto-value '() #:transparent)

