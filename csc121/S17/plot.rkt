#lang racket

#;(plot (sequence/c real?)) ; Scatter-plot by index.
#;(plot (dict/c any/c natural?)) ; Histogram of key to count.

(provide plot)
  
(require (rename-in plot [plot plot′])
         (for-syntax syntax/parse))
  
(define (index a-sequence)
  (in-values-sequence (in-parallel (in-naturals) a-sequence)))

(define (plot′′ s/d s-expr)
  (parameterize ([plot-y-label (substring (~v s-expr) 1)])
    (if (dict? s/d)
        (plot′ (discrete-histogram (dict-map s/d vector))
               #:x-label "")
        (plot′ (points (index s/d))
               #:x-label "Index"))))

(define-syntax plot (syntax-parser [(_ s/d:expr) #'(plot′′ s/d 's/d)]))