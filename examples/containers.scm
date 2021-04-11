; Author: Jordan Randleman -- containers.scm
;   => Demo container objects: Stack, Queue, Deque, Set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERIC UNDERLYING VECTOR CONTAINER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass heist:GenericVectorContainer ()
  (data '#())
  ((hasq? obj) (index (eq? obj) self.data))
  ((hasv? obj) (index (eqv? obj) self.data))
  ((has? obj) (index (equal? obj) self.data))
  ((clear!) (set! self.data '#()))
  ((size) (length self.data))
  ((empty?) (empty? self.data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STACK CONTAINER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Stack (heist:GenericVectorContainer)
  ((push! obj) 
    (vector-push-back! self.data obj))
  ((pop!) 
    (define tmp (last self.data))
    (vector-pop-back! self.data)
    tmp)
  ((self->string)
    (append "#<Stack[" (pointer-address self) "]>"))
  ((Stack (obj '#()))
    (cond ((seq? obj)   (set! self.data (reverse (seq->vector obj)))) ; as if pushed all items
          ((Stack? obj) (set! self.data (shallow-copy obj.data)))
          (else         (error 'Stack "Constructor not given a sequence or Stack!" obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUEUE CONTAINER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Queue (heist:GenericVectorContainer)
  ((enqueue! obj)
    (vector-push-back! self.data obj))
  ((dequeue!)
    (define tmp (head self.data))
    (vector-pop-front! self.data)
    tmp)
  ((self->string)
    (append "#<Queue[" (pointer-address self) "]>"))
  ((Queue (obj '()))
    (cond ((seq? obj)   (set! self.data (reverse (seq->vector obj)))) ; as if enqueued all items
          ((Queue? obj) (set! self.data (shallow-copy obj.data)))
          (else         (error 'Queue "Constructor not given a sequence or Queue!" obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEQUE CONTAINER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Deque (heist:GenericVectorContainer)
  ((push-front! obj)
    (vector-push-front! self.data obj))
  ((push-back! obj)
    (vector-push-back! self.data obj))
  ((pop-front!) 
    (define tmp (head self.data)) 
    (vector-pop-front! self.data)
    tmp)
  ((pop-back!)
    (define tmp (last self.data))
    (vector-pop-back! self.data)
    tmp)
  ((ref idx)
    (ref self.data idx))
  ((self->string)
    (append "#<Deque[" (pointer-address self) "]>"))
  ((Deque (obj '()))
    (cond ((seq? obj)   (set! self.data (seq->vector obj)))
          ((Deque? obj) (set! self.data (shallow-copy obj.data)))
          (else         (error 'Deque "Constructor not given a sequence or Deque!" obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET CONTAINER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Set (heist:GenericVectorContainer) ; Unordered. Equality checked via <equal?>.
  ((insert! obj)
    (if (not (self.has? obj))
        (vector-push-back! self.data obj)))
  ((erase! obj)
    (define idx (self.has? obj))
    (if idx (set! self.data (delete self.data idx))))
  ((union set . sets)
    (apply union (cons equal? (cons self.data (cons set.data (map (lambda (set) set.data) sets))))))
  ((intersection set . sets)
    (apply intersection (cons equal? (cons self.data (cons set.data (map (lambda (set) set.data) sets))))))
  ((difference set . sets)
    (apply difference (cons equal? (cons self.data (cons set.data (map (lambda (set) set.data) sets))))))
  ((symmetric-difference set . sets)
    (apply symmetric-difference (cons equal? (cons self.data (cons set.data (map (lambda (set) set.data) sets))))))
  ((self->string)
    (append "#<Set[" (pointer-address self) "]>"))
  ((Set (obj '()))
    (cond ((seq? obj) (for-each self.insert! obj))
          ((Set? obj) (set! self.data (shallow-copy obj.data)))
          (else       (error 'Set "Constructor not given a sequence or Set!" obj)))))
