(define (generate-next-row row)
  (cons 0 (map + row (append (cdr row) '(0)))))

(define (generate-pascal-triangle n acc)
  (if (zero? n)
      (reverse acc)
      (generate-pascal-triangle (- n 1) (cons (generate-next-row (car acc)) acc))))

(define (get-pascal-triangle n)
  (if (zero? n)
      '((0))
      (generate-pascal-triangle (- n 1) '((0 1 0) (0)))))

(define (print-pascal-triangle n)
  (define (row->string row) (sprintf "%..." row))
  (define (1/2-row-print-length row) (/ (length (row->string row)) 2))
  (define rows (get-pascal-triangle n))
  (define 1/2-max-print-length (1/2-row-print-length (last rows)))
  (define (row-padding row) (+ 1/2-max-print-length (1/2-row-print-length row)))
  (define (row-fmt row) (sprintf "%%%e.0ns\n" (row-padding row)))
  (for-each \(displayf (row-fmt %1) (row->string %1)) rows))

(print-pascal-triangle 20) ; Prints "n" layers of Pascal's triangle