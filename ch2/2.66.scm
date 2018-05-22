(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((current (entry set-of-records))
            (key-of-current (key current))
            (left (left-branch set-of-records))
            (right (right-branch set-of-records)))
        (cond ((= given-key key-of-current) current)
              ((< given-key key-of-current) (lookup given-key left))
              (else (lookup given-ley right))))))
