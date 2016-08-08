;;; datatype


(define-module (eopl datatype)
  #:export (define-datatype cases))


(define-syntax define-datatype
  (lambda (stx)
    (syntax-case stx ()
      ((_ type-name type-predicate-name
          (variant-name (field-name predicate) ...)
          ...)
       (syntax
        (begin
          (define type-name
            '((pred . type-predicate-name)
              (variants . ((variant-name . ((field-name . predicate) ...))
                           ...))))
          (define (type-predicate-name x)
            (if (and (pair? x)
                     (assv (car x)
                           (assv-ref type-name 'variants)))
                #t
                #f))
          (define (variant-name field-name ...)
            `(variant-name ,field-name ...))
          ...))))))


(define-syntax cases
  (lambda (stx)
    (syntax-case stx (else)
      ((_ type-name expression
          (else default))
       (syntax
        default))
      ((_ type-name expression
          (variant-name (field-name ...) consequent)
          ...
          (else default))
       (syntax
        (cond ((eqv? (car expression) 'variant-name)
               (let* ((make-expression
                       (lambda (exp)
                         (lambda ()
                           (set! exp (cdr exp))
                           exp)))
                      (expr (make-expression expression)))
                 (let ((field-name (car (expr)))
                       ...)
                   consequent)))
              ...
              (else default))))
      ((_ type-name expression
          (variant-name (field-name ...) consequent)
          ...)
       (syntax
        (cond ((eqv? (car expression) 'variant-name)
               (let* ((make-expression
                       (lambda (exp)
                         (lambda ()
                           (set! exp (cdr exp))
                           exp)))
                      (expr (make-expression expression)))
                 (let ((field-name (car (expr)))
                       ...)
                   consequent)))
              ...))))))
