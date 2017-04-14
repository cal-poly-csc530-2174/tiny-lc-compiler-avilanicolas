#lang typed/racket
(require typed/rackunit)

; translates a given LC s expression into java script
; it should be mostly valid
(: translate (Sexp -> String))
(define (translate text)
  (match text
      [(? number? text) (number->string text)]
      [(? symbol? text) (symbol->string text)]
      [(list 'λ (list (? symbol? id)) expr)
       (string-append
        "(function("
        (symbol->string id)
        ") { return "
        (translate expr)
        "; })")]
      [(list 'lambda (list (? symbol? id)) expr)
       (string-append
        "(function("
        (symbol->string id)
        ") { return "
        (translate expr)
        "; })")]
      [(list 'println expr)
       (string-append
        "console.log("
        (translate expr)
        ")")]
      [(list 'ifleq0 test then els)
       (string-append
        "("
        (translate test)
        " <= 0) ? "
        (translate then)
        " : "
        (translate els))]
      [(list '+ expr1 expr2)
       (string-append
        "("
        (translate expr1)
        " + "
        (translate expr2)
        ")")]
      [(list '* expr1 expr2)
       (string-append
        "("
        (translate expr1)
        " * "
        (translate expr2)
        ")")]
      [(list expr1 expr2)
       (string-append
        "("
        (translate expr1)
        "("
        (translate expr2)
        "))")]
      [_ (println text) (println "!!") (error "cannot parse")]))

(check-equal? (translate '(+ 5 1)) "(5 + 1)")
(check-equal? (translate '(* 5 1)) "(5 * 1)")
(check-equal? (translate '5) "5")
(check-equal? (translate 'a) "a")
(check-equal? (translate '(lambda (x) x)) "(function(x) { return x; })")
(check-equal? (translate '((lambda (x) x) 5)) "((function(x) { return x; })(5))")
(check-equal? (translate '(ifleq0 ((lambda (x) x) 5) y z))
              "(((function(x) { return x; })(5)) <= 0) ? y : z")
(check-equal? (translate '(println (+ 5 5))) "console.log((5 + 5))")

; translates a given file of a single LC s expression to mostly valid javascript
(: translateFile (String -> String))
(define (translateFile fileName)
  (translate (cast (file->value fileName) Sexp)))
