#lang racket
(require 2htdp/batch-io)

; Denisse Dominguez Bolaños A01702603
; RESALTADOR SINTAXICO

; html
(define html "<!DOCTYPE html>
<html lang=\"en\"><head><meta charset=\"UTF-8\"><meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<link rel=\"stylesheet\" href=\"./styles.css\">
<title>Resaltador de Sintaxis</title>
</head>
<body>
<h3 class=\"variable\"/h3>
<h3 class=\"operadores\" /h3>
<h3 class=\"numero\"/h3>
<h3 class=\"especiales\"/h3>
<h3 class=\"comentarios\"/h3>
<h3 class=\"reservadas\"/h3>
</body")

; definimos los tokens 
(define numero #rx"-*[0-9]+\\.[0-9]+([E][-*][0-9]+)?|-*[0-9]+(\\.[0-9]+)?")
(define variable #rx"[a-zA-Z][a-zA-Z_0-9]*")
(define operadores #rx"[\\*|\\/|\\^|\\=|\\+|\\-]")
(define especiales #rx"[\\)\\(\\#\\;\\.\\}\\{\\<\\>]")
(define comentarios #rx"[\\(\\)]")
(define reservadas #rx"\\auto|\\else|\\long|\\switch|\\break|\\typedef|\\return|\\char|\\float|\\const|\\for|\\sizeof|\\if|\\while|\\do|\\int|\\double")


; usamos regex que nos ayuda a determinar la secuencia
; usamos match para construir el regx en base a un patron de expresiones regulares especificados ;string

(define (looking el)
  (cond
    [(regexp-match? numero el) (string-append "<span class=\"numero\">" el "</span>")]
    [(regexp-match? variable el) (string-append "<span class=\"variable\">" el "</span>")]
    [(regexp-match? operadores el) (string-append "<span class=\"operadores\">" el "</span>")]
    [(regexp-match? especiales el) (string-append "<span class=\"especiales\">" el "</span>")]
    [(regexp-match? comentarios el) (string-append "<span class=\"comentarios\">" el "</span>")]
    [(regexp-match? reservadas el) (string-append "<span class=\"reservadas\">" el "</span>")]
    [else((string-append "<span>" el "</span>"))]))


;; Leemos un archivo de entrada, lee caracter por caracter y regresa una lista
(define file->list-of-chars
  (lambda (filename)
    (flatten
     (map string->list
          (read-1strings filename)))))

(define file->list-of-strings
  (lambda (input-filename)
    ;(reverse que es esto???
     (list-of-chars->list-of-strings
      (file->list-of-chars input-filename) '() '()))))

; Recivimos una strings y la vamos concatenando con el resto
(define list-of-strings->string
  (lambda (input-filename)
    (string-append
     (list-of-strings->string(looking (first lst)))
     (list-of-strings->string(looking(rest lst))))))
                           
; Recivimos una lista de strings y las pone en el output file 
(define listStringToFile
  (lambda (input-filename output-filename)
    (write-file output-filename (list-of-strings->string input-filename))))

; input file
(define input-filename "test.cpp")

; output file
(define output-filename "output.html")
