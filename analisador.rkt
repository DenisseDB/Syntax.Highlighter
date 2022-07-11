#lang racket
; Denisse Dominguez Bolaños
; A01702603
; Resaltador de sintaxis

(require 2htdp/batch-io)

; HTML Y CSS
(define html "<!DOCTYPE html><html lang='esp-mx'> <head> <meta charset='UTF-8'>
<link rel=\"stylesheet\" href=\"./styles_AS.css\">
<title>Resaltador de Sintaxis</title></head><body></body></html>")

; Para lexer, necesitamos establecer todos los tokens, para hacer las comparacines en cadena de longitud
(define tokens #rx"main|iostream|#include|using|namespace|std|cout|cin|else|long|switch|break|typedef|return|char|float|const|for|sizeof|if|while|do|int|double|\\/\\/.*|\".*\"|^\\-*[0-9]+| ^[a-zA-Z]+|\\^|\\/|\\&|\\|\\+|\\-|\\*|\\=|\\(|\\)|\\=>|\\<=|.")

; Establecemos a que pertenece cada expresion
(define reservadas #rx"main|iostream|#include|using|namespace|std|cout|cin|else|long|switch|break|typedef|return|char|float|const|for|sizeof|if|while|do|int|double")
(define comentarios #rx"\\/\\/.*")
(define numero #rx"^\\-*[0-9]+")
(define variable #px"^[a-zA-Z]+")
(define operadores #rx"\\^|\\/|\\&|\\|\\+|\\-|\\*|\\=|\\(|\\)|\\=>|\\<=")

; Vamos detectando los token y emparejando acorde a su pertenencia y haciendo el
; highlight acorde a su clase establecida en css
(define (looking el)
    (cond
      ; especiales
      [(regexp-match? #px"#[[:blank:]]*include" el) (string-append "<span class=\"include\">" el "</span>")]
      [(regexp-match? #px"using" el) (string-append "<span class=\"include\">" el "</span>")]
      [(regexp-match? #px"namespace" el) (string-append "<span class=\"include\">" el "</span>")]
      [(regexp-match? #px"std" el) (string-append "<span class=\"iostream\">" el "</span>")]
      [(regexp-match? #px"iostream" el) (string-append "<span class=\"iostream\">" el "</span>")]
      ; reservadas
      [(regexp-match? reservadas el) (string-append "<span class=\"reservada\">" el "</span>")]
      ; comentarios
      [(regexp-match? comentarios el) (string-append "<span class=\"comentario\">" el "</span>")]
      ; numeros
      [(regexp-match? numero el) (string-append "<span class=\"numero\">" el "</span>")]
      ; variables
      [(regexp-match? variable el) (string-append "<span class=\"variable\">" el "</span>")]
      ; operadores
      [(regexp-match? operadores el) (string-append "<span class=\"operador\">" el "</span>")]
      ; resto
      [else (string-append "<span>" el "</span>")]))

; De una lista incial que obtenemos del input, recorremos toda buscando un match con cada una de ellas acorde a la
; lista de tokens estabelcidas al inciio
(define (firstJoin lst)
  (cond
    [(empty? lst) " "] ; aplicamos espacio para dar claridad al codigo
    [else (string-append (join (regexp-match* tokens (first lst))) (firstJoin (rest lst)))])) ; aplicamos recursion


; Ahora vamos una por una, remarcandolas acorde a la expresion a la que pertenecen
(define (join lst)
  (cond
    [(empty? lst) " "] ; agregamos los espacios para que se separen las lineas y poder hacer el match, ya que si estan juntas, se detectan incorrectamente
    [else (string-append (looking (first lst)) (join (rest lst)))])) ; aplicamos recursion 

; Declaramos nuestro input file
(define input-filename "test.cpp")

; Declaramos nuestro output file
(define output-filename "output.html")

; Escribimos el archivo html de salida
; escribimos archivo "output_FI.html" donde se juntan las strings del formato html con las del archivo "test.cpp" que es el input
(write-file output-filename (string-append html (finalList (read-lines input-filename))))

; funcion time que nos permite conocer el tiempo de ejecuccion
; lo hacemos con el write-file ya que aqui es donde ocurre todo la operacion
; puesto que las funciones deben ejecutarse para poder escribir el archivo
(time (write-file input-filename output-filename)(void))
