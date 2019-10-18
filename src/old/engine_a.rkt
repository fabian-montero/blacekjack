#lang racket


; Agrega una carta a las cartas de un jugador determinado.
;
; Parámetros:
;   name: Nombre del jugador buscado
;   players: lista de jugadores
;   deck: lista de cartas disponibles
; Retorna:
;   La lista players pero el jugador con nombre igual a name adqduiere una carta más del deck.
;
(define (deal name players deck)
  (cond ((empty? players)
         (list))
        ((equal? name (caar players))
         (cons (cons name
                     (list (append (cadar players)
                                   (list (car deck)))))
               (deal name (cdr players) deck)))
        (else
         (cons (car players)
                 (deal name (cdr players) deck))))
)

(define (hand_total lista)
  (cond
    ((empty? lista) 0)
    (else
      (+ (car (cdr (car lista))) (hand_total (cdr lista)))
    )
  )
)


; Revisa si algún jugador de la lista ha obtenido un blackjack
;
; Parámetros:
;   players: lista de jugadores
; Retorna:
;   Verdadero si alguien lo obtuvo o falso si nadie lo consigue.
;
(define (blackjack? players)
  (cond ((empty? players)
          #f)
        ((equal? (caar players) "dealer")
          (blackjack? (cdr players)))
        ((= (hand_total (caddar players)) 21)
          #t)
        (else
          (blackjack? (cdr players)))))

;
; Genera tabla de puntuaciones.
;
; Parámetros:
;   players: lista de jugadores
; Retorna:
;   Lista de lista con jugadores y puntajes.
;   ((jugador puntaje) (jugador puntaje) ...)
;
(define (endgame players)
  (cond ((empty? players)
         (list))
        (else
         (cons (cons (caar players)
                     (cons (hand_total (caddar players))
                           (list)))
               (endgame (cdr players))))))

;
; Le aplica el status stay a un jugador determinado.
;
; Parámetros:
;   players: lista de jugadores
;   name: Jugador objetivo.
; Retorna:
;   Lista de jugadores con el jugador pasado en status stay,
;   si existe más de un jugador con el mismo nombre el status
;   se le aplicará la primera ocurrencia.
;
(define (stay name players)
  (cond ((equal? name (caar players))
         (cons (cons (caar players)
               (cons 'stay
                     (cons (caddar players)
                           (cons (cadr (cddar players))
                                 (list)))))
               (cdr players)))
        (else (cons (car players)
                    (stay name (cdr players))))))