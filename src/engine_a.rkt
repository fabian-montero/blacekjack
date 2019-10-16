;
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
