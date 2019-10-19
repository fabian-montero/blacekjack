#lang racket


; lista players de ejemplo para debugging
(define (get_players_example)
  '(
      ("dealer" "stay" (("four_clubs" 4 0 0) ("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 1 0 0)) ())
      ("fabian" "stay" (("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)) ())
      ("dealer" "stay" (("four_clubs" 4 0 0) ("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)) ())
      ("Pablo" "stay" (("four_clubs" 4 0 0) ("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)) ())
      ("dealer" "stay" (("four_clubs" 4 0 0) ("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)) ())
   )
)


; Genera un deck nuevo de cartas.
;
; Parámetros:
;   N/A  
;
; Retorna:
;   La lista deck
;
(define (gen_deck)
  '(
    ("ace_clubs" 1 0 0) ("two_clubs" 2 0 0) ("three_clubs" 3 0 0)
    ("four_clubs" 4 0 0) ("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)
    ("eight_clubs" 8 0 0) ("nine_clubs" 9 0 0) ("ten_clubs" 10 0 0) ("jack_clubs" 10 0 0)
    ("queen_clubs" 10 0 0) ("king_clubs" 10 0 0)

    ("ace_diamonds" 1 0 0) ("two_diamonds" 2 0 0) ("three_diamonds" 3 0 0)
    ("four_diamonds" 4 0 0) ("five_diamonds" 5 0 0) ("six_diamonds" 6 0 0) ("seven_diamonds" 7 0 0)
    ("eight_diamonds" 8 0 0) ("nine_diamonds" 9 0 0) ("ten_diamonds" 10 0 0) ("jack_diamonds" 10 0 0)
    ("queen_diamonds" 10 0 0) ("king_diamonds" 10 0 0)

    ("ace_hearts" 1 0 0) ("two_hearts" 2 0 0) ("three_hearts" 3 0 0)
    ("four_hearts" 4 0 0) ("five_hearts" 5 0 0) ("six_hearts" 6 0 0) ("seven_hearts" 7 0 0)
    ("eight_hearts" 8 0 0) ("nine_hearts" 9 0 0) ("ten_hearts" 10 0 0) ("jack_hearts" 10 0 0)
    ("queen_hearts" 10 0 0) ("king_hearts" 10 0 0)

    ("ace_spades" 1 0 0) ("two_spades" 2 0 0) ("three_spades" 3 0 0)
    ("four_spades" 4 0 0) ("five_spades" 5 0 0) ("six_spades" 6 0 0) ("seven_spades" 7 0 0)
    ("eight_spades" 8 0 0) ("nine_spades" 9 0 0) ("ten_spades" 10 0 0) ("jack_spades" 10 0 0)
    ("queen_spades" 10 0 0) ("king_spades" 10 0 0)
  )
)


; Obtiene la mano de un jugador
;
; Parámetros:
;   name: Nombre del jugador buscado
;
; Retorna:
;   La mano del jugador seleccionado.
;   Si hay mas de 1 jugador con el mismo nombre,
;   se retorna la mano de la primera ocurrencia.
;
(define (get_hand name)
    (car (cdr (cdr name)))
)


; Obtiene el estado de juego de un jugador
;
; Parámetros:
;   name: Nombre del jugador buscado
;
; Retorna:
;   El estado del jugador seleccionado.
;   Si hay mas de 1 jugador con el mismo nombre,
;   se retorna la mano de la primera ocurrencia.
;
(define (get_status name)
  (car (cdr name))
)


; Obtiene el dealer de la partida
;
; Parámetros:
;   players: Lista de jugadores
;
; Retorna:
;   El dealer de la partida
;
(define (get_dealer players)
  (car players)
)


; Construye la lista players al inicio de la partida
; con el nombre, el status de playing, lista de cartas
; vacía y lista de elementos gráficos vacía.
;
; Parámetros:
;   lista_de_nombres: Lista con los nombres de los jugadores
;   que van a jugar
;   players: Lista vacía que se va a propagar
;
; Retorna:
;   Lista players lista para usarse.
;
(define (deal_init lista_de_nombres players)
  (cond
    ((and (list? lista_de_nombres) (list? players))
      (deal_init_helper lista_de_nombres players)
    )
  )
)

(define (deal_init_helper lista_de_nombres players)
  (cond
    ((empty? lista_de_nombres) players )
    (else
      (deal_init_helper
        (cdr lista_de_nombres)
        (append players (list(list (car lista_de_nombres) "playing" (list) (list))))
      )
    )
  )
)


; Obtiene el valor de una mano
;
; Parámetros:
;   hand: La mano de un jugador
;
; Retorna:
;   Valor numérico de la mano de un jugador
;   de tipo integer.
;
(define (hand_total hand)
  (cond
    ((empty? hand) 0)
    (else
      (+ (car (cdr (car hand))) (hand_total (cdr hand)))
    )
  )
)


; Despues de hacer un deal, se ejecuta esta función para
; ver si alguien obtuvo un bust.
;
; Parámetros:
;   players: Lista de jugadores
;
; Retorna:
;   verdadero si alguien tiene un bust o falso si no
;
(define (bust? players)
  (cond
    ((empty? players) #f)
    ((> (hand_total (get_hand (car players))) 21) #t)
    (else
      (bust? (cdr players))
    )
  )
)


; TO-DO: HAY QUE HACER QUE ESTO IGNORE AL DEALER
; Siempre que alguien se planta, revisa si todos los jugadores
; excepto el dealer estan plantados
;
; Parámetros:
;   players: Lista de jugadores
;
; Retorna:
;   verdadero si todos los jugadores ya se plantaron o falso si no
;
(define (all_players_stayed? players)
  (cond
    ((empty? players) #t)
    ((equal? (get_status (car players)) "playing") #f)
    ((equal? (get_status (car players)) "turn") #f)
    (else
      (all_players_stayed? (cdr players))
    )
  )
)


; Agrega una carta a las cartas de un jugador determinado.
;
; Parámetros:
;   name: Nombre del jugador buscado
;   players: lista de jugadores
;   deck: lista de cartas disponibles
;
; Retorna:
;   La lista players pero el jugador con nombre igual a name
;   adqduiere una carta más del deck.
;
(define (deal name players deck)
  (cond ((empty? players)
         (list))
        ((equal? name (caar players))
         (cons (cons (caar players)
               (cons (cadar players)
                     (cons (append (caddar players) (list (car deck)))
                           (cons (cadr (cddar players))
                                 (list)))))
               (cdr players)))
        (else
         (cons (car players)
                 (deal name (cdr players) deck))))
)


; Revisa si algún jugador de la lista ha obtenido un blackjack
;
; Parámetros:
;   players: lista de jugadores
;
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


; Genera tabla de puntuaciones usando listas
; de la forma: ((jugador puntaje) (jugador puntaje) ...).
;
; Parámetros:
;   players: lista de jugadores
;
; Retorna:
;   Lista de listas con jugadores y puntajes.
;
(define (endgame players)
  (cond ((empty? players)
         (list))
        (else
         (cons (cons (caar players)
                     (cons (hand_total (caddar players))
                           (list)))
               (endgame (cdr players))))))


; Le aplica el status stay a un jugador determinado.
; si existe más de un jugador con el mismo nombre el status
; se le aplicará la primera ocurrencia.
;
; Parámetros:
;   players: lista de jugadores
;   name: Jugador objetivo.
;
; Retorna:
;   Lista de jugadores con el jugador pasado en status stay,
;   si no se encuentra el jugador se retorna lista players intacta.
;
(define (stay name players)
  (cond((empty? players)
         (list))
       ((equal? name (caar players))
        (cons (cons (caar players)
               (cons 'stay
                     (cons (caddar players)
                           (cons (cadr (cddar players))
                                 (list)))))
               (cdr players)))
        (else (cons (car players)
                    (stay name (cdr players))))))


; TO-DO: POR ALGUNA RAZÓN ESTA FUNCIÓN SALE INDEFINIDA, PERO NO SÉ POR QUÉ Y TENGO SUEÑO. DE FIJO SI SIRVE BIEN
; si el dealer tiene menos o igual a 16, pide cartas.
; si tiene mas de 16, se planta. se llama endgame.
;
; Parámetros:
;   players: lista de jugadores
;   deck: Deck actual de juego
;
; Retorna:
;   Llamada a endgame cuando el dealer ya se plantó
;
(define (check_dealer players deck)
  (cond
    ((> (hand_total (get_hand (get_dealer players))) 16  )
      (endgame (stay "dealer" players))
    )
    (else
      (check_dealer (deal "dealer" players deck))
    )
  )
)
