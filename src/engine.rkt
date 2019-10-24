#lang racket

(require games/cards)
(provide (all-defined-out))

; lista players de ejemplo para debugging
(define (get_players_example)
  '(
      ("dealer" "stay" (("five_clubs" 5 0 0 #t) ("ace_clubs" 1 0 0 #f)) ())
      ("fabian" "stay" (("five_clubs" 5 0 0 #t) ("six_clubs" 6 0 0 #t) ("seven_clubs" 7 0 0 #t)) ())
      ("alejandro" "stay" (("king_clubs" 10 0 0 #t) ("ace_clubs" 1 0 0 #f)) ())
      ("vanessa" "stay" (("four_clubs" 4 0 0 #t) ("five_clubs" 5 0 0 #t) ("six_clubs" 6 0 0 #t) ("seven_clubs" 7 0 0 #t)) ())
      ("hazel" "stay" (("four_clubs" 4 0 0 #t) ("five_clubs" 5 0 0 #t) ("six_clubs" 6 0 0 #t) ("seven_clubs" 7 0 0 #t)) ())
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
  (shuffle-list
    '(
      ("ace_clubs" 1 0 0 #f) ("two_clubs" 2 0 0 #f) ("three_clubs" 3 0 0 #f)
      ("four_clubs" 4 0 0 #f) ("five_clubs" 5 0 0 #f) ("six_clubs" 6 0 0 #f) ("seven_clubs" 7 0 0 #f)
      ("eight_clubs" 8 0 0 #f) ("nine_clubs" 9 0 0 #f) ("ten_clubs" 10 0 0 #f) ("jack_clubs" 10 0 0 #f)
      ("queen_clubs" 10 0 0 #f) ("king_clubs" 10 0 0 #f)

      ("ace_diamonds" 1 0 0 #f) ("two_diamonds" 2 0 0 #f) ("three_diamonds" 3 0 0 #f)
      ("four_diamonds" 4 0 0 #f) ("five_diamonds" 5 0 0 #f) ("six_diamonds" 6 0 0 #f) ("seven_diamonds" 7 0 0 #f)
      ("eight_diamonds" 8 0 0 #f) ("nine_diamonds" 9 0 0 #f) ("ten_diamonds" 10 0 0 #f) ("jack_diamonds" 10 0 0 #f)
      ("queen_diamonds" 10 0 0 #f) ("king_diamonds" 10 0 0 #f)

      ("ace_hearts" 1 0 0 #f) ("two_hearts" 2 0 0 #f) ("three_hearts" 3 0 0 #f)
      ("four_hearts" 4 0 0 #f) ("five_hearts" 5 0 0 #f) ("six_hearts" 6 0 0 #f) ("seven_hearts" 7 0 0 #f)
      ("eight_hearts" 8 0 0 #f) ("nine_hearts" 9 0 0 #f) ("ten_hearts" 10 0 0 #f) ("jack_hearts" 10 0 0 #f)
      ("queen_hearts" 10 0 0 #f) ("king_hearts" 10 0 0 #f)

      ("ace_spades" 1 0 0 #f) ("two_spades" 2 0 0 #f) ("three_spades" 3 0 0 #f)
      ("four_spades" 4 0 0 #f) ("five_spades" 5 0 0 #f) ("six_spades" 6 0 0 #f) ("seven_spades" 7 0 0 #f)
      ("eight_spades" 8 0 0 #f) ("nine_spades" 9 0 0 #f) ("ten_spades" 10 0 0 #f) ("jack_spades" 10 0 0 #f)
      ("queen_spades" 10 0 0 #f) ("king_spades" 10 0 0 #f)
    ) 7
  )
)

; Genera un deck nuevo de cartas y lo retorna aleatorizado.
;
; Parámetros:
;   N/A
;
; Retorna:
;   La lista deck en orden aleatoreo
;
(define (gen_random_deck)
  (shuffle-list (gen_deck) 7)
)


; Obtiene la mano de un jugador
;
; Parámetros:
;   name: Jugador
;
; Retorna:
;   La mano del jugador seleccionado.
;   Si hay mas de 1 jugador con el mismo nombre,
;   se retorna la mano de la primera ocurrencia.
;
(define (get_hand player)
    (car (cdr (cdr player)))
)


; Obtiene el estado de juego de un jugador
;
; Parámetros:
;   name: Jugador
;
; Retorna:
;   El estado del jugador seleccionado.
;   Si hay mas de 1 jugador con el mismo nombre,
;   se retorna la mano de la primera ocurrencia.
;
(define (get_status player)
  (car (cdr player))
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

; Obtiene el nombre de juego de un jugador
;
; Parámetros:
;   name: Jugador
;
; Retorna:
;   El nombre del jugador seleccionado.
;   Si hay mas de 1 jugador con el mismo nombre,
;   se retorna el nombre de la primera ocurrencia
;
(define (get_name player)
  (car player)
)

(define (get_player name players)
  (cond ((empty? players) (list))
        ((equal? name (get_name (car players)))
          (car players))
        (else
          (get_player name (cdr players)))))


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
        (append players
          (list(list(car lista_de_nombres) "playing" (list) (list)))
        )
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
  (hand_total_helper hand 0)
)


(define (hand_total_helper hand total)
  (cond
    ((empty? hand) total)
    ((has_ace hand)
      (cond
        ((> (hand_total_helper (cdr hand) (+ total (car (cdr (car hand))))) 21)
          (hand_total_helper (cdr hand) (+ total (car (cdr (car hand)))))
        )
        (else
          (cond
            ((> (+
                  (hand_total_helper (cdr hand) (+ total (car (cdr (car hand)))))
                  10
                ) 21
             ) (hand_total_helper (cdr hand) (+ total (car (cdr (car hand)))))
            )
            (else
              (+
                (hand_total_helper (cdr hand) (+ total (car (cdr (car hand)))))
                10
              )
            )
          )
        )
      )
    )
    (else
      (hand_total_helper (cdr hand) (+ total (car (cdr (car hand)))))
    )
  )
)


; Informa si un jugador tiene alguno de los 4 aces
; en su mano.
;
; Parámetros:
;   hand: La mano de un jugador
;
; Retorna:
;   Verdadero si el jugador tiene un ace en su mano,
;   falso si no.
;
(define (has_ace hand)
  (cond
    ((empty? hand) #f)
    ((or
      (equal? (car (car hand)) "ace_clubs")
      (equal? (car (car hand)) "ace_diamonds")
      (equal? (car (car hand)) "ace_hearts")
      (equal? (car (car hand)) "ace_spades")
     ) #t
    )
    (else
      (has_ace (cdr hand))
    )
  )
)


; Despues de hacer un deal, se ejecuta esta función para
; ver si el jugador en turno obtuvo un bust.
;
; Parámetros:
;   player: Lista de propiedades de un jugador.
;
; Retorna:
;   verdadero si alguien tiene un bust o falso si no
;
(define (bust? player)
  (> (hand_total (get_hand player)) 21)
)


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
  ((cdr all_players_stayed_helper) players)
)

(define (all_players_stayed_helper players)
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


; Despues de hacer un deal, se ejecuta esta función para
; ver si el jugador en turno obtuvo un bust.
;
; Parámetros:
;   players: lista de propiedades de un jugador
;
; Retorna:
;   Verdadero si alguien lo obtuvo o falso si nadie lo consigue.
;
(define (blackjack? player)
  (equal? (hand_total (get_hand player)) 21))


; Genera una lista del total de puntos de cada jugador
;
; Parámetros:
;   players: lista de jugadores
;
; Retorna:
;   Lista con los puntajes de cada jugador
;
(define (get_points_list players)
  (cond
    ((empty? players) players)
    (else
      (append
        (list (hand_total (get_hand (car players))))
        (get_points_list (cdr players))
      )
    )
  )
)


; Genera una lista de los nombres de todos los jugadores
;
; Parámetros:
;   players: lista de jugadores
;
; Retorna:
;   Lista con los nombres de todos los jugadores
;
(define (get_names_list players)
  (cond
    ((empty? players) players)
    (else
      (append
        (list (get_name (car players)))
        (get_names_list (cdr players))
      )
    )
  )
)


; Genera una lista de pares a partir de los elementos de 2 listas.
; Tomado del taller funcional
;
; Parámetros:
;   lista1: primera lista
;   lista2: segunda lista
;
; Retorna:
;   Lista de pares
;
(define (pares lista1 lista2)
	(cond
	  ((null? lista1) lista1)
	  (else
	    (append
	      (list(list(car lista1)(car lista2)))
	      (pares (cdr lista1) (cdr lista2)))
	  )
	)
)


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
  (cond
    ((empty? players) (list))
    (else
      (list
        (list  (get_names_list players))
        (list  (get_points_list players))
      )
    )
  )
)


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
(define (change_status name new_status players)
  (cond((empty? players)
         (list))
       ((equal? name (caar players))
        (cons (cons (caar players)
               (cons new_status
                     (cons (caddar players)
                           (cons (cadr (cddar players))
                                 (list)))))
               (cdr players)))
        (else (cons (car players)
                    (change_status name new_status (cdr players))))))


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
      players
    )
    (else
      (check_dealer (deal "dealer" players deck) deck)
    )
  )
)


; Encuentra los elementos menores a un pivote de una lista
;
; Parámetros:
;   pivote: pivote al que se va a comprar cada elemento de la lista
;   lista: lista
;
; Retorna:
;   Lista de elementos menores al pivote
;
(define (menores pivote lista)
	(cond
	  ((null? lista) lista)
          ((>= pivote (car (cdr (car lista)))) (cons (car lista) (menores pivote (cdr lista))))
          (else
	    (menores pivote (cdr lista))
	  )
	)
)


; Encuentra los elementos mayores a un pivote de una lista
;
; Parámetros:
;   pivote: pivote al que se va a comprar cada elemento de la lista
;   lista: lista
;
; Retorna:
;   Lista de elementos mayores al pivote
;
(define (mayores pivote lista)
	(cond
	  ((null? lista) lista)
          ((< pivote (car (cdr (car lista)))) (cons (car lista) (mayores pivote (cdr lista))))
          (else
	    (mayores pivote (cdr lista))
	  )
	)
)


; Quicksort adaptado para ordenar una lista de pares, tomando en cuenta
; el segundo elemento de cada par. Este quicksort ordena de mayor a menor.
;
; Parámetros:
;   lista: lista de pares a ordenar
;
; Retorna:
;   lista de pares ordenada
;
(define (reverse_qsort lista)
	(cond
	  ((null? lista) lista)
    (else
      (append
        (reverse_qsort (mayores (car (cdr (car lista))) (cdr lista)))
        (list (car lista))
        (reverse_qsort (menores (car (cdr (car lista))) (cdr lista)))
      )
    )
  )
)


; Quicksort adaptado para ordenar una lista de pares, tomando en cuenta
; el segundo elemento de cada par.
;
; Parámetros:
;   lista: lista de pares a ordenar
;
; Retorna:
;   lista de pares ordenada
;
(define (qsort lista)
	(cond
	  ((null? lista) lista)
    (else
      (append
        (qsort (menores (car (cdr (car lista))) (cdr lista)))
        (list (car lista))
        (qsort (mayores (car (cdr (car lista))) (cdr lista)))
      )
    )
  )
)


; Genera dos listas: de primeros elementos y de segundos elementos pares a
; partir de los elementos de una lista de pares.
; Revierte el efecto de (pares lista)
;
; Parámetros:
;   lista: lista de pares
;
; Retorna:
;   lista con ambas listas de primeros elementos y segundos elementos
;
(define (dispares lista)
  (cond
    ((empty? lista) lista)
    (else
      (append
        (list (primeros lista))
        (list (segundos lista))
      )
    )
  )
)


; Obtiene los primeros de cada par de una lista de pares
;
; Parámetros:
;   lista: lista de pares
;
; Retorna:
;   lista con los primeros elementos de cada par de la lista
;
(define (primeros lista)
  (cond
    ((empty? lista) lista)
    (else
      (append
        (list (car (car lista)))
        (primeros (cdr lista))
      )
    )
  )
)


; Obtiene los segundos de cada par de una lista de pares
;
; Parámetros:
;   lista: lista de pares
;
; Retorna:
;   lista con los segundos elementos de cada par de la lista
;
(define (segundos lista)
  (cond
    ((empty? lista) lista)
    (else
      (append
        (list (cadr (car lista)))
        (segundos (cdr lista))
      )
    )
  )
)


; Ejecuta blackjack_sort. Este algoritmo ordena a los elementos
; de mayor a menor, pero cualquier elemento que sea mayor a 21, lo pone
; de último
;
; Parámetros:
;   lista: lista de pares
;
; Retorna:
;   lista ordenada
;
(define (blackjack_sort lista)
  (append
    (reverse_qsort (menores 21 lista))
    (qsort (mayores 21 lista))
  )
)

;(define (natural_blackjack_check players table)
;  (cond ((empty? players)
;          table)
;        (else
;          (natural_blackjack_check (cdr players) (append table (list (list (caar players) (hand_total (get_hand (car players))) (and (or
;                                                                             (string-contains? (caar (get_hand (car players))) "ace")
;                                                                             (string-contains? (caadr (get_hand (car players))) "ace"))
;                                                                           (equal? (hand_total (get_hand (car players))) 21)))))))))