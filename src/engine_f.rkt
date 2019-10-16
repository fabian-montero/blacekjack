; all_players_stayed?
; check_dealer

; lista players
(define (get_players_example)
  '(
      ("dealer" playing (("four_clubs" 4 0 0) ("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)) ())
      ("fabian" playing (("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)) ())
   )
)

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

(define (bust? players)
  (cond
    ((empty? players) #f)
    ((> (hand_total (get_deck (car players))) 21) #t)
    (else
      (bust? (cdr players))
    )
  )
)

(define (hand_total lista)
  (cond
    ((empty? lista) 0)
    (else
      (+ (car (cdr (car lista))) (hand_total (cdr lista)))
    )
  )
)

(define (get_deck player)
    (car (cdr (cdr player)))
)
