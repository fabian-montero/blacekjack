; lista players
(define (get_players_example)
  '(
      ("dealer" "stay" (("four_clubs" 4 0 0) ("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)) ())
      ("fabian" "stay" (("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)) ())
      ("dealer" "stay" (("four_clubs" 4 0 0) ("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)) ())
      ("dealer" "stay" (("four_clubs" 4 0 0) ("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)) ())
      ("dealer" "stay" (("four_clubs" 4 0 0) ("five_clubs" 5 0 0) ("six_clubs" 6 0 0) ("seven_clubs" 7 0 0)) ())
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

(define (hand_total hand)
  (cond
    ((empty? hand) 0)
    (else
      (+ (car (cdr (car hand))) (hand_total (cdr hand)))
    )
  )
)

(define (get_deck player)
    (car (cdr (cdr player)))
)


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

(define (get_status player)
  (car (cdr player))
)

(define (check_dealer players deck)
  (cond
    ((> (hand_total (get_deck (get_dealer players))) 16  )
      (endgame (stay "dealer" players))
    )
    (else
      (check_dealer (deal "dealer" players deck))
    )
  )
)

(define (get_dealer players)
  (car players)
)