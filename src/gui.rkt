#lang racket/gui
(require ffi/unsafe
         dyoo-while-loop
         "engine.rkt"
         pict
         )
(provide (all-defined-out))

; lista players de ejemplo para debugging
(define (get_players_example)
  '(
      ("dealer" "stay" (("four_clubs" 4 0 0 #t)) ())
      ("fabian" "stay" (("five_clubs" 5 0 0 #t) ("six_clubs" 6 0 0 #t) ("seven_clubs" 7 0 0 #t)) ())
      ("alejandro" "stay" (("four_clubs" 4 0 0 #t) ("five_clubs" 5 0 0 #t) ("six_clubs" 6 0 0 #t) ("seven_clubs" 7 0 0 #t)) ())
      ("vanessa" "stay" (("four_clubs" 4 0 0 #t) ("five_clubs" 5 0 0 #t) ("six_clubs" 6 0 0 #t) ("seven_clubs" 7 0 0 #t)) ())
      ("hazel" "stay" (("four_clubs" 4 0 0 #t) ("five_clubs" 5 0 0 #t) ("six_clubs" 6 0 0 #t) ("seven_clubs" 7 0 0 #t)) ())
   )
)

(define players (get_players_example))

(define deck (gen_deck))

(define (get_gui_elements player)
  (cadddr player))

(define testing_frame (new frame% [label "test"]))

(define-values [W H]
  (let ([f testing_frame]
        [tipo (cpointer-tag (send testing_frame get-handle))])
    (cond ((equal? tipo 'GtkWidget)
            (values 1920 1080))
          ((equal? tipo 'HWND)
            (begin0 (send* f (maximize #t) (show #t) (get-client-size))
              (send f show #f))))))


; Descripción: Ventana principal del juego.
;
(define frame_main (new frame%
                        [label "BlackJack"]
                        [min-width (exact-round (* W 0.80))]
                        [min-height (exact-round (* H 0.80))]
                        [style '(no-resize-border)]
                        [stretchable-width #f]
                        [stretchable-height #f]))

; Descripción: Panel con el canvas del dealer y el deck.
;
(define pane_dealer (new horizontal-pane%
                         [parent frame_main]
                         [alignment '(center center)]
                         [stretchable-height #f]
                         [min-height (exact-round (* (send frame_main min-height) 0.4))]
                         [border 5]
                         [spacing 10]
                         ))

; Descripción: Panel con los canvas de los juegadores.
;
(define pane_players (new horizontal-panel%
                         [parent frame_main]
                         [style '(auto-hscroll)]
                         [alignment '(center center)]
                         [border 5]
                         [spacing 20]
                         ))


;(define bitmap2 (let ([x (send (make-object bitmap% 1000 1000) make-dc)]
;                      [bite (read-bitmap "./resources/cards/4S.png")])
;                 (begin
;                   (send x draw-bitmap-section-smooth bite 0 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
;                   (send x draw-bitmap-section-smooth bite (* (* (send bite get-width) 0.29) 1) 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
;                   (send x draw-bitmap-section-smooth bite (* (* (send bite get-width) 0.29) 2) 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
;                   (send x draw-bitmap-section-smooth bite (* (* (send bite get-width) 0.29) 3) 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
;                   ;(send x draw-bitmap-section-smooth  173 0 173 264 0 0 173 264)
;                   (send x get-bitmap))))


; Descripción: canvas para las cartas del dealer.
;
(define canvas_dealer_cards (new horizontal-panel%
                                 [parent pane_dealer]
                                 [style '(auto-hscroll)]
                                 [stretchable-width #f]
                                 [min-width (exact-round (* (send frame_main min-width) 0.5))]
                                 ))





; Descripción: canvas para las cartas del deck.
;
(define canvas_deck (new horizontal-panel%
                                 [parent pane_dealer]
                                 [alignment '(center top)]
                                 [stretchable-width #f]
                                 [min-width (exact-round (* (send frame_main min-width) 0.5))]
                                 ))

(define (draw_deck)
  (new message% [parent canvas_deck]
                [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "./resources/cards/reverse.png"))) 0.3))]))

(define (draw_dealer_cards hand)
  (cond ((empty? hand)
          (list))
        (else
          (cons (append (car hand) (new message% [parent canvas_dealer_cards]
                                                 [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "./resources/cards/JH.png"))) 0.3))]))
                (draw_dealer_cards (cdr hand))))))

(define (hit_event button event)
  (let* ([name (send (caddr (send (send button get-parent) get-children)) get-label)]
        [new_players (deal name players deck)]
        [player (get_player name new_players)]
        [cards (get_hand player)])
    (begin
      (set! players new_players)
      (set! deck (cdr deck))
      (while (not (empty? cards))
        (draw_player_cards (caar cards) (car (get_gui_elements player)))
        (set! cards (cdr cards))))))

; Construye los canvas y páneles necesarios para los jugadores,
; luego los añade al pane_players, además añade a list_players
; los objetos de los botones hit y stay
;
; Parámetros:
;   players: lista de jugadores.
;   total: Cantidad de jugadores.
;
; Retorna:
;   Lista de jugadores con sus elementos gráficos agregados.
;
(define (create_players players total)
  (cond ((empty? players)
          (list))
        (else
         (cons (append (list (get_name (car players)))
                     (cons (get_status (car players))
                           (cons (get_hand (car players))
                                 (create_players_helper (get_name (car players)) total))))
               (create_players (cdr players) total)))))


(define (create_players_helper name total)
  (begin
    (let* ([vertical (new vertical-pane%
                      [parent pane_players]
                      [min-width (exact-round (* (send frame_main min-width) (cond ((>= total 3) 0.33) ((equal? total 2) 0.5) ((equal? total 1) 1))))]
                      [stretchable-width #f])]
            [canvas (new horizontal-panel%
                      [parent vertical]
                      [style '(auto-hscroll)]
                      [alignment '(center center)]
                      [stretchable-height #t])]

            [horizontal (new horizontal-pane%
                      [parent vertical]
                      [alignment '(center center)]
                      [stretchable-height #f])])

          (begin
            ;(let ([cards (overlay/offset (scale 0.3 (bitmap/file (string-append "./resources/cards/JH.png"))) 10 10 (scale 0.3 (bitmap/file (string-append "./resources/cards/QH.png"))))])
            ;(new message% [parent canvas]
                          ;[label cards]))
            (list canvas (new button%
                                [parent horizontal]
                                [label "Hit"]
                                [callback hit_event]
                                [stretchable-height #t])
                          (new button%
                                [parent horizontal]
                                [label "Stay"]
                                [stretchable-height #t])
                          (new message%
                                [parent horizontal]
                                [label name]
                                [auto-resize #t]))))))

(define (draw_player_cards card panel)
  (new message% [parent panel]
                [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "./resources/cards/" card ".png"))) 0.3))]))



; Show the frame
(send frame_main show #t)



; Genera tabla de puntuaciones y la muestra
;
; Parámetros:
;   players: lista de jugadores
;
; Retorna:
;   N/A
;
(define (gen_tabla players)
  (let ([pack (dispares (blackjack_sort (pares (get_names_list players) (get_points_list players))))])
    (gen_tabla_helper (car pack) (car (cdr pack)))
  )
)

(define (gen_tabla_helper lista_de_jugadores lista_de_puntajes)
  (begin
    (define punctuarion_table
      (new dialog%
        [label "Tabla de Puntajes"]
        [width 1000]
        [height 500]
      )
    )
    (define table
      (new list-box%
        [label #f]
        [choices lista_de_jugadores]
        [parent punctuarion_table]
        [columns '("Jugador" "Puntaje")]
        [style '(single column-headers reorderable-headers)]
      )
    )
    (let ([c 0] [puntajes lista_de_puntajes])
      (while (not (empty? puntajes))
        (send table set-string c (number->string (car puntajes)) 1)
        (set! c (+ 1 c))
        (set! puntajes (cdr puntajes))
      )
    )
    (send punctuarion_table show #t)
  )
)