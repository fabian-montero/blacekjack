#lang racket/gui
(require ffi/unsafe
         racket/draw
         dyoo-while-loop
         "engine.rkt")
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


(define (get_gui_elements player)
  (cadddr player))

(define testing_frame (new frame% [label "test"]))

(define-values [W H]
  (let ([f testing_frame]
        [tipo (cpointer-tag (send testing_frame get-handle))])
    (cond ((equal? tipo 'GtkWidget)
            (values 1366 768))
          ((equal? tipo 'HWND)
            (begin0 (send* f (maximize #t) (show #t) (get-client-size))
              (send f show #f))))))


; Descripción: Ventana principal del juego.
;
(define frame_main (new frame%
                        [label "PROFE PÁSEME POR FAVOR"]
                        [min-width (- W 10)]
                        [min-height (- H 10)]
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
                         [border 10]
                         [spacing 10]
                         ))

; Descripción: Panel con los canvas de los juegadores.
;
(define pane_players (new horizontal-panel%
                         [parent frame_main]
                         [style '(auto-hscroll)]
                         [border 5]
                         [spacing 10]
                         ))


(define bitmap2 (let ([x (send (make-object bitmap% 1000 1000) make-dc)]
                      [bite (read-bitmap "src/resources/cards/4S.png")])
                 (begin
                   (send x draw-bitmap-section-smooth bite 0 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
                   (send x draw-bitmap-section-smooth bite (* (* (send bite get-width) 0.29) 1) 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
                   (send x draw-bitmap-section-smooth bite (* (* (send bite get-width) 0.29) 2) 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
                   (send x draw-bitmap-section-smooth bite (* (* (send bite get-width) 0.29) 3) 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
                   ;(send x draw-bitmap-section-smooth  173 0 173 264 0 0 173 264)
                   (send x get-bitmap))))

(define (paint-shit canvas dc)
  (begin
    (call-with-values (lambda () (send canvas_dealer_cards get-view-start)) (lambda (x y) (send dc draw-bitmap-section bitmap2 x y x y (send canvas get-width) (send canvas get-height))))))


; Descripción: canvas para las cartas del dealer.
;
(define canvas_dealer_cards (new canvas%
                                 [parent pane_dealer]
                                 [style '(hscroll)]
                                 [paint-callback paint-shit]
                                 [stretchable-width #f]
                                 [min-width (exact-round (* (send frame_main min-width) 0.6))]
                                 ))

; Descripción: canvas para las cartas del deck.
;
(define canvas_deck (new canvas%
                                 [parent pane_dealer]
                                 [style '(hscroll)]
                                 [stretchable-height #t]
                                 [stretchable-width #f]
                                 [min-width (exact-round (* (send frame_main min-width) 0.4))]
                                 ))

; Construye los canvas y páneles necesarios para los jugadores,
; luego los añade al pane_players, además añade a list_players
; los objetos de los botones hit y stay
;
; Parámetros:
;   players: lista de jugadores
;
; Retorna:
;   Lista de jugadores con sus elementos gráficos agregados.
;
(define (create_players players)
  (cond ((empty? players)
          (list))
        (else
          (cons (get_name (car players))
                (cons (get_status (car players))
                      (cons (get_hand (car players))
                            (cons (create_players_helper (get_name (car players)))
                                  (create_players (cdr players)))))))))


(define (create_players_helper name)
  (begin
    (let* ([vertical (new vertical-pane%
                      [parent pane_players]
                      [min-width (exact-round (* (send frame_main min-width) 0.3333333333333333))]
                      [stretchable-width #f])]
            [canvas (new canvas%
                      [parent vertical]
                      [style '(hscroll)]
                      [paint-callback paint-shit])]

            [horizontal (new horizontal-pane%
                      [parent vertical]
                      [alignment '(center center)]
                      [stretchable-height #f])])

          (list canvas (new button%
                              [parent horizontal]
                              [label "Hit"]
                              [stretchable-height #t])
                        (new button%
                              [parent horizontal]
                              [label "Stay"]
                              [stretchable-height #t])
                        (new message%
                              [parent horizontal]
                              [label name]

                              [auto-resize #t])))))



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