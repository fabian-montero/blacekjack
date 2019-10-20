#lang racket
(require racket/gui
         racket/draw
         dyoo-while-loop
         "engine.rkt")


(define (get_gui_elements player)
  (cadddr player))

; Descripción: Ventana principal del juego.
;
(define frame_main (new frame%
                        [label "BlackJack a lo chuzo"]
                        [min-width 1300]
                        [min-height 800]
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

                         ))

; Descripción: Panel con los canvas de los juegadores.
;
(define pane_players (new horizontal-panel%
                         [parent frame_main]
                         [style '(auto-hscroll)]
                         [border 20]
                         ))


(define bitmap2 (let ([x (send (make-object bitmap% 1000 1000) make-dc)]
                      [bite (read-bitmap "./resources/cards/4S.png")])
                 (begin
                   (send x draw-bitmap-section-smooth bite 0 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
                   (send x draw-bitmap-section-smooth bite (* (* (send bite get-width) 0.29) 1) 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
                   (send x draw-bitmap-section-smooth bite (* (* (send bite get-width) 0.29) 2) 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
                   (send x draw-bitmap-section-smooth bite (* (* (send bite get-width) 0.29) 3) 0 (* (send bite get-width) 0.29) (* (send bite get-height) 0.29) 0 0 (send bite get-width) (send bite get-height))
                   ;(send x draw-bitmap-section-smooth  173 0 173 264 0 0 173 264)
                   (send x get-bitmap))))

(define (paint-shit canvas dc)
  (begin
    (call-with-values (lambda () (send canvas_dealer_cards get-view-start)) (lambda (x y) (send dc draw-bitmap-section bitmap2 x y x y (send canvas get-width) (send canvas get-height))))

    ))


; Descripción: canvas para las cartas del dealer.
;
(define canvas_dealer_cards (new canvas%
                                 [parent pane_dealer]
                                 [style '(hscroll)]
                                 [paint-callback paint-shit]
                                 [stretchable-height #t]
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

(define (create_players players)
  (cond ((empty? players)
          (list))
        (else
          (cons (get_name (car players))
                (cons (get_status (car players))
                      (cons (get_hand (car players))
                            (cons (create_gui_elements (get_name (car players)))
                                  (create_players (cdr players)))))))))


(define (create_gui_elements name)
  (begin
    (let* ([vertical (new vertical-pane%
                      [parent pane_players]
                      [min-width 300]
                      [stretchable-width #f])]
            [canvas (new canvas%
                      [parent vertical]
                      [style '(hscroll vscroll)]
                      [paint-callback paint-shit])]

            [horizontal (new horizontal-pane%
                      [parent vertical]
                      [min-width 300]
                      [stretchable-width #f]
                      [stretchable-height #f])])

          (list canvas (new button%
                              [parent horizontal]
                              [label "Hit"])
                        (new button%
                              [parent horizontal]
                              [label "Stay"])
                        (new message%
                              [parent horizontal]
                              [label name])))))



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
  (gen_tabla_helper (get_names_list players) (get_points_list players))
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