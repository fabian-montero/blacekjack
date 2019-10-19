(require racket/gui)
(require racket/draw
         pict)


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
                         ))


(define bitmap2 (let ([x (send (make-object bitmap% 1000 1000) make-dc)]
                      [bite (read-bitmap "/home/acnissarin/Projects/blacekjack/src/resources/cards/4S.png")])
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

(define (names_list players)
  (cond ((empty? players)
          (list))
        (else
          (cons (caar players)
                (names_list (cdr players))))))

(define (create_players players)
  (begin
    (new tab-panel%
      [choices (names_list players)]
      [parent pane_players])
    ))

(define (fill_players_table players)
  (cond ((empty? players)
          (list))
        (else )))

; Show the frame
(send frame_main show #t)

