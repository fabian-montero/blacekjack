#lang racket/gui
(require ffi/unsafe
         dyoo-while-loop
         "engine.rkt"
         pict)
(provide (all-defined-out))


; Descripción: Lista de jugadores
;
(define players '())


; Descripción: Lista de cartas mezcladas y a utilizar
;
(define deck '())


; Busca el booleano que indica
; si la carta está vuelta hacia arriba o hacia abajo.
;
; Parámetros:
;   card: Lista con las propiedades de la carta.
;
; Retorna:
;   Verdadero si la carta está volteada hacia arriba o falso en el contrario.
;
(define (faced_up? card)
  (cadr (cdddr card)))


; Descripción: Ventana de prueba para obtener el tamaño de la pantalla.
;
(define testing_frame (new frame% [label "test"]))


; Valores de anchura (W) y altura (H) de la pantalla
; obtenidos según el sistema operativo, para windows (HWND)
; el valor se obtiene usando testing_frame, para linux se establece
; una resolución de 1920x1080.
;
; Parámetros:
;   N/A
;
; Retorna:
;   N/A
;
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
                         [spacing 40]
                         ))


; Descripción: Panel con los canvas de los juegadores.
;
(define pane_players (new horizontal-panel%
                         [parent frame_main]
                         [style '(auto-hscroll)]
                         [alignment '(center center)]
                         [border 40]
                         [spacing 100]

                         ))


; Descripción: canvas para las cartas del dealer.
;
(define canvas_dealer_cards (new horizontal-panel%
                                 [parent pane_dealer]
                                 [style '(auto-hscroll)]
                                 [stretchable-width #f]
                                 [alignment '(right center)]
                                 [min-width (exact-round (* (send frame_main min-width) 0.5))]
                                 ))


; Descripción: canvas para las cartas del deck.
;
(define canvas_deck (new horizontal-panel%
                                 [parent pane_dealer]
                                 [alignment '(left center)]
                                 [stretchable-width #f]
                                 [min-width (exact-round (* (send frame_main min-width) 0.5))]
                                 ))


; Dibuja la carta que representa el maso de cartas (deck) o pozo
;
; Parámetros:
;   N/A
;
; Retorna:
;   Objeto de tipo messsage%
;
(define (draw_deck)
  (new message% [parent canvas_deck]
                [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "./resources/cards/reverse.png"))) 0.2))]))


; Dibuja las cartas del dealer (crupier) y muestra una de las cartas boca abajo.
;
; Parámetros:
;   hand: Lista de cartas del dealer.
;
; Retorna:
;   Lista de cartas con el objeto message% de la carta agregado al final de cada carta.
;
(define (draw_dealer_cards hand)
  (cond ((empty? hand)
          (list))
        (else
          (cons (append (car hand) (new message% [parent canvas_dealer_cards]
                                                 [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "./resources/cards/" (cond ((faced_up? (car hand)) (caar hand)) (else "reverse")) ".png"))) 0.2))]))
                (draw_dealer_cards (cdr hand))))))


; Dibuja la carta del jugador en su panel específico.
;
; Parámetros:
;   card: Nombre de la carta a dibujar
;   panel: Panel donde se dibujará la carta.
;
; Retorna:
;   Objeto message% con el dibujo de la carta.
;
(define (draw_player_cards card panel)
  (new message% [parent panel]
                [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "./resources/cards/" card ".png"))) 0.2))]))


; Responde al presionar el botón de hit de un jugador
;
; Parámetros:
;   button: Objeto button% del jugador.
;   event: Nombre del evento del presionado del botón.
;
; Retorna:
;   N/A
;
(define (hit_event button event)
  (let* ([name (send (caddr (send (send button get-parent) get-children)) get-label)]
        [new_players (deal name players deck)]
        [player (get_player name new_players)]
        [stay_button (cadr (send (send button get-parent) get-children))]
        [cards (get_hand player)]
        [panel (car (send (send (send button get-parent) get-parent) get-children))]
        [message_name (caddr (send (send button get-parent) get-children))])
    (begin
      (set! players new_players)
      (set! deck (cdr deck))
      (draw_player_cards (car (list-ref cards (- (length cards) 1))) panel)
      (send (cadddr (send (send button get-parent) get-children)) set-label (string-append "Score: " (number->string (hand_total cards))))
      (cond ((bust? player)
              (send button enable #f)
              (send stay_button enable #f)
              (send message_name set-label (string-append name " (busted)"))
              (set! players (change_status name "stay" new_players))
              (cond ((all_players_stayed? players)
                     (terminator)))
              )
            ((blackjack? players)
              (for-each
                (lambda (panel)
                  (for-each
                    (lambda (but)
                      (cond ((is-a? but button%) (send but enable #f))))
                  (send (cadr (send panel get-children)) get-children)))
              (send (send (send (send button get-parent) get-parent) get-parent) get-children))
              (terminator)
              )))))


; Responde al presionar el botón de stay de un jugador
;
; Parámetros:
;   button: Objeto button% del jugador.
;   event: Nombre del evento del presionado del botón.
;
; Retorna:
;   N/A
;
(define (stay_event button event)
  (let* ([name (send (caddr (send (send button get-parent) get-children)) get-label)]
        [hit_button (car (send (send button get-parent) get-children))]
        [message_name (caddr (send (send button get-parent) get-children))])
    (begin
      (set! players (change_status name "stay" players))
      (send button enable #f)
      (send hit_button enable #f)
      (send message_name set-label (string-append name " (stayed)"))
      (cond ((all_players_stayed? players)
      (terminator)))
      )))


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
                                 (create_players_helper (get_name (car players)) total (get_hand (car players))))))
               (create_players (cdr players) total)))))


(define (create_players_helper name total hand)
  (begin
    (let* ([vertical (new vertical-pane%
                      [parent pane_players]
                      [stretchable-width #f])]
            [canvas (new horizontal-panel%
                      [parent vertical]
                      [min-width 426]
                      [style '(auto-hscroll)]
                      [alignment '(center center)]
                      [stretchable-height #f]
                      [stretchable-width #f])]

            [horizontal (new horizontal-pane%
                      [parent vertical]
                      [alignment '(center center)]
                      [vert-margin 60]
                      [stretchable-height #f])])

          (begin
            ;(let ([cards (overlay/offset (scale 0.3 (bitmap/file (string-append "./resources/cards/JH.png"))) 10 10 (scale 0.3 (bitmap/file (string-append "./resources/cards/QH.png"))))])
            ;(new message% [parent canvas]
                          ;[label cards]))

            (new button%
                 [parent horizontal]
                 [label "Hit"]
                 [callback hit_event]
                 [stretchable-height #t])
            (new button%
                 [parent horizontal]
                 [label "Stay"]
                 [callback stay_event]
                 [stretchable-height #t])
            (new message%
                 [parent horizontal]
                 [label name]
                 [auto-resize #t])
            (new message%
                 [parent horizontal]
                 [label (string-append "Score: " (number->string (hand_total hand)))]
                 [horiz-margin 10]
                 [auto-resize #t])))))


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

; Finaliza el juego mostrando la carta oculta del dealer y lanzando la pantalla
; de jugadores.
;
; Parámetros:
;   N/A
;
; Retorna:
;   N/A
;
(define (terminator)
  (begin
    (for-each (lambda (child) (send canvas_dealer_cards delete-child child)) (send canvas_dealer_cards get-children))
    (set! players (check_dealer players deck))

    (draw_dealer_cards (cons (flip_card (car (get_hand (car players))) #t) (cdr (get_hand (car players)))))
    (gen_tabla players)
    ))

; Inicializa las variables globales y hace la parimera repartición de cartas.
;
; Parámetros:
;   X: Lista de jugadores.
;
; Retorna:
;   N/A
;
(define (bCEj X)
  (let* ([names (append (list "dealer") X)]
        [init_players (deal_init names '())]
        [init_cards '()])
    (begin
      (set! deck (gen_deck))
      (while (not (empty? names))
        (set! init_players (deal (car names) init_players deck))
        (set! deck (cdr deck))
        (set! init_players (deal (car names) init_players deck))
        (set! deck (cdr deck))
        (set! names (cdr names)))
      (set! players init_players)
      (draw_deck)
      (draw_dealer_cards (get_hand (car players)))
      (create_players (cdr players) (length X))
      (set! init_players (cdr init_players))
      (set! init_cards (get_hand (car init_players)))

      (let* ([panels (send pane_players get-children)]
             [current_panel (car (send (car panels) get-children))])
             (while (not (empty? panels))
               (while (not (empty? init_cards))
                 (draw_player_cards (caar init_cards) current_panel)
                 (set! init_cards (cdr init_cards)))
               (set! panels (cdr panels))
               (set! init_players (cdr init_players))
               (cond ((not (empty? init_players)) (set! init_cards (get_hand (car init_players)))))
               (cond ((not (empty? panels)) (set! current_panel (car (send (car panels) get-children)))))
               )
             )

      (cond ((blackjack? players)
             (terminator)
             (send frame_main show #f))
            (else
             (send frame_main show #t))))))