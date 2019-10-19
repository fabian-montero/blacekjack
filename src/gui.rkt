#lang racket

(require racket/gui)
(require racket/draw
         pict)

(define frame_main (new frame%
                        [label "BlackJack a lo chuzo"]
                        [min-width 1300]
                        [min-height 800]
                        [style '(no-resize-border)]
                        [stretchable-width #f]
                        [stretchable-height #f]))

(define pane_dealer (new horizontal-pane%
                         [parent frame_main]
                         [alignment '(center center)]
                         [stretchable-height #f]
                         [min-height (exact-round (* (send frame_main min-height) 0.4))]

                         ))

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
    ;(send dc set-scale 0.55 0.55)
    (call-with-values (lambda () (send canvas_dealer_cards get-view-start)) (lambda (x y) (send dc draw-bitmap-section bitmap2 x y x y (send canvas get-width) (send canvas get-height))))
    ;(send dc draw-bitmap (read-bitmap "/home/acnissarin/4S.xpm") 173 0)
    ;(send dc draw-bitmap (read-bitmap "/home/acnissarin/7H.bmp") (* 173 1) 0)
    ;(send dc draw-bitmap (read-bitmap "/home/acnissarin/7H.bmp") (* 173 2) 0)
    ;(send dc draw-bitmap (read-bitmap "/home/acnissarin/7H.bmp") (* 173 3) 0)
    ;(send dc draw-bitmap (read-bitmap "/home/acnissarin/7H.bmp") (* 173 4) 0)
    ;(send dc draw-bitmap (read-bitmap "/home/acnissarin/7H.bmp") (* 173 5) 0)
    ;(send dc draw-bitmap (read-bitmap "/home/acnissarin/7H.bmp") (* 173 6) 0)
    ;(send dc draw-bitmap (read-bitmap "/home/acnissarin/7H.bmp") (* 173 7) 0)
    ;(send dc draw-bitmap (read-bitmap "/home/acnissarin/4S.xpm") (* 173 1) 0)))
    ))
    

(define canvas_dealer_cards (new canvas%
                                 [parent pane_dealer]
                                 [style '(hscroll)]
                                 [paint-callback paint-shit]
                                 [stretchable-height #t]
                                 [stretchable-width #f]
                                 [min-width (exact-round (* (send frame_main min-width) 0.6))]
                                 ))

(define canvas_deck (new canvas%
                                 [parent pane_dealer]
                                 [style '(hscroll)]
                                 [stretchable-height #t]
                                 [stretchable-width #f]
                                 [min-width (exact-round (* (send frame_main min-width) 0.4))]
                                 ))

(send canvas_dealer_cards init-auto-scrollbars 3000 1 0 0)

; Show the frame
(send frame_main show #t)

(send canvas_dealer_cards get-graphical-min-size)
; Wait a second to let the window get ready
; Draw the face