; gui
(require racket/gui/base)

; crea la frame y le pone título
(define frame (new frame% [label "BlaCEk-Jack"]))

; crea un mensajito cute
(define msg (new message% [parent frame]
  	    [label "Todo está bien."]))

; crea un boton
(new button% [parent frame]
     	     [label "Haga click aqui"]
	     ; este es el callback para ejecutarse cuando se apreta el botón
	     [callback (lambda (button event)
			 (send msg set-label "Alguien hizo click en el botón"))])

; muestra la ventana
(send frame show #t)




