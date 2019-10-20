(require racket/gui)

(define (gen_tabla players)
  (gen_tabla_helper (get_names players) (get_points players))
)

(define (get_names players)
  (append (get_name players) ((get_names players)))
)

(define (get_points players)

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
    (send table set-string 1 "test" 1)
    (send punctuarion_table show #t)
  )


)
