#lang racket/gui

; o and x game
; with a GUI. Attempts to use an MVC-like approach.

(define myFrame (new frame%
                     [label "X and O game"]
                     [width 300] [height 350]
                     ))


;---------------------------------------------------------------------
; Model: State of the game
; use vectors as they are mutable - we can change the state
(define board (vector
               (vector 0 0 0)
               (vector 0 0 0)
               (vector 0 0 0) ))

; Size of game:
(define rows 3)
(define cols 3)

; current go: either 1 or 2
(define currentPlayer 1)

;---------------------------------------------------------------------
; functions to modify state

(define updateState (λ (rowNum colNum val)
                      ; update the state 
                      (vector-set! (vector-ref board rowNum) colNum val)
                      ))

(define switchPlayer (λ () (set! currentPlayer (- 3 currentPlayer))))


;----------------------------------------
#|
row1 (vector-ref board 2
row2 (vector-ref board 1
row3 (vector-ref board 0
    col1 (vector-ref (vector-ref board 2) 2)
         (vector-ref (vector-ref board 1) 2)
         (vector-ref (vector-ref board 0) 2)))
    col2 (vector-ref (vector-ref board 2) 1)
         (vector-ref (vector-ref board 1) 1)
         (vector-ref (vector-ref board 0) 1)))
    col3 (vector-ref (vector-ref board 2) 0)
         (vector-ref (vector-ref board 1) 0)
         (vector-ref (vector-ref board 0) 0)))
        dia1 (vector-ref (vector-ref board 2) 2)
             (vector-ref (vector-ref board 1) 1)
             (vector-ref (vector-ref board 0) 0)))
        dia2 (vector-ref (vector-ref board 2) 0)
             (vector-ref (vector-ref board 1) 1)
             (vector-ref (vector-ref board 0) 2)))
|#

(define non-zero? (λ (a)
                    (not (zero? a))))
(define winner (λ ()
                 (cond
                   ;PLAYER1 WINS
                   ((equal? '(1 1 1) (vector->list (vector-ref board 2))) 1)
                   ((equal? '(1 1 1) (vector->list (vector-ref board 1))) 1)
                   ((equal? '(1 1 1) (vector->list (vector-ref board 0))) 1)
                   ((equal? '(1 1 1) (list (vector-ref (vector-ref board 2) 2)
                                           (vector-ref (vector-ref board 1) 2)
                                           (vector-ref (vector-ref board 0) 2))) 1)
                   ((equal? '(1 1 1) (list (vector-ref (vector-ref board 2) 1)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 1))) 1)
                   ((equal? '(1 1 1) (list (vector-ref (vector-ref board 2) 0)
                                           (vector-ref (vector-ref board 1) 0)
                                           (vector-ref (vector-ref board 0) 0))) 1)
                   ((equal? '(1 1 1) (list (vector-ref (vector-ref board 2) 2)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 0))) 1)
                   ((equal? '(1 1 1) (list (vector-ref (vector-ref board 2) 0)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 2))) 1)
                   ;PLAYER2 WINS
                   ((equal? '(2 2 2) (vector->list (vector-ref board 2))) 2)
                   ((equal? '(2 2 2) (vector->list (vector-ref board 1))) 2)
                   ((equal? '(2 2 2) (vector->list (vector-ref board 0))) 2)
                   ((equal? '(2 2 2) (list (vector-ref (vector-ref board 2) 2)
                                           (vector-ref (vector-ref board 1) 2)
                                           (vector-ref (vector-ref board 0) 2))) 2)
                   ((equal? '(2 2 2) (list (vector-ref (vector-ref board 2) 1)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 1))) 2)
                   ((equal? '(2 2 2) (list (vector-ref (vector-ref board 2) 0)
                                           (vector-ref (vector-ref board 1) 0)
                                           (vector-ref (vector-ref board 0) 0))) 2)
                   ((equal? '(2 2 2) (list (vector-ref (vector-ref board 2) 2)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 0))) 2)
                   ((equal? '(2 2 2) (list (vector-ref (vector-ref board 2) 0)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 2))) 2)
                   ;DRAW
                   ((andmap equal? '(#t #t #t)
                           (list
                            (andmap non-zero? (vector->list (vector-ref board 0)))
                            (andmap non-zero? (vector->list (vector-ref board 1)))
                            (andmap non-zero? (vector->list (vector-ref board 2))))) 3)
                   (#t 0))))



; return winner (1 or 2) or 0 if no winner

; row
; col
; diagonal


;---------------------------------------------------------------------
; View: create button matrix;
(define font1 (make-font #:size 22
                         #:face #f
                         #:family 'script
                         #:style 'normal
                         #:weight 'bold))

; adds a button to container - callback does the actionFunc and calls update to refresh display
; updateFunc added to the updator set
(define addButton (λ (container actionFunc updateFunc rowNum colNum)
                    (let ((newBut
                           (new button% [label ""] [parent container] [callback
                                                                       (lambda (button event)
                                                                         (actionFunc)
                                                                         (update)
                                                                         )]
                                [font font1] [min-width 80] [min-height 60])
                           ))
                      (addUpdator (lambda () (updateFunc newBut)))
                      )
                    ))


(define createButRow (λ (rowContainer rowNum numberOfButtons)
                       ; create a row of buttons
                       (cond
                         ((equal? numberOfButtons 0))
                         (else  (let ((buttonIndex (+ (* cols (- rowNum 1)) numberOfButtons)))
                                  
                                  (addButton
                                   rowContainer
                                   (λ () ; actionFunc: set data item buttonIndex to currentPlayer; change currentPlayer
                                     (updateState (- rowNum 1) (- numberOfButtons 1) currentPlayer)
                                     (switchPlayer)
                                     )
                                   (λ (button) (updateButton button (- rowNum 1) (- numberOfButtons 1) )) ; updateFunc
                                   rowNum numberOfButtons)
                                  
                                  )
                                (createButRow rowContainer rowNum (- numberOfButtons 1)))
                         )))


(define createButtonRows (λ (vContainer rowCount colCount)
                           ; create a rowCount rows of buttons
                           (cond
                             ((equal? rowCount 0) )
                             (else  
                              (createButRow 
                               (new horizontal-panel% [parent vContainer]
                                    [alignment '(center center)])  rowCount colCount)
                              (createButtonRows vContainer (- rowCount 1) colCount) 
                              )
                             )
                           )
  )
(define createBoard (λ (container rowCount colCount) 
                      (createButtonRows 
                       (new vertical-panel% [parent container]
                            [alignment '(center center)])
                       rowCount colCount)
                      )
  )

; add a message box at the bottom: this will be updated by do show whose turn it is
(define font2 (make-font #:size 14
                         #:face #f
                         #:family 'modern
                         #:style 'italic
                         #:weight'bold)) 

(define messageBox (new message% [label ""] [parent myFrame] [font font2] [auto-resize #t]))

(define addMessageArea (λ ()
                         
                         (addUpdator (λ ()
                                       (let ((w (winner)))
                                         (cond
                                           ((equal? w 0) (send messageBox set-label (format "Player ~a to play" currentPlayer)))
                                           ((equal? w 3) (send messageBox set-label "It's a DRAW !!!"))
                                           (#t (send messageBox set-label (format "Winner is Player ~a !!!" w)))
                                           )
                                         ))
                                     )))

;---------------------------------------------------------------------
; update a button - change label and disable

(define updateButton (λ (but rowNum colNum)
                       (let* ((val (vector-ref (vector-ref board  rowNum) colNum))
                              (str (cond
                                     ((= val 1) "X")
                                     ((= val 2) "O")
                                     (#t "")
                                     )))
                         (send but set-label str)
                         (cond ((> val 0) (send but enable #f))
                               ((equal? (send messageBox get-label) "Winner is Player 1 !!!")
                                (send but enable #f))
                               ((equal? (send messageBox get-label) "Winner is Player 2 !!!")
                                (send but enable #f))
                               (#t (send but enable #t))
                               ; disable if already played in that cell
                               )
                         )
                       ))

; list of functions to update display
(define updators (list))
; run all update functions
(define update (λ ()
                 (for-each (λ (func) (func))  updators)
                 ))
(define addUpdator (λ (fun)
                     (set! updators (cons fun updators))))


;---------------------------------------------------------------------
; initialisation - create the view

(createBoard myFrame rows cols)
(addMessageArea)
(send myFrame show #t)

(define restrt (new button%
                    [parent myFrame]
                    [label "Restart"]
                    [callback (λ (o e)
                                (vector-set! board 0 (vector 0 0 0))
                                (vector-set! board 1 (vector 0 0 0))
                                (vector-set! board 2 (vector 0 0 0))
                                (set! currentPlayer 1)
                                (update)
                                )]))

(update)
