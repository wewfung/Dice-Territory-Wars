#lang racket/gui

;Gamble Game
;William Fung and Eric Stevenson
;18 June, 2014

(require racket/class)

(define mapwidth 20)
(define mapheight 14)
(define windowwidth 300)
(define windowheight 300)
(define map (list)) ;6 x 6 map of territories
(define usedpos (list)) ;needed for map generation, becomes empty afterward
(define temppos (list)) ;holds  locations of territories in map
(define players 4)
(define winner -1)
(define aliveplayers (list)) 
(define colours (list "green" "turquoise" "pink" "orange" "yellow"))
(define index -1)
(define playerturn 0)
(define attacker "")
(define defender "")
(define attackroll (list))
(define attacktotal "")
(define defendroll (list))
(define defendtotal "")
(define canclick #t)
(define x-offset 20) ;offsets used for drawing gui
(define y-offset 40)


;indexof method
(define (index-of lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))

(define territory% ;territory class, represents a single territory owned by a player
  (class object%
    (init player)
    (init size)
    (init adj) ;list of coordinates of adjacent territories
    (init x)
    (init y)
    (super-new)
    
    
    
    (define my-player player)
    (define my-size size)
    (define my-adj adj)
    (define my-x x)
    (define my-y y)
    (define clicked #f)
    (define highlight #f)
    
    (define/public (get-player) my-player)
    (define/public (get-size) my-size)
    (define/public (get-x) my-x)
    (define/public (get-y) my-y)
    (define/public (get-adj) my-adj)
    (define/public (get-clicked) clicked)
    (define/public (get-highlight) highlight)
    
    (define/public (set-player p) (set! my-player p))
    (define/public (set-size s) (set! my-size s))
    (define/public (set-adj a) (set! my-adj a))
    (define/public (set-clicked c) (set! clicked c))
    (define/public (set-highlight h) (set! highlight h))
    
    ))

(define (battle ter1 ter2) ;roll dice to determine the victor in a battle between two territories
  (set! attacker (list-ref colours (send ter1 get-player)))
  (set! defender (list-ref colours (send ter2 get-player)))
  (set! attackroll (list))
  (set! defendroll (list))
  (let* (
         (roll1 0)
         (roll2 0)
         (rand 0)
         (attackedplayer (send ter2 get-player))
         (remaining #f))
    
    (for ((i (send ter1 get-size)))
      (for ((j 20)) ;used for visual flair to show dice scramble animation
        (set! attackroll (append (take attackroll i) (list(add1 (random 6)))))
        (send maincanvas2 refresh)
        (sleep/yield 0.003))
      (set! rand (add1 (random 6)))
      (set! roll1 (+ roll1 rand))
      (set! attackroll (append (take attackroll i) (list rand)))
      )
    (for ((i (send ter2 get-size)))
      (for ((j 20))
        (set! defendroll (append (take defendroll i) (list(add1 (random 6)))))
        (send maincanvas2 refresh)
        (sleep/yield 0.003))
      (set! rand (add1 (random 6)))
      (set! roll2 (+ roll2 rand))
      (set! defendroll (append (take defendroll i) (list rand)))
      )
    (set! attacktotal (number->string roll1))
    (set! defendtotal (number->string roll2))
    
    (cond [ (> roll1 roll2) ;changes posession of territory based on who won
            
            (send ter2 set-size (sub1 (send ter1 get-size)))
            (send ter2 set-player (send ter1 get-player))
            (send ter1 set-size 1)]
          [ (<= roll1 roll2)
            (send ter1 set-size 1)])
    
    (for ((i map)) 
      (when (equal? attackedplayer (send i get-player))
        (set! remaining #t)))
    
    (unless remaining ;check if player had died
      (set! aliveplayers (append (take aliveplayers attackedplayer) (list #f) (drop aliveplayers (add1 attackedplayer)))))
    
    (send maincanvas2 refresh)
    (set! canclick #t)
    ))



(define my-canvas%
  (class canvas%
    (inherit  refresh)
    
    (define/override (on-event event) ;mouse event handler
      (let ((clickedpos (list (truncate (/ (- (send event get-x) x-offset) 50)) (truncate (/ (- (send event get-y) y-offset) 50))))) ;get which tile was clicked on
        
        (when (send event button-down? 'left)
          
          (if (not(= index -1)) ;check if a tile has already been selected
              (begin
                (when canclick
                  (for ((i (send (list-ref map index) get-adj)))
                    (when (and (equal? i clickedpos) (not (equal? playerturn (send (list-ref map (index-of temppos i)) get-player)))) ;check if highlighted tile was clicked
                      ;call battle method with territory at index and highlighted territory
                      (set! canclick #f)
                      (battle (list-ref map index) (list-ref map (index-of temppos i)))
                      (send (list-ref map index) set-clicked #f)
                      (for ((i (send (list-ref map index) get-adj)))
                        (send (list-ref map (index-of temppos i)) set-highlight #f))
                      (set! index -1)
                      ))))
              
              (begin
                ;if clicked tile is valid, set all state varibles (clicked, highlight) 
                (when (and (number? (index-of temppos clickedpos)) 
                           (not (equal? 1 (send (list-ref map (index-of temppos clickedpos)) get-size))) 
                           (equal? playerturn (send (list-ref map (index-of temppos clickedpos)) get-player)))
                  (set! index -1)
                  (for ((i (length temppos)))
                    (send (list-ref map i) set-highlight #f)
                    (if (equal? (list-ref temppos i) clickedpos)
                        (begin
                          (send (list-ref map i) set-clicked (not (send (list-ref map i) get-clicked)))
                          (set! index i))
                        (send (list-ref map i) set-clicked #f))
                    
                    )
                  (when (and (not (= -1 index)) (send (list-ref map index) get-clicked))
                    (for ((j (send (list-ref map index) get-adj)))
                      (send (list-ref map (index-of temppos j)) set-highlight #t)))))))
        (refresh)))
    
    (define/override (on-char event) ;keyboard event handler
      (when (equal? (send event get-key-code) #\q) ;q deselects current selection
        (for ((i map))
          (send i set-highlight #f)
          (send i set-clicked #f)
          )
        (set! index -1)
        (refresh))
      (when (equal? (send event get-key-code) #\e) ;e ends turn
        (endturn))
      )
    
    (super-new)))



(define (init-map) ;generates territories in map
  (set! aliveplayers (list))
  (for ((i players))
    (set! aliveplayers (append aliveplayers (list #t))))
  (set! usedpos (list))
  (set! map (list))
  (set! temppos (list))
  
  (for ((i (inexact->exact (truncate (* mapwidth mapheight 0.86 ))))) ;map contains 14% white space
    (let loop()
      (let* (
             [x (random mapwidth)]
             [y (random mapheight)]
             [pos (list (list x y))])
        
        (if  [member (list-ref pos 0) usedpos]
             (loop)
             (begin
               (set! usedpos (append usedpos pos))
               (set! temppos (append temppos pos)))))))
  (let loop() ;initializes territories with random size and player at location generated above
    (unless (zero? (length usedpos))
      (let ([rnd (add1 (random 6))])
        (for ((i players))
          #:break (zero? (length usedpos))
          (define ter
            (new territory%
                 [player i]
                 [size rnd]
                 [adj (list)]
                 [x (list-ref (list-ref usedpos 0) 0)]
                 [y (list-ref (list-ref usedpos 0) 1)]
                 ))
          (set! usedpos (remove (list (send ter get-x) (send ter get-y)) usedpos))
          (set! map (append map (list ter)))))
      (loop))
    )
  (for ((j map)) ;set adjacents in each territory
    (when [member (list (send j get-x) (add1 (send j get-y))) temppos]
      (send j set-adj (append (send j get-adj) (list(list (send j get-x) (add1 (send j get-y)))))))
    
    (when [member (list (send j get-x) (sub1 (send j get-y))) temppos]
      (send j set-adj (append (send j get-adj) (list(list (send j get-x) (sub1 (send j get-y)))))))
    
    (when[member (list (add1 (send j get-x)) (send j get-y)) temppos]
      (send j set-adj (append (send j get-adj) (list(list (add1 (send j get-x)) (send j get-y))))))
    
    (when [member (list (sub1 (send j get-x)) (send j get-y)) temppos]
      (send j set-adj (append (send j get-adj) (list(list (sub1 (send j get-x)) (send j get-y)))))
      ))
  (for ((i map)) ;ensure that map has no islands (territory surrounded by whitespace)
    (when (zero? (length (send i get-adj)))
      (init-map)))
  ;  (for ((i (length temppos))) ;used for debugging
  ;    (display (list-ref temppos i))
  ;    (displayln i))
  (set! windowwidth (+ 50 (* 50 mapwidth)))
  (set! windowheight (+ 250 (* 50 mapheight)))
  (send maincanvas min-client-height (- windowheight 290)) ;adjust frame size to map size
  )







(define (endturn)
  (for((i map))
    (send i set-clicked #f)
    (send i set-highlight #f))
  (set! index -1)
  
  ;add reinforcements
  (let*  (
          [max 0]
          [count 0]
          [colourturn (list)]
          [totalsize 0])
    (for ((i map)) ;go through territories of current player and determine the largest chain of territories
      (when (equal? playerturn (send i get-player))
        (set! totalsize (+ totalsize (send i get-size)))
        (set! colourturn (append colourturn (list i)))
        (set! count (countchains i))
        (when (> count max)
          (set! max count))))
    
    (if (>= max (-  (* 10 (length colourturn)) totalsize)) ;distribute additional armies based on the length of the largest chain
        (begin
          (for ((i colourturn))
            (send i set-size 10)))
        (begin
          (let loop()
            (set! count (random (length colourturn)))
            (unless (>= (send (list-ref colourturn count) get-size) 10)
              (send (list-ref colourturn count) set-size (add1 (send (list-ref colourturn count) get-size)))
              (set! max (sub1 max)))
            
            (unless (equal? max 0) (loop))))) 
    
    (set! attackroll (list))
    (set! defendroll (list))
    (set! attacker "")
    (set! defender "")
    (set! attacktotal "")
    (set! defendtotal "")
    
    (send maincanvas2 refresh)
    
    )
  
  (for((i map))
    (send i set-highlight #f))
  
  
  
  (let loop()
    
    (set! playerturn (modulo (add1 playerturn) players))
    (when (equal? (list-ref aliveplayers playerturn) #f)
      (loop)))
  
  
  (when (equal? (sub1 players) (length (filter false? aliveplayers))) ;check if there is a winner
    (set! winner (index-of aliveplayers #t))
    (send menu show #t))
  
  (send maincanvas refresh)
  (set! canclick #t))


(define (countchains ter) ;recursively go through adjacent territories to find chain length
  (if (send ter get-highlight) 0
      (begin
        (let ((count 1))
          (send ter set-highlight #t)
          (for ((i (send ter get-adj)))  
            (when (equal? playerturn (send (list-ref map (index-of temppos i)) get-player))
              (set! count (+ count (countchains (list-ref map (index-of temppos i)))))))
          count))))




(define frame
  (new frame%
       [label "Gamble"]
       [style (list 'no-resize-border)]       
       [min-width 350]))




(define maincanvas  (new my-canvas% ;canvas on which to display territories
                         [parent frame]
                         
                         [paint-callback
                          (lambda (canvas dc)
                            (for ((t map))
                              (send dc set-pen "black" 1 'solid)
                              (cond [(send t get-clicked)
                                     (send dc set-pen ";lkjsdf" 5 'solid)]
                                    [(send t get-highlight)
                                     (send dc set-pen "purple" 4 'solid)])
                              (send dc set-brush (list-ref colours (send t get-player)) 'solid)
                              (send dc draw-rectangle (+ 20(* 50 (send t get-x))) (+ y-offset (* 50 (send t get-y))) 45 45)
                              (send dc set-text-foreground "black")
                              (send dc draw-text (number->string (send t get-size)) (+ x-offset (* 50 (send t get-x))) (+ 40 (* 50 (send t get-y))))
                              (send dc draw-text (string-append "It is " (list-ref colours playerturn) "'s turn") 50 10))
                            (when (not (equal? winner -1))
                              (send dc draw-text (string-append (string-upcase (list-ref colours winner)) " WINS!") 220 10)))]))


(define maincanvas2  (new my-canvas% ;canvas for showing dice rolls and totals
                          [parent frame]
                          [paint-callback
                           (lambda (canvas dc)
                             (send dc set-text-foreground "black")
                             (send dc draw-text attacker x-offset 0)
                             (send dc draw-text defender x-offset 50)
                             (send dc draw-text "total" 280 0)
                             (send dc draw-text "total" 280 50)
                             
                             (for ((i (length attackroll)))
                               (send dc draw-text (number->string (list-ref attackroll i)) (+ x-offset (* 20 i)) 25))
                             (for ((i (length defendroll)))
                               (send dc draw-text (number->string (list-ref defendroll i)) (+ x-offset (* 20 i)) 75))
                             (send dc draw-text attacktotal 290 25)
                             (send dc draw-text defendtotal 290 75)
                             )]))

(new button%
     [label "End turn"]
     [parent frame]
     [callback 
      (lambda (button event)
        (endturn)
        )])







;various controls for the start menu

(define b (read-bitmap "logo.png"))
(define bi (read-bitmap "rogo.png"))


(define menu
  (new frame%
       [label "Menu"]
       [width 175]
       [height 100]
       [style (list 'no-resize-border)]))


(send menu set-icon b)
(send menu center)

(define chooseplayers
  (new choice%
       [label "Players:                  "]
       [parent menu]
       [choices (list "2" "3" "4" "5")]))

(define choosesize 
  (new choice%
       [label "Map Size: "]
       [parent menu]
       [choices (list "Small" "Medium" "Large")]))

(define startgame
  (new button%
       [label "Start!"]
       [parent menu]
       [callback 
        (lambda (button event)
          (set! winner -1)
          (set! canclick #t)
          (set! players (string->number (send chooseplayers get-string-selection)))
          
          (when (equal? "Small" (send choosesize get-string-selection))
            (set! mapwidth (+ (random 2) 4))
            (set! mapheight (+ (random 2) 4)))
          (when (equal? "Medium" (send choosesize get-string-selection))
            (set! mapwidth (+ (random 3) 6))
            (set! mapheight (+ (random 3) 6)))
          (when (equal? "Large" (send choosesize get-string-selection))
            (set! mapwidth (+ (random 4) 8))
            (set! mapheight (+ (random 4) 8)))
          
          (init-map)
          (send frame resize windowwidth windowheight)
          (send frame center)
          (send frame show #t)
          (send menu show #f))]))



(send frame set-icon bi)
(send menu show #t)