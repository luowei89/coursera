;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor-project-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;; editor-project-starter.rkt
;;
;; In this project you will design a simple one line text editor.  
;;
;; The screen looks like:
;; 
;;     abc|def
;;
;; where | is the cursor.
;;
;; Typing a character inserts that character before the cursor.
;; The backspace key deletes the character before the cursor.
;; The left and right arrow keys move the cursor left and right.



;; =================================================================================
;; Constants:

(define WIDTH  200)
(define HEIGHT  20)

(define TEXT-SIZE  18)
(define TEXT-COLOR "BLACK")

(define CURSOR (rectangle 1 20 "solid" "red"))

(define MTS (empty-scene WIDTH HEIGHT))



;; =================================================================================
;; Data Definitions:

(define-struct editor (txt cp state))
;; Editor is (make-editor String Natural Boolean)
;; interp. the current text (txt) and cursor position (cp) using a 0-based index
;;         state represents the world is running (false) or stopped (true)

(define ED1 (make-editor ""       0 false)) ; empty
(define ED2 (make-editor "abcdef" 0 false)) ; cursor at beginning as in |abcdef
(define ED3 (make-editor "abcdef" 3 false)) ; cursor in middle of text as in abc|def
(define ED4 (make-editor "abcdef" 6 false)) ; cursor at end as in abcdef|

#;
(define (fn-for-editor e)
  (... (editor-txt e)      ;String
       (editor-cp e)       ;Natural
       （editor-state e))  ;Boolean
;; Template Rules Used:
;; - compound: 3 fields

;; =================================================================================
;; Functions:

;; Editor -> Editor
;; start the world with an initial state e, for example (main (make-editor "" 0 false))
(define (main e)
  (big-bang e                                    ; Editor
            (to-draw    render)                  ; Editor -> Image
            (on-key     handle-key)              ; Editor KeyEvent -> Editor
            (stop-when  done?)))                 ; Editor -> Boolean

;; Editor -> Image
;; place text with cursor at left, middle edge of MTS
(check-expect (render (make-editor "abcdef" 3 false))
              (overlay/align "left"
                             "middle"
                             (beside (text "abc" TEXT-SIZE TEXT-COLOR)
                                     CURSOR
                                     (text "def" TEXT-SIZE TEXT-COLOR))
                             MTS))

;(define (render e) MTS) ;stub

; Template from Editor

(define (render e)
  (overlay/align "left"
                 "middle"
                 (txt-cp-img e)
                 MTS))

;; Editor KeyEvent -> Editor
;; call appropriate function for each keyboard command

;(define (handle-key e key) e) ;stub

(define (handle-key e key)
  (cond [(key=? key "left")        (cp-left e)]
        [(key=? key "right")       (cp-right e)]
        [(key=? key "\b")          (backspace e)]
        [(key=? key "escape")      (stop-world e)]
        [(= (string-length key) 1) (insert e key)]
        [else e])) ;ignore other cases

;; Editor -> Image
;; produce an image of text with cursor at the right position
(check-expect (txt-cp-img (make-editor "abcdef" 3 false))
              (beside (text "abc" TEXT-SIZE TEXT-COLOR)
                      CURSOR
                      (text "def" TEXT-SIZE TEXT-COLOR)))

;(define (txt-cp-img e) empty-image) ;stub

; Template from Editor

(define (txt-cp-img e)
  (beside (txt-img (txt-before-cp e))
          CURSOR
          (txt-img (txt-after-cp e))))

;; String -> Image
;; produce the text image base on text provided
(check-expect (txt-img "abc") (text "abc" TEXT-SIZE TEXT-COLOR))

;(define (txt-img txt) empty-image) ;stub

;(define (txt-img txt) ;template  
;  (... txt))

(define (txt-img txt)
  (text txt TEXT-SIZE TEXT-COLOR))

;; Editor -> String
;; produce the text before the cursor position
(check-expect (txt-before-cp (make-editor "" 0 false)) "")
(check-expect (txt-before-cp (make-editor "abc" 0 false)) "")
(check-expect (txt-before-cp (make-editor "abc" 1 false)) "a")
(check-expect (txt-before-cp (make-editor "abc" 3 false)) "abc")

;(define (txt-before-cp e) "") ;stub

; Template from Editor

(define (txt-before-cp e)
  (substring (editor-txt e) 0 (editor-cp e)))

;; Editor -> String
;; produce the text after the cursor position
(check-expect (txt-after-cp (make-editor "" 0 false)) "")
(check-expect (txt-after-cp (make-editor "abc" 3 false)) "")
(check-expect (txt-after-cp (make-editor "abc" 2 false)) "c")
(check-expect (txt-after-cp (make-editor "abc" 0 false)) "abc")

;(define (txt-after-cp e) "") ;stub

; Template from Editor
(define (txt-after-cp e)
  (substring (editor-txt e) (editor-cp e)))

;; Editor -> Editor
;; move the cursor left (unless already at left end of text).
(check-expect (cp-left (make-editor "abc" 0 false)) (make-editor "abc" 0 false))
(check-expect (cp-left (make-editor "abc" 1 false)) (make-editor "abc" 0 false))

;(define (cp-left e) e) ;stub

; Template from Editor

(define (cp-left e)
  (if (> (editor-cp e) 0)
      (make-editor (editor-txt e) (- (editor-cp e) 1) false)
      e))
  
;; Editor -> Editor
;; move the cursor right (unless already at right end of text).
(check-expect (cp-right (make-editor "abc" 3 false)) (make-editor "abc" 3 false))
(check-expect (cp-right (make-editor "abc" 1 false)) (make-editor "abc" 2 false))

;(define (cp-right e) e) ;stub

; Template from Editor

(define (cp-right e)
  (if (< (editor-cp e) (string-length (editor-txt e)))
      (make-editor (editor-txt e) (+ (editor-cp e) 1) false)
      e))

;; Editor -> Editor
;; delete the character before the cursor (if there is one).
(check-expect (backspace (make-editor "abc" 0 false)) (make-editor "abc" 0 false))
(check-expect (backspace (make-editor "abc" 1 false)) (make-editor "bc" 0 false))

;(define (backspace e) e) ;stub

; Template from Editor

(define (backspace e)
  (if (> (editor-cp e) 0)
      (cp-left (make-editor (backspace-txt e) (editor-cp e) false))
      e))

;; Editor -> String
;; produce the string after deleting the character before the cursor
(check-expect (backspace-txt (make-editor "abc" 0 false)) "abc")
(check-expect (backspace-txt (make-editor "abc" 1 false)) "bc")

;(define (backspace-txt e) "") ;stub

; Template from Editor

(define (backspace-txt e)
  (if (> (editor-cp e) 0)
      (string-append (substring (txt-before-cp e) 
                                0 
                                (- (string-length (txt-before-cp e)) 1))
                     (txt-after-cp e))
      (editor-txt e)))

;; Editor -> Editor
;; insert the input character at the cursor position
(check-expect (insert (make-editor "abc" 0 false) "a") (make-editor "aabc" 1 false))
(check-expect (insert (make-editor "abc" 3 false) " ") (make-editor "abc " 4 false))
(check-expect (insert (make-editor "abc" 3 false) "\r") (make-editor "abc" 3 false))

;(define (insert e key) e) ;stub

; Template from Editor

(define (insert e key)
  (cond [(string=? key "\r") e]       ;ignore the "Enter" key
        [(string=? key "\u007F") e]   ;ignore the "Delete" key
        [else (cp-right (make-editor (insert-txt e key) (editor-cp e) false))]))

;; Editor -> String
;; produce the string after insertion
(check-expect (insert-txt (make-editor "ab cd" 3 false) "A") "ab Acd")

;(define (insert-txt e key) "") ;stub

; Template form Editor

(define (insert-txt e key)
  (string-append (txt-before-cp e) key (txt-after-cp e)))

;; Editor -> Boolean
;; return the state of the world
(check-expect (done? (make-editor "" 0 true)) true)
(check-expect (done? (make-editor "" 0 false)) false)

;(define (done? e) false) ;stub

; Template from Editor

(define (done? e)
  (editor-state e))

;; Editor -> Editor
;; Stops the world
(check-expect (stop-world (make-editor "" 0 true)) (make-editor "" 0 true))
(check-expect (stop-world (make-editor "" 0 false)) (make-editor "" 0 true))

;(define (stop-world e) e) ;stub

; Template from Editor

 (define (stop-world e)
   (make-editor (editor-txt e)
                (editor-cp e)
                true))
