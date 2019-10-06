;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hp-family-listing) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

;; hp-family-tree-starter.rkt

; In this problem set you will represent information about descendant family 
; trees from Harry Potter and design functions that operate on those trees.
; 
; To make your task much easier we suggest two things:
;   - you only need a DESCENDANT family tree
;   - read through this entire problem set carefully to see what information 
;     the functions below are going to need. Design your data definitions to
;     only represent that information.
;   - you can find all the information you need by looking at the individual 
;     character pages like the one we point you to for Arthur Weasley.
; 


; PROBLEM 1:
; 
; Design a data definition that represents a family tree from the Harry Potter 
; wiki, which contains all necessary information for the other problems.  You 
; will use this data definition throughout the rest of the homework.
; 

; PROBLEM 2: 
; 
; Define a constant named ARTHUR that represents the descendant family tree for 
; Arthur Weasley. You can find all the infomation you need by starting 
; at: http://harrypotter.wikia.com/wiki/Arthur_Weasley.
; 
; You must include all of Arthur's children and these grandchildren: Lily, 
; Victoire, Albus, James.
; 
; 
; Note that on the Potter wiki you will find a lot of information. But for some 
; people some of the information may be missing. Enter that information with a 
; special value of "" (the empty string) meaning it is not present. Don't forget
; this special value when writing your interp.
; 


(define-struct character (name patronus wand children))
;; Character is (make-character String String String ListOfCharacter
;; interp. Character is Name Patronus name wand matherial and list of children

;; ListOfCharacter is one of:
;; - empty
;; - (cons Character ListOfCharacter
;; interp. A list of Characters

;; Bill Weasley Children

(define VICTOIREW (make-character
                   "Victoire Weasley"
                   ""
                   ""
                   empty))
(define DOMINIQUEW (make-character
                    "Dominique Weasley"
                    ""
                    ""
                    empty))
(define LOUISW (make-character
                "Louis Weaslye"
                ""
                ""
                empty))
;; Percy Weasley children
(define MOLLYW2 (make-character
                 "Molly Weasley 2"
                 ""
                 ""
                 empty))
(define LUCYW (make-character
               "Lucy Weasley"
               ""
               ""
               empty))
;; George Weasley children
(define FREDW2 (make-character
                "Fred Weasley 2"
                ""
                ""
                empty))
(define ROXANNEW (make-character
                  "Roxanne Weasley"
                  ""
                  ""
                  empty))
;; Ron weasley children
(define ROSEGRANGERW (make-character
                      "Rose Granger-Weasley"
                      ""
                      ""
                      empty))
(define HUGOGRANGERW (make-character
                      "Hugo Granger-Weasley"
                      ""
                      ""
                      empty))
;; Ginevra children
(define JAMESPOTTER2 (make-character
                      "James Sirius Potter 2"
                      ""
                      ""
                      empty))
(define ALBUSPOTTER (make-character
                     "Albus Potter"
                     ""
                     ""
                     empty))
(define LILYPOTTER2 (make-character
                     "Lilly Potter 2"
                     ""
                     ""
                     empty))

;; Arthur Weasley Children
(define BILLW (make-character
               "Bill Weasley"
               "Non-corporeal"
               ""
               (list
                VICTOIREW
                DOMINIQUEW
                LOUISW)))
(define CHARLIEW (make-character
                  "Charlie Weasley"
                  "Non-corporeal"
                  "ash"
                  empty))                
(define PERCYW (make-character
                "Percy Weasley"
                "Non-corporeal"
                ""
                (list
                 MOLLYW2
                 LUCYW)))
(define FREDW (make-character
               "Fred Weasley"
               "Magpie"
               ""
               empty))
(define GEORGEW (make-character
                 "George Weasley"
                 "Magpie"
                 ""
                 (list
                  FREDW2
                  ROXANNEW)))
(define RONW (make-character
              "Ron Weasley"
              "Jack Russel Terrier"
              "Chestnut"
              (list
               ROSEGRANGERW
               HUGOGRANGERW)))
(define GINEVRAW (make-character
                  "Ginevra Weasley"
                  "Horse"
                  "Yew"
                  (list
                   JAMESPOTTER2
                   ALBUSPOTTER
                   LILYPOTTER2)))
                  
;; ROOT
(define ARTHURW (make-character
                 "Arthur Weasley"
                 "Weasel"
                 ""
                 (list
                  BILLW
                  CHARLIEW
                  PERCYW
                  FREDW
                  GEORGEW
                  RONW
                  GINEVRAW)))
                                
#;
(define (fn-for-character c)
  (... (character-name c)                    ; String
       (character-patronus c)                ; String
       (character-wand c)                    ; String
       (fn-for-loc (character-children c)))) 

#;
(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-character (first loc))
              (fn-for-loc (rest loc)))])) 


; PROBLEM 3:
; 
; Design a function that produces a pair list (i.e. list of two-element lists)
; of every person in the tree and his or her patronus. For example, assuming 
; that HARRY is a tree representing Harry Potter and that he has no children
; (even though we know he does) the result would be: (list (list "Harry" "Stag")).
; 
; You must use ARTHUR as one of your examples.
; 


;; Character -> ListOfPairsOfString
;; ListOfCharacter -> ListOfPairsOfString
;; produce list of pairs (Name Patronus) for all element of tree
(check-expect (name-patronus-list-for--character FREDW) (list (list "Fred Weasley" "Magpie")))
(check-expect (name-patronus-list-for--loc (list FREDW CHARLIEW)) (list (list "Fred Weasley" "Magpie") (list "Charlie Weasley" "Non-corporeal")))
(check-expect (name-patronus-list-for--character ARTHURW)
             (list
              (list "Arthur Weasley" "Weasel")
              (list "Bill Weasley" "Non-corporeal")
              (list "Victoire Weasley" "")
              (list "Dominique Weasley" "")
              (list "Louis Weaslye" "")
              (list "Charlie Weasley" "Non-corporeal")
              (list "Percy Weasley" "Non-corporeal")
              (list "Molly Weasley 2" "")
              (list  "Lucy Weasley" "")
              (list "Fred Weasley" "Magpie")
              (list "George Weasley" "Magpie")
              (list "Fred Weasley 2" "")
              (list "Roxanne Weasley" "")
              (list "Ron Weasley" "Jack Russel Terrier")
              (list "Rose Granger-Weasley" "")
              (list "Hugo Granger-Weasley" "")
              (list "Ginevra Weasley" "Horse")
              (list "James Sirius Potter 2" "")
              (list "Albus Potter" "")
              (list "Lilly Potter 2" "")))            
              
; (define (name-patronus-list-for--character c) (list (list "" ""))) ; stub
; (define (name-patronus-list-for--loc loc) (list (list "" ""))) ; stub
#;
(define (fn-for-character c)
  (... (character-name c)                    ; String
       (character-patronus c)                ; String
       (character-wand c)                    ; String
       (fn-for-loc (character-children c)))) 

#;
(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-character (first loc))
              (fn-for-loc (rest loc)))]))

(define (name-patronus-list-for--character c)
  (cons
   (list (character-name c)                   
         (character-patronus c))                                   
         (name-patronus-list-for--loc (character-children c)))) 

(define (name-patronus-list-for--loc loc)
  (cond [(empty? loc) empty]
        [else
         (append (name-patronus-list-for--character (first loc))
                 (name-patronus-list-for--loc (rest loc)))]))

; PROBLEM 4:
; 
; Design a function that produces the names of every person in a given tree 
; whose wands are made of a given material. 
; 
; You must use ARTHUR as one of your examples.
; 


;; Character -> ListOfStrings
;; ListOfCharacter -> ListOfStrings
;; produces list of character's name who had wands from consumed matherial
(check-expect (use-wand--caracater ARTHURW "Gold") empty)
(check-expect (use-wand--caracater ARTHURW "Yew") (list "Ginevra Weasley"))
(check-expect (use-wand--caracater ARTHURW "") (list
               "Arthur Weasley"
               "Bill Weasley" 
               "Victoire Weasley"
               "Dominique Weasley"
               "Louis Weaslye"
               "Percy Weasley" 
               "Molly Weasley 2"
               "Lucy Weasley" 
               "Fred Weasley"
               "George Weasley" 
               "Fred Weasley 2" 
               "Roxanne Weasley" 
               "Rose Granger-Weasley"
               "Hugo Granger-Weasley" 
               "James Sirius Potter 2"
               "Albus Potter" 
               "Lilly Potter 2"))            
              
;(define (use-wand--caracater c w) empty) ; stub
;(define (use-wand--caracater loc w) empty) ; stub
#;
(define (fn-for-character c)
  (... (character-name c)                    ; String
       (character-patronus c)                ; String
       (character-wand c)                    ; String
       (fn-for-loc (character-children c)))) 

#;
(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-character (first loc))
              (fn-for-loc (rest loc)))]))


(define (use-wand--caracater c w)
   (if (string=? (character-wand c) w)
       (cons
        (character-name c)
        (use-wand--loc (character-children c) w))
       (use-wand--loc (character-children c) w)))
                 

(define (use-wand--loc loc w)
  (cond [(empty? loc) empty]
        [else
         (if (not (empty? (use-wand--caracater (first loc) w)))
             (append (use-wand--caracater (first loc) w) 
              (use-wand--loc (rest loc) w))
             (use-wand--loc (rest loc) w))]))
