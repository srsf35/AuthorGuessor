#lang racket
(require racket/hash)
(define (inputReader file)
  (define filetxt (port->lines(open-input-file file)))
  (close-input-port (current-input-port))
  filetxt
  )

;Combine the input into one string.
(define (stringConcat textSofar textList)
  (if (empty? textList)
      textSofar
      (string-upcase (stringConcat (string-append (string-append textSofar (first textList)) " ") (rest textList)))
      )
  )

;Change anything that is not a letter, space, or ', and change it into a space.
(define (firstReplace letter)
  (if (not (or (or (char-alphabetic? letter) (char=? #\space letter)) (char=? #\' letter)))
      #\space
      letter
      )
  )

;Creates a list of words. Keeps duplicates.
(define (makeWordList text)
  (string-split (list->string (map firstReplace (string->list (stringConcat "" text)))))
  )

;Counts all occurences of a word. Removes duplicates.
(define (wordCount occurrences currList)
  (if (empty? currList) occurrences (wordCount (append occurrences (list (length (indexes-of currList (first currList))))) (remove* (list (first currList)) currList)))
  )

;Remove duplicates from the list
(define (filterDuplicates newList currList)
  (if (empty? currList) newList (filterDuplicates (append newList (list (first currList))) (remove* (list (first currList)) currList)))
  )

;Create the frequency distributions of each word.
(define (createFrequencies fileName)
  (define currentList (makeWordList fileName))
  (define wordDistr (wordCount '() currentList))
  (define total (foldl + 0 wordDistr))
  (map (lambda (number) (* -1 (log (/ number total) 10))) wordDistr)
  )

;Formats the file correctly for hashing.
(define (assoc file)
  (define (createAssoc currentAssoc listKey listValues)
  (if (empty? listKey) currentAssoc (createAssoc (append currentAssoc (list (list (first listKey) (first listValues)))) (rest listKey) (rest listValues)))
  )

  (createAssoc '() (filterDuplicates '() (makeWordList file)) (createFrequencies file))
  )

;(make-immutable-hash (assoc trainingTXTone)) 
(define (calculateSimilar profile1 profile2)
  (hash-intersect (make-immutable-hash (assoc profile1)) (make-immutable-hash (assoc profile2)) #:combine (lambda (x1 x2) (abs(- (first x1) (first x2)))))
  )

;Calculate the difference score between profiles.
(define (calcDiffscore profile1 profile2)
  (define similarValues (hash-values (calculateSimilar profile1 profile2)))
  (/ (foldl + 0 similarValues) (length similarValues))
  )

(define (main)
  (define Doyle (inputReader "Doyle.txt"))
  (define Lovecraft (inputReader "Lovecraft.txt"))
  (define mysteryOne (inputReader "mystery1.txt"))
  (define mysteryTwo (inputReader "mystery2.txt"))

  (define (analysis text)
    (if (> (calcDiffscore Doyle text) (calcDiffscore Lovecraft text)) "Lovecraft" "Doyle")
    )
  
  (displayln "Anlyzing Mystery Text 1. . .")
  (displayln (string-append "Text one is probably " (analysis mysteryOne)))
  (displayln "Anlyzing Mystery Text 2. . .")
  (displayln (string-append "Text two is probably " (analysis mysteryTwo)))
  )
(main)