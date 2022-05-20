#lang racket
(require net/url)
(require data-science-master)
(require plot)
(require math)
;(require json)
;(require pict)
;(require (only-in srfi/19 string->date))
;(require (only-in srfi/19 date->string))


;dataset

(define data (read-csv "./Output.csv" #:->number? #f #:header? #t))

(define sample_data
  (let ([tmp (map (λ (x) (list (list-ref x 9))) data)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))
(define f_sample_data (flatten sample_data))
;(take f_sample_data 5)
(define all_tweet-text (apply string-append f_sample_data))
;(display "all_tweet-text")
;all_tweet-text
;(for-each (lambda (x) (newline) (display x))
 ;         sample_data)
(define p_tweet-text (string-normalize-spaces
                      (remove-punctuation
                       (string-downcase all_tweet-text) #:websafe? #t)))
;(display "p_tweet-text")
;p_tweet-text

(define words (document->tokens p_tweet-text #:sort? #t))
;(display "words")
;words

(define sentiment (list->sentiment words #:lexicon 'nrc))
;(display "sentiment")
;sentiment
 
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))

(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))

(define sentiment_bing (list->sentiment words #:lexicon 'bing))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ sentiment_bing 'sentiment) ($ sentiment_bing 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
	 #:color "MediumOrchid"
	 #:line-color "MediumOrchid")
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))