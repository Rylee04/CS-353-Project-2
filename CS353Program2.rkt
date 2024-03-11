; CS-353 Project 2 / Ryan Lee / 3-10-24 / Made in conjuncture with ChatGPT for the creation, explanation, and debugging of code
#lang racket
(require csv-reading)
(require data/maybe)

; Define the filename of the CSV file to be read
(define filename "Video Game Sales.csv")

; Create a CSV reader with options to strip leading and trailing whitespace
(define csv-reader (make-csv-reader-maker '((strip-leading-whitespace? . #t) (strip-trailing-whitespace? . #t))))

; Read the CSV file into a list of lists
(define csvlist (csv->list (csv-reader (open-input-file filename))))

; Filter function to search for games by name
(define (name-filter games name)
  (filter (lambda (n) (string-contains? (string-foldcase (caddr n)) (string-foldcase name))) games))

; Prompt the user to input a starting and ending year, returns a list of the date range
(define (get-date)
  (list (display-read "Enter starting year (min 1983): ")
        (display-read "Enter ending year (max 2018): ")))

; Filter function to filter games based on date range
(define (date-filter games date-range)
  (filter (lambda (n) (and (string>=? (car (cddddr n)) (first date-range)) (string<=? (car (cddddr n)) (second date-range)))) games))

; Filter function to search for games by publisher
(define (publisher-filter games publisher)
  (filter (lambda (n) (string-ci=? (caddr (cddddr n)) publisher)) games))

; Maps region names to their corresponding column index
(define (get-region region)
  (cond [(equal? region "North America") 7]
        [(equal? region "Europe") 8]
        [(equal? region "Japan") 9]
        [(equal? region "Rest of World") 10]))

; Returns a list containing the region name and its corresponding column index
(define (region-search region)
  (list region (get-region region)))

; Filter function to search for games by region
(define (region-filter games region)
  (filter (lambda (n) (string-ci=? (caddr n) region)) games))

; Filter function to search for games by genre
(define (genre-filter games genre)
  (filter (lambda (n) (string-ci=? (cadr (cddddr n)) genre)) games))

; Sorts the list of games based on rating in descending order
(define (rating-sort games)
  (sort games (lambda (a b) (string>? (list-ref a 12) (list-ref b 12)))))

; Prompts the user to input and returns the entered value
(define (display-read prompt)
  (display prompt)
  (read-line (current-input-port) 'any))

; Displays a formatted entry of a game with its details
(define (display-entry index entry)
  (fprintf (current-output-port) "~a. ~a: ~a million globally~n North America: ~a million~n Europe: ~a million~n Japan: ~a million~n Other: ~a million~n~n"
           (add1 index)
           (third entry)
           (second (reverse entry))
           (eighth entry)
           (ninth entry)
           (tenth entry)
           (list-ref entry 10)))

; Displays filtered game entries based on user-selected criteria
(define (display-filtered games [region '()])
  (display "Order by sales or rating?:\n 1. Sales\n 2. Rating\n")
  (let ([sort-by (display-read "")])
    (cond
      [(string-ci=? sort-by "1") (display-sales games region)]
      [(string-ci=? sort-by "2") (display-rating games region)]
      [else (displayln "Invalid input") (display-filtered games)])))

; Displays game entries sorted by sales
(define (display-sales games region)
  (for ([entry games])
    (display-entry (index-of games entry) entry)))

; Displays game entries sorted by rating
(define (display-rating games region)
  (for ([entry (rating-sort games)])
    (fprintf (current-output-port) "~a. ~a: Rating: ~a/100~n" 
             (add1 (index-of (rating-sort games) entry))
             (third entry)
             (last entry))
    (fprintf (current-output-port) "Sales in ")
    (if (null? region)
        (fprintf (current-output-port) "Global: ~a million~n Platform: ~a~n Year: ~a~n Genre: ~a~n~n" (second (reverse entry)) (fourth entry) (fifth entry) (sixth entry))
        (fprintf (current-output-port) "~a: ~a million~n Platform: ~a~n Year: ~a~n Genre: ~a~n~n" (first region) (list-ref entry (second region)) (fourth entry) (fifth entry) (sixth entry)))))

; Prompts the user to select search criteria and returns a list of criteria
(define (menu)
  (let loop ([criteria '()])
    (if (equal? (length criteria) 6) criteria 
        (let ([search (display-read "Select the criteria to be searched:\n 1. Name\n 2. Date Range\n 3. Publisher\n 4. Region\n 5. Genre\n Q. Quit\n")])
          (cond
            [(string-ci=? search "1") (loop (append criteria (list "Name" (display-read "Name of the game:\n"))))]
            [(string-ci=? search "2") (loop (append criteria (list "Date" (get-date))))]
            [(string-ci=? search "3") (loop (append criteria (list "Publisher" (display-read "Name of the Publisher:\n"))))]
            [(string-ci=? search "4") (loop (append criteria (list "Region" (region-search (display-read "Region:\n")))))]
            [(string-ci=? search "5") (loop (append criteria (list "Genre" (display-read "Genre:\n"))))]
            [(string-ci=? search "Q") criteria]
            [else (displayln "Invalid input") (loop (criteria))])))))

; Filters games based on selected criteria
(define (criteria-filter search-list games)
  (if (null? search-list) nothing
      (let loop ([search-by search-list]
                 [games-filter games]
                 [region '()])
        (cond
          [(null? search-by) (display-filtered games-filter region)]
          [(equal? (first search-by) "Name")
           (loop (rest (cdr search-by)) (name-filter games-filter (second search-by)) region)]
          [(equal? (first search-by) "Date")
           (loop (rest (cdr search-by)) (date-filter games-filter (second search-by)) region)]
          [(equal? (first search-by) "Publisher")
           (loop (rest (cdr search-by)) (publisher-filter games-filter (second search-by)) region)]
          [(equal? (first search-by) "Region")
           (loop (rest (cdr search-by)) games-filter (second search-by))]
          [(equal? (first search-by) "Genre")
           (loop (rest (cdr search-by)) (genre-filter games-filter (second search-by)) region)]
          ))))

; Main function to execute the program
(let loop ([results (criteria-filter (menu) csvlist)])
  (if (equal? results nothing)
      (display "Exiting")
      (loop (criteria-filter (menu) csvlist))))