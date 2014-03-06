;;;; -*- Scheme48 -*-

(define *result* '())

(define-syntax push!
  (syntax-rules ()
    ((push! item seq)
     (begin (set! seq (cons item seq))
            seq))))

(define (chapter)
  (let ((ch (read-char)))
    (if (eof-object? ch)
	(error "CHAPTER: read <EOF>")
	(begin
	  (push! 'C *result*)
	  (header ch)))))

(define (header ch)
  (if (not (char=? ch #\#))
      (paragraph ch)
      (let loop ((str "")
		   (ch (read-char)))
	  (if (char=? ch #\newline)
	      (begin
		(push! (list 'H str) *result*)
		(paragraph (read-char)))
	      (loop (string-append str (string ch))
		    (read-char))))))

(define (paragraph ch)
  (let loop ((rv '())
	     (ch ch))
    (cond ((eof-object? ch)
	   ch)
	  ((char=? ch #\newline)
	   (begin
	     (if (not (null? rv))
		 (push! (cons 'P (reverse rv))  *result*))
	     (paragraph (read-char))))
	  ((char=? ch #\")
	   (loop (cons (sentence ch) rv)
		 (read-char)))
	  (else (loop (cons (sentence ch) rv)
		      (read-char))))))

(define (sentence ch)
    (let loop ((str (string ch))
	       (ch (read-char)))
      (cond ((eof-object? ch)
	     str)
	    ((string=? str "\"")
	     (quotation str ch))
	    ((char=? ch #\")
	     (quotation #f #\"))
	    ((or
	      (char=? ch #\.)
	      (char=? ch #\!)
	      (char=? ch #\?))
	     (list 'S (string-append str (string ch))))
	    (else
	     (loop (string-append str (string ch)) (read-char))))))

(define (quotation maybe-string ch)
  (let loop ((str (if maybe-string 
		      (string-append maybe-string
				     (string ch))
		      (string ch)))
	     (ch (read-char)))
    (cond ((eof-object? ch)
	   str)
	  ((char=? ch #\")
	   (list 'Q (string-append str (string ch))))
	  (else
	   (loop (string-append str (string ch)) (read-char))))))

(define (test-the-parser file)
  (with-input-from-file file
    (lambda ()
      (chapter)
      (p (reverse *result*))
      (set! *result* '()))))
