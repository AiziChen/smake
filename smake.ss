#!/usr/bin/scheme --script

(define *compiler-name* "cc")
(define *source-suffixs* '(".c" ".h"))

(define display-usage
  (lambda (app-name)
    (display "Usage:\n")
    (printf "  1. ~a s1.c s1.h s2.c s2.h ...~n" app-name)
    (printf "  2. ~a -d src1-dir src2-dir ...~n" app-name)))


(define str-suffix?
  (lambda (ts s)
    (let loop ([tsi (sub1 (string-length ts))]
               [si (sub1 (string-length s))])
      (cond
       [(zero? (add1 si)) #t]
       [else
        (and (equal? (string-ref ts tsi) (string-ref s si))
             (loop (sub1 tsi) (sub1 si)))]))))


(define source-suffix-include?
  (lambda (f suffixs)
    (cond
     [(null? suffixs) #f]
     [else
      (or (and (> (string-length f) (string-length (car suffixs)))
               (str-suffix? f (car suffixs)))
          (source-suffix-include? f (cdr suffixs)))])))


(define files-str
  (lambda (dir-name fs)
    (cond
     [(null? fs) ""]
     [else
      (let ([current-file (car fs)])
        (if (source-suffix-include? current-file *source-suffixs*)
            (string-append " " dir-name "/" current-file
                           (files-str dir-name (cdr fs)))
            (files-str dir-name (cdr fs))))])))

(define folders-str
  (lambda (dirs)
    (cond
     [(null? dirs) ""]
     [else
      (string-append
       (let* ([dir-files (directory-list (car dirs))]
              [files (filter (lambda (f)
                               (not (file-directory? f)))
                             dir-files)])
         (files-str (car dirs) files))
       (folders-str (cdr dirs)))])))

(define compile-sources
  (lambda (fs)
    (system (string-append *compiler-name* (files-str "." fs)))))

(define compile-source-folders
  (lambda (dirs)
    (system (string-append *compiler-name* (folders-str dirs)))))


(let ([args (command-line)])
  (cond
   [(= 1 (length args))
    (display-usage (car args))]
   [(equal? "-d" (cadr args))
    (cond
     [(= 2 (length args))
      (display-usage (car args))]
     [else
      (compile-source-folders (list-tail args 2))])]
   [else
    (compile-sources (list-tail args 1))]))
