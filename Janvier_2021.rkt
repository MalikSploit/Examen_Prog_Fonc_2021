#lang scheme

;;Exercice 1 :
(define (barre n p)
  (cond
    [(< n p) n]
    [else (+ n (barre (- n p) p))]))
;;(barre 5 3)

;;Exercice 2 :
;;Question 1 :
(define SR (lambda (L B C)
             (if (null? L)
                 B
                 (C (car L) (SR (cdr L) B C)))))
(define f
  (lambda (P L)
    (SR L '() (lambda (t qcdr)
                (if (P t)
                    (cons t qcdr)
                    qcdr)))))

(define (SI A L R)
  (cond
    [(null? L) A]
    [else (SI (R A (car L)) (cdr L) R)]))
(define (f2 P L)
  (reverse (SI '() L (lambda (res t)
                      (if (P t)
                          (cons t res)
                          res)))))
;;Question 2 :
;;(f2 even? (list 1 2 3 5 8 13 21))

;;Exercice 3 :
(define (min L)
  (cond
    [(null? (cdr L)) (car L)]
    [(> (car L) (min (cdr L))) (car L)]
    [else (min (cdr L))]))
(define (minL L)
  (cond
    [(null? L) '()]
    [(cons (min (car L)) (minL (cdr L)))]))

;;(minL '((1) (2 3)))
;;(minL '((4 1) (6 4 5)))

;;Exercice 4 :
(define (gen-lists n)
  (if (= n 0)
      '((0))
    (let ((lists (gen-lists (- n 1))))
      (append (map (lambda (lst) (cons 0 lst)) lists)
              (map (lambda (lst) (cons 1 lst)) 
                   (filter (lambda (lst) (not (eq? (car lst) 1))) lists))))))
(define (supprimerDernier L)
  (cond
    [(null? (cdr L)) '()]
    [else (cons (car L) (supprimerDernier (cdr L)))]))
(define (supprimerDernier_Sous_Liste L)
  (cond
    [(null? L) '()]
    [else (cons (supprimerDernier(car L)) (supprimerDernier_Sous_Liste (cdr L)))]))
(define (generer n)
  (supprimerDernier_Sous_Liste (gen-lists n)))
;;(generer 5)

;;Exercice 5 :
;;Question 1 :
(define (carre1? n i) ;;Exécuter une boucle de i = 1 à floor(sqrt(n)) puis de vérifier si le nombre au carre fait n.
  (cond
    [(or (and (= (modulo n i) 0) (= (/ n i) i)) (= n 0)) #t]
    [(<= (* i i) n) (carre1? n (+ 1 i))]
    [else #f]))

(define (carre? n)
  (carre1? n 1))
;;(carre? 3)

;;Question 2 :
(define (segment n p)
  (cond
    [(> n p) '()]
    [else (cons n (segment (+ 1 n) p))]))
(segment 2 5)

;;Question 3 :
(define (scn n p)
  (let loop ((p p) (n n))
    (cond
      ((= n 0) #t)
      ((< n 0) #f)
      ((= p 0) #f)
      (else (let ((x (floor (sqrt n))))
               (or (loop (- p 1) n)
                   (loop (- p 1) (- n (* x x)))))))))

(scn 4 1)
(scn 4 42)
(scn 6 1)
(scn 6 2)
(scn 6 3)
