;; relational sorting in miniKanren
;;
;; code from collaborative mini hacking session on Google Hangouts (miniKanren Uncourse #13)
;;
;; https://www.youtube.com/watch?v=gti6ZCivS30

(load "mk.scm")
(load "test-check.scm")

;; Peano numerals
#|
'z ; 0
(s z) ; 1
(s (s z)) ; 2
|#

; n1 n2
(define peano
  (lambda (n)
    (conde
      ((== 'z n))
      ((fresh (n-1)
         (== `(s ,n-1) n)
         (peano n-1))))))

(define <=o
  (lambda (n1 n2)
    (fresh (m)
      (addo n1 m n2))))

(define addo
  (lambda (n1 n2 out)
    (conde
      ((== 'z n1)
       (== n2 out))
      ((fresh (n1-1 res)
         (== `(s ,n1-1) n1)
         (== `(s ,res) out)
         (addo n1-1 n2 res))))))


(test "<=o-1"
  (run* (q) (<=o 'z 'z))
  '(_.0))

(test "<=o-2"
  (run* (q) (<=o '(s z) 'z))
  '())

(test "<=o-3"
  (run* (q) (<=o 'z '(s z)))
  '(_.0))

(test "<=o-4"
  (run* (q) (<=o '(s z) '(s z)))
  '(_.0))

(test "<=o-5"
  (run* (q) (<=o '(s (s z)) '(s z)))
  '())

(test "<=o-6"
  (run* (q) (<=o `(s (s . ,q)) '(s z)))
  '())

(test "<=o-7"
  (run* (q) (<=o '(s (s z)) `(s . ,q)))
  '(((s _.0))))

(test "<=o-8"
  (run 10 (q) (<=o q q))
  '(z
    (s z)
    (s (s z))
    (s (s (s z)))
    (s (s (s (s z))))
    (s (s (s (s (s z)))))
    (s (s (s (s (s (s z))))))
    (s (s (s (s (s (s (s z)))))))
    (s (s (s (s (s (s (s (s z))))))))
    (s (s (s (s (s (s (s (s (s z)))))))))))


(define nehal-sorto
  (lambda (in out)
    (conde
      ((== '() in) (== in out))
      ((fresh (a d res)
         (== `(,a . ,d) in)
         (nehal-sorto d res)
         (inserto a res out))))))


;; original inserto
(define inserto
  (lambda (x ls out)
    (conde
      ((== '() ls)
       (== `(,x) out))
      ((fresh (a d)
         (== `(,a . ,d) ls)
         (conde
           ((== `(,x . ,ls) out)
            (<=o x a))
           ((=/= x a)
            (fresh (res)
              (== `(,a . ,res) out)
              (<=o a x)
              (inserto x d res)))))))))

(test "nehal-sorto-orginal-inserto-1"
  (run* (q) (inserto '(s (s z)) '(z (s z) (s (s (s z)))) q))
  '((z (s z) (s (s z)) (s (s (s z))))))

(test "nehal-sorto-orginal-inserto-2"
  (run* (q) (nehal-sorto '((s z) (s (s (s z))) z) q))
  '((z (s z) (s (s (s z))))))

;; alas, run 7 diverges
;;
;; run* fails finitely if the 'inserto' call comes before the
;; 'nehal-sorto' call in 'nehal-sorto'.  However,
;; 'nehal-sorto-orginal-inserto-2' then diverges.
(test "nehal-sorto-orginal-inserto-3"
  (run 6 (q) (nehal-sorto q '(z (s z) (s (s (s z))))))
  '(((s z) z (s (s (s z))))
    ((s (s (s z))) z (s z))
    (z (s z) (s (s (s z))))
    ((s z) (s (s (s z))) z)
    ((s (s (s z))) (s z) z)
    (z (s (s (s z))) (s z))))


;; Orchid's inserto

(define sorted-appendo
  (lambda (l1 l2 out)
    (conde
      ((== '() l1)
       (== l2 out))
      ((fresh (a)
         (== `(,a) l1)
         (conde
           ((== '() l2)
            (== `(,a) out))
           ((fresh (a-l2 d-l2)
              (== `(,a-l2 . ,d-l2) l2)
              (== `(,a . ,l2) out)
              (<=o a a-l2))))))
      ((fresh (a d aa dd res)
         (== `(,a . ,d) l1)
         (== `(,aa . ,dd) d)
         (== `(,a . ,res) out)
         (sorted-appendo d l2 res))))))

(define (inserto elt list res)
  (fresh (left right)
    (sorted-appendo left right list)
    (sorted-append3o left `(,elt) right res)))

(define (sorted-append3o a b c r)
  (fresh (m)
    (sorted-appendo a b m)
    (sorted-appendo m c r)))

(test "nehal-sorto-orchid-inserto-1"
  (run* (q) (inserto '(s (s z)) '(z (s z) (s (s (s z)))) q))
  '((z (s z) (s (s z)) (s (s (s z))))))

(test "nehal-sorto-orchid-inserto-2"
  (run* (q) (nehal-sorto '((s z) (s (s (s z))) z) q))
  '((z (s z) (s (s (s z))))))

;; weird, run 4 diverges
;;
;; there should be 6 answers, though (see nehal-sorto-original-inserto-3)
(test "nehal-sorto-orchid-inserto-3"
  (run 3 (q) (nehal-sorto q '(z (s z) (s (s (s z))))))
  '((z (s z) (s (s (s z))))
    ((s z) z (s (s (s z))))
    ((s (s (s z))) z (s z))))

(test "sorted-appendo-1"
  (run* (q) (sorted-appendo '(z (s (s (s z)))) '((s (s (s (s z))))) q))
  '((z (s (s (s z))) (s (s (s (s z)))))))

(test "sorted-appendo-2"
  (run* (q) (sorted-appendo '(z (s (s (s (s z))))) '((s (s (s z)))) q))
  '())

(test "orchid-inserto-1"
  (run* (q) (inserto '(s (s (s z))) '(z (s (s z)) (s (s (s (s z))))) q))
  '((z (s (s z)) (s (s (s z))) (s (s (s (s z)))))))



;;; merge sort
;;;
;;; code and tests adapted from 'http://gauss.ececs.uc.edu/Courses/C511/html/Scheme/mergesrt.html'

(define even-numberso
  (lambda (l out)
    (conde
      ((== '() l)
       (== '() out))
      ((fresh (a)
         (== `(,a) l)
         (== '() out)))
      ((fresh (a ad dd res)
         (== `(,a ,ad . ,dd) l)
         (== `(,ad . ,res) out)
         (even-numberso dd res))))))

(define odd-numberso
  (lambda (l out)
    (conde
      ((== '() l)
       (== '() out))
      ((fresh (a d)
         (== `(,a . ,d) l)
         (conde
           ((== '() d)
            (== `(,a) out))
           ((fresh (ad dd res)
              (== `(,ad . ,dd) d)
              (== `(,a . ,res) out)
              (odd-numberso dd res)))))))))

(define merge-listso
  (lambda (l1 l2 out)
    (conde
      ((== '() l1)
       (== l2 out))
      ((fresh (a d)
         (== `(,a . ,d) l1)
         (== '() l2)
         (== l1 out)))
      ((fresh (a-l1 d-l1 a-l2 d-l2 res)
         (== `(,a-l1 . ,d-l1) l1)
         (== `(,a-l2 . ,d-l2) l2)
         (conde
           ((== `(,a-l1 . ,res) out)
            (<=o a-l1 a-l2)
            (merge-listso d-l1 l2 res))
           ((=/= a-l1 a-l2)
            (== `(,a-l2 . ,res) out)
            (<=o a-l2 a-l1)
            (merge-listso d-l2 l1 res))))))))

(define merge-sorto
  (lambda (l out)
    (conde
      ((== '() l)
       (== l out))
      ((fresh (a d)
         (== `(,a . ,d) l)
         (conde
           ((== '() d)
            (== l out))
           ((fresh (ad dd e o res-e res-o)
              (== `(,ad . ,dd) d)
              (odd-numberso l e)
              (even-numberso l o)
              (merge-sorto e res-e)
              (merge-sorto o res-o)
              (merge-listso res-e res-o out)))))))))

(test "even-numberso-1"
  (run* (q) (even-numberso '(2 7 6 5 4 5 6 7 4) q))
  '((7 5 5 7)))

(test "odd-numberso-1"
  (run* (q) (odd-numberso '(2 7 6 5 4 5 6 7 4) q))
  '((2 6 4 6 4)))

(test "merge-listso-0"
  (run* (q) (merge-listso '() '() q))
  '(()))

(test "merge-listso-1"
  (run* (q) (merge-listso '(z) '() q))
  '((z)))

(test "merge-listso-2"
  (run* (q) (merge-listso '() '(z) q))
  '((z)))

(test "merge-listso-3"
  (run* (q) (merge-listso '(z) '(z) q))
  '((z z)))

(test "merge-listso-4"
  (run* (q) (merge-listso '((s z)) '(z) q))
  '((z (s z))))

(test "merge-listso-5"
  (run* (q) (merge-listso '((s z)) '(z (s (s z))) q))
  '((z (s z) (s (s z)))))

(test "merge-listso-6"
  (run* (q) (merge-listso '(z (s z) (s (s z))) '((s (s (s z))) (s (s (s (s z)))) (s (s (s (s (s z)))))) q))
  '((z (s z) (s (s z)) (s (s (s z))) (s (s (s (s z)))) (s (s (s (s (s z))))))))

(test "merge-sorto-1"
  (run* (q) (merge-sorto '(z (s z) (s (s z)) (s (s (s z))) (s (s (s (s z)))) (s (s (s (s (s z)))))) q))
  '((z (s z) (s (s z)) (s (s (s z))) (s (s (s (s z)))) (s (s (s (s (s z))))))))

(test "merge-sorto-2"
  (run* (q) (merge-sorto '((s z) (s (s (s z))) (s (s (s (s (s z))))) z (s (s (s (s z)))) (s (s z))) q))
  '((z (s z) (s (s z)) (s (s (s z))) (s (s (s (s z)))) (s (s (s (s (s z))))))))

;;; run 7 diverges
(test "merge-sorto-3"
  (run 6 (q) (merge-sorto q '(z (s z) (s (s z)))))
  '((z (s (s z)) (s z))
    (z (s z) (s (s z)))
    ((s z) (s (s z)) z)
    ((s (s z)) (s z) z)
    ((s z) z (s (s z)))
    ((s (s z)) z (s z))))

;;; run 7 diverges
(test "merge-sorto-4"
  (run 6 (n m p) (merge-sorto `(,n ,m ,p) '(z (s z) (s (s z)))))
  '((z (s (s z)) (s z))
    (z (s z) (s (s z)))
    ((s z) (s (s z)) z)
    ((s (s z)) (s z) z)
    ((s z) z (s (s z)))
    ((s (s z)) z (s z))))
