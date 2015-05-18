#lang racket

(require "uw-api.rkt")
(require "a1bonus.rkt")
(require "uw-tools.rkt")

(provide whats-cooking-improved)   ;;(whats-cooking-improved)
(provide whogotbreakfast)          ;;(whogotbreakfast)
(provide whogotlunch)              ;;(whogotlunch)
(provide whogotdinner)             ;;(whogotdinner)
(provide outletopen?)              ;;(outletopen?)
(provide waterloovendors)          ;;(waterloovendors)
#|
available from uw-tools
(course-desc subject catalog)
(online? subject catalog)
(course-sections term subject catalog)
(course-capacity term subject catalog)
(section-info term subject catalog section)
(next-holiday date)
(room-status building room day time)
(gimme-room-info building room)

|#







#|
;;(uw-api "/foodservices/outlets")

Meal checker: Which outlets on campus have breakfast/lunch/dinner?

available functions: 

(whogotbreakfast)
(whogotlunch)
(whogotdinner)

|#

(define (extractor want lstoflst)
  (cond
    [(empty? lstoflst) '()]
    [(equal? want (first (first lstoflst))) (append (rest (first lstoflst)) (extractor want (rest lstoflst)))]
    [else (extractor want (rest lstoflst))]))

(define (2append lst1 lst2 function)
  (cond
    [(and (empty? lst1) (empty? lst2)) '()]
    [else (cons (function (first lst1) (first lst2))
                (2append (rest lst1) (rest lst2) function))]))

(define (3append lst1 lst2 lst3 function)
  (cond
    [(and (empty? lst1) (empty? lst2) (empty? lst3)) '()]
    [else (cons (function (first lst1) (first lst2) (first lst3))
                (3append (rest lst1) (rest lst2) (rest lst3) function))]))


;;extractor: '((A B) (C D) (E F)) -> Any
;;ex. (extractor "A" '(("A" "B") ("C" "D") ("E" "F"))) => '("B")
;;    (extractor "A" '(("A" "B") ("A" "D") ("A" "F"))) => '("B" "D" "F")


(define (gimmefood want lstoflst)
  (cond
    [(empty? lstoflst) '()]
    [else (cons (extractor want (first lstoflst))
                (gimmefood want (rest lstoflst)))]))

(define (lstofoutletinfo trigger)
  (gimmefood trigger (uw-api "/foodservices/outlets")))

(define lstofoutlet (lstofoutletinfo "outlet_name"))
(define lstofbreakfast (lstofoutletinfo "has_breakfast"))
(define lstoflunch (lstofoutletinfo "has_lunch"))
(define lstofdinner (lstofoutletinfo "has_dinner"))


(define (filter lofl)
  (cond
    [(empty? lofl) empty]
    [(= (second (first lofl)) 1) (cons (first (first lofl)) (filter (rest lofl)))]
    [else (filter (rest lofl))]))

(define (whogotbreakfast)
  (filter (2append lstofoutlet lstofbreakfast append)))
(define (whogotlunch)
  (filter (2append lstofoutlet lstoflunch append)))
(define (whogotdinner)
  (filter (2append lstofoutlet lstofdinner append)))



;;========================================================================================================================

#|
;; (uw-api "/foodservices/locations")

restaurant open check: Which outlets on campus are open right now?

available functions: 

(outletopen?)

|#

(define (lstoflocinfo trigger)
  (gimmefood trigger (uw-api "/foodservices/locations")))

(define lstofoutletname (lstoflocinfo "outlet_name"))
(define lstofoutletopen (lstoflocinfo "is_open_now"))
(define lstofhours (lstoflocinfo "opening_hours"))

(define (booltostring boolean)
  (if (equal? boolean #f) "no" "yes"))

(define (open? lstoflst)
  (cond
    [(empty? lstoflst) '()]
    [(string=? "no" (booltostring (first (rest (first lstoflst))))) (open? (rest lstoflst))] 
    [else (cons (first (first lstoflst))
                (open? (rest lstoflst)))]))

(define (outletopen?)
  (open? (2append lstofoutletname lstofoutletopen append)))



;;========================================================================================================================

#|
;; (uw-api "/foodservices/watcard")

watcard check: all places that accepts watcards and their info.
available functions: 

(waterloovendors)

|#

(define allwaterloovendorinfo (uw-api "/foodservices/watcard")) ;;info of all vendors that accepts watcard



(define (watvendorsextractor want lstoflstoflst)
  (cond
    [(empty? lstoflstoflst) empty]
    [else
     (cons (extractor want (first lstoflstoflst))
           (watvendorsextractor want (rest lstoflstoflst)))]))


(define (lstofwat want)
  (watvendorsextractor want allwaterloovendorinfo))

(define lstofwatcardvendor (lstofwat "vendor_name"))
(define lstofwatcardaddress (lstofwat "address"))
(define lstofwatcardphone (lstofwat "phone_number"))

(define (waterloovendors)
  (3append lstofwatcardvendor lstofwatcardaddress lstofwatcardphone append))
;;========================================================================================================================


#|

whats-cooking improved

|#
(define (printer lst)
  (cond
    [(empty? lst) (printf "")]
    [else
     (printf (string-append (first lst) "\n"))
     (printer (rest lst))]))

(define (vendors)
  (printer lstofout))

(define (whats-cooking-improved)
  (printf "Choose between:\n")
  (vendors)
  (printf "to see the menu, put quotations around the name\n")
  (printf "Note: not all vendors have an online menu, only those who do are displayed\n")
  (define selection (read))
  (whats-cooking? selection))