#lang racket
(require "uw-api.rkt")

(provide whats-cooking?)
(provide lstofout)

(define gimmemenu (uw-api "/foodservices/menu"))
#|

  Author: Siyuan Wang
  Last Updated: January 25, 2014
  

  This program will to allow students to have
  access to University of Waterloo's foodservices 
  menu without having go to the website (https://uwaterloo.ca/food-services/menu)


|#



#|

  AL is one of:
  * empty
  * (listof <key data type> <val data type>))

|#


#|

  Outlet is one of:
  * empty
  * (listof AL)

|#


#|

  OutletList is one of:
  * empty
  * (listof Outlet)
  * (cons AL OutletList)

|#


#|

  MenuList is one of:
  * empty
  * (listof (listof AL))

|#




#|

  extractor: (listof AL) String 
          -> (union Num String Bool (listof String) APIResult)

  Extracts the APIResult associated with the trigger string from the given loal
  PRE: you can connect to UW (have online access)
  POST: The items associated with the trigger string might be anything within API
      * This is an abstraction function

|#


(define (extractor loal trigger)
  (cond
    [(empty? loal) empty]
    [(string=? (first (first loal)) trigger)
     (second (first loal))]
    [else (extractor (rest loal) trigger)]))




#|

  outlet-scanner: String OutletList -> Outlet
  Produce the Outlet corresponding to outletname
  by scanning every outlet name in outletlst

  PRE: true
  POST: Return a single Outlet

|#


(define (outlet-scanner outletname outletlst)
  (cond
    [(empty? outletlst) empty]
    [(string=? outletname (extractor (first outletlst) "outlet_name"))
     (cons (extractor (first outletlst) "menu")
           (outlet-scanner outletname (rest outletlst)))]
    [else (outlet-scanner outletname (rest outletlst))]))




#|

  daily-meals: MenuList -> (listof (listof AL))
  Produce [Monday -> Friday]'s lunch and dinner menu from
  the info in menulst
  
  PRE: true
  POST: Returns the lunch and dinner menu for an entire week

|#


(define (daily-meals menulst)
  (cond
    [(empty? menulst) empty]
    [else (cons (extractor (first menulst) "meals")
                (daily-meals (rest menulst)))]))




#|

  all-lunch: MenuList -> (listof (listof AL))
  Extracts everything related to "lunch" in menulst
  [Monday -> Friday]

  PRE: This function is used in conjunction with "daily-meals" function
  POST: Returns this week's lunch as a list of list of AL

|#


(define (all-lunch menulst)
  (cond
    [(empty? menulst) empty]
    [else (cons (extractor (first menulst) "lunch")
                (all-lunch (rest menulst)))]))




#|

  all-dinner: MenuList -> (listof (listof AL))
  Extracts everything related to "dinner" in menulst
  [Monday -> Friday]

  PRE: This function is used in conjunction with "daily-meals" function
  POST: Returns this week's dinner as a list of list of AL

|#


(define (all-dinner menulst)
  (cond
    [(empty? menulst) empty]
    [else (cons (extractor (first menulst) "dinner")
                (all-dinner (rest menulst)))]))




#|

  lunch-or-din-extractor: (list of (listof AL)) -> (listof String)
  Produces a list containing all lunch or all dinner,
  assuming the given list contains those info

  PRE: true
  POST: Returns a list of String in the format
  '("[lunch1/dinner1]"  "[lunch2/dinner2]" ...)

|#


(define (lunch-or-din-extractor lunch-or-din-lst)
  (cond
    [(empty? lunch-or-din-lst) empty]
    [else (cons (extractor (first lunch-or-din-lst) "product_name")
                (lunch-or-din-extractor (rest lunch-or-din-lst)))]))




#|

  menu-extractor-all: MenuList -> (listof (listof String))
  Produce a list containing all the food names from menulst

  PRE: This function is used in conjunction with "all-lunch" or "all-dinner" function
  POST: Returns a list of list of String in the format
'(("[Mon lunch#1/Mon dinner#1]"  "[Mon lunch#2/Mon dinner#2]" ...)
  ("[Tues lunch#1/Tues dinner#1]"  "[Tues lunch#2/Tues dinner#2]" ...) ...)

|#


(define (menu-extractor-all menulst)
  (cond
    [(empty? menulst) empty]
    [else (cons (lunch-or-din-extractor (first menulst))
                (menu-extractor-all (rest menulst)))]))




#|

  all-date: MenuList -> (listof String)
  Produces all dates in menulst as a list

  PRE: true
  POST: dates is in the form YYYY-DD-MM

|#


(define (all-date menulst)
  (cond
    [(empty? menulst) empty]
    [else (cons (extractor (first menulst) "date")
                (all-date (rest menulst)))]))




#|

  all-weekdays: MenuList -> (listof String)
  Produces all weekdays in menulst as a list

  PRE: true
  POST: weekdays is one of: Monday, Tuesday, Wednesday, Thursday, or Friday

|#


(define (all-weekdays menulst)
  (cond
    [(empty? menulst) empty]
    [else (cons (extractor (first menulst) "day")
                (all-weekdays (rest menulst)))]))




#|

  combinator: (listof String) (listof String) (listof String) (listof String)  
           -> (listof (list String (list String) (list String)))
  Combines all the list info and group them together by doing lockstep recursion

  PRE: This function requires the following function to work:
       * all-weekdays
       * all-date
       * outlet-scanner
       * menu-extractor-all
       * all-lunch
       * all-dinner

  POST: Returns a list of list in the format
  '(
   ("[weekday#1], [date#1]" ("Lunch: "[lunch1]" "[lunch2]"...)
                            ("Dinner: "[dinner1]" "[dinner2]")) ...)
    
|#


(define (combinator weekdaylst datelst lunchlst dinnerlst)
  (cond
    [(or (empty? weekdaylst)
         (empty? datelst)
         (empty? lunchlst)
         (empty? dinnerlst)) empty]
    
    [else (cons (list (string-append (first weekdaylst) ", " (first datelst))
                      (append '("Lunch: ") (first lunchlst))
                      (append '("Dinner: ") (first dinnerlst)))
                (combinator (rest weekdaylst)
                          (rest datelst)
                          (rest lunchlst)
                          (rest dinnerlst)))]))




#|

  whats-cooking? : String -> (listof (list String (list String) (list String)))
  Produce this week's lunch and dinner, given a outletname (restaurant's name)

  PRE: This function requires the "combinator" function

  POST: Returns a list of list in the format
  '(
   ("[weekday#1], [date#1]" ("Lunch: "[lunch1]" "[lunch2]"...)
                            ("Dinner: "[dinner1]" "[dinner2]")) ...)
    
|#


(define (whats-cooking? outletname)
  (combinator 
   (all-weekdays (first (outlet-scanner outletname 
                                        (extractor gimmemenu "outlets"))))
   (all-date (first (outlet-scanner outletname 
                                    (extractor gimmemenu "outlets"))))
   (menu-extractor-all (all-lunch (daily-meals 
                                   (first (outlet-scanner outletname (extractor gimmemenu "outlets"))))))
   (menu-extractor-all (all-dinner (daily-meals 
                                    (first (outlet-scanner outletname (extractor gimmemenu "outlets"))))))))



#|
(printf "Which restaurant's weekly menu would you like to see?\n")
(printf "Choose between: Bon Appetit, Mudie's, Festival Fare, REVelation, PAS Lounge, Pastry Plus, and Liquid Assets (case sensitive)\n")
(printf "note: put double quotation marks besides the restaurant's name\n")
(define selection (read))
(whats-cooking? selection)
|#


(define (outletextractor outletlst)
  (cond
    [(empty? outletlst) '()]
    [else
     (cons (extractor (first outletlst) "outlet_name")
           (outletextractor (rest outletlst)))]))

(define lstofout (outletextractor (extractor gimmemenu "outlets")))