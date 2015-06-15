#lang racket

#|
(course-desc subject catalog)
(online? subject catalog)
(course-sections term subject catalog)
(course-capacity term subject catalog)
(section-info term subject catalog section)
(next-holiday date)

(room-status building room day time)
(gimme-room-info building room)
|#

(require "uw-api.rkt")

(provide course-desc)
(provide online?)
(provide course-sections)
(provide course-capacity)
(provide section-info)
(provide next-holiday)
(provide room-status)
(provide gimme-room-info)

;;to see the actual code, skip through the first 260 lines of code


;;an AL is one of:
;; * empty
;; * (list <key data type> <val data type>)

;;a Section is one of:
;; * empty
;; * (listof AL)

; an APIResult is one of:
; * (list "key" (union Num String Bool (listof String))) [where value is (union Num String)]
; * (list "key" APIResult)               
; * (listof APIResult)


;;Helper functions definitions:


;;golookup-course: String Num -> APIResult
;;produces APIResult based on the given subject and catalog number
;;   PRE:  You can connect to UW (have online access)
;;   POST: Produces an APIResult (see above) or
;;         #f if invalid subject or catalog or an empty result


(define (golookup-course subject catalog)  
  (uw-api (string-append "/courses/" subject "/" (number->string catalog))))




;;golookup-schedule: Num String Num -> APIResult
;;produces APIResult based on the given subject and catalog number
;;   PRE:  You can connect to UW (have online access)
;;   POST: Produces an APIResult (see above) or
;;         #f if invalid subject or catalog or an empty result


(define (golookup-schedule term subject catalog) 
  (uw-api (string-append "/terms/" (number->string term)
                         "/" subject "/" (number->string catalog) "/" "schedule")))




;;golookup-holidays: -> APIResult
;;produces APIResult based on the given subject and catalog number
;;   PRE:  You can connect to UW (have online access)
;;   POST: Produces an APIResult (see above) or
;;         #f if invalid subject or catalog or an empty result


(define (golookup-holidays)  
  (uw-api "/events/holidays"))




;;golookup-room: String Num -> APIResult
;;produces APIResult based on the given subject and catalog number
;;   PRE:  You can connect to UW (have online access)
;;   POST: Produces an APIResult (see above) or
;;         #f if invalid subject or catalog or an empty result


(define (golookup-room building room) 
  (uw-api (string-append "/buildings/" building "/" (number->string room) "/courses")))




;;extractor: (listof AL) String 
;;        -> (union Num String Bool (listof String) APIResult)

;;Extracts the APIResult associated with the trigger string from the given loal
;;PRE: you can connect to UW (have online access)
;;POST: The items associated with the trigger string might be anything within API
;;      * This is an abstraction function


(define (extractor loal trigger)
  (cond
    [(empty? loal) empty]
    [(string=? (first (first loal)) trigger)
     (second (first loal))]
    [else (extractor (rest loal) trigger)]))




;;output-allsec: (listof Section) (APIResult -> APIResult) 
;;            -> (listof (union Num String Bool (listof String)))

;;Extracts a list of data from allsec 
;;and result is dependent on the given function but 
;;might be anything within the API
;;PRE: true
;;POST: The output will be dependent on the given function
;;      * This is an abstraction function


(define (output-allsec allsec function)
  (cond
    [(empty? allsec) empty]
    [else (cons (function allsec)
                (output-allsec (rest allsec) function))]))




;;enrollment-capacity-lookup: Section -> Nat
;;Produce the enrollment capacity of the given section 
;;PRE: true
;;POST: returns a Nat


(define (enrollment-capacity-lookup section) 
  (cond 
    [(string=? (first (first section)) "enrollment_capacity") (second (first section))] 
    [else (enrollment-capacity-lookup (rest section))])) 




;;enrollment-total-lookup: Section -> Nat
;;Produce the enrollment total of the given section 
;;PRE: true
;;POST: returns a Nat


(define (enrollment-total-lookup section) 
  (cond 
    [(string=? (first (first section)) "enrollment_total") (second (first section))] 
    [else (enrollment-total-lookup (rest section))]))




;;start-time-extractor: (listof AL) -> String
;;Produce the string associated with "start_time"
;;PRE: true
;;POST: returns a String


(define (start-time-extractor loal)
  (extractor (first loal) "start_time"))




;;end-time-extractor: (listof AL) -> String
;;Produce the string associated with "end_time"
;;PRE: true
;;POST: returns a String


(define (end-time-extractor loal)
  (extractor (first loal) "end_time"))




;;weekdays-extractor: (listof AL) -> String
;;Produce the string associated with "weekdays"
;;PRE: true
;;POST: returns a String


(define (weekdays-extractor loal)
  (extractor (first loal) "weekdays"))




;;all-start-time: String Num -> (listof String)
;;Produce a list of all starting times for
;;all the lectures in the given room in
;;the given building
;;PRE: you can connect to UW (have online access)
;;POST: returns a list of String


(define (all-start-time building room)
  (output-allsec (golookup-room building room) start-time-extractor))




;;all-end-time: String Num -> (listof String)
;;Produce a list of all ending times for
;;all the lectures in the given room in
;;the given building
;;PRE: you can connect to UW (have online access)
;;POST: returns a list of String


(define (all-end-time building room)
  (output-allsec (golookup-room building room) end-time-extractor))




;;all-weekdays: String Num -> (listof String)
;;Produce a list of all weekday for
;;all the lectures in the given room in
;;the given building
;;PRE: you can connect to UW (have online access)
;;POST: returns a list of String


(define (all-weekdays building room)
  (output-allsec (golookup-room building room) weekdays-extractor))




;;title-outputter: (listof Section) -> (listof String)
;;Produce a list of all lecture titles by string-appending
;;the subject, catalog number, and title from sectionlst
;;PRE: you can connect to UW (have online access)
;;POST: returns a list of String


(define (title-outputter sectionlst)
  (cond
    [(empty? sectionlst) empty]
    [else (cons (string-append (extractor (first sectionlst) "subject") " "
                               (extractor (first sectionlst) "catalog_number") " "
                               (extractor (first sectionlst) "title"))
                (title-outputter (rest sectionlst)))]))




;;all-title-outputter: String Num -> (listof String)
;;Produce a list of all lecture titles in the
;;given building in the given room
;;PRE: you can connect to UW (have online access)
;;POST: returns a list of String


(define (all-title-outputter building room)
  (title-outputter (golookup-room building room)))




#|**********************************************************
* Assignment 1, Problem 5a *
* *
* @description: Produces a string with the calendar "description" of the course. *
* @author: Siyuan Wang [20465973] *
**********************************************************|#




;;course-description-extractor: APIResult -> String

;;Extracts the item associated with the trigger 
;;string "description" from the given course

;;PRE: you can connect to UW (have online access)
;;POST: returns a String, but can also
;;be anything within course, if the value 
;;associated with "description" is not a String (invalid inputs)


(define (course-description-extractor course)
  (extractor course "description"))




;;course-desc: String Num -> String 

;;Extracts the item associated with the trigger 
;;string "description" from the given course

;;PRE: you can connect to UW (have online access)
;;POST: returns a String, but can also
;;be anything within course, if the value 
;;associated with "description" is not a String (invalid inputs)


(define (course-desc subject catalog)
  (course-description-extractor (golookup-course subject catalog)))




#|**********************************************************
* Assignment 1, Problem 5b *
* *
* @description: Produces true if the course is 
* available online (or is only available online), and false otherwise. *
* *
* @author: Siyuan Wang [20465973] *
**********************************************************|#




;;online-or-online-only?: (listof AL) -> Boolean

;;Produces #t if either conditions are met:
;; * the value associated with "online" is #t 
;; * the value associated with "online_only" is #t
;;Produce #f in all other cases

;;PRE: true
;;POST: returns #t or #f


(define (online-or-online-only? loal)
  (cond
    [(or
      (equal? (extractor loal "online") #t)
      (equal? (extractor loal "online_only") #t)) #t]
    [else #f]))
    



;;offerings-extractor: APIResult -> (union APIResult Boolean)

;;Produces all APIResult associated with 
;;the trigger string "offerings"

;;PRE: true
;;POST: returns either an APIResult or #f


(define (offerings-extractor course)
  (cond
    [(empty? course) #f]
    [(string=? "offerings" (first (first course)))
     (rest (first course))]
    [else (offerings-extractor (rest course))]))




;;online?: String Num -> Boolean

;;Determines if a given course is offered online

;;PRE: you can connect to UW (have online access)
;;POST: returns a Boolean


(define (online? subject catalog)
  (online-or-online-only? 
   (first (offerings-extractor (golookup-course subject catalog)))))




#|**********************************************************
* Assignment 1, Problem 5c *
* *
* @description: Produces a list of strings that correspond
* to the section names for the course. *
* *
* @author: Siyuan Wang [20465973] *
**********************************************************|#




;;lecture-outputter-onesec: Section -> (list String)

;;Produces the lecture string as a list, given a section

;;PRE: you can connect to UW (have online access)
;;POST: returns a String as a list


(define (lecture-outputter-onesec section)
  (cond
    [(empty? section) empty]
    [(string=? "section" (first (first section)))
     (cons (second (first section))
           (lecture-outputter-onesec (rest section)))]
    [else (lecture-outputter-onesec (rest section))]))




;;all-lecture-outputter: (listof Section) -> (listof String)

;;Produces a lecture list of all section

;;PRE: you can connect to UW (have online access)
;;POST: returns a list in the format
;;(list [lecture1] [lecture2]...)


(define (all-lecture-outputter allsec)
  (cond
    [(empty? allsec) empty]
    [else (cons (lecture-outputter-onesec (first allsec))
                (all-lecture-outputter (rest allsec)))]))




;;course-sections: Num String Num -> (listof String)

;;Produces a list of Strings that correspond to the section names for the course

;;PRE: you can connect to UW (have online access)
;;POST: returns a list in the format
;;(list [lecture1] [lecture2]...)


(define (course-sections term subject catalog)
  (map first (all-lecture-outputter (golookup-schedule term subject catalog))))

 


#|**********************************************************
* Assignment 1, Problem 5d *
* *
* @description: Produces a list of lists illustrating how "full" a course is. *
* @author: Siyuan Wang [20465973] *
**********************************************************|#




;;lecture-outputter: Num String Num -> (listof String)

;;Produces a list of all Lecture sections based 
;;on the given term, subject, and catalog

;;PRE: you can connect to UW (have online access)
;;POST: returns a list of String


(define (lecture-outputter term subject catalog) 
  (filter (lambda (x) (equal? (substring x 0 3) "LEC"))
          (course-sections term subject catalog)))




;;secinfo-outputter-onelec: (listof Section) String 
;;                       -> (list String Nat Nat)

;;Produces a section info based on allsec and lecture

;;PRE: you can connect to UW (have online access)
;;POST: returns a list in the format 
;;(list [lecture #] [enrollment-cap] [enrollment-total])


(define (secinfo-outputter-onelec allsec lecture)
  (cond
    [(empty? allsec) empty]
    [(equal? lecture
             (extractor (first allsec) "section"))
     (list lecture
           (enrollment-capacity-lookup (first allsec))
           (enrollment-total-lookup (first allsec)))]
    [else (secinfo-outputter-onelec (rest allsec) lecture)]))




;;secinfo-outputter-all-lec: (listof Section) (listof String)
;;                        -> (listof (list String Nat Nat))

;;Produces all section info based on sectionlst and lecturelst

;;PRE: you can connect to UW (have online access)
;;POST: returns a list in the format
;;(list (list [lecture1] [enrollment-cap1] [enrollment-total1])
;;      (list [lecture2] [enrollment-cap2] [enrollment-total2])...)


(define (secinfo-outputter-all-lec sectionlst lecturelst)
  (cond
    [(empty? lecturelst) empty]
    [else (cons (secinfo-outputter-onelec sectionlst (first lecturelst))
                (secinfo-outputter-all-lec sectionlst (rest lecturelst)))]))




;;course-capacity: Num String Num 
;;             -> (listof (list String Nat Nat))

;;Produces a list of lists illustrating how "full" a course is

;;PRE: you can connect to UW (have online access)
;;POST: returns a list in the format
;;(list (list [lecture1] [enrollment-cap1] [enrollment-total1])
;;      (list [lecture2] [enrollment-cap2] [enrollment-total2])...)


(define (course-capacity term subject catalog)
  (secinfo-outputter-all-lec 
   (golookup-schedule term subject catalog)
   (lecture-outputter term subject catalog)))




#|**********************************************************
* Assignment 1, Problem 5e *
* *
* @description: Produces a single string with information about a particular section. *
* @author: Siyuan Wang [20465973] *
**********************************************************|#




;;building-room-outputter: Section -> String

;;Produces the room info from loal by extracting
;;the values associated with trigger string 
;;"building" and "room"

;;PRE: true
;;POST: returns a string in the format "[building] [room]"


(define (building-room-outputter loal)
  (string-append (extractor loal "building") " "
                 (extractor loal "room")))




;;sectioninfo-combinator: Num String (listof Section) String -> String

;;Produces the section info based on the given 
;;term, subject, catalog, sectionlst 
;;(sectionlst is derived from term, subject, and catalog), and section 

;;PRE: you can connect to UW (have online access)
;;POST: returns a String in the format
;;"[SUBJECT] [CATALOG] [SECTION] [start_time]-[end_time] [weekdays] [building] [room] [instructor]"


(define (sectioninfo-combinator term subject catalog sectionlst section)
  (cond
    [(string=? section (extractor (first sectionlst) "section"))
     
     ;;starting huge string-append...
     
     (string-append
      subject " "
      (number->string catalog) " "
      section " "
      
      (extractor 
       (extractor 
        (first (extractor (first sectionlst) "classes")) "date") "start_time") "-" 
                                                                                                                 
      (extractor 
       (extractor 
        (first (extractor (first sectionlst) "classes")) "date") "end_time") " "
                                                                                                               
      (extractor 
       (extractor        
        (first (extractor (first sectionlst) "classes")) "date") "weekdays") " "
                                                                                                               
      (building-room-outputter (extractor (first (extractor (first sectionlst) "classes")) "location")) " "
      
      (first (extractor (first (extractor (first sectionlst) "classes")) "instructors")))]        
    ;;end of the huge string-append
    
    [else (sectioninfo-combinator term subject catalog (rest sectionlst) section)]))
     

     
     
;;section-info: Num String Num String -> String

;;Produces the section info based on the given term, subject, catalog, 
;;sectionlst (sectionlst is derived from term, subject, and catalog), and section 

;;PRE: you can connect to UW (have online access)
;;POST: returns a String in the format
;;"[SUBJECT] [CATALOG] [SECTION] [start_time]-[end_time] [weekdays] [building] [room] [instructor]"


(define (section-info term subject catalog section)
  (sectioninfo-combinator 
   term 
   subject 
   catalog 
   (golookup-schedule term subject catalog) 
   section))




#|**********************************************************
* Assignment 1, Problem 5f *
* *
* @description: Produces a string with the next holiday on or after the date provided. *
* @author: Siyuan Wang [20465973] *
**********************************************************|#




;;date-name-outputter: (listof AL) -> String

;;Produce a holiday name by extracting the 
;;trigger string "date" and "name" from lstofdates

;;PRE: true
;;POST: returns a String with the format
;;"[date] [holiday name]"


(define (date-name-outputter lstofdates)
  (string-append (extractor lstofdates "date") " "
                 (extractor lstofdates "name")))



    
;;all-dates-sorter: (listof AL) -> (listof AL)

;;Sort the given lstofal based on the outcome
;;from the extractor with the trigger string "date"

;;PRE: true
;;POST: returns a (listof AL)


(define (all-dates-sorter lstofal)
  (cond
    [(empty? (rest lstofal)) (cons (first lstofal) empty)]
    [(string<? (extractor (first lstofal) "date")
               (extractor (second lstofal) "date"))
     (cons (first lstofal)
           (all-dates-sorter (cons (second lstofal)
                            (rest (rest lstofal)))))]
    [else (cons (second lstofal)
                (all-dates-sorter (cons (first lstofal)
                                 (rest (rest lstofal)))))]))




;;holiday-outputter: String (listof (listof AL)) -> String

;;Produces the holiday after the given date from the holidays-all
;;If the date is a holiday, it'll produce that as well

;;PRE: true
;;POST: returns a String


(define (holiday-outputter date holidays-all)
  (cond
    [(equal? #t (string<=? date (extractor (first holidays-all) "date")))
     (string-append (extractor (first holidays-all) "date") " "
                    (extractor (first holidays-all) "name"))]
    [else (holiday-outputter date (rest holidays-all))]))
    



;;next-holiday: String -> String

;;Produces a string with the next holiday on or after the date provided

;;PRE: you can connect to UW (have online access)
;;POST: returns a String in the format 
;;"[date] [holiday name]"


(define (next-holiday date)
  (holiday-outputter date (all-dates-sorter (golookup-holidays))))




#|**********************************************************
* Assignment 1, Problem 5g *
* *
* @description: Produces a string that displays the course
* in the room at that day and time, or "FREE" if the room is not in use. *
* *
* @author: Siyuan Wang [20465973] *
**********************************************************|#




;;weekday-start-end-combinator: (listof String) (listof String) (listof String) 
;;                           -> (listof (list String String String))

;;Combine the elements in lststart, lstend, and lstweekday into
;;lists by doing lockstep recursion

;;PRE: true
;;POST: returns a list of lists in the format
;;(list (list [weekdays1] [starttime1] [endtime1])
;;      (list [weekdays2] [starttime2] [endtime2])...)


(define (weekday-start-end-combinator lststart lstend lstweekday)
  (cond
    [(or (empty? lststart)
         (empty? lstend)
         (empty? lstweekday)) empty]
    [else (cons (list (first lststart)
                      (first lstend)
                      (first lstweekday))
                (weekday-start-end-combinator (rest lststart)
                            (rest lstend)
                            (rest lstweekday)))]))




;;weekday-start-end-outputter: String Num 
;;                         -> (listof (list String String String))

;;Produces a list of course info in the given 
;;building and the room by doing lockstep recursion

;;PRE: true
;;POST: returns a list of lists in the format
;;(list (list [weekdays1] [starttime1] [endtime1])
;;      (list [weekdays2] [starttime2] [endtime2])...)


(define (weekday-start-end-outputter building room)
  (weekday-start-end-combinator 
   (all-weekdays building room)
   (all-start-time building room)
   (all-end-time building room)))




;;MTWThF-splitter : (listof Char) -> (listof String)

;;Produce a String for every char from lst 
;;[split M, T, W, Th, F into individual strings]

;;PRE: true
;;POST: Produce a list in the format
;;      (list [weekday1] [weekday2]...)
;;
;;      EXCEPTION:
;;      Produce "Th" if #\T #\h are found to be consecutive char.
;;      Otherwise simply produce that char as a string


(define (MTWThF-splitter lst)
  (cond
    [(empty? lst) empty]
    [(and (char=? (first lst) #\T)
          (empty? (rest lst))) '("T")]
    [(and (char=? (first lst) #\T)
          (char=? (second lst) #\h))
     (cons "Th" (MTWThF-splitter (rest (rest lst))))]
    [else (cons (list->string (list (first lst)))
                (MTWThF-splitter (rest lst)))]))




;;day-start-end-title-maker: (listof String) String String String 
;;                        -> (listof (list String String String String)

;;Produce a list of lists by grouping the first element of days
;;with starttime, endtime, and title

;;PRE: true
;;POST: returns a list of lists in the format
;;(list (list [day1] [start-time] [end-time] [title])
;;      (list [day2] [start-time] [end-time] [title])...
;;      (list [dayk] [start-time] [end-time] [title]))


(define (day-start-end-title-maker days starttime endtime title)
  (map (lambda (x) (list x starttime endtime title)) days))




;;all-day-start-end-title-combinator: 
;;   (listof String) (listof String) (listof String) (listof String)
;;-> (listof (list String String String String)

;;Produce a list of lists by grouping every element in daylst with
;;every elements from starttimelst, endtimelst, and titlelst

;;PRE: true
;;POST: every list will be in the format [day] [start-time] [end-time] [course title]


(define (all-day-start-end-title-combinator daylst starttimelst endtimelst titlelst)
  (cond
    [(empty? daylst) empty]
    [else (append (day-start-end-title-maker 
                   (first daylst) 
                   (first starttimelst)
                   (first endtimelst) 
                   (first titlelst))
                  
                  (all-day-start-end-title-combinator 
                   (rest daylst) 
                   (rest starttimelst) 
                   (rest endtimelst) 
                   (rest titlelst)))]))




;;individual-weekday-splitter: (listof (list String String String))
;;                          -> (listof (list String String String))

;;Split and produce the weekdays associated with every elements from hugelst

;;PRE: true
;;POST: returns a list of lists in the format
;;(list (list [day1-1] [day1-2]...[day1-k])
;;      (list [day2-1] [day2-2]...[day2-k])...)


  (define (individual-weekday-splitter roominfo-all)
  (cond
    [(empty? roominfo-all) empty]
    [else (cons (MTWThF-splitter (string->list (first (first roominfo-all))))
                (individual-weekday-splitter (rest roominfo-all)))]))
  
  
  
  
;;all-day-start-end-title-outputter: String Nat 
;;                               -> (listof (list String String String String)
  
;;Produce a complete schedule for the given building and room that
;;includes the day, starting time, ending time, and the lecture title
  
;;PRE: you can connect to UW (have online access)
;;POST: returns a list of lists in the format
;;(list (list [day1-1] [start-time1] [end-time1] [title1])
;;      (list [day1-2] [start-time1] [end-time1] [title1])...
;;      (list [day1-k] [start-time1] [end-time1] [title1])
;;      (list [day2-1] [start-time2] [end-time2] [title2])
;;      (list [day2-2] [start-time2] [end-time2] [title2])...
;;      (list [day2-k] [start-time2] [end-time2] [title2])...)

(define (all-day-start-end-title-outputter building room)
  (roomsort (all-day-start-end-title-combinator 
   (individual-weekday-outputter building room)
   (all-start-time building room)
   (all-end-time building room)
   (all-title-outputter building room)) first))



  
;;individual-weekday-outputter: String Nat 
;;                          -> (listof (list String String String))

;;Split and produce the weekdays associated with every elements 
;;in the given building in the given room

;;PRE: you can connect to UW (have online access)
;;POST: A wrapper for the function individual-weekday-splitter


(define (individual-weekday-outputter building room)
  (individual-weekday-splitter (weekday-start-end-outputter building room)))




;;FREE-or-title-outputter: 
;;(listof (list String String String String)) String String -> String

;;Produces the title of the course if an element in
;;biglst is found with all the conditions match:
;; #1 - time is in between starttime and endtime
;; #2 - date matches the given day
;;Otherwise, produce "FREE"

;;PRE: true
;;POST: returns a String


(define (FREE-or-title-outputter splitted-room-info-all day time)
  (cond
    [(empty? splitted-room-info-all) "FREE"]
    [(and (string=? day (first (first splitted-room-info-all)))
          (string>=? time (second (first splitted-room-info-all)))
          (string<=? time (third (first splitted-room-info-all)))) 
     (fourth (first splitted-room-info-all))]
    [else (FREE-or-title-outputter (rest splitted-room-info-all) day time)]))




;;room-status: String Num String String -> String

;;Produces a string that displays the course in the
;;room at that day and time, or "FREE" if the room is not in use.

;;PRE: you can connect to UW (have online access)
;;POST: returns a String


(define (room-status building room day time)
  (FREE-or-title-outputter (all-day-start-end-title-outputter building room) day time))






;;end of assignment






#|

UW-campus room search improved

|#



(define (insert lst lol order)
  (cond
    [(empty? lol) (cons lst empty)]
    [(string<? (order lst) (order (first lol))) (cons lst lol)]
    [else (cons (first lol) (insert lst (rest lol) order))]))

#|
used in:
gimme-room-info - custom
all-day-start-end-title-outputter - strip the function to see original soln

order = what's the position of the item I'm trying to sort?
|#
(define (roomsort lol order)
  (cond
    [(empty? lol) empty]
    [else (insert (first lol) (roomsort (rest lol) order) order)]))

(define (singledaysorter lol day)
  (cond
    [(empty? lol) empty]
    [(equal? (first (first lol)) day)
     (cons (first lol) (singledaysorter (rest lol) day))]
    [else (singledaysorter (rest lol) day)]))

(define (weekdaysort lol lowk)
  (cond
    [(empty? lowk) empty]
    [else (append (singledaysorter lol (first lowk))
                  (weekdaysort lol (rest lowk)))]))

;; This one will sort according to start time
(define (gimme-room-info building room)
  (weekdaysort (roomsort (all-day-start-end-title-outputter building room) second) '("M" "T" "W" "Th" "F")))
