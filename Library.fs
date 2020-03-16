module Synthesis

let abelar x = x>12 && x<3097 && x%12=0

let area x y = match x<0.0 || y<0.0 with
    | false -> (x*y)/2.0
    | true -> failwith "shouldFail"
    

let zollo x = match x<=0 with
    | false -> x*2
    | true -> -1*x

let min x y = match x<=y with
    | false -> y
    | true -> x

let max x y = match x>=y with
    | false -> y
    | true -> x
    

let ofTime x y z = (x*3600)+(y*60)+z

let toTime x = match x>0 with
               | true ->   let hours = x/3600
                           let minutes = (x%3600)/60
                           let seconds = (x%3600)%60
                           (hours,minutes,seconds)
               | false -> (0,0,0)

let rec digits x = 
      match ((x<10 && x>=0) || (x<0 && x> -10)) with
      | true -> 1
      | _ -> 1+ digits (x/10)
    

let minmax (a,b,c,d) = 
   let smallest = min a,b,c,d
   let biggest = max a,b,c,d
   (smallest,biggest)
    

let isLeap currentYear =
    match currentYear % 4 = 0 && (currentYear%100<>0 || (currentYear%400=0 && currentYear%100=0)) with
    | true -> true
    | false -> failwith "shouldFail"
    | _ -> false

let month givenNumber = match givenNumber with
    |1 -> "January", 31
    |2 -> "February", 28
    |3 -> "March", 31
    |4 -> "April", 30
    |5 -> "May", 31
    |6 -> "June", 30
    |7 -> "July", 31
    |8 -> "August", 31
    |9 -> "September", 30
    |10 -> "October", 31
    |11 -> "November", 30
    |12 -> "December", 31
    |_ -> failwith "shouldFail"

let rec toBinary converter = match converter >=0 with
                         | true -> match converter with
                                    |0 | 1 -> string converter
                                    |_ -> let byte = string (converter%2)
                                          (toBinary (converter/2))+byte
                         | false -> failwith "shouldFail"
    

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"