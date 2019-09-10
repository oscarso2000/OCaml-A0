(** @author Oscar So (ons4) *)

(** This is a Function that takes an int y, a string m, 
 * and an int d as input. Returns true if they form a valid date,
 * and return false otherwise. Y represents year,
 * m represents month, and d represents day. Validity 
 * of date: Year is positive (>=1), month is 3-letter 
 * capitalized abbreviations, and day is 1 to no. of 
 * days in the particular month, including leap years. **)
let valid_date y m d =
  if (y >= 1) then (*Checking year*)
    begin
      if (m = "Jan" || m = "Mar" || m = "May" || m = "Jul" ||  (*Months with 31 days*)
      m = "Aug" || m = "Oct" || m = "Dec") then
        begin
          if ((d > 0) && (d < 32)) then
            true
          else
            false
        end
      else if (m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov") then (*Months with 30 days*)
        begin
          if ((d > 0) && (d < 31)) then
            true
          else
            false
        end
    else if (m = "Feb") then
        begin
          if ((y mod 100) = 0) then  (*Century years*)
            begin
              if ((y mod 400) = 0) then (*Check 400*)
                begin
                  if ((d > 0) && (d < 30)) then
                    true
                  else
                    false
                end
              else
                begin
                  if ((d > 0) && (d < 29)) then
                    true
                  else
                    false
                end
            end
          else if ((y mod 4) = 0) then (*Check leap year*)
            begin
              if ((d > 0) && (d < 30)) then
                true
              else
                false
            end
          else
            begin
              if ((d > 0) && (d < 29)) then
                true
              else
                false
            end
        end
      else 
        false
    end
  else
    false


(** Function syr_count counts the number of times Collatz operation has to be done on input n before value reaches 1. *) 
let rec syr_count n count =
  if (n = 1) then
    count
  else if (n > 1) then
    begin
      if((n mod 2) = 1) then
        syr_count ((n*3)+1) (count+1)
      else
        syr_count (n/2) (count+1)
    end
  else
    -1

(** Funcation syr provides a first input to check if input is already 1. Else, it will call function syr_count to continue the Collatz operation *)
let syr n =
  let count = 0 in
    begin
      if (n = 1) then
        count
      else if (n > 1) then
        begin
          if((n mod 2) = 1) then
            syr_count ((n*3)+1) (count+1)
          else
            syr_count (n/2) (count+1)
        end
      else
        -1
    end


(* Functions below all for nacci assignment *)

(* Function sum provides a way in getting the next number in the n-nacci sequence 
 * if the size of the current list is greater or equal to n. *)
let rec sum k n i li =
  if n = 0 then 0
  else if (n > i+1) then List.nth li i + sum k n (i+1) li
  else List.nth li (n-1)

(* Function total is a method that adds all numbers inside the list and produces an int.
 * This is useful when we do not have enough numbers yet to fulfill the previous n terms
 * in an n-nacci sequence.
 * Eg: if a call on nacci 3 10, our initial list is only [1,1]. Thus function will take 1+1
 * and return 2 to be added inside nacci_tail. *)
let total li = List.fold_left (+) 0 li

(* Recursive function nacci_tail is what keeps count of how many repetitions 
 * it has been recursivel called to maintain k elements in the return list.
 * It should take in an int n, int k, List l, and int count and check if the number of
 * elements in the list is equal to k. If so, return the list, else, check to see
 * whether to use function "sum" or function "total" by comparing n with list li.*)
let rec nacci_tail n k li count =
  if ((List.length li < n) && (count < k)) then nacci_tail n k (List.rev_append ((total li) :: []) li) (count+1)
  else if (count < k) then nacci_tail n k (List.rev_append (sum k n 0 li :: []) li) (count+1)
  else List.rev li

(* Function nacci is the initial function that is called from a person. 
 * It takes in an int n and int k for the nacci sequence and should 
 * create an initial list given that F1 and F2 are both always 1.
 * Return first element if k is 1, return both is k is 2. Else,
 * call the nacci_tail recursion function to start the counting.*)
let nacci n k =
  let li = 1 :: 1 :: [] in 
    let count = 2 in
      if (k = 1) then (List.hd li :: []) 
      else if (k = 2) then li
      else nacci_tail n k li count




(* TODO: add assertions for testing *)

(** Assertions for valid_date **)
(* Test leap day in a non-century leap year. *)
let () = assert (valid_date 2020 "Feb" 29)

(* Test leap day in a century leap year that is divisible by 400. *)
let () = assert (valid_date 2000 "Feb" 29)

(* Test leap day in a century leap year that isn't divisible by 400. *)
let () = assert (not (valid_date 2100 "Feb" 29))

(* Test invalid day. *)
let () = assert (not (valid_date 2010 "Jun" 50))

(* Test regular valid day. *)
let () = assert (valid_date 1999 "Jul" 15)

(* Test invalid year *)
let () = assert (not (valid_date 0 "May" 20))

(*Test invalid month *)
let () = assert (not (valid_date 2011 "jul" 22))


(** Assertions for syr **)
(* Test when n=1 *)
let () = assert (syr 1 = 0)

(* Tests when n is even *)
let () = assert (syr 2 = 1)

(* Tests bigger even number than requires an odd inside *)
let () = assert (syr 10 = 6)

(* Tests when n is odd *)
let () = assert (syr 3 = 7)

(* Tests when n is not strictly positive *)
let () = assert (syr 0 = -1)


(** Assertions for nacci **)
(* Tests when n is 1 *)
let () = assert (nacci 1 6 = [1;1;1;1;1;1])

(* Test fibonacci *)
let () = assert (nacci 2 6 = [1;1;2;3;5;8])

(* Test when k is 1 *)
let () = assert (nacci 3 1 = [1])

(*Test when k is 2 *)
let () = assert (nacci 3 2 = [1;1])

(* Test 3-nacci *)
let () = assert (nacci 3 6 = [1;1;2;4;7;13])

(* Test 10-nacci when 9 k *)
let () = assert (nacci 10 9 = [1; 1; 2; 4; 8; 16; 32; 64; 128])

(* Test 10-nacci with 12 k *)
let () = assert (nacci 10 12 = [1; 1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1023])


(* Set the value below to the number of hours
   you spent working on this assignment, rounded to
   the nearest integer *)

let hours_worked = 5
