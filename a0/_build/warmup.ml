(** @author Oscar So (ons4) *)

(** This is a Function that takes an int y, a string m, 
 * and an int d as input. Returns true if they form a valid date,
 * and return false otherwise. Y represents year,
 * m represents month, and d represents day. Validity 
 * of date: Year is positive (>=1), month is 3-letter 
 * capitalized abbreviations, and day is 1 to no. of 
 * days in the particular month, including leap years. **)

(** Leap year definition: “Every year that is exactly divisible
 * by four is a leap year, except for years that are exactly 
 * divisible by 100, but these centurial years are leap years 
 * if they are exactly divisible by 400.  ”**)

let valid_date y m d =
  if (y >= 1) then 
    begin
      if (m = "Jan" || m = "Mar" || m = "May" || m = "Jul" ||
      m = "Aug" || m = "Oct" || m = "Dec") then
        begin
          if ((d > 0) && (d < 32)) then
            true
          else
            false
        end
      else if (m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov") then
        begin
          if ((d > 0) && (d < 31)) then
            true
          else
            false
        end
    else if (m = "Feb") then
        begin
          if ((y mod 100) = 0) then
            begin
              if ((y mod 400) = 0) then
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
          else if ((y mod 4) = 0) then
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

(** TODO: document *)  
let syr n =
  failwith "Unimplemented"

(** TODO: document *)
let nacci n k =
  failwith "Unimplemented"

(* TODO: add assertions for testing *)

(* TODO: set the value below to the number of hours
   you spent working on this assignment, rounded to
   the nearest integer *)
   
let hours_worked = -1
