let position elem list = 
  if list=[] then []
  else let rec position_rec element remaining i result =
         if remaining=[] then result
         else let check = (List.hd remaining)
              in match check with
                    check when elem=check -> position_rec element (List.tl remaining) (i+1) (i::result)
                  | _ -> position_rec element (List.tl remaining) (i+1) result 
      
  in (List.rev (position_rec elem list 0 []));;
  

position 1 [1;3;4;1;2;1;6;8;9];;
position 10 [1;3;4;1;2;1;6;8;9];;
position 1 [];;
