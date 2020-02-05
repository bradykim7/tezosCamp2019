

module Calc_v2 =struct 
	let rec calc : string list -> int
	= fun str_list ->
		match srt_list with
		| [] -> 0
		| hd1::hd2::tl -> 
			let v = int_of_string hd1 in
			if (String.compare hd2 "+" ) =0 
			 then
				v + (calc tl)
			 else (* String.compare hd "-" *)
			  v- (calc tl)
		| hd::tl -> int_of_string hd


type exp = INT of int 
          | ADD 
          | SUB 
(* tag 라고 한다. add 와 sub 는 연산기 이기도하면서 값이다,*)
		
let get_num : exp-> int
= fun e ->
	match e wtih
	| INT n-> n
	| _ -> failwith "invaild"
in

let rec calc: exp list -> int
= fun lst ->
		match lst with
		| []-> 0
		| hd1::hd2::tl ->
			(match hd2 with
			| ADD -> (get_num hd1) + (calc tl)
			| SUB -> (get_num hd1) + (calc tl)
			| INT n -> failwith"invaild"
			)
			
  	| hd :: tl -> get_num hd (* 길이가 1 인 리스트 *)
  	| [] -> 0  
end
			
			
			
			
			
			
			