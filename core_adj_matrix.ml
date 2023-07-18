open Printf;;
open Sys;;
let n = 1501;;
let domains = int_of_string argv.(1);;
let seconds = 0.02;;
let fname = "testfile.txt";;
let distname = "dist.txt";;

(* making log file *)
let distf = Atomic.make (open_out distname);;

(* making adjacency matrix *)
let adj_matrix = Array.make_matrix n n 0;;

let print_adjacency_matrix () = 
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      print_int adj_matrix.(i).(j);
      print_string " "
    done;
    print_string "\n"
  done;;

(* print_adjacency_matrix ();; *)
(* graph construction from file *)

let file = open_in fname;;
let quit_loop = ref false in
  while not !quit_loop do 
    try
      let line = input_line file in
      let ls = List.map int_of_string (String.split_on_char ' ' line) in
      let i = List.hd ls in
      let lst = List.tl ls in
      List.iter (fun j -> if j>i then adj_matrix.(i).(j) <- 1 ; ) lst with
      End_of_file -> quit_loop := true
  done;;

close_in file;;

(* print_adjacency_matrix ();; *)

(* making an in-degree array *)

let in_degree = Array.make n (Atomic.make 0);;
for i = 1 to (Array.length in_degree) - 1 do
  in_degree.(i) <- Atomic.make 0
done;;

(* function to construct in-degree array *)
let make_in_degree adj_matrix in_degree =
  for i = 1 to n-1 do
    for j = 1 to n-1 do
      if adj_matrix.(i).(j) = 1 then
        Atomic.incr in_degree.(j)
    done
  done;;

make_in_degree adj_matrix in_degree;;

let print_in_degree () = 
  for i = 1 to n-1 do
    print_int (Atomic.get in_degree.(i));
    print_string " "
  done;
  print_newline ();;

(* print_in_degree ();; *)

(* MPMCQueue for function list *)

let fqueue = Lockfree.Mpmc_relaxed_queue.create ~size_exponent:11 ()
;;
module MPMCQueue = Lockfree.Mpmc_relaxed_queue.Not_lockfree;;

let counter = Atomic.make 0;;

let minisleep (sec: float) =
  ignore (Unix.select [] [] [] sec);;

let rec execute x = 
  printf "Begin %d\n%!" x;
  minisleep seconds;
  let ls = Array.to_list adj_matrix.(x) in
  List.iteri (fun index y -> 
    if y = 1 then 
    if Atomic.fetch_and_add in_degree.(index) (-1) = 1 then ignore (MPMCQueue.push fqueue index)) 
  ls;
  printf "executed %d\n%!" x;
  Atomic.incr counter;
  (* print_in_degree (); *)
;;

let thread_loop () =
  let num = ref 0 in 
  while (Atomic.get counter) != (n-1) do
    let x = MPMCQueue.pop fqueue in 
    match x with
    None -> ()
    | Some x1 -> ignore (execute x1); num := !num + 1;
  done;
  let f = Atomic.get distf in 
  output_string f (string_of_int !num);
  output_string f "\n";

  printf "Exit Thread\n %!"
;;

let simulation () =
  for i = 1 to (n-1) do 
    if Atomic.get in_degree.(i) = 0 then (ignore (MPMCQueue.push fqueue i); print_int i; print_string " ";)
  done;
  print_newline ();
  let p = Array.make domains (Domain.spawn (fun () -> thread_loop ())) in 
  for a=2 to domains do 
    p.(a-1) <- Domain.spawn (fun () -> thread_loop ())
  done;

  for a=1 to domains do 
    Domain.join p.(a-1)
  done;

  printf "\n\nValidated \n%!";;

  let time f =
    let t = Unix.gettimeofday () in
    let res = f () in
    Printf.printf "\n \nExecution time: %f seconds \n"
                  (Unix.gettimeofday () -. t);
    res
  ;;

  let main() = 
    time(simulation)
  
  

let _ = main ();;