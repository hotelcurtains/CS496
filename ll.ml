
(* Encoding linked-lists using records with mutable fields *)
(*   26 March 2025 *)
  
type 'a node = { mutable data: 'a;
                 mutable next: 'a node option}

type 'a ll = { mutable head: 'a node option;
               mutable size: int}

let l1 : int ll =
  { head = None;
    size = 0}

let l2 : int ll =
  { head = Some {data=3; next=None};
    size=1
  }

let add_first l e =
  l.head <- Some {data=e; next=l.head};
  l.size <- l.size+1

let l3 = 
  let l = { head = None;
  size = 0} in 
  begin List.iter (add_first l) (List.init 10 ((+)1));
  l end

(* 3/28 *)

let list_of_ll l =
  let rec helper no =
    match no with
    | None -> []
    | Some n -> n.data :: helper n.next
  in helper l.head

let rec mem e l =
  let rec helper no =
    match no with
    | None -> false
    | Some n -> n.data==e || helper n.next
  in helper l.head

let rec map f l =
  let rec helper no =
    match no with
    | None -> ()
    | Some n -> begin n.data<-(f n.data); helper n.next end
  in helper l.head

let get : 'a ll -> int -> 'a = 
  fun l idx ->
  let rec helper i no =
    match i, no with
    | _, None -> failwith "index out of bounds"
    | 0, Some n -> n.data
    | j, Some n -> helper (j-1) n.next
  in helper idx l.head

  