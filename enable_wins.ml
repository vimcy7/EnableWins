open Lwt.Infix

let info = Irmin_unix.info

let (+) = Int64.add
let (-) = Int64.sub

module EnableWins : Irmin.Contents.S with type t = int64 * int64 = struct

  type t = int64 * int64
  let t = Irmin.Type.(pair int64 int64)

  let merge ~old a b =
    let open Irmin.Merge.Infix in
    let (a1, a2) = a and (b1, b2) = b in
    old () >|=* function
    | None -> (a1 + b1, a2 + b2)
    | Some (o1, o2) -> (a1 + b1 - o1, a2 + b2 - o2)

  let merge = Irmin.Merge.(option (v t merge))

end

module Store = Irmin_unix.Git.FS.KV (EnableWins)

let init_counter t k = 
  Store.set_exn t ~info:(info "Initialising counter") k (0L, 0L)

let enable t k = 
  Store.get t k >>= fun c -> let (c1, c2) = c in
  Store.set_exn t ~info:(info "Received Enable") k (c1 + 1L, c2)

let disable t k = 
    Store.get t k >>= fun c -> let (c1, _) = c in
    Store.set_exn t ~info:(info "Received Disable") k (c1, c1)

let get_counter t k = Store.get t k

let int_to_bool x =
  if x > 0L then true else false

let get_result t1 t2 =
  Printf.printf "RESULT --> %b\n" (int_to_bool (t1 - t2) ); Lwt.return_unit

let inc_first t k = 
    Store.get t k >>= fun c -> let (c1, c2) = c in
    Store.set_exn t ~info:(info "Incrementing first counter") k (c1 + 1L, c2)
   
let path = ["tmp"; "count"]

let main () =
  let config = Irmin_git.config ~bare:true "/tmp/test" in
  Store.Repo.v config >>= fun repo ->
  Store.master repo >>= fun t ->
  Store.clone ~src:t ~dst:"b2" >>= fun x -> 
  init_counter t path >>= fun () ->
  inc_first t path >>= fun () ->
  get_counter t path >>= fun (t1, t2) ->
  Printf.printf "Operation 1: Enable Master -> (%Ld, %Ld)\n" t1 t2;
  init_counter x path >>= fun () -> 
  enable x path  >>= fun () ->
  get_counter x path >>= fun (x1, x2) ->
  Printf.printf "Operation 2: Enable b2 -> (%Ld, %Ld)\n" x1 x2;
  Store.merge_into ~info:(info "Merging b2 into master") x ~into:t >>= (function
  | Ok () -> get_counter t path >>= fun (t1, t2) -> get_counter x path >>= fun (x1, x2) ->
              Printf.printf "Operation 3: Merging b2 into master; master -> (%Ld, %Ld) ; b2 -> (%Ld, %Ld)\n" t1 t2 x1 x2; Lwt.return_unit
  | Error _ -> failwith "Error!!!") >>= fun () ->

  enable x path >>= fun () ->
  get_counter x path >>= fun (x1, x2) ->
  Printf.printf "Operation 4: Enable b2 -> (%Ld, %Ld)\n" x1 x2; 
  disable t path >>= fun () ->
  get_counter t path >>= fun (t1, t2) ->
  Printf.printf "Operation 5: Disable master -> (%Ld, %Ld)\n" t1 t2; 
  Store.merge_into ~info:(info "Merging b2 into master") x ~into:t >>= (function
  | Ok () -> get_counter t path >>= fun (t1, t2) -> get_counter x path >>= fun (x1, x2) ->
              Printf.printf "Operation 6: Merging b2 into master; master -> (%Ld, %Ld) ; b2 -> (%Ld, %Ld)\n" t1 t2 x1 x2; Lwt.return_unit
  | Error _ -> failwith "Error!!!") >>= fun () ->

  disable t path >>= fun () ->
  get_counter t path >>= fun (t1, t2) ->
  Printf.printf "Operation 7: Disable master -> (%Ld, %Ld)\n" t1 t2; 
  Store.merge_into ~info:(info "Merging b2 into master") x ~into:t >>= (function
  | Ok () -> get_counter t path >>= fun (t1, t2) -> get_counter x path >>= fun (x1, x2) ->
              Printf.printf "Operation 8: Merging b2 into master; master -> (%Ld, %Ld) ; b2 -> (%Ld, %Ld)\n" t1 t2 x1 x2; Lwt.return_unit
  | Error _ -> failwith "Error!!!") >>= fun () ->

  get_counter t path >>= fun (t1, t2) ->
  get_result t1 t2 >>= fun () -> Lwt.return_unit 
  
let () = Lwt_main.run (main ())