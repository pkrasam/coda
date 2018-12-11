open Core_kernel
open Protocols
open Coda_pow

module Make (Inputs : sig
  module Ledger_hash : Ledger_hash_intf

  module Ledger_proof : sig
    type t [@@deriving sexp, bin_io]
  end

  module Staged_ledger_aux_hash : Staged_ledger_aux_hash_intf

  module Staged_ledger_hash :
    Staged_ledger_hash_intf
    with type staged_ledger_aux_hash := Staged_ledger_aux_hash.t
     and type ledger_hash := Ledger_hash.t

  module Compressed_public_key : Compressed_public_key_intf

  module User_command :
    User_command_intf with type public_key := Compressed_public_key.t

  module Transaction_snark_work :
    Transaction_snark_work_intf
    with type public_key := Compressed_public_key.t
     and type statement := Transaction_snark.Statement.t
     and type proof := Ledger_proof.t
end) :
  Coda_pow.Staged_ledger_diff_intf
  with type user_command := Inputs.User_command.t
   and type user_command_with_valid_signature :=
              Inputs.User_command.With_valid_signature.t
   and type staged_ledger_hash := Inputs.Staged_ledger_hash.t
   and type public_key := Inputs.Compressed_public_key.t
   and type completed_work := Inputs.Transaction_snark_work.t
   and type completed_work_checked := Inputs.Transaction_snark_work.Checked.t =
struct
  open Inputs

  module At_most_two = struct
    type 'a t = Zero | One of 'a option | Two of ('a * 'a option) option
    [@@deriving sexp, bin_io]

    let increase t ws =
      match (t, ws) with
      | Zero, [] -> Ok (One None)
      | Zero, [a] -> Ok (One (Some a))
      | One _, [] -> Ok (Two None)
      | One _, [a] -> Ok (Two (Some (a, None)))
      | One _, [a; a'] -> Ok (Two (Some (a', Some a)))
      | _ -> Or_error.error_string "Error incrementing coinbase parts"
  end

  module At_most_one = struct
    type 'a t = Zero | One of 'a option [@@deriving sexp, bin_io]

    let increase t ws =
      match (t, ws) with
      | Zero, [] -> Ok (One None)
      | Zero, [a] -> Ok (One (Some a))
      | _ -> Or_error.error_string "Error incrementing coinbase parts"
  end

  type diff =
    { completed_works: Transaction_snark_work.t list
    ; user_commands: User_command.t list }
  [@@deriving sexp, bin_io]

  type diff_with_at_most_two_coinbase =
    {diff: diff; coinbase_parts: Transaction_snark_work.t At_most_two.t}
  [@@deriving sexp, bin_io]

  type diff_with_at_most_one_coinbase =
    {diff: diff; coinbase_added: Transaction_snark_work.t At_most_one.t}
  [@@deriving sexp, bin_io]

  type pre_diffs =
    ( diff_with_at_most_one_coinbase
    , diff_with_at_most_two_coinbase * diff_with_at_most_one_coinbase )
    Either.t
  [@@deriving sexp, bin_io]

  type t =
    { pre_diffs: pre_diffs
    ; prev_hash: Staged_ledger_hash.t
    ; creator: Compressed_public_key.t }
  [@@deriving sexp, bin_io]

  (* "checked" versions of these types *)

  type diff_checked =
    { completed_works_checked: Transaction_snark_work.Checked.t list
    ; user_commands: User_command.t list }
  [@@deriving sexp, bin_io]

  type diff_with_at_most_two_coinbase_checked =
    { diff_checked: diff_checked
    ; coinbase_parts_checked: Transaction_snark_work.Checked.t At_most_two.t }
  [@@deriving sexp, bin_io]

  type diff_with_at_most_one_coinbase_checked =
    { diff_checked: diff_checked
    ; coinbase_added_checked: Transaction_snark_work.Checked.t At_most_one.t }
  [@@deriving sexp, bin_io]

  type pre_diffs_checked =
    ( diff_with_at_most_one_coinbase_checked
    , diff_with_at_most_two_coinbase_checked
      * diff_with_at_most_one_coinbase_checked )
    Either.t
  [@@deriving sexp, bin_io]

  type checked =
    { checked_pre_diffs: pre_diffs_checked
    ; prev_hash: Staged_ledger_hash.t
    ; creator: Compressed_public_key.t }
  [@@deriving sexp, bin_io]


  module With_valid_signatures_and_proofs = struct
    type diff =
      { completed_works: Transaction_snark_work.Checked.t list
      ; user_commands: User_command.With_valid_signature.t list }
    [@@deriving sexp]

    type diff_with_at_most_two_coinbase =
      { diff: diff
      ; coinbase_parts: Inputs.Transaction_snark_work.Checked.t At_most_two.t
      }
    [@@deriving sexp]

    type diff_with_at_most_one_coinbase =
      { diff: diff
      ; coinbase_added: Inputs.Transaction_snark_work.Checked.t At_most_one.t
      }
    [@@deriving sexp]

    type pre_diffs =
      ( diff_with_at_most_one_coinbase
      , diff_with_at_most_two_coinbase * diff_with_at_most_one_coinbase )
      Either.t
    [@@deriving sexp]

    type t =
      { pre_diffs: pre_diffs
      ; prev_hash: Staged_ledger_hash.t
      ; creator: Compressed_public_key.t }
    [@@deriving sexp]

    let user_commands t =
      Either.value_map t.pre_diffs
        ~first:(fun d -> d.diff.user_commands)
        ~second:(fun d ->
          (fst d).diff.user_commands @ (snd d).diff.user_commands )
  end

  let forget_diff
      {With_valid_signatures_and_proofs.completed_works; user_commands} =
    { completed_works= List.map ~f:Transaction_snark_work.forget completed_works
    ; user_commands= (user_commands :> User_command.t list) }

  let forget_work_opt = Option.map ~f:Transaction_snark_work.forget

  let forget_pre_diff_with_at_most_two
      {With_valid_signatures_and_proofs.diff; coinbase_parts} =
    let forget_cw =
      match coinbase_parts with
      | At_most_two.Zero -> At_most_two.Zero
      | One cw -> One (forget_work_opt cw)
      | Two cw_pair ->
          Two
            (Option.map cw_pair ~f:(fun (cw, cw_opt) ->
                 (Transaction_snark_work.forget cw, forget_work_opt cw_opt) ))
    in
    {diff= forget_diff diff; coinbase_parts= forget_cw}

  let forget_pre_diff_with_at_most_one
      {With_valid_signatures_and_proofs.diff; coinbase_added} =
    let forget_cw =
      match coinbase_added with
      | At_most_one.Zero -> At_most_one.Zero
      | One cw -> One (forget_work_opt cw)
    in
    {diff= forget_diff diff; coinbase_added= forget_cw}

  let forget (t : With_valid_signatures_and_proofs.t) =
    { pre_diffs=
        Either.map t.pre_diffs ~first:forget_pre_diff_with_at_most_one
          ~second:(fun d ->
            ( forget_pre_diff_with_at_most_two (fst d)
            , forget_pre_diff_with_at_most_one (snd d) ) )
    ; prev_hash= t.prev_hash
    ; creator= t.creator }

  let user_commands (t : t) =
    Either.value_map t.pre_diffs
      ~first:(fun d -> d.diff.user_commands)
      ~second:(fun d -> (fst d).diff.user_commands @ (snd d).diff.user_commands)

  let uncheck_completed_work _cw = failwith "uncheck_completed_work: not implemented"
end
