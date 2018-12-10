module type S = sig
  module Protocol_state : Protocol_state.S

  module Ledger_builder_diff : sig
    type t [@@deriving bin_io, sexp]
    type checked [@@deriving sexp, bin_io]
  end

  type t [@@deriving sexp, bin_io, compare, eq]
  type checked [@@deriving sexp, bin_io, compare, eq]

  val create :
       protocol_state:Protocol_state.value
    -> protocol_state_proof:Proof.t
    -> ledger_builder_diff:Ledger_builder_diff.t
    -> t

  val create_checked :
       checked_protocol_state:Protocol_state.value
    -> checked_protocol_state_proof:Proof.t
    -> checked_ledger_builder_diff:Ledger_builder_diff.checked
    -> checked

  val protocol_state : t -> Protocol_state.value

  val protocol_state_proof : t -> Proof.t

  val checked_protocol_state : checked -> Protocol_state.value

  val checked_protocol_state_proof : checked -> Proof.t

  val ledger_builder_diff : t -> Ledger_builder_diff.t

  val checked_ledger_builder_diff : checked -> Ledger_builder_diff.checked

  val timestamp : t -> Block_time.t

end

module Make (Ledger_builder_diff : sig
             type t [@@deriving bin_io, sexp]
             type checked [@@deriving bin_io, sexp]
           end)
         (Protocol_state : Protocol_state.S) :
  S
  with module Ledger_builder_diff = Ledger_builder_diff
   and module Protocol_state = Protocol_state = struct
  module Ledger_builder_diff = Ledger_builder_diff
  module Protocol_state = Protocol_state
  module Blockchain_state = Protocol_state.Blockchain_state

  type t =
    { protocol_state: Protocol_state.value
    ; protocol_state_proof: Proof.Stable.V1.t
    ; ledger_builder_diff: Ledger_builder_diff.t }
  [@@deriving sexp, fields, bin_io]

  type checked =
    { checked_protocol_state: Protocol_state.value
    ; checked_protocol_state_proof: Proof.Stable.V1.t
    ; checked_ledger_builder_diff: Ledger_builder_diff.checked }
  [@@deriving sexp, fields, bin_io]

  (* TODO: Important for bkase to review *)
  let compare t1 t2 =
    Protocol_state.compare t1.protocol_state t2.protocol_state

  let compare_checked ch1 ch2 =
    Protocol_state.compare ch1.checked_protocol_state ch2.checked_protocol_state

  let equal t1 t2 =
    Protocol_state.equal_value t1.protocol_state t2.protocol_state

  let equal_checked ch1 ch2 =
    Protocol_state.equal_value ch1.checked_protocol_state ch2.checked_protocol_state

  let create ~protocol_state ~protocol_state_proof ~ledger_builder_diff =
    {protocol_state; protocol_state_proof; ledger_builder_diff}

  let create_checked ~checked_protocol_state ~checked_protocol_state_proof ~checked_ledger_builder_diff =
    {checked_protocol_state; checked_protocol_state_proof; checked_ledger_builder_diff}

  let timestamp {protocol_state; _} =
    Protocol_state.blockchain_state protocol_state
    |> Blockchain_state.timestamp
end
