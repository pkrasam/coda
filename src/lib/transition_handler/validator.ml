open Async_kernel
open Core_kernel
open Pipe_lib.Strict_pipe

module Make (Inputs : Inputs.S) = struct
  open Inputs
  open Consensus_mechanism
  open Deferred.Let_syntax

  let validate_transition ~logger ~frontier ~time_received t =
    let time_received =
      Time.to_span_since_epoch time_received
      |> Time.Span.to_ms |> Unix_timestamp.of_int64
    in
    let log_assert condition error_msg =
      let log () =
        Logger.info logger "transition rejected: %s" error_msg ;
        false
      in
      condition || log ()
    in
    let consensus_state =
      Fn.compose Protocol_state.consensus_state
        External_transition.protocol_state
    in
    let root =
      With_hash.data
        (Transition_frontier.Breadcrumb.transition_with_hash
           (Transition_frontier.root frontier))
    in
    if
      log_assert
        (Consensus_mechanism.is_valid (consensus_state t) ~time_received)
        "failed consensus validation"
      && log_assert
           ( Consensus_mechanism.select ~logger
               ~existing:(consensus_state root) ~candidate:(consensus_state t)
               ~time_received
           = `Take )
           "was not better than transition frontier root"
    then
      (* TODO:
      let length = External_transition.protocol_state t |> Protocol_state.blockchain_state |> Blockchain_state.length in
      log_assert
        (match Root_history.find (Transition_frontier.root_history frontier) (length - k) with
        | `Known h -> State_hash.equal h (External_transition.frontier_root_hash t)
        | `Unknown -> true
        | `Out_of_bounds ->
          Logger.info logger "expected root of transition was out of bounds";
          false)
        "transition frontier root hash was invalid"
      *)
      let%map proof_is_valid =
        Proof.verify
          (External_transition.protocol_state_proof t)
          (External_transition.protocol_state t)
      in
      log_assert proof_is_valid "proof was invalid"
    else Deferred.return false

  let run ~logger ~frontier ~transition_reader ~valid_transition_writer =
    let logger = Logger.child logger "transition_handler_validator" in
    don't_wait_for
      (Reader.iter transition_reader
         ~f:(fun (`Transition transition, `Time_received time_received) ->
           if%map
             validate_transition ~logger ~frontier ~time_received transition
           then Writer.write valid_transition_writer transition
           else
             (* TODO: punish *)
             Logger.warn logger "failed to verify transition from the network!"
       ))
end

(*
let%test_module "Validator tests" = (module struct
  module Inputs = struct
    module External_transition = struct
      include Test_stubs.External_transition.Full(struct
        type t = int
      end)

      let is_valid n = n >= 0
      (* let select n = n > *)
    end

    module Consensus_mechanism = Consensus_mechanism.Proof_of_stake
    module Transition_frontier = Test_stubs.Transition_frontier.Constant_root (struct
      let root = Consensus_mechanism.genesis
    end)
  end
  module Transition_handler = Make (Inputs)

  open Inputs
  open Consensus_mechanism

  let%test "validate_transition" =
    let test ~inputs ~expectations =
      let result = Ivar.create () in
      let (in_r, in_w) = Linear_pipe.create () in
      let (out_r, out_w) = Linear_pipe.create () in
      run ~transition_reader:in_r ~valid_transition_writer:out_w frontier;
      don't_wait_for (Linear_pipe.flush inputs in_w);
      don't_wait_for (Linear_pipe.fold_maybe out_r ~init:expectations ~f:(fun expect result ->
          let open Option.Let_syntax in
          let%bind expect = match expect with
            | h :: t ->
                if External_transition.equal result expect then
                  Some t
                else (
                  Ivar.fill result false;
                  None)
            | [] ->
                failwith "read more transitions than expected"
          in
          if expect = [] then (
            Ivar.fill result true;
            None)
          else
            Some expect));
      assert (Ivar.wait result)
    in
    Quickcheck.test (List.gen Int.gen) ~f:(fun inputs ->
      let expectations = List.map inputs ~f:(fun n -> n > 5) in
      test ~inputs ~expectations)
end)
*)
