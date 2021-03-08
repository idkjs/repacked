/* Suppress warning 4, "fragile pattern matching," in this file only, due to

     https://caml.inria.fr/mantis/view.php?id=7451

   This can be removed if/when Lwt requires a minimum OCaml version 4.05. */

module LwtSeq = {
  /* This file is part of Lwt, released under the MIT license. See LICENSE.md for
     details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */

  exception Empty;

  type t('a) = {
    mutable prev: t('a),
    mutable next: t('a),
  };

  type node('a) = {
    mutable node_prev: t('a),
    mutable node_next: t('a),
    mutable node_data: 'a,
    mutable node_active: bool,
  };

  external seq_of_node: node('a) => t('a) = "%identity";
  external node_of_seq: t('a) => node('a) = "%identity";

  /* +-----------------------------------------------------------------+
     | Operations on nodes                                             |
     +-----------------------------------------------------------------+ */

  let get = node => node.node_data;

  let set = (node, data) => node.node_data = data;

  let remove = node =>
    if (node.node_active) {
      node.node_active = false;
      let seq = seq_of_node(node);
      seq.prev.next = seq.next;
      seq.next.prev = seq.prev;
    };

  /* +-----------------------------------------------------------------+
     | Operations on sequences                                         |
     +-----------------------------------------------------------------+ */

  let create = () => {
    let rec seq = {prev: seq, next: seq};
    seq;
  };

  let clear = seq => {
    seq.prev = seq;
    seq.next = seq;
  };

  let is_empty = seq => seq.next === seq;

  let length = seq => {
    let rec loop = (curr, len) =>
      if (curr === seq) {
        len;
      } else {
        let node = node_of_seq(curr);
        loop(node.node_next, len + 1);
      };

    loop(seq.next, 0);
  };

  let add_l = (data, seq) => {
    let node = {
      node_prev: seq,
      node_next: seq.next,
      node_data: data,
      node_active: true,
    };
    seq.next.prev = seq_of_node(node);
    seq.next = seq_of_node(node);
    node;
  };

  let add_r = (data, seq) => {
    let node = {
      node_prev: seq.prev,
      node_next: seq,
      node_data: data,
      node_active: true,
    };
    seq.prev.next = seq_of_node(node);
    seq.prev = seq_of_node(node);
    node;
  };

  let take_l = seq =>
    if (is_empty(seq)) {
      raise(Empty);
    } else {
      let node = node_of_seq(seq.next);
      remove(node);
      node.node_data;
    };

  let take_r = seq =>
    if (is_empty(seq)) {
      raise(Empty);
    } else {
      let node = node_of_seq(seq.prev);
      remove(node);
      node.node_data;
    };

  let take_opt_l = seq =>
    if (is_empty(seq)) {
      None;
    } else {
      let node = node_of_seq(seq.next);
      remove(node);
      Some(node.node_data);
    };

  let take_opt_r = seq =>
    if (is_empty(seq)) {
      None;
    } else {
      let node = node_of_seq(seq.prev);
      remove(node);
      Some(node.node_data);
    };

  let transfer_l = (s1, s2) => {
    s2.next.prev = s1.prev;
    s1.prev.next = s2.next;
    s2.next = s1.next;
    s1.next.prev = s2;
    s1.prev = s1;
    s1.next = s1;
  };

  let transfer_r = (s1, s2) => {
    s2.prev.next = s1.next;
    s1.next.prev = s2.prev;
    s2.prev = s1.prev;
    s1.prev.next = s2;
    s1.prev = s1;
    s1.next = s1;
  };

  let iter_l = (f, seq) => {
    let rec loop = curr =>
      if (curr !== seq) {
        let node = node_of_seq(curr);
        if (node.node_active) {
          f(node.node_data);
        };
        loop(node.node_next);
      };

    loop(seq.next);
  };

  let iter_r = (f, seq) => {
    let rec loop = curr =>
      if (curr !== seq) {
        let node = node_of_seq(curr);
        if (node.node_active) {
          f(node.node_data);
        };
        loop(node.node_prev);
      };

    loop(seq.prev);
  };

  let iter_node_l = (f, seq) => {
    let rec loop = curr =>
      if (curr !== seq) {
        let node = node_of_seq(curr);
        if (node.node_active) {
          f(node);
        };
        loop(node.node_next);
      };

    loop(seq.next);
  };

  let iter_node_r = (f, seq) => {
    let rec loop = curr =>
      if (curr !== seq) {
        let node = node_of_seq(curr);
        if (node.node_active) {
          f(node);
        };
        loop(node.node_prev);
      };

    loop(seq.prev);
  };

  let fold_l = (f, seq, acc) => {
    let rec loop = (curr, acc) =>
      if (curr === seq) {
        acc;
      } else {
        let node = node_of_seq(curr);
        if (node.node_active) {
          loop(node.node_next, f(node.node_data, acc));
        } else {
          loop(node.node_next, acc);
        };
      };

    loop(seq.next, acc);
  };

  let fold_r = (f, seq, acc) => {
    let rec loop = (curr, acc) =>
      if (curr === seq) {
        acc;
      } else {
        let node = node_of_seq(curr);
        if (node.node_active) {
          loop(node.node_prev, f(node.node_data, acc));
        } else {
          loop(node.node_prev, acc);
        };
      };

    loop(seq.prev, acc);
  };

  let find_node_l = (f, seq) => {
    let rec loop = curr =>
      if (curr !== seq) {
        let node = node_of_seq(curr);
        if (node.node_active) {
          if (f(node.node_data)) {
            node;
          } else {
            loop(node.node_next);
          };
        } else {
          loop(node.node_next);
        };
      } else {
        raise(Not_found);
      };

    loop(seq.next);
  };

  let find_node_r = (f, seq) => {
    let rec loop = curr =>
      if (curr !== seq) {
        let node = node_of_seq(curr);
        if (node.node_active) {
          if (f(node.node_data)) {
            node;
          } else {
            loop(node.node_prev);
          };
        } else {
          loop(node.node_prev);
        };
      } else {
        raise(Not_found);
      };

    loop(seq.prev);
  };

  let find_node_opt_l = (f, seq) =>
    try(Some(find_node_l(f, seq))) {
    | Not_found => None
    };

  let find_node_opt_r = (f, seq) =>
    try(Some(find_node_r(f, seq))) {
    | Not_found => None
    };
};
// [@ocaml.warning "-4"];
// module Lwt_sequence = Lwt_sequence;

// [@ocaml.warning "-4"];

[@ocaml.warning "-3"];
module Lwt_sequence = LwtDllist;
[@ocaml.warning "+3"];

module Storage_map =
  Map.Make({
    type t = int;
    let compare = compare;
  });

type storage = Storage_map.t(unit => unit);

module Main_internal_types = {
  [@ocaml.warning "-37"];

  type underlying = pri | Underlying_and_this_constructor_is_not_used;
  type proxy = pri | Proxy_and_this_constructor_is_not_used;
  type resolved = pri | Resolved_and_this_constructor_is_not_used;
  type pending = pri | Pending_and_this_constructor_is_not_used;

  [@ocaml.warning "+37"];

  type promise('a, 'u, 'c) = {mutable state: state('a, 'u, 'c)}

  and state(_, _, _) =
    | Fulfilled('a): state('a, underlying, resolved)
    | Rejected(exn): state(_, underlying, resolved)
    | Pending(callbacks('a)): state('a, underlying, pending)
    | Proxy(promise('a, _, 'c)): state('a, proxy, 'c)

  and callbacks('a) = {
    mutable regular_callbacks: regular_callback_list('a),
    mutable cancel_callbacks: cancel_callback_list('a),
    mutable how_to_cancel,
    mutable cleanups_deferred: int,
  }
  and regular_callback('a) = resolved_state('a) => unit
  and cancel_callback = unit => unit
  and resolved_state('a) = state('a, underlying, resolved)
  and how_to_cancel =
    | Not_cancelable: how_to_cancel
    | Cancel_this_promise: how_to_cancel
    | Propagate_cancel_to_one(promise(_, _, _)): how_to_cancel
    | Propagate_cancel_to_several(list(promise(_, _, _))): how_to_cancel
  and regular_callback_list('a) =
    | Regular_callback_list_empty
    | Regular_callback_list_concat(
        regular_callback_list('a),
        regular_callback_list('a),
      )
    | Regular_callback_list_implicitly_removed_callback(regular_callback('a))
    | Regular_callback_list_explicitly_removable_callback(
        ref(option(regular_callback('a))),
      )
  and cancel_callback_list(_) =
    | Cancel_callback_list_empty: cancel_callback_list(_)
    | Cancel_callback_list_concat(
        cancel_callback_list('a),
        cancel_callback_list('a),
      )
      : cancel_callback_list('a)
    | Cancel_callback_list_callback(storage, cancel_callback)
      : cancel_callback_list(_)
    | Cancel_callback_list_remove_sequence_node(
        Lwt_sequence.node(promise('a, _, _)),
      )
      : cancel_callback_list('a);
};

open Main_internal_types;

module Public_types = {
  module Result = {
    type result('a, 'b) =
      | Ok('a)
      | Error('b);
  };
  type t(+'a);
  type u(-'a);
  let to_public_promise: promise('a, _, _) => t('a) = Obj.magic;
  let to_public_resolver: promise('a, _, _) => u('a) = Obj.magic;
  [@ocaml.unboxed]
  type packed_promise(_) =
    | Internal(promise('a, _, _)): packed_promise('a);
  let to_internal_promise = (p: t('a)): packed_promise('a) =>
    Internal(Obj.magic(p));
  let to_internal_resolver = (r: u('a)): packed_promise('a) =>
    Internal(Obj.magic(r));

  type lwt_result(+'a) = Result.result('a, exn);
  let state_of_result =
    fun
    | Result.Ok(x) => Fulfilled(x)
    | Result.Error(exn) => Rejected(exn);
};
include Public_types;

module Basic_helpers: {
  let identical: (promise('a, _, _), promise('a, _, _)) => bool;
  let underlying: promise('a, 'u, 'c) => promise('a, underlying, 'c);
  [@ocaml.unboxed]
  type state_changed('a, 'u, 'c) =
    | State_may_have_changed(promise('a, 'u, 'c));
  let set_promise_state:
    (promise('a, _, _), state('a, 'u, 'c)) => state_changed('a, 'u, 'c);
  [@ocaml.unboxed]
  type may_now_be_proxy('a) =
    | State_may_now_be_pending_proxy(promise('a, _, pending))
      : may_now_be_proxy('a);
  let may_now_be_proxy:
    promise('a, underlying, pending) => may_now_be_proxy('a);
} = {
  let identical = (p1, p2) =>
    to_public_promise(p1) === to_public_promise(p2);
  let rec underlying:
    type u c. promise('a, u, c) => promise('a, underlying, c) =
    p =>
      switch (p.state) {
      | Fulfilled(_) => (p: promise(_, underlying, _))
      | Rejected(_) => p
      | Pending(_) => p
      | Proxy(p') =>
        let p'' = underlying(p');
        if (!identical(p'', p')) {
          p.state = Proxy(p'');
        };
        p'';
      };
  [@ocaml.unboxed]
  type state_changed('a, 'u, 'c) =
    | State_may_have_changed(promise('a, 'u, 'c));
  let set_promise_state = (p, state) => {
    let p: promise(_, _, _) = Obj.magic(p);
    p.state = state;
    State_may_have_changed(p);
  };
  [@ocaml.unboxed]
  type may_now_be_proxy('a) =
    | State_may_now_be_pending_proxy(promise('a, _, pending))
      : may_now_be_proxy('a);
  let may_now_be_proxy = p => State_may_now_be_pending_proxy(p);
  /* Many functions, for example [Lwt.bind] and [Lwt.join], create a fresh
     pending promise [p] and return it to the user.

     They do not return a corresponding resolver. That means that only the
     function itself (typically, a callback registered by it) can resolve [p].
     The only thing the user can do directly is try to cancel [p], but, since
     [p] is not an initial promise, the cancellation attempt simply propagates
     past [p] to [p]'s predecessors. If that eventually results in canceling
     [p], it will be through the normal mechanisms of the function (e.g.
     [Lwt.bind]'s callback).

     As a result, the only possible state change, before the callback, is that
     [p] may have become a proxy. Now,

     - If [p] does not undergo this state change and become a proxy, it remains
       an underlying, pending promise.
     - If [p] does become a proxy, it will be a proxy for another promise [p']
       created fresh by [Lwt.bind], to which this same argument applies. See
       [make_into_proxy].

     So, by induction on the length of the proxy ([Proxy _]) chain, at the time
     the callback is called, [p] is either an underlying, pending promise, or a
     proxy for a pending promise.

     The cast

       let State_may_now_be_pending_proxy p = may_now_be_proxy p in ...

     encodes the possibility of this state change. It replaces a reference

       p : ('a, underlying, pending)

     with

       p : ('a, $Unknown, pending)

     and is typically seen at the beginning of callbacks registered by
     [Lwt.bind] and similar functions.

     The cast is a no-op cast. The introduction and immediate elimination of
     [State_may_have_changed _] seems to be optimized away even on old versions
     of OCaml. */
};
open Basic_helpers;

module Sequence_associated_storage: {
  /* Public interface */
  type key('v);
  let new_key: unit => key(_);
  let get: key('v) => option('v);
  let with_value: (key('v), option('v), unit => 'b) => 'b;
  /* Internal interface */
  let current_storage: ref(storage);
} = {
  /* The idea behind sequence-associated storage is to preserve some values
     during a call to [bind] or other sequential composition operation, and
     restore those values in the callback function:

       Lwt.with_value my_key (Some "foo") (fun () ->
       p >|= fun () ->
       assert (Lwt.get my_key = Some "foo"))
         (* Will succeed even if this callback is called later. *)

     Note that it does not matter that the callback is defined within an
     argument of [with_value], i.e., this does the same:

       let f = fun () -> assert (Lwt.get my_key = Some "foo") in
       Lwt.with_value my_key (Some "foo") (fun () -> p >|= f)

     All that matters is that the top-most sequencing operation (in this case,
     map) is executed by that argument.

     This is implemented using a single global heterogeneous key-value map.
     Sequential composition functions snapshot this map when they are called,
     and restore the snapshot right before calling the user's callback. The same
     happens for cancel triggers added by [on_cancel].

     Maintainer's note: I think using this mechanism should be discouraged in
     new code. */
  type key('v) = {
    id: int,
    mutable value: option('v),
  };
  let next_key_id = ref(0);
  let new_key = () => {
    let id = next_key_id^;
    next_key_id := id + 1;
    {id, value: None};
  };
  let current_storage = ref(Storage_map.empty);
  let get = key =>
    if (Storage_map.mem(key.id, current_storage^)) {
      let refresh = Storage_map.find(key.id, current_storage^);
      refresh();
      let value = key.value;
      key.value = None;
      value;
    } else {
      None;
    };
  let with_value = (key, value, f) => {
    let new_storage =
      switch (value) {
      | Some(_) =>
        let refresh = () => key.value = value;
        Storage_map.add(key.id, refresh, current_storage^);
      | None => Storage_map.remove(key.id, current_storage^)
      };
    let saved_storage = current_storage^;
    current_storage := new_storage;
    try({
      let result = f();
      current_storage := saved_storage;
      result;
    }) {
    | exn =>
      current_storage := saved_storage;
      raise(exn);
    };
  };
};

include Sequence_associated_storage;

module Pending_callbacks: {
  /* Mutating callback lists attached to pending promises */
  let add_implicitly_removed_callback:
    (callbacks('a), regular_callback('a)) => unit;
  let add_explicitly_removable_callback_to_each_of:
    (list(t('a)), regular_callback('a)) => unit;
  let add_explicitly_removable_callback_and_give_remove_function:
    (list(t('a)), regular_callback('a), unit) => unit;
  let add_cancel_callback: (callbacks('a), unit => unit) => unit;
  let merge_callbacks: (~from: callbacks('a), ~into: callbacks('a)) => unit;
} = {
  let concat_regular_callbacks = (l1, l2) =>
    [@ocaml.warning "-4"]
    (
      switch (l1, l2) {
      | (Regular_callback_list_empty, _) => l2
      | (_, Regular_callback_list_empty) => l1
      | (_, _) => [@implicit_arity] Regular_callback_list_concat(l1, l2)
      }
    );
  let concat_cancel_callbacks = (l1, l2) =>
    [@ocaml.warning "-4"]
    (
      switch (l1, l2) {
      | (Cancel_callback_list_empty, _) => l2
      | (_, Cancel_callback_list_empty) => l1
      | (_, _) => [@implicit_arity] Cancel_callback_list_concat(l1, l2)
      }
    );
  /* In a callback list, filters out cells of explicitly removable callbacks
     that have been removed. */
  let rec clean_up_callback_cells =
    fun
    | Regular_callback_list_explicitly_removable_callback({contents: None}) =>
      Regular_callback_list_empty
    | (
        Regular_callback_list_explicitly_removable_callback({
          contents: Some(_),
        }) |
        Regular_callback_list_implicitly_removed_callback(_) |
        Regular_callback_list_empty
      ) as callbacks => callbacks
    | [@implicit_arity] Regular_callback_list_concat(l1, l2) => {
        let l1 = clean_up_callback_cells(l1);
        let l2 = clean_up_callback_cells(l2);
        concat_regular_callbacks(l1, l2);
      };
  /* See [clear_explicitly_removable_callback_cell] and [merge_callbacks]. */
  let cleanup_throttle = 42;
  /* Explicitly removable callbacks are added (mainly) by [Lwt.choose] and its
     similar functions. In [Lwt.choose [p; p']], if [p'] resolves first, the
     callback added by [Lwt.choose] to [p] is removed.

     The removal itself is accomplished when this function clears the reference
     cell [cell], which contains the reference to that callback.

     If [p] is a long-pending promise that repeatedly participates in
     [Lwt.choose], perhaps in a loop, it will accumulate a large number of
     cleared reference cells in this fashion. To avoid a memory leak, they must
     be cleaned up. However, the cells are not cleaned up on *every* removal,
     presumably because scanning the callback list that often, and rebuilding
     it, can get expensive.

     Cleanup is throttled by maintaining a counter, [cleanups_deferred], on each
     pending promise. The counter is incremented each time this function wants
     to clean the callback list (right after clearing a cell). When the counter
     reaches [cleanup_throttle], the callback list is actually scanned and
     cleared callback cells are removed. */
  let clear_explicitly_removable_callback_cell =
      (cell, ~originally_added_to as ps) => {
    cell := None;
    /* Go through the promises the cell had originally been added to, and either
       defer a cleanup, or actually clean up their callback lists. */
    ps
    |> List.iter(p => {
         let Internal(p) = to_internal_promise(p);
         switch (underlying(p).state) {
         /* Some of the promises may already have been resolved at the time this
            function is called. */
         | Fulfilled(_) => ()
         | Rejected(_) => ()
         | Pending(callbacks) =>
           switch (callbacks.regular_callbacks) {
           /* If the promise has only one regular callback, and it is removable, it
              must have been the cell cleared in this function, above. In that
              case, just set its callback list to empty. */
           | Regular_callback_list_explicitly_removable_callback(_) =>
             callbacks.regular_callbacks = Regular_callback_list_empty
           /* Maintainer's note: I think this function shouldn't try to trigger a
              cleanup in the first two cases, but I am preserving them for now, as
              this is how the code was written in the past. */
           | Regular_callback_list_empty
           | Regular_callback_list_implicitly_removed_callback(_)
           | Regular_callback_list_concat(_) =>
             let cleanups_deferred = callbacks.cleanups_deferred + 1;
             if (cleanups_deferred > cleanup_throttle) {
               callbacks.cleanups_deferred = 0;
               callbacks.regular_callbacks =
                 clean_up_callback_cells(callbacks.regular_callbacks);
             } else {
               callbacks.cleanups_deferred = cleanups_deferred;
             };
           }
         };
       });
  };
  /* Concatenates both kinds of callbacks on [~from] to the corresponding lists
     of [~into]. The callback lists on [~from] are *not* then cleared, because
     this function is called only by [Sequential_composition.make_into_proxy],
     which immediately changes the state of [~from] and loses references to the
     original callback lists.

     The [cleanups_deferred] fields of both promises are summed, and if the sum
     exceeds [cleanup_throttle], a cleanup of regular callbacks is triggered.
     This is to prevent memory leaks; see
     [clear_explicitly_removable_callback_cell]. */
  let merge_callbacks = (~from, ~into) => {
    let regular_callbacks =
      concat_regular_callbacks(
        into.regular_callbacks,
        from.regular_callbacks,
      );
    let cleanups_deferred = into.cleanups_deferred + from.cleanups_deferred;
    let (regular_callbacks, cleanups_deferred) =
      if (cleanups_deferred > cleanup_throttle) {
        (clean_up_callback_cells(regular_callbacks), 0);
      } else {
        (regular_callbacks, cleanups_deferred);
      };
    let cancel_callbacks =
      concat_cancel_callbacks(into.cancel_callbacks, from.cancel_callbacks);
    into.regular_callbacks = regular_callbacks;
    into.cancel_callbacks = cancel_callbacks;
    into.cleanups_deferred = cleanups_deferred;
  };
  /* General, internal, function for adding a regular callback. */
  let add_regular_callback_list_node = (callbacks, node) =>
    callbacks.regular_callbacks = (
      switch (callbacks.regular_callbacks) {
      | Regular_callback_list_empty => node
      | (
          Regular_callback_list_implicitly_removed_callback(_) |
          Regular_callback_list_explicitly_removable_callback(_) |
          Regular_callback_list_concat(_)
        ) as existing =>
        [@implicit_arity] Regular_callback_list_concat(node, existing)
      }
    );
  let add_implicitly_removed_callback = (callbacks, f) =>
    add_regular_callback_list_node(
      callbacks,
      Regular_callback_list_implicitly_removed_callback(f),
    );
  /* Adds [callback] as removable to each promise in [ps]. The first promise in
     [ps] to trigger [callback] removes [callback] from the other promises; this
     guarantees that [callback] is called at most once. All the promises in [ps]
     must be pending.

     This is an internal function, indirectly used by the implementations of
     [Lwt.choose] and related functions. */
  let add_explicitly_removable_callback_and_give_cell = (ps, f) => {
    let rec cell = ref(Some(self_removing_callback_wrapper))
    and self_removing_callback_wrapper = result => {
      clear_explicitly_removable_callback_cell(cell, ~originally_added_to=ps);
      f(result);
    };
    let node = Regular_callback_list_explicitly_removable_callback(cell);
    ps
    |> List.iter(p => {
         let Internal(p) = to_internal_promise(p);
         switch (underlying(p).state) {
         | Pending(callbacks) =>
           add_regular_callback_list_node(callbacks, node)
         | Fulfilled(_) => assert(false)
         | Rejected(_) => assert(false)
         };
       });
    cell;
  };
  let add_explicitly_removable_callback_to_each_of = (ps, f) =>
    ignore(add_explicitly_removable_callback_and_give_cell(ps, f));
  /* This is basically just to support [Lwt.protected], which needs to remove
     the callback in circumstances other than the callback being called. */
  let add_explicitly_removable_callback_and_give_remove_function = (ps, f) => {
    let cell = add_explicitly_removable_callback_and_give_cell(ps, f);
    () =>
      clear_explicitly_removable_callback_cell(cell, ~originally_added_to=ps);
  };
  let add_cancel_callback = (callbacks, f) => {
    /* Ugly cast :( */
    let cast_cancel_callback: (unit => unit) => cancel_callback = Obj.magic;
    let f = cast_cancel_callback(f);
    let node =
      [@implicit_arity] Cancel_callback_list_callback(current_storage^, f);
    callbacks.cancel_callbacks = (
      switch (callbacks.cancel_callbacks) {
      | Cancel_callback_list_empty => node
      | Cancel_callback_list_callback(_)
      | Cancel_callback_list_remove_sequence_node(_)
      | Cancel_callback_list_concat(_) =>
        [@implicit_arity]
        Cancel_callback_list_concat(node, callbacks.cancel_callbacks)
      }
    );
  };
};

open Pending_callbacks;

module Resolution_loop: {
  /* All user-provided callbacks are called by Lwt only through this module. It
     tracks the current callback stack depth, and decides whether each callback
     call should be deferred or not. */
  /* Internal interface used only in this module Lwt */
  let resolve:
    (
      ~allow_deferring: bool=?,
      ~maximum_callback_nesting_depth: int=?,
      promise('a, underlying, pending),
      resolved_state('a)
    ) =>
    state_changed('a, underlying, resolved);
  let run_callbacks_or_defer_them:
    (
      ~allow_deferring: bool=?,
      ~maximum_callback_nesting_depth: int=?,
      callbacks('a),
      resolved_state('a)
    ) =>
    unit;
  let run_callback_or_defer_it:
    (
      ~run_immediately_and_ensure_tail_call: bool=?,
      ~callback: unit => 'a,
      ~if_deferred: unit => ('a, regular_callback('b), resolved_state('b))
    ) =>
    'a;
  let handle_with_async_exception_hook: ('a => unit, 'a) => unit;
  /* Internal interface exposed to other modules in Lwt */
  let abandon_wakeups: unit => unit;
  /* Public interface */
  exception Canceled;
  let async_exception_hook: ref(exn => unit);
} = {
  /* When Lwt needs to call a callback, it enters the resolution loop. This
     typically happens when Lwt sets the state of one promise to [Fulfilled _]
     or [Rejected _]. The callbacks that were attached to the promise when it
     was pending must then be called.

     This also happens in a few other situations. For example, when [Lwt.bind]
     is called on a promise, but that promise is already resolved, the callback
     passed to [bind] must be called.

     The callbacks triggered during the resolution loop might resolve more
     promises, triggering more callbacks, and so on. This is what makes the
     resolution loop a {e loop}.

     Lwt generally tries to call each callback immediately. However, this can
     lead to a progressive deepening of the call stack, until there is a stack
     overflow. This can't be avoided by doing tail calls, because Lwt always
     needs to do exception handling around callbacks calls: each callback call
     is followed by an exception handler. Instead, what Lwt does is track the
     current callback call depth. Once that depth reaches a certain number,
     [default_maximum_callback_nesting_depth], defined below, further callbacks
     are deferred into a queue instead. That queue is drained when Lwt exits
     from the top-most callback call that triggered the resolution loop in the
     first place.

     To ensure that this deferral mechanism is always properly invoked, all
     callbacks called by Lwt are called through one of three functions provided
     by this module:

     - [resolve], which calls all the callbacks associated to a pending promise
       (and resolves it, changing its state).
     - [run_callbacks_or_defer_them], which is internally used by [resolve] to
       call callbacks that are in a record of type ['a callbacks], which records
       are associated with pending promises. This function is exposed because
       the current implementation of [Lwt.cancel] needs to call it directly.
       Promise resolution and callback calling are separated in a unique way in
       [cancel].
     - [run_callback_or_defer_it], which is used by [Lwt.bind] and similar
       functions to call single callbacks when the promises passed to
       [Lwt.bind], etc., are already resolved.

     Current Lwt actually has a messy mix of callback-calling behaviors. For
     example, [Lwt.bind] is expected to always call its callback immediately,
     while [Lwt.wakeup_later] is expected to defer all callbacks of the promise
     resolved, {e unless} Lwt is not already inside the resolution loop.

     We planned to make these behaviors uniform in Lwt 4.0.0, but decided
     against it due to the risk of breaking users. See

     - https://github.com/ocsigen/lwt/pull/500
     - https://github.com/ocsigen/lwt/pull/519

     As part of the preparation for the change, the above callback-invoking
     functions support several optional arguments to emulate the various
     behaviors. We decided not to remove this machinery, because we might want
     to expose different APIs to Lwt in the future.

     - [~allow_deferring:false] allows ignoring the callback stack depth, and
       calling the callbacks immediately. This emulates the old resolution
       behavior.
     - [~maximum_callback_nesting_depth:1] allows limiting the depth which
       triggers deferral on a per-call-site basis. This is used by
       [Lwt.wakeup_later].
     - [~run_immediately_and_ensure_tail_call:true] is like
       [~allow_deferring:false], which ignores the callback stack depth.
       However, to ensure that the callback is tail-called, Lwt doesn't even
       update the callback stack depth for the benefit of *other* callback
       calls. It just blindly calls the callback.

     See discussion of callback-calling semantics in:

       https://github.com/ocsigen/lwt/issues/329

     * Context

     The resolution loop effectively handles all promises that can be resolved
     immediately, without blocking on I/O. A complete program that does I/O
     calls [Lwt_main.run]. See "No I/O" in the Overview. */
  let async_exception_hook =
    ref(exn => {
      prerr_string("Fatal error: exception ");
      prerr_string(Printexc.to_string(exn));
      prerr_char('\n');
      Printexc.print_backtrace(stderr);
      flush(stderr);
      exit(2);
    });
  let handle_with_async_exception_hook = (f, v) =>
    /* Note that this function does not care if [f] evaluates to a promise. In
       particular, if [f v] evaluates to [p] and [p] is already rejected or will
       be reject later, it is not the responsibility of this function to pass
       the exception to [!async_exception_hook]. */
    try(f(v)) {
    | exn => async_exception_hook^(exn)
    };
  exception Canceled;
  /* Runs the callbacks (formerly) associated to a promise. Cancel callbacks are
     run first, if the promise was canceled. These are followed by regular
     callbacks.

     The reason for the "formerly" is that the promise's state has already been
     set to [Fulfilled _] or [Rejected _], so the callbacks are no longer
     reachable through the promise reference. This is why the direct [callbacks]
     record must be given to this function. */
  let run_callbacks =
      (callbacks: callbacks('a), result: resolved_state('a)): unit => {
    let run_cancel_callbacks = fs => {
      let rec iter_callback_list = (fs, rest) =>
        switch (fs) {
        | Cancel_callback_list_empty => iter_list(rest)
        | [@implicit_arity] Cancel_callback_list_callback(storage, f) =>
          current_storage := storage;
          handle_with_async_exception_hook(f, ());
          iter_list(rest);
        | Cancel_callback_list_remove_sequence_node(node) =>
          Lwt_sequence.remove(node);
          iter_list(rest);
        | [@implicit_arity] Cancel_callback_list_concat(fs, fs') =>
          iter_callback_list(fs, [fs', ...rest])
        }
      and iter_list = rest =>
        switch (rest) {
        | [] => ()
        | [fs, ...rest] => iter_callback_list(fs, rest)
        };
      iter_callback_list(fs, []);
    };
    let run_regular_callbacks = fs => {
      let rec iter_callback_list = (fs, rest) =>
        switch (fs) {
        | Regular_callback_list_empty => iter_list(rest)
        | Regular_callback_list_implicitly_removed_callback(f) =>
          f(result);
          iter_list(rest);
        | Regular_callback_list_explicitly_removable_callback({
            contents: None,
          }) =>
          iter_list(rest)
        | Regular_callback_list_explicitly_removable_callback({
            contents: Some(f),
          }) =>
          f(result);
          iter_list(rest);
        | [@implicit_arity] Regular_callback_list_concat(fs, fs') =>
          iter_callback_list(fs, [fs', ...rest])
        }
      and iter_list = rest =>
        switch (rest) {
        | [] => ()
        | [fs, ...rest] => iter_callback_list(fs, rest)
        };
      iter_callback_list(fs, []);
    };
    /* Pattern matching is much faster than polymorphic comparison. */
    let is_canceled =
      switch (result) {
      | Rejected(Canceled) => true
      | Rejected(_) => false
      | Fulfilled(_) => false
      };
    if (is_canceled) {
      run_cancel_callbacks(callbacks.cancel_callbacks);
    };
    run_regular_callbacks(callbacks.regular_callbacks);
  };
  let default_maximum_callback_nesting_depth = 42;
  let current_callback_nesting_depth = ref(0);
  [@ocaml.unboxed]
  type deferred_callbacks =
    | Deferred((callbacks('a), resolved_state('a))): deferred_callbacks;
  let deferred_callbacks: Queue.t(deferred_callbacks) = Queue.create();
  /* Before entering a resolution loop, it is necessary to take a snapshot of
     the current state of sequence-associated storage. This is because many of
     the callbacks that will be run will modify the storage. The storage is
     restored to the snapshot when the resolution loop is exited. */
  let enter_resolution_loop = () => {
    current_callback_nesting_depth := current_callback_nesting_depth^ + 1;
    let storage_snapshot = current_storage^;
    storage_snapshot;
  };
  let leave_resolution_loop = (storage_snapshot: storage): unit => {
    if (current_callback_nesting_depth^ == 1) {
      while (!Queue.is_empty(deferred_callbacks)) {
        let [@implicit_arity] Deferred(callbacks, result) =
          Queue.pop(deferred_callbacks);
        run_callbacks(callbacks, result);
      };
    };
    current_callback_nesting_depth := current_callback_nesting_depth^ - 1;
    current_storage := storage_snapshot;
  };
  let run_in_resolution_loop = f => {
    let storage_snapshot = enter_resolution_loop();
    let result = f();
    leave_resolution_loop(storage_snapshot);
    result;
  };
  /* This is basically a hack to fix https://github.com/ocsigen/lwt/issues/48.
     If currently resolving promises, it immediately exits all recursive
     entries of the resolution loop, goes to the top level, runs any deferred
     callbacks, and exits the top-level resolution loop.

     The name should probably be [abaondon_resolution_loop]. */
  let abandon_wakeups = () =>
    if (current_callback_nesting_depth^ != 0) {
      leave_resolution_loop(Storage_map.empty);
    };
  let run_callbacks_or_defer_them =
      (
        ~allow_deferring=true,
        ~maximum_callback_nesting_depth=default_maximum_callback_nesting_depth,
        callbacks,
        result,
      ) => {
    let should_defer =
      allow_deferring
      && current_callback_nesting_depth^ >= maximum_callback_nesting_depth;
    if (should_defer) {
      Queue.push(
        [@implicit_arity] Deferred(callbacks, result),
        deferred_callbacks,
      );
    } else {
      run_in_resolution_loop(() => run_callbacks(callbacks, result));
    };
  };
  let resolve =
      (~allow_deferring=?, ~maximum_callback_nesting_depth=?, p, result) => {
    let Pending(callbacks) = p.state;
    let p = set_promise_state(p, result);
    run_callbacks_or_defer_them(
      ~allow_deferring?,
      ~maximum_callback_nesting_depth?,
      callbacks,
      result,
    );
    p;
  };
  let run_callback_or_defer_it =
      (
        ~run_immediately_and_ensure_tail_call=false,
        ~callback as f,
        ~if_deferred,
      ) =>
    if (run_immediately_and_ensure_tail_call) {
      f();
    } else {
      let should_defer =
        current_callback_nesting_depth^
        >= default_maximum_callback_nesting_depth;
      if (should_defer) {
        let (immediate_result, deferred_callback, deferred_result) =
          if_deferred();
        let deferred_record = {
          regular_callbacks:
            Regular_callback_list_implicitly_removed_callback(
              deferred_callback,
            ),
          cancel_callbacks: Cancel_callback_list_empty,
          how_to_cancel: Not_cancelable,
          cleanups_deferred: 0,
        };
        Queue.push(
          [@implicit_arity] Deferred(deferred_record, deferred_result),
          deferred_callbacks,
        );
        immediate_result;
      } else {
        run_in_resolution_loop(() => f());
      };
    };
};

include Resolution_loop;

module Resolving: {
  let wakeup_later_result: (u('a), lwt_result('a)) => unit;
  let wakeup_later: (u('a), 'a) => unit;
  let wakeup_later_exn: (u(_), exn) => unit;
  let wakeup_result: (u('a), lwt_result('a)) => unit;
  let wakeup: (u('a), 'a) => unit;
  let wakeup_exn: (u(_), exn) => unit;
  let cancel: t('a) => unit;
} = {
  /* Note that this function deviates from the "ideal" callback deferral
     behavior: it runs callbacks directly on the current stack. It should
     therefore be possible to cause a stack overflow using this function. */
  let wakeup_general = (api_function_name, r, result) => {
    let Internal(p) = to_internal_resolver(r);
    let p = underlying(p);
    switch (p.state) {
    | Rejected(Canceled) => ()
    | Fulfilled(_) =>
      Printf.ksprintf(invalid_arg, "Lwt.%s", api_function_name)
    | Rejected(_) => Printf.ksprintf(invalid_arg, "Lwt.%s", api_function_name)
    | Pending(_) =>
      let result = state_of_result(result);
      let State_may_have_changed(p) =
        resolve(~allow_deferring=false, p, result);
      ignore(p);
    };
  };
  let wakeup_result = (r, result) =>
    wakeup_general("wakeup_result", r, result);
  let wakeup = (r, v) => wakeup_general("wakeup", r, Result.Ok(v));
  let wakeup_exn = (r, exn) =>
    wakeup_general("wakeup_exn", r, Result.Error(exn));
  let wakeup_later_general = (api_function_name, r, result) => {
    let Internal(p) = to_internal_resolver(r);
    let p = underlying(p);
    switch (p.state) {
    | Rejected(Canceled) => ()
    | Fulfilled(_) =>
      Printf.ksprintf(invalid_arg, "Lwt.%s", api_function_name)
    | Rejected(_) => Printf.ksprintf(invalid_arg, "Lwt.%s", api_function_name)
    | Pending(_) =>
      let result = state_of_result(result);
      let State_may_have_changed(p) =
        resolve(~maximum_callback_nesting_depth=1, p, result);
      ignore(p);
    };
  };
  let wakeup_later_result = (r, result) =>
    wakeup_later_general("wakeup_later_result", r, result);
  let wakeup_later = (r, v) =>
    wakeup_later_general("wakeup_later", r, Result.Ok(v));
  let wakeup_later_exn = (r, exn) =>
    wakeup_later_general("wakeup_later_exn", r, Result.Error(exn));
  [@ocaml.unboxed]
  type packed_callbacks =
    | Packed(callbacks(_)): packed_callbacks;
  /* Note that this function deviates from the "ideal" callback deferral
     behavior: it runs callbacks directly on the current stack. It should
     therefore be possible to cause a stack overflow using this function. */
  let cancel = p => {
    let canceled_result = Rejected(Canceled);
    /* Walks the promise dependency graph backwards, looking for cancelable
       initial promises, and cancels (only) them.

       Found initial promises are canceled immediately, as they are found, by
       setting their state to [Rejected Canceled]. This is to prevent them from
       being "found twice" if they are reachable by two or more distinct paths
       through the promise dependency graph.

       The callbacks of these initial promises are then run, in a separate
       phase. These callbacks propagate cancellation forwards to any dependent
       promises. See "Cancellation" in the Overview. */
    let propagate_cancel: promise(_, _, _) => list(packed_callbacks) =
      p => {
        let rec cancel_and_collect_callbacks:
          'a 'u 'c.
          (list(packed_callbacks), promise('a, 'u, 'c)) =>
          list(packed_callbacks)
         =
          (type c, callbacks_accumulator, p: promise(_, _, c)) => {
            let p = underlying(p);
            switch (p.state) {
            /* If the promise is not still pending, it can't be canceled. */
            | Fulfilled(_) => callbacks_accumulator
            | Rejected(_) => callbacks_accumulator
            | Pending(callbacks) =>
              switch (callbacks.how_to_cancel) {
              | Not_cancelable => callbacks_accumulator
              | Cancel_this_promise =>
                let State_may_have_changed(p) =
                  set_promise_state(p, canceled_result);
                ignore(p);
                [Packed(callbacks), ...callbacks_accumulator];
              | Propagate_cancel_to_one(p') =>
                cancel_and_collect_callbacks(callbacks_accumulator, p')
              | Propagate_cancel_to_several(ps) =>
                List.fold_left(
                  cancel_and_collect_callbacks,
                  callbacks_accumulator,
                  ps,
                )
              }
            };
          };
        cancel_and_collect_callbacks([], p);
      };
    let Internal(p) = to_internal_promise(p);
    let callbacks = propagate_cancel(p);
    callbacks
    |> List.iter((Packed(callbacks)) =>
         run_callbacks_or_defer_them(
           ~allow_deferring=false,
           callbacks,
           canceled_result,
         )
       );
  };
};

include Resolving;

module Trivial_promises: {
  let return: 'a => t('a);
  let fail: exn => t(_);
  let of_result: lwt_result('a) => t('a);
  let return_unit: t(unit);
  let return_true: t(bool);
  let return_false: t(bool);
  let return_none: t(option(_));
  let return_some: 'a => t(option('a));
  let return_ok: 'a => t(Result.result('a, _));
  let return_error: 'e => t(Result.result(_, 'e));
  let return_nil: t(list(_));
  let fail_with: string => t(_);
  let fail_invalid_arg: string => t(_);
} = {
  let return = v => to_public_promise({state: Fulfilled(v)});
  let of_result = result =>
    to_public_promise({state: state_of_result(result)});
  let fail = exn => to_public_promise({state: Rejected(exn)});
  let return_unit = return();
  let return_none = return(None);
  let return_some = x => return(Some(x));
  let return_nil = return([]);
  let return_true = return(true);
  let return_false = return(false);
  let return_ok = x => return(Result.Ok(x));
  let return_error = x => return(Result.Error(x));
  let fail_with = msg => to_public_promise({state: Rejected(Failure(msg))});
  let fail_invalid_arg = msg =>
    to_public_promise({state: Rejected(Invalid_argument(msg))});
};

include Trivial_promises;

module Pending_promises: {
  /* Internal */
  let new_pending:
    (~how_to_cancel: how_to_cancel) => promise('a, underlying, pending);
  let propagate_cancel_to_several: list(t(_)) => how_to_cancel;
  /* Initial pending promises (public) */
  let wait: unit => (t('a), u('a));
  let task: unit => (t('a), u('a));
  let waiter_of_wakener: u('a) => t('a);
  let add_task_r: Lwt_sequence.t(u('a)) => t('a);
  let add_task_l: Lwt_sequence.t(u('a)) => t('a);
  let protected: t('a) => t('a);
  let no_cancel: t('a) => t('a);
} = {
  let new_pending = (~how_to_cancel) => {
    let state =
      Pending({
        regular_callbacks: Regular_callback_list_empty,
        cancel_callbacks: Cancel_callback_list_empty,
        how_to_cancel,
        cleanups_deferred: 0,
      });
    {state: state};
  };
  let propagate_cancel_to_several = ps => {
    /* Using a dirty cast here to avoid rebuilding the list :( Not bothering
       with the invariants, because [Propagate_cancel_to_several] packs them,
       and code that matches on [Propagate_cancel_to_several] doesn't care about
       them anyway. */
    let cast_promise_list: list(t('a)) => list(promise('a, _, _)) = Obj.magic;
    Propagate_cancel_to_several(cast_promise_list(ps));
  };
  let wait = () => {
    let p = new_pending(~how_to_cancel=Not_cancelable);
    (to_public_promise(p), to_public_resolver(p));
  };
  let task = () => {
    let p = new_pending(~how_to_cancel=Cancel_this_promise);
    (to_public_promise(p), to_public_resolver(p));
  };
  let waiter_of_wakener = r => {
    let Internal(r) = to_internal_resolver(r);
    let p = r;
    to_public_promise(p);
  };
  let cast_sequence_node =
      (
        node: Lwt_sequence.node(u('a)),
        _actual_content: promise('a, 'u, 'c),
      )
      : Lwt_sequence.node(promise('a, 'u, 'c)) =>
    Obj.magic(node);
  let add_task_r = sequence => {
    let p = new_pending(~how_to_cancel=Cancel_this_promise);
    let node = Lwt_sequence.add_r(to_public_resolver(p), sequence);
    let node = cast_sequence_node(node, p);
    let Pending(callbacks) = p.state;
    callbacks.cancel_callbacks =
      Cancel_callback_list_remove_sequence_node(node);
    to_public_promise(p);
  };
  let add_task_l = sequence => {
    let p = new_pending(~how_to_cancel=Cancel_this_promise);
    let node = Lwt_sequence.add_l(to_public_resolver(p), sequence);
    let node = cast_sequence_node(node, p);
    let Pending(callbacks) = p.state;
    callbacks.cancel_callbacks =
      Cancel_callback_list_remove_sequence_node(node);
    to_public_promise(p);
  };
  let protected = p => {
    let Internal(p_internal) = to_internal_promise(p);
    switch (underlying(p_internal).state) {
    | Fulfilled(_) => p
    | Rejected(_) => p
    | Pending(_) =>
      let p' = new_pending(~how_to_cancel=Cancel_this_promise);
      let callback = p_result => {
        let State_may_now_be_pending_proxy(p') = may_now_be_proxy(p');
        let p' = underlying(p');
        /* In this callback, [p'] will either still itself be pending, or it
           will have become a proxy for a pending promise. The reasoning for
           this is almost the same as in the comment at [may_now_be_proxy]. The
           differences are:

           - [p'] *is* an initial promise, so it *can* get canceled. However, if
             it does, the [on_cancel] handler installed below will remove this
             callback.
           - [p'] never gets passed to [make_into_proxy], the only effect of
             which is that it cannot be the underlying promise of another
             (proxy) promise. So, [p'] can only appear at the head of a chain of
             [Proxy _] links, and it's not necessary to worry about whether the
             inductive reasoning at [may_now_be_proxy] applies. */
        let State_may_have_changed(p') =
          resolve(~allow_deferring=false, p', p_result);
        ignore(p');
      };
      let remove_the_callback =
        add_explicitly_removable_callback_and_give_remove_function(
          [p],
          callback,
        );
      let Pending(p'_callbacks) = p'.state;
      add_cancel_callback(p'_callbacks, remove_the_callback);
      to_public_promise(p');
    };
  };
  let no_cancel = p => {
    let Internal(p_internal) = to_internal_promise(p);
    switch (underlying(p_internal).state) {
    | Fulfilled(_) => p
    | Rejected(_) => p
    | Pending(p_callbacks) =>
      let p' = new_pending(~how_to_cancel=Not_cancelable);
      let callback = p_result => {
        let State_may_now_be_pending_proxy(p') = may_now_be_proxy(p');
        let p' = underlying(p');
        /* In this callback, [p'] will either still itself be pending, or it
           will have become a proxy for a pending promise. The reasoning for
           this is as in [protected] and [may_now_be_proxy], but even simpler,
           because [p'] is not cancelable. */
        let State_may_have_changed(p') =
          resolve(~allow_deferring=false, p', p_result);
        ignore(p');
      };
      add_implicitly_removed_callback(p_callbacks, callback);
      to_public_promise(p');
    };
  };
};

include Pending_promises;

module Sequential_composition: {
  /* Main interface (public) */
  let bind: (t('a), 'a => t('b)) => t('b);
  let map: ('a => 'b, t('a)) => t('b);
  let catch: (unit => t('a), exn => t('a)) => t('a);
  let finalize: (unit => t('a), unit => t(unit)) => t('a);
  let try_bind: (unit => t('a), 'a => t('b), exn => t('b)) => t('b);
  /* Cancel callbacks (public). */
  let on_cancel: (t('a), unit => unit) => unit;
  /* Non-promise callbacks (public) */
  let on_success: (t('a), 'a => unit) => unit;
  let on_failure: (t(_), exn => unit) => unit;
  let on_termination: (t(_), unit => unit) => unit;
  let on_any: (t('a), 'a => unit, exn => unit) => unit;
  /* Backtrace support (internal; for use by the PPX) */
  let backtrace_bind: (exn => exn, t('a), 'a => t('b)) => t('b);
  let backtrace_catch: (exn => exn, unit => t('a), exn => t('a)) => t('a);
  let backtrace_finalize:
    (exn => exn, unit => t('a), unit => t(unit)) => t('a);
  let backtrace_try_bind:
    (exn => exn, unit => t('a), 'a => t('b), exn => t('b)) => t('b);
} = {
  /* There are five primary sequential composition functions: [bind], [map],
     [catch], [finalize], and [try_bind]. Of these, [try_bind] is the most
     general -- all the others can be implemented in terms of it.

     Lwt conflates concurrency with error propagation. If Lwt did not do this,
     there would be only two primary functions: [bind] and [map], and, of these
     two, [bind] is the most general. Since [bind] is the most relevant
     specifically to concurrency, and is also the most familiar function in Lwt,
     its implementation serves as a kind of "model" for the rest. It is the most
     commented, and all the other functions follow a similar pattern to [bind].

     Four of the primary functions have [backtrace_*] versions, which are not
     truly public, and exist to support the PPX. [backtrace_map] does not exist
     because the PPX does not need it.

     The remaining four functions in this section attach "lower-level-ish"
     non-promise-producing callbacks to promises: these are the [on_*]
     functions. Of these, [on_any] is the most general. If Lwt did not conflate
     concurrency with error handling, there would only be one: [on_success]. */
  /* Makes [~user_provided_promise] into a proxy of [~outer_promise]. After
     [make_into_proxy], these two promise references "behave identically."

     Note that this is not symmetric: [user_provided_promise] always becomes the
     proxy. [make_into_proxy] is called only by [bind] and similar functions in
     this module. This means that:

     - the only way for a promise to become a proxy is by being returned from
       the callback given by the user to [bind], or a similar function, and
     - the only way for a promise to become underlying for a promise other than
       itself is to be the outer promise originally returned to the user from
       [bind], or a similar function.

     These two facts are important for reasoning about how and which promises
     can become proxies, underlying, etc.; in particular, it is used in the
     argument in [may_now_be_proxy] for correct predictions about state changes.

     [~outer_promise] is always a pending promise when [make_into_proxy] is
     called; for the explanation, see [may_now_be_proxy] (though the caller of
     [make_into_proxy] always calls [underlying] first to pass the underlying
     pending promise to [make_into_proxy]).

     The reasons proxying is used, instead of adding a callback to
     [~user_provided_promise] to resolve [~outer_promise] when the former
     becomes resolved probably are:

     - Promises have more behaviors than resolution. One would have to add a
       cancellation handler to [~outer_promise] to propagate the cancellation
       back to [~user_provided_promise], for example. It may be easier to just
       think of them as the same promise.
     - If using callbacks, resolving [~user_provided_promise] would not
       immediately resolve [~outer_promise]. Another callback added to
       [~user_provided_promise] might see [~user_provided_promise] resolved,
       but [~outer_promise] still pending, depending on the order in which
       callbacks are run. */
  let make_into_proxy =
      (
        type c,
        ~outer_promise: promise('a, underlying, pending),
        ~user_provided_promise: promise('a, _, c),
      )
      : state_changed('a, underlying, c) => {
    /* Using [p'] as it's the name used inside [bind], etc., for promises with
       this role -- [p'] is the promise returned by the user's function. */
    let p' = underlying(user_provided_promise);
    if (identical(p', outer_promise)) {
      State_may_have_changed(p');
    } else {
      /* We really want to return [State_may_have_changed outer_promise], but
         the reference through [p'] has the right type. */
      switch (p'.state) {
      | Fulfilled(_) =>
        resolve(~allow_deferring=false, outer_promise, p'.state)
      | Rejected(_) =>
        resolve(~allow_deferring=false, outer_promise, p'.state)
      | Pending(p'_callbacks) =>
        let Pending(outer_callbacks) = outer_promise.state;
        merge_callbacks(~from=p'_callbacks, ~into=outer_callbacks);
        outer_callbacks.how_to_cancel = p'_callbacks.how_to_cancel;
        let State_may_have_changed(p') =
          set_promise_state(p', Proxy(outer_promise));
        ignore(p');
        State_may_have_changed(outer_promise);
      };
    };
  };
  /* The state hasn't actually changed, but we still have to wrap
     [outer_promise] for type checking. */
  /* The state of [p'] may instead have changed -- it may have become a
     proxy. However, callers of [make_into_proxy] don't know if
     [user_provided_promise] was a proxy or not (that's why we call
     underlying on it at the top of this function, to get [p']). We can
     therefore take a dangerous shortcut and not bother returning a new
     reference to [user_provided_promise] for shadowing. */
  /* Maintainer's note: a lot of the code below can probably be deduplicated in
     some way, especially if assuming Flambda. */
  let bind = (p, f) => {
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    /* In case [Lwt.bind] needs to defer the call to [f], this function will be
        called to create:

        1. The promise, [p''], that must be returned to the caller immediately.
        2. The callback that resolves [p''].

        [Lwt.bind] defers the call to [f] in two circumstances:

        1. The promise [p] is pending.
        2. The promise [p] is fulfilled, but the current callback call nesting
           depth is such that the call to [f] must go into the callback queue, in
           order to avoid stack overflow.

       Mechanism (2) is currently disabled. It may be used in an alternative Lwt
       API.

       Functions other than [Lwt.bind] have analogous deferral behavior. */
    let create_result_promise_and_callback_if_deferred = () => {
      let p'' = new_pending(~how_to_cancel=Propagate_cancel_to_one(p));
      /* The result promise is a fresh pending promise.

         Initially, trying to cancel this fresh pending promise [p''] will
         propagate the cancellation attempt to [p] (backwards through the
         promise dependency graph). If/when [p] is fulfilled, Lwt will call the
         user's callback [f] below, which will provide a new promise [p'], and
         [p'] will become a proxy of [p'']. At that point, trying to cancel
         [p''] will be equivalent to trying to cancel [p'], so the behavior will
         depend on how the user obtained [p']. */
      let saved_storage = current_storage^;
      let callback = p_result =>
        switch (p_result) {
        | Fulfilled(v) =>
          current_storage := saved_storage;
          let p' =
            try(f(v)) {
            | exn => fail(exn)
            };
          let Internal(p') = to_internal_promise(p');
          /* Run the user's function [f]. */
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          /* [p''] was an underlying promise when it was created above, but it
             may have become a proxy by the time this code is being executed.
             However, it is still either an underlying pending promise, or a
             proxy for a pending promise. Therefore, [may_now_be_proxy] produces
             a reference with the right type variables. We immediately get
             [p'']'s current underlying promise. */
          let State_may_have_changed(p'') =
            make_into_proxy(~outer_promise=p'', ~user_provided_promise=p');
          ignore(p'');
        /* Make the outer promise [p''] behaviorally identical to the promise
           [p'] returned by [f] by making [p'] into a proxy of [p'']. */
        | Rejected(_) as p_result =>
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            resolve(~allow_deferring=false, p'', p_result);
          ignore(p'');
        };
      (to_public_promise(p''), callback);
    };
    switch (p.state) {
    | Fulfilled(v) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => f(v),
        ~if_deferred=
          () => {
            let (p'', callback) =
              create_result_promise_and_callback_if_deferred();
            (p'', callback, p.state);
          },
      )
    | Rejected(_) as result => to_public_promise({state: result})
    | Pending(p_callbacks) =>
      let (p'', callback) = create_result_promise_and_callback_if_deferred();
      add_implicitly_removed_callback(p_callbacks, callback);
      p'';
    };
  };
  let backtrace_bind = (add_loc, p, f) => {
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    let create_result_promise_and_callback_if_deferred = () => {
      let p'' = new_pending(~how_to_cancel=Propagate_cancel_to_one(p));
      let saved_storage = current_storage^;
      let callback = p_result =>
        switch (p_result) {
        | Fulfilled(v) =>
          current_storage := saved_storage;
          let p' =
            try(f(v)) {
            | exn => fail(add_loc(exn))
            };
          let Internal(p') = to_internal_promise(p');
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            make_into_proxy(~outer_promise=p'', ~user_provided_promise=p');
          ignore(p'');
        | Rejected(exn) =>
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            resolve(~allow_deferring=false, p'', Rejected(add_loc(exn)));
          ignore(p'');
        };
      (to_public_promise(p''), callback);
    };
    switch (p.state) {
    | Fulfilled(v) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => f(v),
        ~if_deferred=
          () => {
            let (p'', callback) =
              create_result_promise_and_callback_if_deferred();
            (p'', callback, p.state);
          },
      )
    | Rejected(exn) => to_public_promise({state: Rejected(add_loc(exn))})
    | Pending(p_callbacks) =>
      let (p'', callback) = create_result_promise_and_callback_if_deferred();
      add_implicitly_removed_callback(p_callbacks, callback);
      p'';
    };
  };
  let map = (f, p) => {
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    let create_result_promise_and_callback_if_deferred = () => {
      let p'' = new_pending(~how_to_cancel=Propagate_cancel_to_one(p));
      let saved_storage = current_storage^;
      let callback = p_result =>
        switch (p_result) {
        | Fulfilled(v) =>
          current_storage := saved_storage;
          let p''_result =
            try(Fulfilled(f(v))) {
            | exn => Rejected(exn)
            };
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            resolve(~allow_deferring=false, p'', p''_result);
          ignore(p'');
        | Rejected(_) as p_result =>
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            resolve(~allow_deferring=false, p'', p_result);
          ignore(p'');
        };
      (to_public_promise(p''), callback);
    };
    switch (p.state) {
    | Fulfilled(v) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=
          () =>
            to_public_promise({
              state:
                try(Fulfilled(f(v))) {
                | exn => Rejected(exn)
                },
            }),
        ~if_deferred=
          () => {
            let (p'', callback) =
              create_result_promise_and_callback_if_deferred();
            (p'', callback, p.state);
          },
      )
    | Rejected(_) as result => to_public_promise({state: result})
    | Pending(p_callbacks) =>
      let (p'', callback) = create_result_promise_and_callback_if_deferred();
      add_implicitly_removed_callback(p_callbacks, callback);
      p'';
    };
  };
  let catch = (f, h) => {
    let p =
      try(f()) {
      | exn => fail(exn)
      };
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    let create_result_promise_and_callback_if_deferred = () => {
      let p'' = new_pending(~how_to_cancel=Propagate_cancel_to_one(p));
      let saved_storage = current_storage^;
      let callback = p_result =>
        switch (p_result) {
        | Fulfilled(_) as p_result =>
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            resolve(~allow_deferring=false, p'', p_result);
          ignore(p'');
        | Rejected(exn) =>
          current_storage := saved_storage;
          let p' =
            try(h(exn)) {
            | exn => fail(exn)
            };
          let Internal(p') = to_internal_promise(p');
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            make_into_proxy(~outer_promise=p'', ~user_provided_promise=p');
          ignore(p'');
        };
      (to_public_promise(p''), callback);
    };
    switch (p.state) {
    | Fulfilled(_) => to_public_promise(p)
    | Rejected(exn) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => h(exn),
        ~if_deferred=
          () => {
            let (p'', callback) =
              create_result_promise_and_callback_if_deferred();
            (p'', callback, p.state);
          },
      )
    | Pending(p_callbacks) =>
      let (p'', callback) = create_result_promise_and_callback_if_deferred();
      add_implicitly_removed_callback(p_callbacks, callback);
      p'';
    };
  };
  let backtrace_catch = (add_loc, f, h) => {
    let p =
      try(f()) {
      | exn => fail(exn)
      };
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    let create_result_promise_and_callback_if_deferred = () => {
      let p'' = new_pending(~how_to_cancel=Propagate_cancel_to_one(p));
      let saved_storage = current_storage^;
      let callback = p_result =>
        switch (p_result) {
        | Fulfilled(_) as p_result =>
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            resolve(~allow_deferring=false, p'', p_result);
          ignore(p'');
        | Rejected(exn) =>
          current_storage := saved_storage;
          let p' =
            try(h(exn)) {
            | exn => fail(add_loc(exn))
            };
          let Internal(p') = to_internal_promise(p');
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            make_into_proxy(~outer_promise=p'', ~user_provided_promise=p');
          ignore(p'');
        };
      (to_public_promise(p''), callback);
    };
    switch (p.state) {
    | Fulfilled(_) => to_public_promise(p)
    | Rejected(exn) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => h(add_loc(exn)),
        ~if_deferred=
          () => {
            let (p'', callback) =
              create_result_promise_and_callback_if_deferred();
            (p'', callback, p.state);
          },
      )
    | Pending(p_callbacks) =>
      let (p'', callback) = create_result_promise_and_callback_if_deferred();
      add_implicitly_removed_callback(p_callbacks, callback);
      p'';
    };
  };
  let try_bind = (f, f', h) => {
    let p =
      try(f()) {
      | exn => fail(exn)
      };
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    let create_result_promise_and_callback_if_deferred = () => {
      let p'' = new_pending(~how_to_cancel=Propagate_cancel_to_one(p));
      let saved_storage = current_storage^;
      let callback = p_result =>
        switch (p_result) {
        | Fulfilled(v) =>
          current_storage := saved_storage;
          let p' =
            try(f'(v)) {
            | exn => fail(exn)
            };
          let Internal(p') = to_internal_promise(p');
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            make_into_proxy(~outer_promise=p'', ~user_provided_promise=p');
          ignore(p'');
        | Rejected(exn) =>
          current_storage := saved_storage;
          let p' =
            try(h(exn)) {
            | exn => fail(exn)
            };
          let Internal(p') = to_internal_promise(p');
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            make_into_proxy(~outer_promise=p'', ~user_provided_promise=p');
          ignore(p'');
        };
      (to_public_promise(p''), callback);
    };
    switch (p.state) {
    | Fulfilled(v) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => f'(v),
        ~if_deferred=
          () => {
            let (p'', callback) =
              create_result_promise_and_callback_if_deferred();
            (p'', callback, p.state);
          },
      )
    | Rejected(exn) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => h(exn),
        ~if_deferred=
          () => {
            let (p'', callback) =
              create_result_promise_and_callback_if_deferred();
            (p'', callback, p.state);
          },
      )
    | Pending(p_callbacks) =>
      let (p'', callback) = create_result_promise_and_callback_if_deferred();
      add_implicitly_removed_callback(p_callbacks, callback);
      p'';
    };
  };
  let backtrace_try_bind = (add_loc, f, f', h) => {
    let p =
      try(f()) {
      | exn => fail(exn)
      };
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    let create_result_promise_and_callback_if_deferred = () => {
      let p'' = new_pending(~how_to_cancel=Propagate_cancel_to_one(p));
      let saved_storage = current_storage^;
      let callback = p_result =>
        switch (p_result) {
        | Fulfilled(v) =>
          current_storage := saved_storage;
          let p' =
            try(f'(v)) {
            | exn => fail(add_loc(exn))
            };
          let Internal(p') = to_internal_promise(p');
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            make_into_proxy(~outer_promise=p'', ~user_provided_promise=p');
          ignore(p'');
        | Rejected(exn) =>
          current_storage := saved_storage;
          let p' =
            try(h(exn)) {
            | exn => fail(add_loc(exn))
            };
          let Internal(p') = to_internal_promise(p');
          let State_may_now_be_pending_proxy(p'') = may_now_be_proxy(p'');
          let p'' = underlying(p'');
          let State_may_have_changed(p'') =
            make_into_proxy(~outer_promise=p'', ~user_provided_promise=p');
          ignore(p'');
        };
      (to_public_promise(p''), callback);
    };
    switch (p.state) {
    | Fulfilled(v) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => f'(v),
        ~if_deferred=
          () => {
            let (p'', callback) =
              create_result_promise_and_callback_if_deferred();
            (p'', callback, p.state);
          },
      )
    | Rejected(exn) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => h(add_loc(exn)),
        ~if_deferred=
          () => {
            let (p'', callback) =
              create_result_promise_and_callback_if_deferred();
            (p'', callback, p.state);
          },
      )
    | Pending(p_callbacks) =>
      let (p'', callback) = create_result_promise_and_callback_if_deferred();
      add_implicitly_removed_callback(p_callbacks, callback);
      p'';
    };
  };
  let finalize = (f, f') =>
    try_bind(
      f,
      x => bind(f'(), () => return(x)),
      e => bind(f'(), () => fail(e)),
    );
  let backtrace_finalize = (add_loc, f, f') =>
    backtrace_try_bind(
      add_loc,
      f,
      x => bind(f'(), () => return(x)),
      e => bind(f'(), () => fail(add_loc(e))),
    );
  let on_cancel = (p, f) => {
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    switch (p.state) {
    | Rejected(Canceled) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => handle_with_async_exception_hook(f, ()),
        ~if_deferred=
          () =>
            ((), _ => handle_with_async_exception_hook(f, ()), Fulfilled()),
      )
    | Rejected(_) => ()
    | Fulfilled(_) => ()
    | Pending(callbacks) => add_cancel_callback(callbacks, f)
    };
  };
  let on_success = (p, f) => {
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    let callback_if_deferred = () => {
      let saved_storage = current_storage^;
      result =>
        switch (result) {
        | Fulfilled(v) =>
          current_storage := saved_storage;
          handle_with_async_exception_hook(f, v);
        | Rejected(_) => ()
        };
    };
    switch (p.state) {
    | Fulfilled(v) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => handle_with_async_exception_hook(f, v),
        ~if_deferred=
          () => {
            let callback = callback_if_deferred();
            ((), callback, p.state);
          },
      )
    | Rejected(_) => ()
    | Pending(p_callbacks) =>
      let callback = callback_if_deferred();
      add_implicitly_removed_callback(p_callbacks, callback);
    };
  };
  let on_failure = (p, f) => {
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    let callback_if_deferred = () => {
      let saved_storage = current_storage^;
      result =>
        switch (result) {
        | Fulfilled(_) => ()
        | Rejected(exn) =>
          current_storage := saved_storage;
          handle_with_async_exception_hook(f, exn);
        };
    };
    switch (p.state) {
    | Fulfilled(_) => ()
    | Rejected(exn) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => handle_with_async_exception_hook(f, exn),
        ~if_deferred=
          () => {
            let callback = callback_if_deferred();
            ((), callback, p.state);
          },
      )
    | Pending(p_callbacks) =>
      let callback = callback_if_deferred();
      add_implicitly_removed_callback(p_callbacks, callback);
    };
  };
  let on_termination = (p, f) => {
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    let callback_if_deferred = () => {
      let saved_storage = current_storage^;
      _result => {
        current_storage := saved_storage;
        handle_with_async_exception_hook(f, ());
      };
    };
    switch (p.state) {
    | Fulfilled(_) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => handle_with_async_exception_hook(f, ()),
        ~if_deferred=
          () => {
            let callback = callback_if_deferred();
            ((), callback, p.state);
          },
      )
    | Rejected(_) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => handle_with_async_exception_hook(f, ()),
        ~if_deferred=
          () => {
            let callback = callback_if_deferred();
            ((), callback, p.state);
          },
      )
    | Pending(p_callbacks) =>
      let callback = callback_if_deferred();
      add_implicitly_removed_callback(p_callbacks, callback);
    };
  };
  let on_any = (p, f, g) => {
    let Internal(p) = to_internal_promise(p);
    let p = underlying(p);
    let callback_if_deferred = () => {
      let saved_storage = current_storage^;
      result =>
        switch (result) {
        | Fulfilled(v) =>
          current_storage := saved_storage;
          handle_with_async_exception_hook(f, v);
        | Rejected(exn) =>
          current_storage := saved_storage;
          handle_with_async_exception_hook(g, exn);
        };
    };
    switch (p.state) {
    | Fulfilled(v) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => handle_with_async_exception_hook(f, v),
        ~if_deferred=
          () => {
            let callback = callback_if_deferred();
            ((), callback, p.state);
          },
      )
    | Rejected(exn) =>
      run_callback_or_defer_it(
        ~run_immediately_and_ensure_tail_call=true,
        ~callback=() => handle_with_async_exception_hook(g, exn),
        ~if_deferred=
          () => {
            let callback = callback_if_deferred();
            ((), callback, p.state);
          },
      )
    | Pending(p_callbacks) =>
      let callback = callback_if_deferred();
      add_implicitly_removed_callback(p_callbacks, callback);
    };
  };
};

include Sequential_composition;

/* This belongs with the [protected] and such, but it depends on primitives from
   [Sequential_composition]. */
let wrap_in_cancelable = p => {
  let Internal(p_internal) = to_internal_promise(p);
  let p_underlying = underlying(p_internal);
  switch (p_underlying.state) {
  | Fulfilled(_) => p
  | Rejected(_) => p
  | Pending(_) =>
    let (p', r) = task();
    on_cancel(p', () => cancel(p));
    on_any(p, wakeup(r), wakeup_exn(r));
    p';
  };
};

module Concurrent_composition: {
  let dont_wait: (unit => t(_), exn => unit) => unit;
  let async: (unit => t(_)) => unit;
  let ignore_result: t(_) => unit;
  let both: (t('a), t('b)) => t(('a, 'b));
  let join: list(t(unit)) => t(unit);
  let all: list(t('a)) => t(list('a));
  let choose: list(t('a)) => t('a);
  let pick: list(t('a)) => t('a);
  let nchoose: list(t('a)) => t(list('a));
  let npick: list(t('a)) => t(list('a));
  let nchoose_split: list(t('a)) => t((list('a), list(t('a))));
} = {
  external reraise: exn => 'a = "%reraise";
  let dont_wait = (f, h) => {
    let p =
      try(f()) {
      | exn => fail(exn)
      };
    let Internal(p) = to_internal_promise(p);
    switch (underlying(p).state) {
    | Fulfilled(_) => ()
    | Rejected(exn) => h(exn)
    | Pending(p_callbacks) =>
      let callback = result =>
        switch (result) {
        | Fulfilled(_) => ()
        | Rejected(exn) => h(exn)
        };
      add_implicitly_removed_callback(p_callbacks, callback);
    };
  };
  let async = f => {
    let p =
      try(f()) {
      | exn => fail(exn)
      };
    let Internal(p) = to_internal_promise(p);
    switch (underlying(p).state) {
    | Fulfilled(_) => ()
    | Rejected(exn) => async_exception_hook^(exn)
    | Pending(p_callbacks) =>
      let callback = result =>
        switch (result) {
        | Fulfilled(_) => ()
        | Rejected(exn) => async_exception_hook^(exn)
        };
      add_implicitly_removed_callback(p_callbacks, callback);
    };
  };
  let ignore_result = p => {
    let Internal(p) = to_internal_promise(p);
    switch (underlying(p).state) {
    | Fulfilled(_) => ()
    | Rejected(exn) => reraise(exn)
    | Pending(p_callbacks) =>
      let callback = result =>
        switch (result) {
        | Fulfilled(_) => ()
        | Rejected(exn) => async_exception_hook^(exn)
        };
      add_implicitly_removed_callback(p_callbacks, callback);
    };
  };
  let join = ps => {
    let p' = new_pending(~how_to_cancel=propagate_cancel_to_several(ps));
    let number_pending_in_ps = ref(0);
    let join_result = ref(Fulfilled());
    /* Callback attached to each promise in [ps] that is still pending at the
       time [join] is called. */
    let callback = new_result => {
      let State_may_now_be_pending_proxy(p') = may_now_be_proxy(p');
      switch (new_result) {
      | Fulfilled () => ()
      | Rejected(_) =>
        /* For the first promise in [ps] to be rejected, set the result of the
           [join] to rejected with the same exception.. */
        switch (join_result^) {
        | Fulfilled () => join_result := new_result
        | Rejected(_) => ()
        }
      };
      /* In all cases, decrement the number of promises still pending, and
         resolve the [join] once all promises resolve. */
      number_pending_in_ps := number_pending_in_ps^ - 1;
      if (number_pending_in_ps^ == 0) {
        let p' = underlying(p');
        let State_may_have_changed(p') =
          resolve(~allow_deferring=false, underlying(p'), join_result^);
        ignore(p');
      };
    };
    /* Attach the above callback. Simultaneously count how many pending promises
       there are in [ps] (initially). If that number is zero, the [join] must
       resolve immediately. */
    let rec attach_callback_or_resolve_immediately = ps =>
      switch (ps) {
      | [] =>
        if (number_pending_in_ps^ == 0) {
          to_public_promise({state: join_result^});
        } else {
          to_public_promise(p');
        }
      | [p, ...ps] =>
        let Internal(p) = to_internal_promise(p);
        switch (underlying(p).state) {
        | Pending(p_callbacks) =>
          number_pending_in_ps := number_pending_in_ps^ + 1;
          add_implicitly_removed_callback(p_callbacks, callback);
          attach_callback_or_resolve_immediately(ps);
        | Rejected(_) as p_result =>
          /* As in the callback above, but for already-resolved promises in
             [ps]: reject the [join] with the same exception as in the first
             rejected promise found. [join] still waits for any pending promises
             before actually resolving, though. */
          switch (join_result^) {
          | Fulfilled () => join_result := p_result
          | Rejected(_) => ()
          };
          attach_callback_or_resolve_immediately(ps);
        | Fulfilled () => attach_callback_or_resolve_immediately(ps)
        };
      };
    attach_callback_or_resolve_immediately(ps);
  };
  /* this is 3 words, smaller than the 2 times 2 words a pair of references
     would take. */
  type pair('a, 'b) = {
    mutable x1: option('a),
    mutable x2: option('b),
  };
  let both = (p1, p2) => {
    let pair = {x1: None, x2: None};
    let p1' =
      bind(
        p1,
        v => {
          pair.x1 = Some(v);
          return_unit;
        },
      );
    let p2' =
      bind(
        p2,
        v => {
          pair.x2 = Some(v);
          return_unit;
        },
      );
    join([p1', p2'])
    |> map(() =>
         switch (pair.x1, pair.x2) {
         | (Some(v1), Some(v2)) => (v1, v2)
         | _ => assert(false)
         }
       );
  };
  let all = ps =>
    switch (ps) {
    | [] => return_nil
    | [x] => map(y => [y], x)
    | [x, y] => map(((x, y)) => [x, y], both(x, y))
    | _ =>
      let vs = Array.make(List.length(ps), None);
      ps
      |> List.mapi((index, p) =>
           bind(
             p,
             v => {
               vs[index] = Some(v);
               return_unit;
             },
           )
         )
      |> join
      |> map(() => {
           let rec to_list_unopt = (i, acc) =>
             if (i < 0) {
               acc;
             } else {
               switch (Array.unsafe_get(vs, i)) {
               | None => assert(false)
               | Some(x) => to_list_unopt(i - 1, [x, ...acc])
               };
             };
           to_list_unopt(Array.length(vs) - 1, []);
         });
    };
  /* Maintainer's note: the next few functions are helpers for [choose] and
     [pick]. Perhaps they should be factored into some kind of generic
     [choose]/[pick] implementation, which may actually be optimal anyway with
     Flambda. */
  let count_resolved_promises_in = (ps: list(t(_))) => {
    let accumulate = (total, p) => {
      let Internal(p) = to_internal_promise(p);
      switch (underlying(p).state) {
      | Fulfilled(_) => total + 1
      | Rejected(_) => total + 1
      | Pending(_) => total
      };
    };
    List.fold_left(accumulate, 0, ps);
  };
  /* Evaluates to the [n]th promise in [ps], among only those promises in [ps]
     that are resolved. The caller is expected to ensure that there are at
     least [n] resolved promises in [ps]. */
  let rec nth_resolved = (ps: list(t('a)), n: int): t('a) =>
    switch (ps) {
    | [] => assert(false)
    | [p, ...ps] =>
      let Internal(p') = to_internal_promise(p);
      switch (underlying(p').state) {
      | Pending(_) => nth_resolved(ps, n)
      | Fulfilled(_) =>
        if (n <= 0) {
          p;
        } else {
          nth_resolved(ps, n - 1);
        }
      | Rejected(_) =>
        if (n <= 0) {
          p;
        } else {
          nth_resolved(ps, n - 1);
        }
      };
    };
  /* Like [nth_resolved], but cancels all pending promises found while
     traversing [ps]. */
  let rec nth_resolved_and_cancel_pending =
          (ps: list(t('a)), n: int): t('a) =>
    switch (ps) {
    | [] => assert(false)
    | [p, ...ps] =>
      let Internal(p') = to_internal_promise(p);
      switch (underlying(p').state) {
      | Pending(_) =>
        cancel(p);
        nth_resolved_and_cancel_pending(ps, n);
      | Fulfilled(_) =>
        if (n <= 0) {
          List.iter(cancel, ps);
          p;
        } else {
          nth_resolved_and_cancel_pending(ps, n - 1);
        }
      | Rejected(_) =>
        if (n <= 0) {
          List.iter(cancel, ps);
          p;
        } else {
          nth_resolved_and_cancel_pending(ps, n - 1);
        }
      };
    };
  /* The PRNG state is initialized with a constant to make non-IO-based programs
     deterministic. */
  /* Maintainer's note: is this necessary? */
  let prng = lazy(Random.State.make([||]));
  let choose = ps => {
    if (ps == []) {
      invalid_arg(
        "Lwt.choose [] would return a promise that is pending forever",
      );
    };
    switch (count_resolved_promises_in(ps)) {
    | 0 =>
      let p = new_pending(~how_to_cancel=propagate_cancel_to_several(ps));
      let callback = result => {
        let State_may_now_be_pending_proxy(p) = may_now_be_proxy(p);
        let p = underlying(p);
        let State_may_have_changed(p) =
          resolve(~allow_deferring=false, p, result);
        ignore(p);
      };
      add_explicitly_removable_callback_to_each_of(ps, callback);
      to_public_promise(p);
    | 1 => nth_resolved(ps, 0)
    | n => nth_resolved(ps, Random.State.int(Lazy.force(prng), n))
    };
  };
  let pick = ps => {
    if (ps == []) {
      invalid_arg(
        "Lwt.pick [] would return a promise that is pending forever",
      );
    };
    switch (count_resolved_promises_in(ps)) {
    | 0 =>
      let p = new_pending(~how_to_cancel=propagate_cancel_to_several(ps));
      let callback = result => {
        let State_may_now_be_pending_proxy(p) = may_now_be_proxy(p);
        List.iter(cancel, ps);
        let p = underlying(p);
        let State_may_have_changed(p) =
          resolve(~allow_deferring=false, p, result);
        ignore(p);
      };
      add_explicitly_removable_callback_to_each_of(ps, callback);
      to_public_promise(p);
    | 1 => nth_resolved_and_cancel_pending(ps, 0)
    | n =>
      nth_resolved_and_cancel_pending(
        ps,
        Random.State.int(Lazy.force(prng), n),
      )
    };
  };
  /* If [nchoose ps] or [npick ps] found all promises in [ps] pending, the
     callback added to each promise in [ps] eventually calls this function. The
     function collects promises in [ps] that have become fulfilled, or finds one
     promise in [ps] that has been rejected. It then returns the desired state
     of the final promise: either the list of results collected, or the
     exception found. */
  let rec collect_fulfilled_promises_after_pending =
          (results: list('a), ps: list(t('a))): resolved_state(list('a)) =>
    switch (ps) {
    | [] => Fulfilled(List.rev(results))
    | [p, ...ps] =>
      let Internal(p) = to_internal_promise(p);
      switch (underlying(p).state) {
      | Fulfilled(v) =>
        collect_fulfilled_promises_after_pending([v, ...results], ps)
      | Rejected(_) as result => result
      | Pending(_) => collect_fulfilled_promises_after_pending(results, ps)
      };
    };
  let nchoose = ps => {
    /* If at least one promise in [ps] is found fulfilled, this function is
       called to find all such promises. */
    if (ps == []) {
      invalid_arg(
        "Lwt.nchoose [] would return a promise that is pending forever",
      );
    };
    let rec collect_already_fulfilled_promises_or_find_rejected = (acc, ps) =>
      switch (ps) {
      | [] => return(List.rev(acc))
      | [p, ...ps] =>
        let Internal(p) = to_internal_promise(p);
        switch (underlying(p).state) {
        | Fulfilled(v) =>
          collect_already_fulfilled_promises_or_find_rejected(
            [v, ...acc],
            ps,
          )
        | Rejected(_) as result => to_public_promise({state: result})
        | Pending(_) =>
          collect_already_fulfilled_promises_or_find_rejected(acc, ps)
        };
      };
    /* Looks for already-resolved promises in [ps]. If none are fulfilled or
       rejected, adds a callback to all promises in [ps] (all of which are
       pending). */
    let rec check_for_already_resolved_promises = ps' =>
      switch (ps') {
      | [] =>
        let p = new_pending(~how_to_cancel=propagate_cancel_to_several(ps));
        let callback = _result => {
          let State_may_now_be_pending_proxy(p) = may_now_be_proxy(p);
          let p = underlying(p);
          let result = collect_fulfilled_promises_after_pending([], ps);
          let State_may_have_changed(p) =
            resolve(~allow_deferring=false, p, result);
          ignore(p);
        };
        add_explicitly_removable_callback_to_each_of(ps, callback);
        to_public_promise(p);
      | [p, ...ps] =>
        let Internal(p) = to_internal_promise(p);
        switch (underlying(p).state) {
        | Fulfilled(v) =>
          collect_already_fulfilled_promises_or_find_rejected([v], ps)
        | Rejected(_) as result => to_public_promise({state: result})
        | Pending(_) => check_for_already_resolved_promises(ps)
        };
      };
    let p = check_for_already_resolved_promises(ps);
    p;
  };
  /* See [nchoose]. This function differs only in having additional calls to
     [cancel]. */
  let npick = ps => {
    if (ps == []) {
      invalid_arg(
        "Lwt.npick [] would return a promise that is pending forever",
      );
    };
    let rec collect_already_fulfilled_promises_or_find_rejected = (acc, ps') =>
      switch (ps') {
      | [] =>
        List.iter(cancel, ps);
        return(List.rev(acc));
      | [p, ...ps'] =>
        let Internal(p) = to_internal_promise(p);
        switch (underlying(p).state) {
        | Fulfilled(v) =>
          collect_already_fulfilled_promises_or_find_rejected(
            [v, ...acc],
            ps',
          )
        | Rejected(_) as result =>
          List.iter(cancel, ps);
          to_public_promise({state: result});
        | Pending(_) =>
          collect_already_fulfilled_promises_or_find_rejected(acc, ps')
        };
      };
    let rec check_for_already_resolved_promises = ps' =>
      switch (ps') {
      | [] =>
        let p = new_pending(~how_to_cancel=propagate_cancel_to_several(ps));
        let callback = _result => {
          let State_may_now_be_pending_proxy(p) = may_now_be_proxy(p);
          let p = underlying(p);
          let result = collect_fulfilled_promises_after_pending([], ps);
          List.iter(cancel, ps);
          let State_may_have_changed(p) =
            resolve(~allow_deferring=false, p, result);
          ignore(p);
        };
        add_explicitly_removable_callback_to_each_of(ps, callback);
        to_public_promise(p);
      | [p, ...ps'] =>
        let Internal(p) = to_internal_promise(p);
        switch (underlying(p).state) {
        | Fulfilled(v) =>
          collect_already_fulfilled_promises_or_find_rejected([v], ps')
        | Rejected(_) as result =>
          List.iter(cancel, ps);
          to_public_promise({state: result});
        | Pending(_) => check_for_already_resolved_promises(ps')
        };
      };
    let p = check_for_already_resolved_promises(ps);
    p;
  };
  /* Same general pattern as [npick] and [nchoose]. */
  let nchoose_split = ps => {
    if (ps == []) {
      invalid_arg(
        "Lwt.nchoose_split [] would return a promise that is pending forever",
      );
    };
    let rec finish =
            (
              to_resolve:
                promise((list('a), list(t('a))), underlying, pending),
              fulfilled: list('a),
              pending: list(t('a)),
              ps: list(t('a)),
            )
            : state_changed((list('a), list(t('a))), underlying, resolved) =>
      switch (ps) {
      | [] =>
        resolve(
          ~allow_deferring=false,
          to_resolve,
          [@implicit_arity]
          Fulfilled(List.rev(fulfilled), List.rev(pending)),
        )
      | [p, ...ps] =>
        let Internal(p_internal) = to_internal_promise(p);
        switch (underlying(p_internal).state) {
        | Fulfilled(v) => finish(to_resolve, [v, ...fulfilled], pending, ps)
        | Rejected(_) as result =>
          resolve(~allow_deferring=false, to_resolve, result)
        | Pending(_) => finish(to_resolve, fulfilled, [p, ...pending], ps)
        };
      };
    let rec collect_already_resolved_promises = (results, pending, ps) =>
      switch (ps) {
      | [] =>
        /* Maintainer's note: should the pending promise list also be
           reversed? It is reversed in finish. */
        return((List.rev(results), pending))
      | [p, ...ps] =>
        let Internal(p_internal) = to_internal_promise(p);
        switch (underlying(p_internal).state) {
        | Fulfilled(v) =>
          collect_already_resolved_promises([v, ...results], pending, ps)
        | Rejected(_) as result => to_public_promise({state: result})
        | Pending(_) =>
          collect_already_resolved_promises(results, [p, ...pending], ps)
        };
      };
    let rec check_for_already_resolved_promises = (pending_acc, ps') =>
      switch (ps') {
      | [] =>
        let p = new_pending(~how_to_cancel=propagate_cancel_to_several(ps));
        let callback = _result => {
          let State_may_now_be_pending_proxy(p) = may_now_be_proxy(p);
          let p = underlying(p);
          let State_may_have_changed(p) = finish(p, [], [], ps);
          ignore(p);
        };
        add_explicitly_removable_callback_to_each_of(ps, callback);
        to_public_promise(p);
      | [p, ...ps'] =>
        let Internal(p_internal) = to_internal_promise(p);
        switch (underlying(p_internal).state) {
        | Fulfilled(v) =>
          collect_already_resolved_promises([v], pending_acc, ps')
        | Rejected(_) as result => to_public_promise({state: result})
        | Pending(_) =>
          check_for_already_resolved_promises([p, ...pending_acc], ps')
        };
      };
    let p = check_for_already_resolved_promises([], ps);
    p;
  };
};

include Concurrent_composition;

module Miscellaneous: {
  /* Promise state query */
  type state('a) =
    | Return('a)
    | Fail(exn)
    | Sleep;
  let state: t('a) => state('a);
  let is_sleeping: t('a) => bool;
  let debug_state_is: (state('a), t('a)) => t(bool);
  /* Function lifters */
  let apply: ('a => t('b), 'a) => t('b);
  let wrap: (unit => 'b) => t('b);
  let wrap1: ('a1 => 'b, 'a1) => t('b);
  let wrap2: (('a1, 'a2) => 'b, 'a1, 'a2) => t('b);
  let wrap3: (('a1, 'a2, 'a3) => 'b, 'a1, 'a2, 'a3) => t('b);
  let wrap4: (('a1, 'a2, 'a3, 'a4) => 'b, 'a1, 'a2, 'a3, 'a4) => t('b);
  let wrap5:
    (('a1, 'a2, 'a3, 'a4, 'a5) => 'b, 'a1, 'a2, 'a3, 'a4, 'a5) => t('b);
  let wrap6:
    (('a1, 'a2, 'a3, 'a4, 'a5, 'a6) => 'b, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6) =>
    t('b);
  let wrap7:
    (
      ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) => 'b,
      'a1,
      'a2,
      'a3,
      'a4,
      'a5,
      'a6,
      'a7
    ) =>
    t('b);
  /* Paused promises */
  let pause: unit => t(unit);
  let wakeup_paused: unit => unit;
  let paused_count: unit => int;
  let register_pause_notifier: (int => unit) => unit;
  let abandon_paused: unit => unit;
  /* Internal interface for other modules in Lwt */
  let poll: t('a) => option('a);
} = {
  type state('a) =
    | Return('a)
    | Fail(exn)
    | Sleep;
  external reraise: exn => 'a = "%reraise";
  let state = p => {
    let Internal(p) = to_internal_promise(p);
    switch (underlying(p).state) {
    | Fulfilled(v) => Return(v)
    | Rejected(exn) => Fail(exn)
    | Pending(_) => Sleep
    };
  };
  let debug_state_is = (expected_state, p) =>
    return(state(p) == expected_state);
  let is_sleeping = p => {
    let Internal(p) = to_internal_promise(p);
    switch (underlying(p).state) {
    | Fulfilled(_) => false
    | Rejected(_) => false
    | Pending(_) => true
    };
  };
  let poll = p => {
    let Internal(p) = to_internal_promise(p);
    switch (underlying(p).state) {
    | Rejected(e) => reraise(e)
    | Fulfilled(v) => Some(v)
    | Pending(_) => None
    };
  };
  let apply = (f, x) =>
    try(f(x)) {
    | exn => fail(exn)
    };
  let wrap = f =>
    try(return(f())) {
    | exn => fail(exn)
    };
  let wrap1 = (f, x1) =>
    try(return(f(x1))) {
    | exn => fail(exn)
    };
  let wrap2 = (f, x1, x2) =>
    try(return(f(x1, x2))) {
    | exn => fail(exn)
    };
  let wrap3 = (f, x1, x2, x3) =>
    try(return(f(x1, x2, x3))) {
    | exn => fail(exn)
    };
  let wrap4 = (f, x1, x2, x3, x4) =>
    try(return(f(x1, x2, x3, x4))) {
    | exn => fail(exn)
    };
  let wrap5 = (f, x1, x2, x3, x4, x5) =>
    try(return(f(x1, x2, x3, x4, x5))) {
    | exn => fail(exn)
    };
  let wrap6 = (f, x1, x2, x3, x4, x5, x6) =>
    try(return(f(x1, x2, x3, x4, x5, x6))) {
    | exn => fail(exn)
    };
  let wrap7 = (f, x1, x2, x3, x4, x5, x6, x7) =>
    try(return(f(x1, x2, x3, x4, x5, x6, x7))) {
    | exn => fail(exn)
    };
  let pause_hook = ref(ignore);
  let paused = Lwt_sequence.create();
  let paused_count = ref(0);
  let pause = () => {
    let p = add_task_r(paused);
    incr(paused_count);
    pause_hook^(paused_count^);
    p;
  };
  let wakeup_paused = () =>
    if (Lwt_sequence.is_empty(paused)) {
      paused_count := 0;
    } else {
      let tmp = Lwt_sequence.create();
      Lwt_sequence.transfer_r(paused, tmp);
      paused_count := 0;
      Lwt_sequence.iter_l(r => wakeup(r, ()), tmp);
    };
  let register_pause_notifier = f => pause_hook := f;
  let abandon_paused = () => {
    Lwt_sequence.clear(paused);
    paused_count := 0;
  };
  let paused_count = () => paused_count^;
};

include Miscellaneous;

module Let_syntax = {
  module Let_syntax = {
    let return = return;
    let map = (t, ~f) => map(f, t);
    let bind = (t, ~f) => bind(t, f);
    let both = both;
    module Open_on_rhs = {};
  };
};
module Infix = {
  let (>>=) = bind;
  let (=<<) = (f, p) => bind(p, f);
  let (>|=) = (p, f) => map(f, p);
  let (=|<) = map;
  let (<&>) = (p, p') => join([p, p']);
  let (<?>) = (p, p') => choose([p, p']);
  include Let_syntax;
};

include (
          Infix:
             (module type of Infix) with
              module Let_syntax := Let_syntax.Let_syntax
        );

module Syntax = {
  // let ( let* ) = bind;
  // let (let_ )= return;
  // // let ( and* ) = both;
  // let ( and_ ) = both;
  // let (letplus) = (x, f) => map(f, x);
  // let (andplus) = both;
  // let (let*) = bind
  // let (and*) = both
  // let (let+) x f = map f x
  // let (and+) = both
  // end
  let await = return;
  let (let.await) = bind;
  let (and.await) = both;
  let (let+await) = (v, f) => map(f, v);
};

module Lwt_result_type = {
  type result(+'a) = lwt_result('a);
  /* Deprecated. */
  let make_value = v => Result.Ok(v);
  let make_error = exn => Result.Error(exn);
};

include Lwt_result_type;
