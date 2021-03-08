/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */
exception Empty;

type t('a) = {
  mutable prev: t('a),
  mutable next: t('a)
};

type node('a) = {
  mutable node_prev: t('a),
  mutable node_next: t('a),
  mutable node_data: 'a,
  mutable node_active: bool
};

external seq_of_node : node('a) => t('a) = "%identity";

external node_of_seq : t('a) => node('a) = "%identity";

/* +-----------------------------------------------------------------+
   | Operations on nodes                                             |
   +-----------------------------------------------------------------+ */
let get = (node) => node.node_data;

let set = (node, data) => node.node_data = data;

let remove = (node) =>
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

let is_empty = (seq) => seq.next === seq;

let length = (seq) => {
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
  let node = {node_prev: seq, node_next: seq.next, node_data: data, node_active: true};
  seq.next.prev = seq_of_node(node);
  seq.next = seq_of_node(node);
  node;
};

let add_r = (data, seq) => {
  let node = {node_prev: seq.prev, node_next: seq, node_data: data, node_active: true};
  seq.prev.next = seq_of_node(node);
  seq.prev = seq_of_node(node);
  node;
};

let take_l = (seq) =>
  if (is_empty(seq)) {
    raise(Empty);
  } else {
    let node = node_of_seq(seq.next);
    remove(node);
    node.node_data;
  };
let clear = (seq) => {
  seq.prev = seq;
  seq.next = seq;
};
let take_r = (seq) =>
  if (is_empty(seq)) {
    raise(Empty);
  } else {
    let node = node_of_seq(seq.prev);
    remove(node);
    node.node_data;
  };

let take_opt_l = (seq) =>
  if (is_empty(seq)) {
    None;
  } else {
    let node = node_of_seq(seq.next);
    remove(node);
    Some(node.node_data);
  };

let take_opt_r = (seq) =>
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
  let rec loop = (curr) =>
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
  let rec loop = (curr) =>
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
  let rec loop = (curr) =>
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
  let rec loop = (curr) =>
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
  let rec loop = (curr) =>
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
  let rec loop = (curr) =>
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
  try (Some(find_node_l(f, seq))) {
  | Not_found => None
  };

let find_node_opt_r = (f, seq) =>
  try (Some(find_node_r(f, seq))) {
  | Not_found => None
  };
