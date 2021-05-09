// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

var Empty = /* @__PURE__ */Caml_exceptions.create("LwtDllist-Repacked.Empty");

function get(node) {
  return node.node_data;
}

function set(node, data) {
  node.node_data = data;
  
}

function remove(node) {
  if (node.node_active) {
    node.node_active = false;
    node.prev.next = node.next;
    node.next.prev = node.prev;
    return ;
  }
  
}

function create(param) {
  var seq = {};
  seq.prev = seq;
  seq.next = seq;
  return seq;
}

function is_empty(seq) {
  return seq.next === seq;
}

function length(seq) {
  var _curr = seq.next;
  var _len = 0;
  while(true) {
    var len = _len;
    var curr = _curr;
    if (curr === seq) {
      return len;
    }
    _len = len + 1 | 0;
    _curr = curr.node_next;
    continue ;
  };
}

function add_l(data, seq) {
  var node = {
    node_prev: seq,
    node_next: seq.next,
    node_data: data,
    node_active: true
  };
  seq.next.prev = node;
  seq.next = node;
  return node;
}

function add_r(data, seq) {
  var node = {
    node_prev: seq.prev,
    node_next: seq,
    node_data: data,
    node_active: true
  };
  seq.prev.next = node;
  seq.prev = node;
  return node;
}

function take_l(seq) {
  if (seq.next === seq) {
    throw {
          RE_EXN_ID: Empty,
          Error: new Error()
        };
  }
  var node = seq.next;
  remove(node);
  return node.node_data;
}

function clear(seq) {
  seq.prev = seq;
  seq.next = seq;
  
}

function take_r(seq) {
  if (seq.next === seq) {
    throw {
          RE_EXN_ID: Empty,
          Error: new Error()
        };
  }
  var node = seq.prev;
  remove(node);
  return node.node_data;
}

function take_opt_l(seq) {
  if (seq.next === seq) {
    return ;
  }
  var node = seq.next;
  remove(node);
  return Caml_option.some(node.node_data);
}

function take_opt_r(seq) {
  if (seq.next === seq) {
    return ;
  }
  var node = seq.prev;
  remove(node);
  return Caml_option.some(node.node_data);
}

function transfer_l(s1, s2) {
  s2.next.prev = s1.prev;
  s1.prev.next = s2.next;
  s2.next = s1.next;
  s1.next.prev = s2;
  s1.prev = s1;
  s1.next = s1;
  
}

function transfer_r(s1, s2) {
  s2.prev.next = s1.next;
  s1.next.prev = s2.prev;
  s2.prev = s1.prev;
  s1.prev.next = s2;
  s1.prev = s1;
  s1.next = s1;
  
}

function iter_l(f, seq) {
  var _curr = seq.next;
  while(true) {
    var curr = _curr;
    if (curr === seq) {
      return ;
    }
    if (curr.node_active) {
      Curry._1(f, curr.node_data);
    }
    _curr = curr.node_next;
    continue ;
  };
}

function iter_r(f, seq) {
  var _curr = seq.prev;
  while(true) {
    var curr = _curr;
    if (curr === seq) {
      return ;
    }
    if (curr.node_active) {
      Curry._1(f, curr.node_data);
    }
    _curr = curr.node_prev;
    continue ;
  };
}

function iter_node_l(f, seq) {
  var _curr = seq.next;
  while(true) {
    var curr = _curr;
    if (curr === seq) {
      return ;
    }
    if (curr.node_active) {
      Curry._1(f, curr);
    }
    _curr = curr.node_next;
    continue ;
  };
}

function iter_node_r(f, seq) {
  var _curr = seq.prev;
  while(true) {
    var curr = _curr;
    if (curr === seq) {
      return ;
    }
    if (curr.node_active) {
      Curry._1(f, curr);
    }
    _curr = curr.node_prev;
    continue ;
  };
}

function fold_l(f, seq, acc) {
  var _curr = seq.next;
  var _acc = acc;
  while(true) {
    var acc$1 = _acc;
    var curr = _curr;
    if (curr === seq) {
      return acc$1;
    }
    if (curr.node_active) {
      _acc = Curry._2(f, curr.node_data, acc$1);
      _curr = curr.node_next;
      continue ;
    }
    _curr = curr.node_next;
    continue ;
  };
}

function fold_r(f, seq, acc) {
  var _curr = seq.prev;
  var _acc = acc;
  while(true) {
    var acc$1 = _acc;
    var curr = _curr;
    if (curr === seq) {
      return acc$1;
    }
    if (curr.node_active) {
      _acc = Curry._2(f, curr.node_data, acc$1);
      _curr = curr.node_prev;
      continue ;
    }
    _curr = curr.node_prev;
    continue ;
  };
}

function find_node_l(f, seq) {
  var _curr = seq.next;
  while(true) {
    var curr = _curr;
    if (curr !== seq) {
      if (curr.node_active) {
        if (Curry._1(f, curr.node_data)) {
          return curr;
        }
        _curr = curr.node_next;
        continue ;
      }
      _curr = curr.node_next;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function find_node_r(f, seq) {
  var _curr = seq.prev;
  while(true) {
    var curr = _curr;
    if (curr !== seq) {
      if (curr.node_active) {
        if (Curry._1(f, curr.node_data)) {
          return curr;
        }
        _curr = curr.node_prev;
        continue ;
      }
      _curr = curr.node_prev;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function find_node_opt_l(f, seq) {
  try {
    return find_node_l(f, seq);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return ;
    }
    throw exn;
  }
}

function find_node_opt_r(f, seq) {
  try {
    return find_node_r(f, seq);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return ;
    }
    throw exn;
  }
}

exports.Empty = Empty;
exports.get = get;
exports.set = set;
exports.remove = remove;
exports.create = create;
exports.is_empty = is_empty;
exports.length = length;
exports.add_l = add_l;
exports.add_r = add_r;
exports.take_l = take_l;
exports.clear = clear;
exports.take_r = take_r;
exports.take_opt_l = take_opt_l;
exports.take_opt_r = take_opt_r;
exports.transfer_l = transfer_l;
exports.transfer_r = transfer_r;
exports.iter_l = iter_l;
exports.iter_r = iter_r;
exports.iter_node_l = iter_node_l;
exports.iter_node_r = iter_node_r;
exports.fold_l = fold_l;
exports.fold_r = fold_r;
exports.find_node_l = find_node_l;
exports.find_node_r = find_node_r;
exports.find_node_opt_l = find_node_opt_l;
exports.find_node_opt_r = find_node_opt_r;
/* No side effect */
