Js.log("Hello, ReScript!");

let x = 10;
let a = Some(10);
let b = Some(3);
let (let.opt) = (x, f) =>
  switch (x) {
  | None => None
  | Some(x) => f(x)
  };
let (and.opt) = (a, b) =>
  switch (a, b) {
  | (Some(a), Some(b)) => Some((a, b))
  | _ => None
  };
let z = {
  let.opt a = a
  and.opt b = b;
  Some(a + b);
};
Js.log2("ok", z);
Js.log2("ok2", z);

let (let.await) = (promise, map) => Js.Promise.then_(map, promise);

let data = Js.Promise.resolve("Hello");

let p = {
  let.await data = data;
  Js.Promise.resolve(Js.log(data));
};
// Js.log2("ok", z);
// let usePromise = () => {
//   let (message, setMessage) = React.useState(() => "Aguardando...");

//   React.useEffect0(() => {
//     let data = Js.Promise.resolve("Hello");

//     let _ = {
//       let%Await res = data;

//       setMessage(_=> res);
//       Js.Promise.resolve();
//     };

//     None
//   });

//   message;
// };
// // let x = get_input(1) >>= (a => get_input(2) >>= (b => return(a + b)));

// // let x = {
// //   %lwt
// //   {
// //     let.await a = get_input(1);
// //     %lwt
// //     {
// //       let.await b = get_input(2);
// //       return(a) + b;
// //     };
// //   };
