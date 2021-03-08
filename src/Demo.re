Js.log("Hello, ReScript!");
// open Lwt.Infix;
// let get_input = (input: int) =>
//   switch (input) {
//   | 1 => 1
//   | 2 => 2
//   | _ => 0
//   };
// let x = {
//   let a = get_input(1);
//   let b = get_input(2);
//   a + b;
// };

// let x = get_input(1) >>= (a => get_input(2) >>= (b => return(a + b)));

// // let x = {
// //   %lwt
// //   {
// //     let a = get_input(1);
// //     %lwt
// //     {
// //       let b = get_input(2);
// //       return(a) + b;
// //     };
// //   };
// // };
