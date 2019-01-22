open Prelude

let marshall obj fileName  =
  let objString = Marshal.to_string obj [] in
  let file = File.open_out fileName in
  BatInnerIO.write_string file objString;
  BatInnerIO.close_out file


let unmarshall fileName = 
    let file = File.open_in fileName in
    let marshalled = BatInnerIO.read_string file in
    Marshal.from_string marshalled 0 