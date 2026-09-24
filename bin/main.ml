open Converter

let () = 
  let val_to_conv = 100.0 in
  let result = convert_length val_to_conv (Meter 0.0) (Kilometer 0.0) in
  match result with
  | Kilometer v -> Printf.printf "%.2f meters is %.4f kilometers\n" val_to_conv v
  | _ -> ()

  let temp_val = 25.0 in
  let temp_res = convert_temp temp_val (Celsius 0.0) (Fahrenheit 0.0) in
  match temp_res with
  | Fahrenheit v -> Printf.printf "%.2f Celsius is %.2f Fahrenheit\n" temp_val v
  | _ -> ()