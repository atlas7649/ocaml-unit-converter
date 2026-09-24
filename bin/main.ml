open Converter

let () = 
  let val_to_conv = 100.0 in
  let result = convert_length val_to_conv Meter Kilometer in
  Printf.printf "%.2f meters is %.4f kilometers\n" val_to_conv result

  let temp_val = 25.0 in
  let temp_res = convert_temp temp_val Celsius Fahrenheit in
  Printf.printf "%.2f Celsius is %.2f Fahrenheit\n" temp_val temp_res

  let mass_val = 2.5 in
  let mass_res = convert_mass mass_val Kilogram Pound in
  Printf.printf "%.2f kilograms is %.2f pounds\n" mass_val mass_res