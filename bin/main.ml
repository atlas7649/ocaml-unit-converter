open Converter

let print_conv value from_unit to_unit result to_string_fn = 
  Printf.printf "%.2f %s is %.4f %s\n" 
    value (to_string_fn from_unit) result (to_string_fn to_unit)

let () = 
  let val_to_conv = 100.0 in
  let result = convert_length val_to_conv Meter Kilometer in
  print_conv val_to_conv Meter Kilometer result length_unit_to_string

  let temp_val = 25.0 in
  let temp_res = convert_temp temp_val Celsius Fahrenheit in
  print_conv temp_val Celsius Fahrenheit temp_res temp_unit_to_string

  let mass_val = 2.5 in
  let mass_res = convert_mass mass_val Kilogram Pound in
  print_conv mass_val Kilogram Pound mass_res mass_unit_to_string

  let mass_val2 = 500.0 in
  let mass_res2 = convert_mass mass_val2 Gram Ounce in
  print_conv mass_val2 Gram Ounce mass_res2 mass_unit_to_string