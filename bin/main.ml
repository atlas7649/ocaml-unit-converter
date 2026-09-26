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

  let vol_val = 2.0 in
  let vol_res = convert_volume vol_val Gallon Liter in
  print_conv vol_val Gallon Liter vol_res volume_unit_to_string

  let vol_val2 = 500.0 in
  let vol_res2 = convert_volume vol_val2 Milliliter Cup in
  print_conv vol_val2 Milliliter Cup vol_res2 volume_unit_to_string

  let press_val = 1.0 in
  let press_res = convert_pressure press_val Atmosphere PSI in
  print_conv press_val Atmosphere PSI press_res pressure_unit_to_string

  let press_val2 = 101325.0 in
  let press_res2 = convert_pressure press_val2 Pascal Bar in
  print_conv press_val2 Pascal Bar press_res2 pressure_unit_to_string

  let time_val = 1.0 in
  let time_res = convert_time time_val Day Second in
  print_conv time_val Day Second time_res time_unit_to_string

  let time_val2 = 120.0 in
  let time_res2 = convert_time time_val2 Minute Hour in
  print_conv time_val2 Minute Hour time_res2 time_unit_to_string

  let energy_val = 1.0 in
  let energy_res = convert_energy energy_val KilowattHour Joule in
  print_conv energy_val KilowattHour Joule energy_res energy_unit_to_string

  let energy_val2 = 1000.0 in
  let energy_res2 = convert_energy energy_val2 Calorie WattHour in
  print_conv energy_val2 Calorie WattHour energy_res2 energy_unit_to_string

  let area_val = 1.0 in
  let area_res = convert_area area_val Acre SquareMeter in
  print_conv area_val Acre SquareMeter area_res area_unit_to_string

  let area_val2 = 100.0 in
  let area_res2 = convert_area area_val2 SquareFoot SquareMeter in
  print_conv area_val2 SquareFoot SquareMeter area_res2 area_unit_to_string

  let speed_val = 100.0 in
  let speed_res = convert_speed speed_val KilometersPerHour MilesPerHour in
  print_conv speed_val KilometersPerHour MilesPerHour speed_res speed_unit_to_string

  let speed_val2 = 20.0 in
  let speed_res2 = convert_speed speed_val2 Knot MetersPerSecond in
  print_conv speed_val2 Knot MetersPerSecond speed_res2 speed_unit_to_string

  let stor_val = 1.0 in
  let stor_res = convert_storage stor_val Gigabyte Megabyte in
  print_conv stor_val Gigabyte Megabyte stor_res storage_unit_to_string

  let stor_val2 = 1024.0 in
  let stor_res2 = convert_storage stor_val2 Kilobyte Byte in
  print_conv stor_val2 Kilobyte Byte stor_res2 storage_unit_to_string

  let angle_val = 180.0 in
  let angle_res = convert_angle angle_val Degree Radian in
  print_conv angle_val Degree Radian angle_res angle_unit_to_string

  let angle_val2 = 1.0 in
  let angle_res2 = convert_angle angle_val2 Radian Gradian in
  print_conv angle_val2 Radian Gradian angle_res2 angle_unit_to_string

  let freq_val = 2.4 in
  let freq_res = convert_frequency freq_val Gigahertz Megahertz in
  print_conv freq_val Gigahertz Megahertz freq_res frequency_unit_to_string

  let freq_val2 = 100.0 in
  let freq_res2 = convert_frequency freq_val2 Kilohertz Hertz in
  print_conv freq_val2 Kilohertz Hertz freq_res2 frequency_unit_to_string

  let force_val = 10.0 in
  let force_res = convert_force force_val Newton PoundForce in
  print_conv force_val Newton PoundForce force_res force_unit_to_string

  let force_val2 = 1.0 in
  let force_res2 = convert_force force_val2 KilogramForce Newton in
  print_conv force_val2 KilogramForce Newton force_res2 force_unit_to_string