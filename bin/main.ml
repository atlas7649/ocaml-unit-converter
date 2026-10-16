open Converter

let print_conv value from_unit to_unit result to_string_fn = 
  Printf.printf "%.2f %s is %.4f %s\n" 
    value (to_string_fn from_unit) result (to_string_fn to_unit)

let run_test_length () = 
  Printf.printf "--- Length Conversions ---\n";
  let v = 100.0 in
  print_conv v Meter Kilometer (convert_length v Meter Kilometer) length_unit_to_string;
  let v2 = 1.0 in
  print_conv v2 Mile Foot (convert_length v2 Mile Foot) length_unit_to_string

let run_test_temp () = 
  Printf.printf "\n--- Temperature Conversions ---\n";
  let v = 25.0 in
  print_conv v Celsius Fahrenheit (convert_temp v Celsius Fahrenheit) temp_unit_to_string;
  let v2 = 300.0 in
  print_conv v2 Kelvin Celsius (convert_temp v2 Kelvin Celsius) temp_unit_to_string

let run_test_mass () = 
  Printf.printf "\n--- Mass Conversions ---\n";
  let v = 2.5 in
  print_conv v Kilogram Pound (convert_mass v Kilogram Pound) mass_unit_to_string;
  let v2 = 500.0 in
  print_conv v2 Gram Ounce (convert_mass v2 Gram Ounce) mass_unit_to_string

let run_test_volume () = 
  Printf.printf "\n--- Volume Conversions ---\n";
  let v = 2.0 in
  print_conv v Gallon Liter (convert_volume v Gallon Liter) volume_unit_to_string;
  let v2 = 500.0 in
  print_conv v2 Milliliter Cup (convert_volume v2 Milliliter Cup) volume_unit_to_string

let run_test_pressure () = 
  Printf.printf "\n--- Pressure Conversions ---\n";
  let v = 1.0 in
  print_conv v Atmosphere PSI (convert_pressure v Atmosphere PSI) pressure_unit_to_string;
  let v2 = 101325.0 in
  print_conv v2 Pascal Bar (convert_pressure v2 Pascal Bar) pressure_unit_to_string;
  let v3 = 1013.25 in
  print_conv v3 Millibar Atmosphere (convert_pressure v3 Millibar Atmosphere) pressure_unit_to_string

let run_test_time () = 
  Printf.printf "\n--- Time Conversions ---\n";
  let v = 1.0 in
  print_conv v Day Second (convert_time v Day Second) time_unit_to_string;
  let v2 = 120.0 in
  print_conv v2 Minute Hour (convert_time v2 Minute Hour) time_unit_to_string

let run_test_energy () = 
  Printf.printf "\n--- Energy Conversions ---\n";
  let v = 1.0 in
  print_conv v KilowattHour Joule (convert_energy v KilowattHour Joule) energy_unit_to_string;
  let v2 = 1000.0 in
  print_conv v2 Calorie WattHour (convert_energy v2 Calorie WattHour) energy_unit_to_string

let run_test_area () = 
  Printf.printf "\n--- Area Conversions ---\n";
  let v = 1.0 in
  print_conv v Acre SquareMeter (convert_area v Acre SquareMeter) area_unit_to_string;
  let v2 = 100.0 in
  print_conv v2 SquareFoot SquareMeter (convert_area v2 SquareFoot SquareMeter) area_unit_to_string

let run_test_speed () = 
  Printf.printf "\n--- Speed Conversions ---\n";
  let v = 100.0 in
  print_conv v KilometersPerHour MilesPerHour (convert_speed v KilometersPerHour MilesPerHour) speed_unit_to_string;
  let v2 = 20.0 in
  print_conv v2 Knot MetersPerSecond (convert_speed v2 Knot MetersPerSecond) speed_unit_to_string

let run_test_storage () = 
  Printf.printf "\n--- Storage Conversions ---\n";
  let v = 1.0 in
  print_conv v Gigabyte Megabyte (convert_storage v Gigabyte Megabyte) storage_unit_to_string;
  let v2 = 1024.0 in
  print_conv v2 Kilobyte Byte (convert_storage v2 Kilobyte Byte) storage_unit_to_string;
  let v3 = 1.0 in
  print_conv v3 Yottabyte Zettabyte (convert_storage v3 Yottabyte Zettabyte) storage_unit_to_string

let run_test_angle () = 
  Printf.printf "\n--- Angle Conversions ---\n";
  let v = 180.0 in
  print_conv v Degree Radian (convert_angle v Degree Radian) angle_unit_to_string;
  let v2 = 1.0 in
  print_conv v2 Radian Gradian (convert_angle v2 Radian Gradian) angle_unit_to_string

let run_test_freq () = 
  Printf.printf "\n--- Frequency Conversions ---\n";
  let v = 2.4 in
  print_conv v Gigahertz Megahertz (convert_frequency v Gigahertz Megahertz) frequency_unit_to_string;
  let v2 = 100.0 in
  print_conv v2 Kilohertz Hertz (convert_frequency v2 Kilohertz Hertz) frequency_unit_to_string

let run_test_force () = 
  Printf.printf "\n--- Force Conversions ---\n";
  let v = 10.0 in
  print_conv v Newton PoundForce (convert_force v Newton PoundForce) force_unit_to_string;
  let v2 = 1.0 in
  print_conv v2 KilogramForce Newton (convert_force v2 KilogramForce Newton) force_unit_to_string

let run_test_fuel () = 
  Printf.printf "\n--- Fuel Efficiency Conversions ---\n";
  let v = 8.0 in
  print_conv v LitersPer100km MilesPerGallonUS (convert_fuel v LitersPer100km MilesPerGallonUS) fuel_unit_to_string;
  let v2 = 30.0 in
  print_conv v2 MilesPerGallonUS LitersPer100km (convert_fuel v2 MilesPerGallonUS LitersPer100km) fuel_unit_to_string

let run_test_currency () = 
  Printf.printf "\n--- Currency Conversions ---\n";
  let v = 100.0 in
  print_conv v EUR USD (convert_currency v EUR USD) currency_unit_to_string;
  let v2 = 1000.0 in
  print_conv v2 JPY GBP (convert_currency v2 JPY GBP) currency_unit_to_string

let run_test_rate () = 
  Printf.printf "\n--- Transfer Rate Conversions ---\n";
  let v = 100.0 in
  print_conv v MegabitsPerSecond MegabytesPerSecond (convert_transfer_rate v MegabitsPerSecond MegabytesPerSecond) transfer_rate_unit_to_string;
  let v2 = 1.0 in
  print_conv v2 GigabitsPerSecond MegabitsPerSecond (convert_transfer_rate v2 GigabitsPerSecond MegabitsPerSecond) transfer_rate_unit_to_string

let run_test_lum () = 
  Printf.printf "\n--- Illuminance Conversions ---\n";
  let v = 100.0 in
  print_conv v Lux FootCandle (convert_illuminance v Lux FootCandle) illuminance_unit_to_string

let run_test_mag () = 
  Printf.printf "\n--- Magnetic Flux Density Conversions ---\n";
  let v = 1.0 in
  print_conv v Tesla Gauss (convert_magnetic_flux_density v Tesla Gauss) magnetic_flux_density_unit_to_string

let run_test_visc () = 
  Printf.printf "\n--- Viscosity Conversions ---\n";
  let v = 1.0 in
  print_conv v PascalSecond Centipoise (convert_viscosity v PascalSecond Centipoise) viscosity_unit_to_string

let run_test_res () = 
  Printf.printf "\n--- Resistance Conversions ---\n";
  let v = 10.0 in
  print_conv v Kilohm Ohm (convert_resistance v Kilohm Ohm) resistance_unit_to_string

let run_test_flux () = 
  Printf.printf "\n--- Luminous Flux Conversions ---\n";
  let v = 500.0 in
  print_conv v Lumen CandelaSteradian (convert_luminous_flux v Lumen CandelaSteradian) luminous_flux_unit_to_string

let run_test_pow () = 
  Printf.printf "\n--- Power Conversions ---\n";
  let v = 1.0 in
  print_conv v Kilowatt Horsepower (convert_power v Kilowatt Horsepower) power_unit_to_string

let run_test_dens () = 
  Printf.printf "\n--- Data Density Conversions ---\n";
  let v = 1000.0 in
  print_conv v BitsPerSquareInch BitsPerSquareMeter (convert_data_density v BitsPerSquareInch BitsPerSquareMeter) data_density_unit_to_string;
  let v2 = 1.0 in
  print_conv v2 BytesPerSquareMeter BitsPerSquareMeter (convert_data_density v2 BytesPerSquareMeter BitsPerSquareMeter) data_density_unit_to_string

let run_test_torque () = 
  Printf.printf "\n--- Torque Conversions ---\n";
  let v = 10.0 in
  print_conv v NewtonMeter PoundFoot (convert_torque v NewtonMeter PoundFoot) torque_unit_to_string;
  let v2 = 1.0 in
  print_conv v2 KilogramMeter NewtonMeter (convert_torque v2 KilogramMeter NewtonMeter) torque_unit_to_string

let () = 
  run_test_length ();
  run_test_temp ();
  run_test_mass ();
  run_test_volume ();
  run_test_pressure ();
  run_test_time ();
  run_test_energy ();
  run_test_area ();
  run_test_speed ();
  run_test_storage ();
  run_test_angle ();
  run_test_freq ();
  run_test_force ();
  run_test_fuel ();
  run_test_currency ();
  run_test_rate ();
  run_test_lum ();
  run_test_mag ();
  run_test_visc ();
  run_test_res ();
  run_test_flux ();
  run_test_pow ();
  run_test_dens ();
  run_test_torque ();
