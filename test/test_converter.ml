open Converter

let close a b =
  let scale = max 1.0 (max (abs_float a) (abs_float b)) in
  abs_float (a -. b) <= scale *. 1e-6

let expect name cond =
  if not cond then failwith ("expectation failed: " ^ name)

let () =
  expect "length meters to kilometers"
    (close (convert_length 100.0 Meter Kilometer) 0.1);
  expect "length meters to feet"
    (close (convert_length 1.0 Meter Foot) 3.280839895);
  expect "round-trip length"
    (close (convert_length (convert_length 7.25 Mile Meter) Meter Mile) 7.25);
  expect "length unit string"
    (length_unit_to_string Foot = "feet");

  expect "temperature celsius to fahrenheit"
    (close (convert_temp 25.0 Celsius Fahrenheit) 77.0);
  expect "temperature celsius to kelvin"
    (close (convert_temp 100.0 Celsius Kelvin) 373.15);
  expect "temperature fahrenheit to celsius"
    (close (convert_temp 32.0 Fahrenheit Celsius) 0.0);
  expect "temperature kelvin to celsius"
    (close (convert_temp 300.0 Kelvin Celsius) 26.85);
  expect "round-trip temperature"
    (close (convert_temp (convert_temp 7.25 Celsius Fahrenheit) Fahrenheit Celsius) 7.25);
  expect "temperature unit string"
    (temp_unit_to_string Kelvin = "Kelvin");

  expect "mass kilograms to pounds"
    (close (convert_mass 2.5 Kilogram Pound) 5.5115565);
  expect "mass grams to ounces"
    (close (convert_mass 500.0 Gram Ounce) 17.63698);
  expect "round-trip mass"
    (close (convert_mass (convert_mass 7.25 Kilogram Gram) Gram Kilogram) 7.25);
  expect "mass unit string"
    (mass_unit_to_string Pound = "pounds");

  expect "volume gallons to liters"
    (close (convert_volume 2.0 Gallon Liter) 7.57082);
  expect "volume milliliters to cups"
    (close (convert_volume 500.0 Milliliter Cup) 2.1133764);
  expect "round-trip volume"
    (close (convert_volume (convert_volume 7.25 Cup Liter) Liter Cup) 7.25);
  expect "volume unit string"
    (volume_unit_to_string Milliliter = "milliliters");

  expect "pressure atmospheres to psi"
    (close (convert_pressure 1.0 Atmosphere PSI) 14.69595);
  expect "pressure pascals to bars"
    (close (convert_pressure 101325.0 Pascal Bar) 1.01325);
  expect "pressure millibar to atmospheres"
    (close (convert_pressure 1013.25 Millibar Atmosphere) 1.0);
  expect "round-trip pressure"
    (close (convert_pressure (convert_pressure 7.25 PSI Pascal) Pascal PSI) 7.25);
  expect "pressure unit string"
    (pressure_unit_to_string Millibar = "millibars/hPa");

  expect "time days to seconds"
    (close (convert_time 1.0 Day Second) 86400.0);
  expect "time minutes to hours"
    (close (convert_time 120.0 Minute Hour) 2.0);
  expect "round-trip time"
    (close (convert_time (convert_time 7.25 Hour Minute) Minute Hour) 7.25);
  expect "time unit string"
    (time_unit_to_string Day = "days");

  expect "energy kilowatt-hours to joules"
    (close (convert_energy 1.0 KilowattHour Joule) 3600000.0);
  expect "energy calories to joules"
    (close (convert_energy 1000.0 Calorie Joule) 4184.0);
  expect "round-trip energy"
    (close (convert_energy (convert_energy 7.25 WattHour Joule) Joule WattHour) 7.25);
  expect "energy unit string"
    (energy_unit_to_string KilowattHour = "kilowatt-hours");

  expect "area acres to square meters"
    (close (convert_area 1.0 Acre SquareMeter) 4046.86);
  expect "area square feet to square meters"
    (close (convert_area 100.0 SquareFoot SquareMeter) 9.2903);
  expect "area square kilometers to square meters"
    (close (convert_area 1.0 SquareKilometer SquareMeter) 1000000.0);
  expect "area hectares to square meters"
    (close (convert_area 2.5 Hectare SquareMeter) 25000.0);
  expect "round-trip area"
    (close (convert_area (convert_area 7.25 Acre SquareMeter) SquareMeter Acre) 7.25);
  expect "area unit string"
    (area_unit_to_string Hectare = "hectares");

  expect "speed kilometers per hour to miles per hour"
    (close (convert_speed 100.0 KilometersPerHour MilesPerHour) 62.137119);
  expect "speed knots to meters per second"
    (close (convert_speed 20.0 Knot MetersPerSecond) 10.28888);
  expect "round-trip speed"
    (close (convert_speed (convert_speed 7.25 MilesPerHour MetersPerSecond) MetersPerSecond MilesPerHour) 7.25);
  expect "speed unit string"
    (speed_unit_to_string Knot = "knots");

  expect "storage gigabytes to megabytes"
    (close (convert_storage 1.0 Gigabyte Megabyte) 1024.0);
  expect "storage kilobytes to bytes"
    (close (convert_storage 1024.0 Kilobyte Byte) 1048576.0);
  expect "storage yottabytes to zettabytes"
    (close (convert_storage 1.0 Yottabyte Zettabyte) 1024.0);
  expect "round-trip storage"
    (close (convert_storage (convert_storage 7.25 Gigabyte Terabyte) Terabyte Gigabyte) 7.25);
  expect "storage unit string"
    (storage_unit_to_string Zettabyte = "zettabytes");

  expect "angle degrees to radians"
    (close (convert_angle 180.0 Degree Radian) 3.141592654);
  expect "angle radians to gradians"
    (close (convert_angle 1.0 Radian Gradian) 63.661977);
  expect "round-trip angle"
    (close (convert_angle (convert_angle 7.25 Degree Radian) Radian Degree) 7.25);
  expect "angle unit string"
    (angle_unit_to_string Radian = "radians");

  expect "frequency gigahertz to megahertz"
    (close (convert_frequency 2.4 Gigahertz Megahertz) 2400.0);
  expect "frequency kilohertz to hertz"
    (close (convert_frequency 100.0 Kilohertz Hertz) 100000.0);
  expect "round-trip frequency"
    (close (convert_frequency (convert_frequency 7.25 Megahertz Hertz) Hertz Megahertz) 7.25);
  expect "frequency unit string"
    (frequency_unit_to_string Gigahertz = "gigahertz");

  expect "force newtons to pound-force"
    (close (convert_force 10.0 Newton PoundForce) 2.24809);
  expect "force kilogram-force to newtons"
    (close (convert_force 1.0 KilogramForce Newton) 9.80665);
  expect "round-trip force"
    (close (convert_force (convert_force 7.25 KilogramForce Newton) Newton KilogramForce) 7.25);
  expect "force unit string"
    (force_unit_to_string Newton = "newtons");

  expect "fuel liters per 100km to us mpg"
    (close (convert_fuel 8.0 LitersPer100km MilesPerGallonUS) 29.401875);
  expect "fuel us mpg to liters per 100km"
    (close (convert_fuel 30.0 MilesPerGallonUS LitersPer100km) 7.8405);
  expect "round-trip fuel"
    (close (convert_fuel (convert_fuel 7.25 LitersPer100km MilesPerGallonUS) MilesPerGallonUS LitersPer100km) 7.25);
  expect "fuel unit string"
    (fuel_unit_to_string MilesPerGallonUK = "UK mpg");

  expect "currency euros to us dollars"
    (close (convert_currency 100.0 EUR USD) 108.0);
  expect "currency yen to pounds"
    (close (convert_currency 1000.0 JPY GBP) 5.27559);
  expect "round-trip currency"
    (close (convert_currency (convert_currency 7.25 GBP USD) USD GBP) 7.25);
  expect "currency unit string"
    (currency_unit_to_string JPY = "JPY");

  expect "transfer rate megabits to megabytes"
    (close (convert_transfer_rate 100.0 MegabitsPerSecond MegabytesPerSecond) 12.5);
  expect "transfer rate gigabits to megabits"
    (close (convert_transfer_rate 1.0 GigabitsPerSecond MegabitsPerSecond) 1000.0);
  expect "round-trip transfer rate"
    (close (convert_transfer_rate (convert_transfer_rate 7.25 GigabytesPerSecond MegabitsPerSecond) MegabitsPerSecond GigabytesPerSecond) 7.25);
  expect "transfer rate unit string"
    (transfer_rate_unit_to_string MegabytesPerSecond = "MB/s");

  expect "illuminance lux to foot-candles"
    (close (convert_illuminance 100.0 Lux FootCandle) 9.29031);
  expect "round-trip illuminance"
    (close (convert_illuminance (convert_illuminance 7.25 FootCandle Lux) Lux FootCandle) 7.25);
  expect "illuminance unit string"
    (illuminance_unit_to_string Lux = "lux");

  expect "magnetic flux density tesla to gauss"
    (close (convert_magnetic_flux_density 1.0 Tesla Gauss) 10000.0);
  expect "round-trip magnetic flux density"
    (close (convert_magnetic_flux_density (convert_magnetic_flux_density 7.25 Gauss Tesla) Tesla Gauss) 7.25);
  expect "magnetic flux density unit string"
    (magnetic_flux_density_unit_to_string Tesla = "teslas");

  expect "viscosity pascal-seconds to centipoise"
    (close (convert_viscosity 1.0 PascalSecond Centipoise) 1000.0);
  expect "round-trip viscosity"
    (close (convert_viscosity (convert_viscosity 7.25 Centipoise Poise) Poise Centipoise) 7.25);
  expect "viscosity unit string"
    (viscosity_unit_to_string Centipoise = "centipoise");

  expect "resistance kilohms to ohms"
    (close (convert_resistance 10.0 Kilohm Ohm) 10000.0);
  expect "round-trip resistance"
    (close (convert_resistance (convert_resistance 7.25 Megohm Kilohm) Kilohm Megohm) 7.25);
  expect "resistance unit string"
    (resistance_unit_to_string Ohm = "ohms");

  expect "luminous flux lumens to candela-steradians"
    (close (convert_luminous_flux 500.0 Lumen CandelaSteradian) 500.0);
  expect "round-trip luminous flux"
    (close (convert_luminous_flux (convert_luminous_flux 7.25 CandelaSteradian Lumen) Lumen CandelaSteradian) 7.25);
  expect "luminous flux unit string"
    (luminous_flux_unit_to_string Lumen = "lumens");

  expect "power kilowatts to horsepower"
    (close (convert_power 1.0 Kilowatt Horsepower) 1.341022);
  expect "round-trip power"
    (close (convert_power (convert_power 7.25 Horsepower Watt) Watt Horsepower) 7.25);
  expect "power unit string"
    (power_unit_to_string Horsepower = "horsepower");

  expect "data density bits per square inch to bits per square meter"
    (close (convert_data_density 1000.0 BitsPerSquareInch BitsPerSquareMeter) 1550003.0);
  expect "data density bytes per square meter to bits per square meter"
    (close (convert_data_density 1.0 BytesPerSquareMeter BitsPerSquareMeter) 8.0);
  expect "round-trip data density"
    (close (convert_data_density (convert_data_density 7.25 BitsPerSquareMeter BytesPerSquareMeter) BytesPerSquareMeter BitsPerSquareMeter) 7.25);
  expect "data density unit string"
    (data_density_unit_to_string BytesPerSquareMeter = "bytes/m²");

  expect "torque newton-meters to pound-feet"
    (close (convert_torque 10.0 NewtonMeter PoundFoot) 7.37562);
  expect "torque kilogram-meters to newton-meters"
    (close (convert_torque 1.0 KilogramMeter NewtonMeter) 9.80665);
  expect "round-trip torque"
    (close (convert_torque (convert_torque 7.25 PoundFoot KilogramMeter) KilogramMeter PoundFoot) 7.25);
  expect "torque unit string"
    (torque_unit_to_string NewtonMeter = "Newton-meters")
