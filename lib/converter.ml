type length_unit = 
  | Meter
  | Kilometer
  | Foot
  | Mile

type temp_unit = 
  | Celsius
  | Fahrenheit
  | Kelvin

type mass_unit = 
  | Gram
  | Kilogram
  | Pound
  | Ounce

type volume_unit = 
  | Liter
  | Milliliter
  | Gallon
  | Cup

type pressure_unit = 
  | Pascal
  | Bar
  | PSI
  | Atmosphere

type time_unit = 
  | Second
  | Minute
  | Hour
  | Day

type energy_unit = 
  | Joule
  | Calorie
  | WattHour
  | KilowattHour

let length_unit_to_string = function
  | Meter -> "meters"
  | Kilometer -> "kilometers"
  | Foot -> "feet"
  | Mile -> "miles"

let temp_unit_to_string = function
  | Celsius -> "Celsius"
  | Fahrenheit -> "Fahrenheit"
  | Kelvin -> "Kelvin"

let mass_unit_to_string = function
  | Gram -> "grams"
  | Kilogram -> "kilograms"
  | Pound -> "pounds"
  | Ounce -> "ounces"

let volume_unit_to_string = function
  | Liter -> "liters"
  | Milliliter -> "milliliters"
  | Gallon -> "gallons"
  | Cup -> "cups"

let pressure_unit_to_string = function
  | Pascal -> "pascals"
  | Bar -> "bars"
  | PSI -> "psi"
  | Atmosphere -> "atmospheres"

let time_unit_to_string = function
  | Second -> "seconds"
  | Minute -> "minutes"
  | Hour -> "hours"
  | Day -> "days"

let energy_unit_to_string = function
  | Joule -> "joules"
  | Calorie -> "calories"
  | WattHour -> "watt-hours"
  | KilowattHour -> "kilowatt-hours"

(* Generic helper for linear conversions *)
let convert_linear value from_unit to_unit to_base from_base = 
  let base_val = value *. (to_base from_unit) in
  base_val /. (to_base to_unit)

let to_meters = function
  | Meter -> 1.0
  | Kilometer -> 1000.0
  | Foot -> 0.3048
  | Mile -> 1609.34

let convert_length value from_unit to_unit = 
  convert_linear value from_unit to_unit to_meters to_meters

let convert_temp value from_unit to_unit = 
  let kelvin = match from_unit with
    | Celsius -> value +. 273.15
    | Fahrenheit -> (value -. 32.0) *. (5.0 /. 9.0) +. 273.15
    | Kelvin -> value
  in
  match to_unit with
  | Celsius -> kelvin -. 273.15
  | Fahrenheit -> (kelvin -. 273.15) *. (9.0 /. 5.0) +. 32.0
  | Kelvin -> kelvin

let to_grams = function
  | Gram -> 1.0
  | Kilogram -> 1000.0
  | Pound -> 453.592
  | Ounce -> 28.3495

let convert_mass value from_unit to_unit = 
  convert_linear value from_unit to_unit to_grams to_grams

let to_liters = function
  | Liter -> 1.0
  | Milliliter -> 0.001
  | Gallon -> 3.78541
  | Cup -> 0.236588

let convert_volume value from_unit to_unit = 
  convert_linear value from_unit to_unit to_liters to_liters

let to_pascals = function
  | Pascal -> 1.0
  | Bar -> 100000.0
  | PSI -> 6894.76
  | Atmosphere -> 101325.0

let convert_pressure value from_unit to_unit = 
  convert_linear value from_unit to_unit to_pascals to_pascals

let to_seconds = function
  | Second -> 1.0
  | Minute -> 60.0
  | Hour -> 3600.0
  | Day -> 86400.0

let convert_time value from_unit to_unit = 
  convert_linear value from_unit to_unit to_seconds to_seconds

let to_joules = function
  | Joule -> 1.0
  | Calorie -> 4.184
  | WattHour -> 3600.0
  | KilowattHour -> 3600000.0

let convert_energy value from_unit to_unit = 
  convert_linear value from_unit to_unit to_joules to_joules