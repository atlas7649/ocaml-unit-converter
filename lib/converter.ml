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

let to_meters = function
  | Meter -> 1.0
  | Kilometer -> 1000.0
  | Foot -> 0.3048
  | Mile -> 1609.34

let from_meters v target = 
  match target with
  | Meter -> Meter, v
  | Kilometer -> Kilometer, (v /. 1000.0)
  | Foot -> Foot, (v /. 0.3048)
  | Mile -> Mile, (v /. 1609.34)

let convert_length value from_unit to_unit = 
  let meters = value *. (to_meters from_unit) in
  let (unit, result) = from_meters meters to_unit in
  result

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

let from_grams v target = 
  match target with
  | Gram -> Gram, v
  | Kilogram -> Kilogram, (v /. 1000.0)
  | Pound -> Pound, (v /. 453.592)
  | Ounce -> Ounce, (v /. 28.3495)

let convert_mass value from_unit to_unit = 
  let grams = value *. (to_grams from_unit) in
  let (_, result) = from_grams grams to_unit in
  result

let to_liters = function
  | Liter -> 1.0
  | Milliliter -> 0.001
  | Gallon -> 3.78541
  | Cup -> 0.236588

let from_liters v target = 
  match target with
  | Liter -> Liter, v
  | Milliliter -> Milliliter, (v /. 0.001)
  | Gallon -> Gallon, (v /. 3.78541)
  | Cup -> Cup, (v /. 0.236588)

let convert_volume value from_unit to_unit = 
  let liters = value *. (to_liters from_unit) in
  let (_, result) = from_liters liters to_unit in
  result

let to_pascals = function
  | Pascal -> 1.0
  | Bar -> 100000.0
  | PSI -> 6894.76
  | Atmosphere -> 101325.0

let from_pascals v target = 
  match target with
  | Pascal -> Pascal, v
  | Bar -> Bar, (v /. 100000.0)
  | PSI -> PSI, (v /. 6894.76)
  | Atmosphere -> Atmosphere, (v /. 101325.0)

let convert_pressure value from_unit to_unit = 
  let pascals = value *. (to_pascals from_unit) in
  let (_, result) = from_pascals pascals to_unit in
  result

let to_seconds = function
  | Second -> 1.0
  | Minute -> 60.0
  | Hour -> 3600.0
  | Day -> 86400.0

let from_seconds v target = 
  match target with
  | Second -> Second, v
  | Minute -> Minute, (v /. 60.0)
  | Hour -> Hour, (v /. 3600.0)
  | Day -> Day, (v /. 86400.0)

let convert_time value from_unit to_unit = 
  let seconds = value *. (to_seconds from_unit) in
  let (_, result) = from_seconds seconds to_unit in
  result