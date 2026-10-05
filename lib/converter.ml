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

type area_unit = 
  | SquareMeter
  | SquareKilometer
  | SquareFoot
  | Acre
  | Hectare

type speed_unit = 
  | MetersPerSecond
  | KilometersPerHour
  | MilesPerHour
  | Knot

type storage_unit = 
  | Byte
  | Kilobyte
  | Megabyte
  | Gigabyte
  | Terabyte

type angle_unit = 
  | Degree
  | Radian
  | Gradian

type frequency_unit = 
  | Hertz
  | Kilohertz
  | Megahertz
  | Gigahertz

type force_unit = 
  | Newton
  | PoundForce
  | KilogramForce

type fuel_unit = 
  | LitersPer100km
  | MilesPerGallonUS
  | MilesPerGallonUK

type currency_unit = 
  | USD
  | EUR
  | GBP
  | JPY

type transfer_rate_unit = 
  | BitsPerSecond
  | KilobitsPerSecond
  | MegabitsPerSecond
  | GigabitsPerSecond
  | BytesPerSecond
  | KilobytesPerSecond
  | MegabytesPerSecond
  | GigabytesPerSecond

type illuminance_unit = 
  | Lux
  | FootCandle

type magnetic_flux_density_unit = 
  | Tesla
  | Gauss

type viscosity_unit = 
  | PascalSecond
  | Poise
  | Centipoise

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

let area_unit_to_string = function
  | SquareMeter -> "square meters"
  | SquareKilometer -> "square kilometers"
  | SquareFoot -> "square feet"
  | Acre -> "acres"
  | Hectare -> "hectares"

let speed_unit_to_string = function
  | MetersPerSecond -> "m/s"
  | KilometersPerHour -> "km/h"
  | MilesPerHour -> "mph"
  | Knot -> "knots"

let storage_unit_to_string = function
  | Byte -> "bytes"
  | Kilobyte -> "kilobytes"
  | Megabyte -> "megabytes"
  | Gigabyte -> "gigabytes"
  | Terabyte -> "terabytes"

let angle_unit_to_string = function
  | Degree -> "degrees"
  | Radian -> "radians"
  | Gradian -> "gradians"

let frequency_unit_to_string = function
  | Hertz -> "hertz"
  | Kilohertz -> "kilohertz"
  | Megahertz -> "megahertz"
  | Gigahertz -> "gigahertz"

let force_unit_to_string = function
  | Newton -> "newtons"
  | PoundForce -> "pound-force"
  | KilogramForce -> "kilogram-force"

let fuel_unit_to_string = function
  | LitersPer100km -> "L/100km"
  | MilesPerGallonUS -> "US mpg"
  | MilesPerGallonUK -> "UK mpg"

let currency_unit_to_string = function
  | USD -> "USD"
  | EUR -> "EUR"
  | GBP -> "GBP"
  | JPY -> "JPY"

let transfer_rate_unit_to_string = function
  | BitsPerSecond -> "bps"
  | KilobitsPerSecond -> "kbps"
  | MegabitsPerSecond -> "Mbps"
  | GigabitsPerSecond -> "Gbps"
  | BytesPerSecond -> "B/s"
  | KilobytesPerSecond -> "KB/s"
  | MegabytesPerSecond -> "MB/s"
  | GigabytesPerSecond -> "GB/s"

let illuminance_unit_to_string = function
  | Lux -> "lux"
  | FootCandle -> "foot-candles"

let magnetic_flux_density_unit_to_string = function
  | Tesla -> "teslas"
  | Gauss -> "gauss"

let viscosity_unit_to_string = function
  | PascalSecond -> "Pa·s"
  | Poise -> "poise"
  | Centipoise -> "centipoise"

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

let to_square_meters = function
  | SquareMeter -> 1.0
  | SquareKilometer -> 1000000.0
  | SquareFoot -> 0.092903
  | Acre -> 4046.86
  | Hectare -> 10000.0

let convert_area value from_unit to_unit = 
  convert_linear value from_unit to_unit to_square_meters to_square_meters

let to_meters_per_second = function
  | MetersPerSecond -> 1.0
  | KilometersPerHour -> 1.0 /. 3.6
  | MilesPerHour -> 0.44704
  | Knot -> 0.514444

let convert_speed value from_unit to_unit = 
  convert_linear value from_unit to_unit to_meters_per_second to_meters_per_second

let to_bytes = function
  | Byte -> 1.0
  | Kilobyte -> 1024.0
  | Megabyte -> 1048576.0
  | Gigabyte -> 1073741824.0
  | Terabyte -> 1099511627776.0

let convert_storage value from_unit to_unit = 
  convert_linear value from_unit to_unit to_bytes to_bytes

let to_radians = function
  | Radian -> 1.0
  | Degree -> 3.141592653589793 /. 180.0
  | Gradian -> 3.141592653589793 /. 200.0

let convert_angle value from_unit to_unit = 
  convert_linear value from_unit to_unit to_radians to_radians

let to_hertz = function
  | Hertz -> 1.0
  | Kilohertz -> 1000.0
  | Megahertz -> 1000000.0
  | Gigahertz -> 1000000000.0

let convert_frequency value from_unit to_unit = 
  convert_linear value from_unit to_unit to_hertz to_hertz

let to_newtons = function
  | Newton -> 1.0
  | PoundForce -> 4.44822
  | KilogramForce -> 9.80665

let convert_force value from_unit to_unit = 
  convert_linear value from_unit to_unit to_newtons to_newtons

let convert_fuel value from_unit to_unit = 
  let l_per_100km = match from_unit with
    | LitersPer100km -> value
    | MilesPerGallonUS -> 235.215 /. value
    | MilesPerGallonUK -> 282.481 /. value
  in
  match to_unit with
  | LitersPer100km -> l_per_100km
  | MilesPerGallonUS -> 235.215 /. l_per_100km
  | MilesPerGallonUK -> 282.481 /. l_per_100km

let to_usd = function
  | USD -> 1.0
  | EUR -> 1.08
  | GBP -> 1.27
  | JPY -> 0.0067

let convert_currency value from_unit to_unit = 
  convert_linear value from_unit to_unit to_usd to_usd

let to_bits_per_second = function
  | BitsPerSecond -> 1.0
  | KilobitsPerSecond -> 1000.0
  | MegabitsPerSecond -> 1000000.0
  | GigabitsPerSecond -> 1000000000.0
  | BytesPerSecond -> 8.0
  | KilobytesPerSecond -> 8000.0
  | MegabytesPerSecond -> 8000000.0
  | GigabytesPerSecond -> 8000000000.0

let convert_transfer_rate value from_unit to_unit = 
  convert_linear value from_unit to_unit to_bits_per_second to_bits_per_second

let to_lux = function
  | Lux -> 1.0
  | FootCandle -> 10.7639

let convert_illuminance value from_unit to_unit = 
  convert_linear value from_unit to_unit to_lux to_lux

let to_teslas = function
  | Tesla -> 1.0
  | Gauss -> 0.0001

let convert_magnetic_flux_density value from_unit to_unit = 
  convert_linear value from_unit to_unit to_teslas to_teslas

let to_pascal_seconds = function
  | PascalSecond -> 1.0
  | Poise -> 0.1
  | Centipoise -> 0.001

let convert_viscosity value from_unit to_unit = 
  convert_linear value from_unit to_unit to_pascal_seconds to_pascal_seconds