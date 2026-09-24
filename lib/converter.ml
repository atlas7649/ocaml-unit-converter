type unit = 
  | Meter of float
  | Kilometer of float
  | Foot of float
  | Mile of float
  | Celsius of float
  | Fahrenheit of float
  | Kelvin of float

let to_meters = function
  | Meter v -> v
  | Kilometer v -> v *. 1000.0
  | Foot v -> v *. 0.3048
  | Mile v -> v *. 1609.34
  | _ -> failwith "Unit is not a length unit"

let from_meters v target = 
  match target with
  | Meter _ -> Meter v
  | Kilometer _ -> Kilometer (v /. 1000.0)
  | Foot _ -> Foot (v /. 0.3048)
  | Mile _ -> Mile (v /. 1609.34)
  | _ -> failwith "Target is not a length unit"

let convert_length value from_unit to_unit = 
  let meters = to_meters (from_unit value) in
  from_meters meters to_unit

let convert_temp value from_unit to_unit = 
  let kelvin = match from_unit with
    | Celsius _ -> value +. 273.15
    | Fahrenheit _ -> (value -. 32.0) *. (5.0 /. 9.0) +. 273.15
    | Kelvin _ -> value
    | _ -> failwith "Not a temperature unit"
  in
  match to_unit with
  | Celsius _ -> Celsius (kelvin -. 273.15)
  | Fahrenheit _ -> Fahrenheit ((kelvin -. 273.15) *. (9.0 /. 5.0) +. 32.0)
  | Kelvin _ -> Kelvin kelvin
  | _ -> failwith "Target is not a temperature unit"