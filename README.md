# OCaml Unit Converter

A small, type-safe library for converting between different physical units.

## Features
- Length conversion (Millimeters, Centimeters, Decimeters, Meters, Kilometers, Feet, Miles, and more)
- Temperature conversion (Celsius, Fahrenheit, Kelvin, Rankine)
- Mass conversion (Grams, Kilograms, Milligrams, Centigrams, Decigrams, Micrograms, Nanograms, Pounds, Ounces)
- Volume conversion (Liters, Milliliters, Centiliters, Deciliters, Microliters, Nanoliters, Gallons, Cups)
- Pressure conversion (Pascals, Bars, PSI, Atmospheres, Millibars/hPa)
- Time conversion (Seconds, Minutes, Hours, Days)
- Energy conversion (Joules, Calories, Watt-hours, Kilowatt-hours)
- Area conversion (Square Millimeters, Square Centimeters, Square Decimeters, Square Meters, Square Micrometers, Square Nanometers, Square Kilometers, Square Feet, Acres, Hectares)
- Speed conversion (Meters/Second, Kilometers/Hour, Miles/Hour, Knots)
- Storage conversion (Bytes through Yottabytes)
- Angle conversion (Degrees, Radians, Gradians)
- Frequency conversion (Hertz through Gigahertz)
- Force conversion (Newtons, Pound-force, Kilogram-force)
- Fuel efficiency conversion (Liters/100km, US/UK mpg)
- Currency conversion (USD, EUR, GBP, JPY)
- Transfer rate conversion (Bits/Bytes per second through gigabits/gigabytes)
- Illuminance conversion (Lux, Foot-candles)
- Magnetic flux density conversion (Tesla, Gauss)
- Viscosity conversion (Pascal-seconds, Poise, Centipoise)
- Resistance conversion (Ohms, Kilohms, Megohms)
- Luminous flux conversion (Lumens, Candela-steradians)
- Power conversion (Watts, Kilowatts, Horsepower)
- Data density conversion (Bits/Bytes per square meter/inch)
- Torque conversion (Newton-meters, Pound-feet, Kilogram-meters)
- Acceleration conversion (Meters/second², Feet/second², Standard gravity, Gal)
- Angular velocity conversion (Radians/second, Degrees/second, Revolutions/minute, Revolutions/second)
- Density conversion (Kilograms/Cubic Meter, Grams/Cubic Centimeter, Pounds/Cubic Foot, Pounds/US Gallon)
- Voltage conversion (Volts, Millivolts, Microvolts, Nanovolts, Kilovolts, Megavolts, Gigavolts)
- Current conversion (Amperes, Milliamperes, Microamperes, Nanoamperes, Kiloamperes, Megaamperes, Gigaamperes)

## Setup

Build and test the project with Dune:

```sh
dune build
dune test
```

Run the demonstration executable:

```sh
dune exec ocaml-unit-converter
```

## Usage

Use the `Converter` module to perform conversions between units.

```ocaml
open Converter

let () =
  Printf.printf "%.4f kilometers\n" (convert_length 100.0 Meter Kilometer);
  Printf.printf "%.4f grams\n" (convert_mass 1000.0 Milligram Gram);
  Printf.printf "%.4f liters\n" (convert_volume 100.0 Centiliter Liter)
```
