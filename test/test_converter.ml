open Converter

let approx_eq a b = Float.abs (a -. b) < 1e-6

let assert_conv label value from_unit to_unit convert expected =
  let got = convert value from_unit to_unit in
  if not (approx_eq got expected) then
    failwith (Printf.sprintf "%s: expected %f got %f" label expected got)

let roundtrip label convert units value =
  List.iter (fun from_unit ->
    List.iter (fun to_unit ->
      let result = convert (convert value from_unit to_unit) to_unit from_unit in
      if not (approx_eq result value) then
        failwith (Printf.sprintf "round-trip %s: %f" label result)
    ) units
  ) units

let () =
  assert_conv "length m->km" 100.0 Meter Kilometer convert_length 0.1;
  assert_conv "mass g->kg" 1000.0 Gram Kilogram convert_mass 1.0;
  assert_conv "temp c->f" 0.0 Celsius Fahrenheit convert_temp 32.0;
  assert_conv "temp c->k" 0.0 Celsius Kelvin convert_temp 273.15;
  assert_conv "volume l->ml" 1.0 Liter Milliliter convert_volume 1000.0;
  assert_conv "area m2->km2" 1000000.0 SquareMeter SquareKilometer convert_area 1.0;
  assert_conv "speed kmh->ms" 3.6 KilometersPerHour MetersPerSecond convert_speed 1.0;
  assert_conv "angle deg->rad" 180.0 Degree Radian convert_angle 3.14159265;
  assert_conv "torque nm->pf" 1.0 NewtonMeter PoundFoot convert_torque 0.73756;
  assert_conv "accel g->ms2" 1.0 StandardGravity MetersPerSecondSquared convert_acceleration 9.80665;
  assert_conv "angvel rpm->rads" 60.0 RevolutionsPerMinute RadiansPerSecond convert_angular_velocity 6.2831853;
  assert_conv "density gcm3->kgm3" 1.0 GramPerCubicCentimeter KilogramPerCubicMeter convert_density 1000.0;
  assert_conv "length mi->ft" 1.0 Mile Foot convert_length 5280.0;
  assert_conv "temp k->c" 0.0 Kelvin Celsius convert_temp (-273.15);
  assert_conv "mass lb->g" 1.0 Pound Gram convert_mass 453.592;
  assert_conv "volume gal->ml" 1.0 Gallon Milliliter convert_volume 3785.41;
  assert_conv "pressure atm->pa" 1.0 Atmosphere Pascal convert_pressure 101325.0;
  assert_conv "time day->hr" 1.0 Day Hour convert_time 24.0;
  assert_conv "energy kwh->j" 1.0 KilowattHour Joule convert_energy 3600000.0;
  assert_conv "area acre->m2" 1.0 Acre SquareMeter convert_area 4046.86;
  assert_conv "speed knot->ms" 1.0 Knot MetersPerSecond convert_speed 0.514444;
  assert_conv "storage gb->mb" 1.0 Gigabyte Megabyte convert_storage 1024.0;
  assert_conv "freq ghz->mhz" 1.0 Gigahertz Megahertz convert_frequency 1000.0;
  assert_conv "force kgf->n" 1.0 KilogramForce Newton convert_force 9.80665;
  assert_conv "fuel l100->usmpg" 8.0 LitersPer100km MilesPerGallonUS convert_fuel 29.401875;
  assert_conv "currency eur->usd" 100.0 EUR USD convert_currency 108.0;
  assert_conv "rate gbps->mbps" 1.0 GigabitsPerSecond MegabitsPerSecond convert_transfer_rate 1000.0;
  assert_conv "illuminance fc->lux" 1.0 FootCandle Lux convert_illuminance 10.7639;
  assert_conv "mag t->gauss" 1.0 Tesla Gauss convert_magnetic_flux_density 10000.0;
  assert_conv "viscosity p->pas" 1.0 Poise PascalSecond convert_viscosity 0.1;
  assert_conv "resistance mohm->ohm" 1.0 Megohm Ohm convert_resistance 1000000.0;
  assert_conv "flux lm->cdsr" 1.0 Lumen CandelaSteradian convert_luminous_flux 1.0;
  assert_conv "power hp->w" 1.0 Horsepower Watt convert_power 745.7;
  assert_conv "data density bitsin2->bitsm2" 1.0 BitsPerSquareInch BitsPerSquareMeter convert_data_density 1550.003;
  assert_conv "accel ftss->ms2" 1.0 FeetPerSecondSquared MetersPerSecondSquared convert_acceleration 0.3048;
  assert_conv "angvel degs->rads" 1.0 DegreesPerSecond RadiansPerSecond convert_angular_velocity (3.141592653589793 /. 180.0);
  assert_conv "density lbft3->kgm3" 1.0 PoundPerCubicFoot KilogramPerCubicMeter convert_density 16.018463;

  roundtrip "length" convert_length [Meter; Kilometer; Foot; Mile] 42.0;
  roundtrip "temperature" convert_temp [Celsius; Fahrenheit; Kelvin] 42.0;
  roundtrip "mass" convert_mass [Gram; Kilogram; Pound; Ounce] 42.0;
  roundtrip "volume" convert_volume [Liter; Milliliter; Gallon; Cup] 42.0;
  roundtrip "pressure" convert_pressure [Pascal; Bar; PSI; Atmosphere; Millibar] 42.0;
  roundtrip "time" convert_time [Second; Minute; Hour; Day] 42.0;
  roundtrip "energy" convert_energy [Joule; Calorie; WattHour; KilowattHour] 42.0;
  roundtrip "area" convert_area [SquareMeter; SquareKilometer; SquareFoot; Acre; Hectare] 42.0;
  roundtrip "speed" convert_speed [MetersPerSecond; KilometersPerHour; MilesPerHour; Knot] 42.0;
  roundtrip "storage" convert_storage [Byte; Kilobyte; Megabyte; Gigabyte; Terabyte; Petabyte; Exabyte; Zettabyte; Yottabyte] 42.0;
  roundtrip "angle" convert_angle [Degree; Radian; Gradian] 42.0;
  roundtrip "frequency" convert_frequency [Hertz; Kilohertz; Megahertz; Gigahertz] 42.0;
  roundtrip "force" convert_force [Newton; PoundForce; KilogramForce] 42.0;
  roundtrip "fuel" convert_fuel [LitersPer100km; MilesPerGallonUS; MilesPerGallonUK] 42.0;
  roundtrip "currency" convert_currency [USD; EUR; GBP; JPY] 42.0;
  roundtrip "transfer rate" convert_transfer_rate [BitsPerSecond; KilobitsPerSecond; MegabitsPerSecond; GigabitsPerSecond; BytesPerSecond; KilobytesPerSecond; MegabytesPerSecond; GigabytesPerSecond] 42.0;
  roundtrip "illuminance" convert_illuminance [Lux; FootCandle] 42.0;
  roundtrip "magnetic flux density" convert_magnetic_flux_density [Tesla; Gauss] 42.0;
  roundtrip "viscosity" convert_viscosity [PascalSecond; Poise; Centipoise] 42.0;
  roundtrip "resistance" convert_resistance [Ohm; Kilohm; Megohm] 42.0;
  roundtrip "luminous flux" convert_luminous_flux [Lumen; CandelaSteradian] 42.0;
  roundtrip "power" convert_power [Watt; Kilowatt; Horsepower] 42.0;
  roundtrip "data density" convert_data_density [BitsPerSquareMeter; BitsPerSquareInch; BytesPerSquareMeter] 42.0;
  roundtrip "torque" convert_torque [NewtonMeter; PoundFoot; KilogramMeter] 42.0;
  roundtrip "acceleration" convert_acceleration [MetersPerSecondSquared; FeetPerSecondSquared; StandardGravity; Gal] 42.0;
  roundtrip "angular velocity" convert_angular_velocity [RadiansPerSecond; DegreesPerSecond; RevolutionsPerMinute; RevolutionsPerSecond] 42.0;
  roundtrip "density" convert_density [KilogramPerCubicMeter; GramPerCubicCentimeter; PoundPerCubicFoot; PoundPerUSGallon] 42.0;

  print_endline "All conversion tests passed."