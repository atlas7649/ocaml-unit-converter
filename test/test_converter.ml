open Converter

let approx_eq a b = Float.abs (a -. b) < 1e-6

let assert_conv label value from_unit to_unit convert expected =
  let got = convert value from_unit to_unit in
  if not (approx_eq got expected) then
    failwith (Printf.sprintf "%s: expected %f got %f" label expected got)

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
  print_endline "All conversion tests passed."