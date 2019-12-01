let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";

let dummy_input = [|"12", "14", "1969", "100756"|];

module Part1 = {
  let input =
    inputPath
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String.split("\n")
    |> Array.map(int_of_string);

  let result =
    input |> Array.map(mass => mass / 3 - 2) |> Array.fold_left((+), 0);

  Js.log("Part1 result: " ++ string_of_int(result));
};

module Part2 = {
  let input =
    inputPath
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String.split("\n")
    |> Array.map(int_of_string);

  let calc_total_fuel = mass => {
    let rec go = (acc, mass) => {
      let fuel = max(mass / 3 - 2, 0);
      fuel == 0 ? acc + fuel : go(acc + fuel, fuel);
    };
    go(0, mass);
  };

  let result =
    input |> Array.map(calc_total_fuel) |> Array.fold_left((+), 0);

  Js.log("Part2 result: " ++ string_of_int(result));
};