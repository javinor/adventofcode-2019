let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync;

let toChunks = (xs, size) => {
  let rec go = (result, xs, size) =>
    if (List.length(xs) == 0) {
      result;
    } else {
      let (chunk, rest) = Belt.List.splitAt(xs, size) |> Belt.Option.getExn;
      go([chunk, ...result], rest, size);
    };

  go([], xs, size) |> List.rev;
};

let countNum = (xs, num) =>
  xs |> List.map(x => x == num ? 1 : 0) |> List.fold_left((+), 0);

let width = 25;
let height = 6;
let nLayerPixels = width * height;

module Part1 = {
  let layers =
    input
    |> Js.String.split("")
    |> Array.map(int_of_string)
    |> Array.to_list
    |> toChunks(_, nLayerPixels)
    |> Array.of_list;

  let zeroCount = layers |> Array.map(l => countNum(l, 0));

  let minZeroes = zeroCount |> Array.fold_left(min, max_int);
  let minZeroesIndex = zeroCount |> Js.Array.findIndex(z => z == minZeroes);

  let ones = layers[minZeroesIndex] |> countNum(_, 1);
  let twos = layers[minZeroesIndex] |> countNum(_, 2);

  Js.log2("Part1 result: ", ones * twos);
};

module Part2 = {
  type color =
    | Black
    | White
    | Transparent;

  let layers =
    input
    |> Js.String.split("")
    |> Array.map(int_of_string)
    |> Array.to_list
    |> toChunks(_, nLayerPixels)
    |> List.map(l =>
         l
         |> List.map(n =>
              switch (n) {
              | 0 => Black
              | 1 => White
              | 2 => Transparent
              | _ =>
                failwith("invalid input: unknown color " ++ string_of_int(n))
              }
            )
       );

  let image =
    layers
    |> List.fold_left(
         (image, layer) =>
           Belt.List.zipBy(image, layer, (topColor, bottomColor) =>
             switch (topColor) {
             | Black
             | White => topColor
             | Transparent => bottomColor
             }
           ),
         Belt.List.make(nLayerPixels, Transparent),
       )
    |> List.map(c =>
         switch (c) {
         | Black => " "
         | White => "#"
         | Transparent =>
           failwith("should not have transparent colors remaining")
         }
       )
    |> toChunks(_, width)
    |> List.map(row => row |> Array.of_list |> Js.Array.joinWith(""))
    |> Array.of_list
    |> Js.Array.joinWith("\n");

  Js.log("Part2 result:");
  Js.log(image);
};