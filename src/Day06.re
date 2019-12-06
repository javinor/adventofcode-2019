let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");
let dummy_input = [|
  "COM)B",
  "B)C",
  "C)D",
  "D)E",
  "E)F",
  "B)G",
  "G)H",
  "D)I",
  "E)J",
  "J)K",
  "K)L",
|];

type tree('a) =
  | Node('a, list(tree('a)))
  | Leaf('a);

module Part1 = {
  let objects =
    input
    |> Js.Array.joinWith(")")
    |> Js.String.split(")")
    |> Array.fold_left(
         (xs, x) => Belt.Set.String.add(xs, x),
         Belt.Set.String.empty,
       );

  let orbitedBy =
    input
    |> Array.map(Js.String.split(")"))
    |> Array.map(arr => (arr[0], arr[1]))
    |> Array.fold_left(
         (orbitedBy, (center, satellite)) =>
           Belt.Map.String.update(orbitedBy, center, satellites =>
             Some([satellite, ...Js.Option.getWithDefault([], satellites)])
           ),
         Belt.Map.String.empty,
       );

  let rec tree_of_map = (map, root) => {
    switch (Belt.Map.String.get(map, root)) {
    | None => Leaf(root)
    | Some(values) => Node(root, values |> List.map(tree_of_map(map)))
    };
  };

  let sumTreeDepths = tree => {
    let rec go = (tree, depth) => {
      switch (tree) {
      | Leaf(_) => depth
      | Node(_, children) =>
        children
        |> List.fold_left((acc, t) => acc + go(t, depth + 1), depth)
      };
    };

    go(tree, 0);
  };

  let orbitsTree = tree_of_map(orbitedBy, "COM");
  let result = sumTreeDepths(orbitsTree);

  Js.log2("Part1 result: ", result);
};

module Part2 = {
  // let input = dummy_input;

  Js.log2("Part2 result: ", "TBD");
};