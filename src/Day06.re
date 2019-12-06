let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");

module Graph: {
  type t;

  let empty: t;
  let vertices: t => Belt.Set.String.t;
  let addVertex: (t, string) => t;
  let addEdge: (t, string, string) => t;
  let neighbors: (t, string) => Belt.Set.String.t;
} = {
  type vertices = Belt.Set.String.t;
  type edges = Belt.Map.String.t(vertices);
  type t = (vertices, edges);

  let mappend = (mxs, mys) =>
    switch (mxs, mys) {
    | (None, mys) => mys
    | (mxs, None) => mxs
    | (Some(xs), Some(ys)) => Some(Belt.Set.String.union(xs, ys))
    };

  let empty = (Belt.Set.String.empty, Belt.Map.String.empty);
  let vertices = fst;
  let addVertex = ((vs, es), v) => (Belt.Set.String.add(vs, v), es);
  let addEdge = ((vs, es), v1, v2) => {
    let vs' = Belt.Set.String.mergeMany(vs, [|v1, v2|]);

    let newEdges =
      Belt.Map.String.fromArray([|
        (v1, Belt.Set.String.fromArray([|v2|])),
        (v2, Belt.Set.String.fromArray([|v1|])),
      |]);

    let es' =
      Belt.Map.String.merge(es, newEdges, (_, v1, v2) => mappend(v1, v2));

    (vs', es');
  };

  let neighbors = ((_, es), v) => Belt.Map.String.getExn(es, v);
};

type tree('a) =
  | Node('a, list(tree('a)))
  | Leaf('a);

let tree_of_graph = (g, root) => {
  open Belt.Set.String;

  let rec go = (g, visited, v) => {
    let neighbors = diff(Graph.neighbors(g, v), visited) |> toList;
    List.length(neighbors) == 0
      ? Leaf(v) : Node(v, List.map(go(g, add(visited, v)), neighbors));
  };

  go(g, empty, root);
};

module Part1 = {
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

  // let input = dummy_input;

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

  let graph =
    input
    |> Array.map(Js.String.split(")"))
    |> Array.map(arr => (arr[0], arr[1]))
    |> Array.fold_left(
         (g, (v1, v2)) => Graph.addEdge(g, v1, v2),
         Graph.empty,
       );
  let tree = tree_of_graph(graph, "COM");
  let result = sumTreeDepths(tree);

  Js.log2("Part1 result: ", result);
};

module Part2 = {
  let findDepthOf = (tree, name) => {
    let rec go = (tree, depth, name) =>
      switch (tree) {
      | Leaf(name') => name == name' ? Some(depth) : None
      | Node(name', children) =>
        name == name'
          ? Some(depth)
          : children
            |> List.map(t => go(t, depth + 1, name))
            |> List.fold_left(Js.Option.firstSome, None)
      };

    go(tree, 0, name) |> Js.Option.getExn;
  };

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
    "K)YOU",
    "I)SAN",
  |];

  // let input = dummy_input;

  let graph =
    input
    |> Array.map(Js.String.split(")"))
    |> Array.map(arr => (arr[0], arr[1]))
    |> Array.fold_left(
         (g, (v1, v2)) => Graph.addEdge(g, v1, v2),
         Graph.empty,
       );

  let tree = tree_of_graph(graph, "YOU");
  let result = findDepthOf(tree, "SAN") - 2; // exclude "YOU" and "SAN"

  Js.log2("Part2 result: ", result);
};