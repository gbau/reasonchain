[@bs.module "js-sha256"] external sha256 : string => string = "sha256";

/* types */
type block = {
  index: int,
  timestamp: float,
  data: string,
  previousHash: string,
  hash: string,
};
type blockchain = list(block);

/* create block hash */
let hashBlock = (~index, ~timestamp, ~data, ~previousHash): string => {
  let toHash = {j|$index$timestamp$data$previousHash|j};
  sha256(toHash);
};

/* create next block given last block*/
let createBlock = (block: block): block => {
  let index = block.index + 1;
  let timestamp = Js.Date.now();
  let data = {j|$index-$timestamp|j};
  let previousHash = block.hash;

  let hash = hashBlock(
    ~index=index,
    ~timestamp=timestamp,
    ~data=data,
    ~previousHash=previousHash
  );

  {
    index,
    data,
    timestamp,
    previousHash,
    hash,
  };
};

/* generate range */
let rec range = (a, b) =>
  switch (a > b) {
    | true => []
    | false => [a, ...range(a + 1, b)]
  };

/* genesis block */
let genesis = {
  index: 0,
  timestamp: Js.Date.now(),
  data: "genesis-block",
  previousHash: "prev-hash",
  hash: "genesis-hash",
};

let reasonchain = List.fold_left(
  (blockchain, i) => {
    let lastBlock = List.nth(blockchain, i - 1);
    let nextBlock = createBlock(lastBlock);
    List.append(blockchain, [nextBlock]);
  },
  [genesis],
  range(1, 14)
);

List.iter(block => Js.log(block), reasonchain);
