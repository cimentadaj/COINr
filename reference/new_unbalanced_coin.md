# Create a new unbalanced coin

Create a new unbalanced coin

## Usage

``` r
new_unbalanced_coin(
  iData,
  iMeta,
  exclude = NULL,
  split_to = NULL,
  level_names = NULL,
  retain_all_uCodes_on_split = FALSE,
  quietly = FALSE
)
```

## Arguments

- iData:

  The indicator data and metadata of each unit

- iMeta:

  Indicator metadata

- exclude:

  Optional character vector of any indicator codes (`iCode`s) to exclude
  from the coin(s).

- split_to:

  This is used to split panel data into multiple coins, a so-called
  "purse". Should be either `"all"`, or a subset of entries in
  `iData$Time`. See Details.

- level_names:

  Optional character vector of names of levels. Must have length equal
  to the number of levels in the hierarchy
  (`max(iMeta$Level, na.rm = TRUE)`).

- retain_all_uCodes_on_split:

  Logical: if panel data is input and split to a purse using `split_to`,
  this controls how units with no data at certain time points are
  handled. If set `FALSE`, then unit at time t with no data in any
  indicators will be removed completely from the coin for that time
  point. If `TRUE`, all units will be included in every time point. The
  latter option may be useful if you impute over time.

- quietly:

  If `TRUE`, suppresses all messages

## Value

An object of class `unbalanced_coin` that delegates to the standard coin
pipeline while keeping track of placeholder nodes used to balance the
hierarchy internally.
