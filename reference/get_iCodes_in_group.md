# Find children or parents of an iCode

Finds the children or parents of any iCode `iCode_group` at a specified
level `at_level`. This is useful for identifying which indicators belong
to a particular pillar or sub-index, or for finding the parent groups of
a set of indicators.

## Usage

``` r
get_iCodes_in_group(coin, iCode_group, at_level)
```

## Arguments

- coin:

  A coin

- iCode_group:

  An iCode to find children/parents for

- at_level:

  Level of framework to identify children/parents at

## Value

Character vector of iCodes at the specified level that are
children/parents of `iCode_group`

## Details

The function works by using the hierarchical structure stored in
`coin$Meta$Lineage` to identify all indicators that share the same
parent at the specified levels. It works with both balanced and
unbalanced frameworks.

## Examples

``` r
# Build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# Find all level 2 indicators (pillars) within the "Conn" sub-index (level 3)
get_iCodes_in_group(coin, iCode_group = "Conn", at_level = 2)
#> [1] "ConEcFin"  "Instit"    "P2P"       "Physical"  "Political"

# Find all level 1 indicators within the "Political" pillar
get_iCodes_in_group(coin, iCode_group = "Political", at_level = 1)
#> [1] "Embs"   "IGOs"   "UNVote"
```
