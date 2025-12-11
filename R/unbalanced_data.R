#' Prototype unbalanced indicator data
#'
#' A minimal indicator table used in tests and examples for
#' `new_unbalanced_coin()`. Each row is a unit (`uCode`) and the remaining
#' columns are raw indicators (`IndA1`, `IndA2`, `IndB`). The hierarchy is
#' intentionally unbalanced: `IndA1`/`IndA2` are aggregated into `SubA`, while
#' `IndB` feeds directly into the top-level `Index`.
#'
#' @format A data frame with 3 rows and 4 variables:
#' \describe{
#'   \item{uCode}{Character unit identifier.}
#'   \item{IndA1}{First component indicator contributing to `SubA`.}
#'   \item{IndA2}{Second component indicator contributing to `SubA`.}
#'   \item{IndB}{Indicator that links directly to the top-level `Index`.}
#' }
#' @seealso [unbal_iMeta]
"unbal_iData"

#' Prototype unbalanced metadata
#'
#' Companion metadata for [unbal_iData] defining the unbalanced hierarchy.
#' `IndA1` and `IndA2` roll up into `SubA`, which together with `IndB`
#' aggregates into the top-level `Index`. Weights are set to 0.5 so that each
#' child contributes equally within its parent group.
#'
#' @format A data frame with 5 rows and 6 variables:
#' \describe{
#'   \item{iCode}{Indicator or aggregate code.}
#'   \item{Level}{Intended hierarchy level (1 = indicator).}
#'   \item{Parent}{Parent code (blank/`NA` at the top level).}
#'   \item{Direction}{Indicator direction (`1` = higher is better).}
#'   \item{Weight}{Nominal weight within the parent group.}
#'   \item{Type}{`"Indicator"` or `"Aggregate"`.}
#' }
#' @seealso [unbal_iData]
"unbal_iMeta"

#' ASEM indicator data with an unbalanced hierarchy
#'
#' A copy of [ASEM_iData] paired with a modified hierarchy that introduces
#' uneven depth across the connectivity pillars. Several aggregates such as
#' "Connectivity" and "Physical" are split into additional intermediate
#' nodes (see [ASEM_unbal_iMeta]), which causes some indicators to roll up
#' through two steps while others feed directly into the higher levels. The
#' data are unchanged from the published ASEM release; only the hierarchy is
#' altered to illustrate the behaviour of [new_unbalanced_coin()].
#'
#' @format A data frame with 51 rows. Columns include the unit name (`uName`),
#'   unit code (`uCode`), four denominator columns (`Area`, `Energy`, `GDP`,
#'   `Population`), grouping variables, a `Time` stamp, and 55 indicator columns
#'   referenced by `iCode` values in [ASEM_unbal_iMeta].
#' @seealso [ASEM_unbal_iMeta], [ASEM_iData], [new_unbalanced_coin()]
"ASEM_unbal_iData"

#' ASEM metadata defining an unbalanced hierarchy
#'
#' Companion metadata for [ASEM_unbal_iData] that adds intermediate aggregates
#' beneath selected pillars. The helper nodes (`PhysTrans`, `ConFinance`,
#' `P2PCulture`, and others) create branches of different depths beneath the
#' `Connectivity` top level, resulting in a hierarchy that contains both
#' four-level and five-level paths. When constructing an unbalanced coin the
#' metadata should be supplied without the `iName` column so that automatically
#' generated placeholder nodes remain valid.
#'
#' @format A data frame with 81 rows and 10 variables:
#' \describe{
#'   \item{Level}{Integer depth inferred from the modified hierarchy.}
#'   \item{iCode}{Indicator or aggregate code.}
#'   \item{iName}{Human-readable name (remove before calling
#'     [new_unbalanced_coin()] so placeholder nodes can be inserted).}
#'   \item{Direction}{Indicator direction (`1` = higher is better, `-1` = lower is better).}
#'   \item{Weight}{Nominal weights carried over from the published ASEM metadata.}
#'   \item{Unit}{Original indicator measurement units.}
#'   \item{Target}{Target values, where available.}
#'   \item{Denominator}{Optional denominators used for normalisation.}
#'   \item{Parent}{Immediate parent code in the unbalanced hierarchy.}
#'   \item{Type}{Entry type (`"Indicator"`, `"Aggregate"`, `"Group"`, or `"Denominator"`).}
#' }
#' @details Intermediate aggregates are introduced for the physical, economic
#' connectivity, people-to-people, environmental, social, and sustainable
#' finance pillars. These additions yield paths of different depths (e.g.
#' `Flights → PhysTrans → Physical → Conn → Index` versus `CostImpEx →
#' Instit → Conn → Index`). The dataset provides a convenient starting point
#' for testing unbalanced workflows without modifying the raw ASEM indicators.
#' @seealso [ASEM_unbal_iData], [ASEM_iMeta], [new_unbalanced_coin()]
"ASEM_unbal_iMeta"
