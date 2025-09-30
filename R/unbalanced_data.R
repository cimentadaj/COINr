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
