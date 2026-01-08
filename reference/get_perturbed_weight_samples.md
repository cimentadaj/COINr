# Generate samples of perturbed weights respecting requirement to sum to 1

Given a vector of weights `w`, returns a matrix of `Nrep` rows where
each row is `w` with a random uniform perturbation applied, as specified
by `pert_by`. Uses a type of rejection sampling, with the requirement
that weight sets must sum to 1 +/- `tolerance`.

## Usage

``` r
get_perturbed_weight_samples(
  w,
  pert_by,
  Nrep,
  tolerance = 0.01,
  quietly = TRUE
)
```

## Arguments

- w:

  Vector of numeric weights

- pert_by:

  A positive number representing the fraction by which to perturb
  weights. E.g. setting to 0.5 will perturb weights +/- 50% of their
  nominal values.

- Nrep:

  The number of weight sets to generate.

- tolerance:

  The tolerance (see description).

- quietly:

  Logical: if `FALSE` will also report the total number of weight draws.

## Value

A matrix with `Nrep` rows, where each row is a weight sample.

## Examples

``` r
# vector of four equal weights
w <- c(0.25, 0.25, 0.25, 0.25)

# perturb by +/-10%, generate 10 vectors of perturbed weights
get_perturbed_weight_samples(w, pert_by = 0.1, Nrep = 10, quietly = FALSE)
#> Generated 10 constrained weight samples based on 29 weight draws.
#>            [,1]      [,2]      [,3]      [,4]
#>  [1,] 0.2707959 0.2259526 0.2301587 0.2653287
#>  [2,] 0.2468957 0.2594019 0.2266378 0.2710643
#>  [3,] 0.2571889 0.2646760 0.2398506 0.2284056
#>  [4,] 0.2410702 0.2581962 0.2358866 0.2657348
#>  [5,] 0.2328403 0.2341565 0.2593226 0.2648917
#>  [6,] 0.2287056 0.2666069 0.2684465 0.2356014
#>  [7,] 0.2587016 0.2307075 0.2607462 0.2472742
#>  [8,] 0.2497510 0.2467646 0.2578017 0.2459110
#>  [9,] 0.2323219 0.2423362 0.2629651 0.2664889
#> [10,] 0.2607529 0.2673060 0.2312830 0.2378729

```
