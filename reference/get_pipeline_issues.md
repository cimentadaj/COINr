# Get pipeline issues only

Convenience function to extract only pipeline steps with validation
issues.

## Usage

``` r
get_pipeline_issues(coin, SA_specs)
```

## Arguments

- coin:

  A coin object.

- SA_specs:

  A list of sensitivity specifications.

## Value

A data frame containing only steps with issues, or an empty data frame
if no issues are detected. A message is printed if no issues are found.

## See also

[`get_sensitivity_pipeline()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity_pipeline.md)

## Examples

``` r
if (FALSE) { # \dontrun{
issues <- get_pipeline_issues(coin, SA_specs)
if (nrow(issues) > 0) {
  print(issues[, c("step_id", "issue_type", "issue_message")])
}
} # }
```
