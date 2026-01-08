# Visualise the regeneration pipeline targeted by `get_sensitivity()`

Generates a static `ggplot2` graphic showing the order and configuration
of each build step that will be executed during sensitivity or
uncertainty analysis. Steps touched by the supplied specifications are
highlighted and annotated with the corresponding overrides.

## Usage

``` r
plot_sensitivity_pipeline(
  coin,
  SA_specs,
  focus = c("all", "modified", "issues"),
  show_issues = TRUE
)
```

## Arguments

- coin:

  A coin object.

- SA_specs:

  A list of sensitivity specifications as supplied to
  [`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md).

- focus:

  Either `"all"` (default) to show the full pipeline, `"modified"` to
  keep only steps affected by the specifications, or `"issues"` to show
  only steps with validation issues.

- show_issues:

  Logical. If `TRUE` (default), adds visual indicators for validation
  issues detected in the pipeline.

## Value

A `ggplot` object with the pipeline data frame attached as an attribute
(`attr(plot, "pipeline_df")`).

## See also

[`get_sensitivity_pipeline()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity_pipeline.md)
for table-based pipeline extraction with validation.
