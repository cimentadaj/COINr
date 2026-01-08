# Extract sensitivity analysis pipeline as a table with validation

Generates a data frame showing the order and configuration of each build
step that will be executed during sensitivity or uncertainty analysis.
Unlike
[`plot_sensitivity_pipeline()`](https://bluefoxr.github.io/COINr/reference/plot_sensitivity_pipeline.md),
this function returns a table that can be programmatically inspected,
and includes validation checks to detect pipeline configuration issues
before running expensive SA computations.

## Usage

``` r
get_sensitivity_pipeline(
  coin,
  SA_specs,
  validate = TRUE,
  focus = c("all", "modified", "issues")
)
```

## Arguments

- coin:

  A coin object.

- SA_specs:

  A list of sensitivity specifications as supplied to
  [`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md).

- validate:

  Logical. If `TRUE` (default), runs validation checks on the pipeline
  to detect issues such as dataset order mismatches, data overwriting,
  and missing datasets.

- focus:

  Either `"all"` (default) to show the full pipeline, `"modified"` to
  keep only steps affected by the specifications, or `"issues"` to keep
  only steps with validation issues (requires `validate = TRUE`).

## Value

A data frame with columns:

- step_id:

  Name of the pipeline step (e.g., "new_coin", "Impute").

- order:

  Sequential order of the step in the pipeline.

- input_dset:

  Name of the input dataset for this step.

- output_dset:

  Name of the output dataset for this step.

- log_args:

  Arguments logged for this step (e.g., "dset = Raw").

- spec_args:

  Specifications that will override defaults during SA.

- modified:

  Logical indicating if this step is modified by SA_specs.

- has_issues:

  Logical indicating if validation detected issues (when
  `validate = TRUE`).

- issue_type:

  Type of issue detected (e.g., "dataset_mismatch").

- issue_severity:

  Severity level: "error", "warning", or NA.

- issue_message:

  Human-readable description of the issue.

## Details

### Validation Checks

When `validate = TRUE`, the function performs the following checks:

1.  **Dataset Order Validation**: Ensures each step's input dataset
    matches the previous step's output dataset, preventing issues like
    `new_coin → treat → normalise → impute` (wrong order).

2.  **Data Overwriting Detection**: Flags when multiple steps write to
    the same dataset, which can cause data loss and unpredictable
    results.

3.  **Missing Dataset Check**: Verifies that input datasets exist in the
    current coin state, preventing runtime errors.

### Focus Modes

- `focus = "all"`: Returns complete pipeline table (default)

- `focus = "modified"`: Returns only steps affected by SA_specs

- `focus = "issues"`: Returns only steps with validation issues

## See also

[`plot_sensitivity_pipeline()`](https://bluefoxr.github.io/COINr/reference/plot_sensitivity_pipeline.md),
[`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md),
[`get_sensitivity2()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity2.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get full pipeline with validation
pipeline <- get_sensitivity_pipeline(coin, SA_specs)
print(pipeline)

# Check only for issues
issues <- get_sensitivity_pipeline(coin, SA_specs, focus = "issues")
if (nrow(issues) > 0) {
  print("Pipeline has issues:")
  print(issues[, c("step_id", "issue_type", "issue_message")])
}

# Get modified steps only
modified <- get_sensitivity_pipeline(coin, SA_specs, focus = "modified")
} # }
```
