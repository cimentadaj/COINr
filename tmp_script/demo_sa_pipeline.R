# Example usage of plot_sensitivity_pipeline()

suppressPackageStartupMessages({
  if(requireNamespace("devtools", quietly = TRUE)){
    devtools::load_all(quiet = TRUE)
  }
  if(!"package:COINr" %in% search()){
    library(COINr)
  }
})

# Build example coin
coin <- build_example_coin(quietly = TRUE)

# Define illustrative sensitivity specifications
SA_specs <- list(
  Winmax = list(
    Address = "$Log$Treat$global_specs$f1_para$winmax",
    Distribution = 3:7,
    Type = "discrete"
  ),
  AggDset = list(
    Address = "$Log$Aggregate$dset",
    Distribution = c("Normalised", "Treated"),
    Type = "discrete"
  )
)

# Visualise pipeline and extract attached summary
plot_sensitivity_pipeline(coin, SA_specs, focus = "all")
