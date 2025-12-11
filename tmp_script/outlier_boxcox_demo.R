# Quick comparison of legacy vs new treatment defaults

suppressMessages(pkgload::load_all(path = "."))

set.seed(263)
x <- c(rnorm(120, 0, 1), rnorm(5, 5, 0.2))

legacy_pass <- check_SkewKurt(x, na.rm = TRUE, skew_thresh = 2.25)$Pass
new_pass <- check_SkewKurt(x, na.rm = TRUE)$Pass

cat("\nSkew:", round(skew(x, TRUE), 2),
    "Kurtosis:", round(kurt(x, TRUE), 2), "\n")
cat("Legacy rule passes? ", legacy_pass, "\n", sep = "")
cat("Handbook rule passes? ", new_pass, "\n", sep = "")

# Simulate the fallback step: Treat() used to log-transform, now picks Box-Cox.
# (winmax = 0 forces the second-stage transform so we can compare directly.)

legacy <- Treat.numeric(
  x,
  f1 = "winsorise",
  f1_para = list(winmax = 0, skew_thresh = 2.25, kurt_thresh = 3.5),
  f2 = "log_CT",
  f2_para = list(na.rm = TRUE),
  f_pass = "check_SkewKurt",
  f_pass_para = list(skew_thresh = 2.25, kurt_thresh = 3.5)
)

modern <- Treat.numeric(
  x,
  f1 = "winsorise",
  f1_para = list(winmax = 0),
  f2 = "boxcox_auto",
  f2_para = list(na.rm = TRUE),
  f_pass = "check_SkewKurt",
  f_pass_para = list(skew_thresh = 2, kurt_thresh = 3.5)
)

lambda <- modern$Dets_Table$boxcox_auto$lambda

cat("\nLegacy log skew:", round(skew(legacy$x, TRUE), 2), "\n")
cat("Box-Cox skew:", round(skew(modern$x, TRUE), 2),
    " (lambda =", round(lambda, 2), ")\n")

# In the full pipeline you simply call Treat()/qTreat(); the defaults now match
# the 'modern' branch (handbook thresholds + automatic Box-Cox).
