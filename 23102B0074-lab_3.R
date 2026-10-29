library(microbenchmark)

set.seed(42)

n <- 300
heart <- data.frame(
  age       = sample(29:77, n, replace = TRUE),
  trestbps  = round(rnorm(n, mean = 131, sd = 17)),
  chol      = round(rnorm(n, mean = 246, sd = 52)),
  thalach   = round(rnorm(n, mean = 149, sd = 23))
)

corrupt_idx_neg  <- sample(1:n, 8)
corrupt_idx_na   <- sample(setdiff(1:n, corrupt_idx_neg), 10)
corrupt_idx_high <- sample(setdiff(1:n, c(corrupt_idx_neg, corrupt_idx_na)), 6)

heart$trestbps[corrupt_idx_neg]  <- -heart$trestbps[corrupt_idx_neg]
heart$trestbps[corrupt_idx_na]   <- NA
heart$trestbps[corrupt_idx_high] <- sample(305:360, length(corrupt_idx_high), replace = TRUE)

cat("Sample of raw (corrupted) trestbps values:\n")
print(head(heart$trestbps, 20))

clean_bp_value <- function(bp) {
  if (is.na(bp)) {
    return(NA)
  } else if (bp < 0) {
    return(NA)
  } else if (bp > 250) {
    return(250)
  } else {
    return(bp)
  }
}

clean_bp_vector <- function(bp_vector) {
  sapply(bp_vector, clean_bp_value)
}

heart$trestbps_cleaned <- clean_bp_vector(heart$trestbps)

cat("\nBefore vs after cleaning (first 20 values):\n")
print(data.frame(
  original = head(heart$trestbps, 20),
  cleaned  = head(heart$trestbps_cleaned, 20)
))

safe_mean_bp <- function(bp_vector) {
  tryCatch({
    m <- mean(bp_vector, na.rm = TRUE)
    if (is.nan(m)) {
      stop("All values are missing; mean cannot be computed.")
    }
    return(m)
  },
  error = function(e) {
    message("Error while computing mean BP: ", conditionMessage(e))
    return(NA)
  },
  warning = function(w) {
    message("Warning while computing mean BP: ", conditionMessage(w))
    return(NA)
  })
}

mean_bp <- safe_mean_bp(heart$trestbps_cleaned)
cat("\nSafe mean of cleaned BP:", mean_bp, "\n")

safe_ratio <- function(numerator, denominator) {
  tryCatch({
    if (is.na(denominator) || is.na(numerator)) {
      warning("Missing value encountered in ratio calculation.")
    }
    if (!is.na(denominator) && denominator == 0) {
      stop("Denominator is zero; ratio is undefined.")
    }
    ratio <- numerator / denominator
    return(ratio)
  },
  error = function(e) {
    message("Error computing ratio: ", conditionMessage(e))
    return(NA)
  },
  warning = function(w) {
    message("Warning computing ratio: ", conditionMessage(w))
    return(NA)
  })
}

heart$chol_bp_ratio <- mapply(safe_ratio, heart$chol, heart$trestbps_cleaned)

cat("\nSample chol/trestbps ratios (with NA/zero handled safely):\n")
print(head(heart$chol_bp_ratio, 15))

bp_raw <- heart$trestbps

loop_based_outliers <- function(bp_vector) {
  outlier_flags <- logical(length(bp_vector))
  for (i in seq_along(bp_vector)) {
    val <- bp_vector[i]
    if (is.na(val)) {
      outlier_flags[i] <- NA
    } else if (val < 0 || val > 250) {
      outlier_flags[i] <- TRUE
    } else {
      outlier_flags[i] <- FALSE
    }
  }
  return(outlier_flags)
}

vectorized_outliers <- function(bp_vector) {
  ifelse(is.na(bp_vector), NA, bp_vector < 0 | bp_vector > 250)
}

loop_result <- loop_based_outliers(bp_raw)
vec_result  <- vectorized_outliers(bp_raw)
cat("\nDo loop-based and vectorized results match?",
    identical(loop_result, vec_result), "\n")

cat("\n--- system.time() comparison (single run) ---\n")
cat("Loop-based:\n")
print(system.time(loop_based_outliers(bp_raw)))
cat("Vectorized:\n")
print(system.time(vectorized_outliers(bp_raw)))

cat("\n--- microbenchmark comparison (multiple runs) ---\n")
bench_result <- microbenchmark(
  loop_based = loop_based_outliers(bp_raw),
  vectorized = vectorized_outliers(bp_raw),
  times = 100
)
print(bench_result)

cat("\n============================\n")
cat("VALIDATION OF CLEANED BP DATA\n")
cat("============================\n")

n_missing <- sum(is.na(heart$trestbps_cleaned))
cat("Count of missing BP values (after cleaning):", n_missing, "\n")

cat("Minimum BP:", min(heart$trestbps_cleaned, na.rm = TRUE), "\n")
cat("Maximum BP:", max(heart$trestbps_cleaned, na.rm = TRUE), "\n")
cat("Mean BP   :", round(mean(heart$trestbps_cleaned, na.rm = TRUE), 2), "\n")
cat("Median BP :", median(heart$trestbps_cleaned, na.rm = TRUE), "\n")

no_negative <- all(heart$trestbps_cleaned[!is.na(heart$trestbps_cleaned)] >= 0)
no_over_250 <- all(heart$trestbps_cleaned[!is.na(heart$trestbps_cleaned)] <= 250)

cat("No negative values remain? ", no_negative, "\n")
cat("No values above 250 remain?", no_over_250, "\n")

write.csv(heart, "cleaned_heart_data.csv", row.names = FALSE)
cat("\nSaved cleaned_heart_data.csv\n")
