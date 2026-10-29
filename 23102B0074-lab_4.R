library(naniar)
library(skimr)
library(dplyr)

set.seed(42)

n <- 400
adult <- data.frame(
  age        = sample(17:75, n, replace = TRUE),
  workclass  = sample(c("Private", "Self-emp", "Govt", "Without-pay"), n, replace = TRUE),
  education  = sample(c("Bachelors", "HS-grad", "Masters", "Doctorate", "Some-college"),
                       n, replace = TRUE),
  occupation = sample(c("Tech-support", "Sales", "Exec-managerial", "Craft-repair", "Other"),
                       n, replace = TRUE),
  hours_per_week = sample(10:70, n, replace = TRUE),
  income     = sample(c("<=50K", ">50K"), n, replace = TRUE),
  stringsAsFactors = FALSE
)

na_idx    <- sample(1:n, 15)
blank_idx <- sample(setdiff(1:n, na_idx), 12)
nan_idx   <- sample(setdiff(1:n, c(na_idx, blank_idx)), 8)
bad_age_idx <- sample(setdiff(1:n, c(na_idx, blank_idx, nan_idx)), 6)

adult$hours_per_week[na_idx]  <- NA
adult$workclass[blank_idx]    <- ""
adult$hours_per_week[nan_idx] <- NaN
adult$age[bad_age_idx]        <- 999

cat("Dataset dimensions:", dim(adult)[1], "rows,", dim(adult)[2], "columns\n")
cat("\nFirst 10 rows of raw (corrupted) data:\n")
print(head(adult, 10))

cat("\n============================\n")
cat("TASK 1: MISSING-VALUE DETECTION\n")
cat("============================\n")

cat("Number of NA in hours_per_week:", sum(is.na(adult$hours_per_week)), "\n")

cat("Number of NaN in hours_per_week:", sum(is.nan(adult$hours_per_week)), "\n")

demo_object <- NULL
cat("\nis.null(demo_object):", is.null(demo_object), "\n")
cat("Class of NULL object:", class(demo_object), "\n")
cat("Explanation: NULL means 'this R object does not exist'. A data frame",
    "\ncell cannot literally hold NULL -- if you try adult$col <- NULL,",
    "\nit deletes the column rather than storing NULL values in it.",
    "\nThat is why blank strings (\"\") are used to represent missing",
    "\ncategorical entries instead of NULL.\n")

cat("\nNumber of blank strings in workclass:",
    sum(adult$workclass == "", na.rm = TRUE), "\n")

cat("Number of impossible age values (999):", sum(adult$age == 999), "\n")

cat("\nnaniar::miss_var_summary() output:\n")
print(miss_var_summary(adult))

cat("\n============================\n")
cat("TASK 2: DATA CLEANING / TREATMENT\n")
cat("============================\n")

adult_clean <- adult

adult_clean$age[adult_clean$age == 999] <- NA

adult_clean$workclass[adult_clean$workclass == ""] <- "Unknown"

median_hours <- median(adult_clean$hours_per_week, na.rm = TRUE)
cat("Median hours_per_week used for imputation:", median_hours, "\n")

adult_clean$hours_per_week[is.na(adult_clean$hours_per_week)] <- median_hours

median_age <- median(adult_clean$age, na.rm = TRUE)
adult_clean$age[is.na(adult_clean$age)] <- median_age
cat("Median age used for imputation:", median_age, "\n")

complete_flags <- complete.cases(adult)
cat("\nComplete observations in RAW data:", sum(complete_flags), "\n")
cat("Incomplete observations in RAW data:", sum(!complete_flags), "\n")

remaining_bad <- !complete.cases(adult_clean)
cat("Rows still incomplete after treatment:", sum(remaining_bad), "\n")
adult_clean <- adult_clean[complete.cases(adult_clean), ]

median_impute <- function(numeric_vector) {
  if (!is.numeric(numeric_vector)) {
    stop("median_impute() requires a numeric vector.")
  }
  missing_flags <- is.na(numeric_vector)
  med_value <- median(numeric_vector, na.rm = TRUE)
  numeric_vector[missing_flags] <- med_value
  return(numeric_vector)
}

demo_vector <- adult$hours_per_week
cat("\nBefore custom imputation - missing count:", sum(is.na(demo_vector)), "\n")
demo_vector_imputed <- median_impute(demo_vector)
cat("After custom imputation - missing count:", sum(is.na(demo_vector_imputed)), "\n")

cat("\n============================\n")
cat("TASK 4: BEFORE / AFTER MISSINGNESS COMPARISON\n")
cat("============================\n")

before_summary <- miss_var_summary(adult)
after_summary  <- miss_var_summary(adult_clean)

cat("\nMissingness BEFORE cleaning:\n")
print(before_summary)

cat("\nMissingness AFTER cleaning:\n")
print(after_summary)

n_missing_before <- sum(is.na(adult))
n_missing_after  <- sum(is.na(adult_clean))
pct_before <- round(100 * n_missing_before / (nrow(adult) * ncol(adult)), 2)
pct_after  <- round(100 * n_missing_after  / (nrow(adult_clean) * ncol(adult_clean)), 2)

cat("\nTotal missing cells before:", n_missing_before, "(", pct_before, "% )\n")
cat("Total missing cells after :", n_missing_after,  "(", pct_after,  "% )\n")

vis_miss_plot <- vis_miss(adult)
print(vis_miss_plot)

gg_miss_var_plot <- gg_miss_var(adult)
print(gg_miss_var_plot)

cat("\n============================\n")
cat("TASK 5: VALIDATION\n")
cat("============================\n")

cat("\nskimr::skim() summary of cleaned data:\n")
print(skim(adult_clean))

cat("\nAny age == 999 remaining?", any(adult_clean$age == 999), "\n")

cat("Missing values remaining in hours_per_week:",
    sum(is.na(adult_clean$hours_per_week)), "\n")
cat("Missing values remaining in age:", sum(is.na(adult_clean$age)), "\n")

cat("Blank strings remaining in workclass:",
    sum(adult_clean$workclass == ""), "\n")
cat("Rows now labeled 'Unknown' in workclass:",
    sum(adult_clean$workclass == "Unknown"), "\n")

write.csv(adult_clean, "cleaned_adult_data.csv", row.names = FALSE)
cat("\nSaved cleaned_adult_data.csv\n")
