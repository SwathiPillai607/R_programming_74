required <- c("naniar", "skimr")
missing_pkgs <- required[!sapply(required, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(naniar)
library(skimr)

set.seed(42)
options(stringsAsFactors = FALSE)

heart_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heart_file <- "processed.cleveland.data"

if (!file.exists(heart_file)) {
  download.file(heart_url, heart_file, mode = "wb")
}

heart_names <- c(
  "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
  "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num"
)

heart <- read.csv(
  heart_file,
  header = FALSE,
  col.names = heart_names,
  na.strings = "?"
)

str(heart)
head(heart)

heart_lab3 <- heart
heart_lab3$trestbps[c(1, 2, 3)] <- c(-120, NA, 320)
heart_lab3$trestbps[c(4, 5)] <- c(-80, 350)

cat("Injected problematic trestbps values:\n")
print(heart_lab3$trestbps[1:10])

clean_bp_value <- function(bp) {
  if (is.na(bp)) {
    return(NA_real_)
  } else if (bp < 0) {
    return(NA_real_)
  } else if (bp > 250) {
    return(250)
  } else {
    return(bp)
  }
}

test_values <- c(-20, NA, 120, 260, 350)
sapply(test_values, clean_bp_value)

heart_lab3$trestbps_clean <- Vectorize(clean_bp_value)(heart_lab3$trestbps)

head(
  heart_lab3[, c("trestbps", "trestbps_clean")],
  10
)

safe_mean_bp <- function(x) {
  tryCatch(
    {
      if (!is.numeric(x)) stop("BP input must be numeric.")
      if (all(is.na(x))) stop("Cannot calculate mean: all BP values are missing.")

      result <- mean(x, na.rm = TRUE)
      message("Mean BP calculated successfully.")
      result
    },
    warning = function(w) {
      message("Warning while calculating mean BP: ", conditionMessage(w))
      NA_real_
    },
    error = function(e) {
      message("BP mean error handled safely: ", conditionMessage(e))
      NA_real_
    }
  )
}

safe_ratio <- function(chol, bp) {
  tryCatch(
    {
      if (!is.numeric(chol) || !is.numeric(bp)) {
        stop("Both cholesterol and BP must be numeric.")
      }
      if (is.na(bp) || bp <= 0) {
        stop("Invalid denominator: BP must be a positive, non-missing value.")
      }
      if (is.na(chol)) {
        stop("Invalid numerator: cholesterol is missing.")
      }

      chol / bp
    },
    warning = function(w) {
      message("Ratio warning handled safely: ", conditionMessage(w))
      NA_real_
    },
    error = function(e) {
      message("Ratio error handled safely: ", conditionMessage(e))
      NA_real_
    }
  )
}

mean_bp <- safe_mean_bp(heart_lab3$trestbps_clean)
mean_bp

cat("\nRatio examples:\n")
print(safe_ratio(200, 120))
print(safe_ratio(200, 0))
print(safe_ratio(200, NA))

clean_bp_loop <- function(x) {
  result <- x

  for (i in seq_along(result)) {
    if (is.na(result[i])) {
      next
    } else if (result[i] < 0) {
      result[i] <- NA_real_
    } else if (result[i] > 250) {
      result[i] <- 250
    }
  }

  result
}

system.time({
  bp_loop <- clean_bp_loop(heart_lab3$trestbps)
})

clean_bp_vectorized <- function(x) {
  result <- x
  result[!is.na(result) & result < 0] <- NA_real_
  result[!is.na(result) & result > 250] <- 250
  result
}

system.time({
  bp_vectorized <- clean_bp_vectorized(heart_lab3$trestbps)
})

benchmark_iterations <- 1000

loop_time <- system.time({
  for (j in seq_len(benchmark_iterations)) {
    invisible(clean_bp_loop(heart_lab3$trestbps))
  }
})

vector_time <- system.time({
  for (j in seq_len(benchmark_iterations)) {
    invisible(clean_bp_vectorized(heart_lab3$trestbps))
  }
})

benchmark_results <- data.frame(
  Approach = c("For loop", "Vectorized"),
  User_Time_Seconds = c(loop_time["user.self"], vector_time["user.self"]),
  System_Time_Seconds = c(loop_time["sys.self"], vector_time["sys.self"]),
  Elapsed_Time_Seconds = c(loop_time["elapsed"], vector_time["elapsed"])
)

print(benchmark_results)

heart_lab3$trestbps <- bp_vectorized

lab3_validation <- data.frame(
  Missing_BP = sum(is.na(heart_lab3$trestbps)),
  Minimum_BP = min(heart_lab3$trestbps, na.rm = TRUE),
  Maximum_BP = max(heart_lab3$trestbps, na.rm = TRUE),
  Mean_BP = mean(heart_lab3$trestbps, na.rm = TRUE),
  Median_BP = median(heart_lab3$trestbps, na.rm = TRUE),
  Negative_Remaining = sum(heart_lab3$trestbps < 0, na.rm = TRUE),
  Above_250_Remaining = sum(heart_lab3$trestbps > 250, na.rm = TRUE)
)

print(lab3_validation)

stopifnot(
  lab3_validation$Negative_Remaining == 0,
  lab3_validation$Above_250_Remaining == 0
)
cat("\nValidation passed: no negative or >250 BP values remain.\n")

write.csv(
  heart_lab3,
  "cleaned_heart_data.csv",
  row.names = FALSE
)

cat("Created: cleaned_heart_data.csv\n")

adult_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
adult_file <- "adult.data"

if (!file.exists(adult_file)) {
  download.file(adult_url, adult_file, mode = "wb")
}

adult_names <- c(
  "age", "workclass", "fnlwgt", "education", "education_num",
  "marital_status", "occupation", "relationship", "race", "sex",
  "capital_gain", "capital_loss", "hours_per_week", "native_country",
  "income"
)

adult <- read.csv(
  adult_file,
  header = FALSE,
  col.names = adult_names,
  na.strings = "?",
  strip.white = TRUE
)

str(adult)
head(adult)

adult_lab4 <- adult
adult_lab4$age[c(1, 2)] <- NA
adult_lab4$workclass[c(3, 4)] <- ""
adult_lab4$education_num[c(5, 6)] <- NaN
adult_lab4$age[c(7, 8)] <- 999
null_demo <- NULL

cat("is.null(null_demo):", is.null(null_demo), "\n")
cat("A NULL object is different from a data-frame cell containing NA/blank/NaN.\n")

cat("NA count in age:", sum(is.na(adult_lab4$age)), "\n")
cat("NaN count in education_num:", sum(is.nan(adult_lab4$education_num)), "\n")
cat("NULL demonstration:", is.null(null_demo), "\n")
cat("Blank workclass values:", sum(adult_lab4$workclass == "", na.rm = TRUE), "\n")
cat("Impossible age = 999:", sum(adult_lab4$age == 999, na.rm = TRUE), "\n")

missing_summary_before <- data.frame(
  Variable = names(adult_lab4),
  Missing_Count = sapply(adult_lab4, function(x) sum(is.na(x))),
  Missing_Percentage = sapply(adult_lab4, function(x) mean(is.na(x)) * 100)
)

print(missing_summary_before)

miss_var_summary(adult_lab4)

median_impute <- function(x) {
  if (!is.numeric(x)) {
    stop("median_impute() requires a numeric vector.")
  }

  valid_values <- x[!is.na(x) & !is.nan(x)]

  if (length(valid_values) == 0) {
    stop("No valid observations available to calculate a median.")
  }

  med <- median(valid_values)
  x[is.na(x) | is.nan(x)] <- med
  x
}

demo_numeric <- c(20, 22, NA, 25, NaN, 30)
median_impute(demo_numeric)

adult_lab4$age[adult_lab4$age == 999] <- NA
adult_lab4$workclass[adult_lab4$workclass == ""] <- "Unknown"
adult_lab4$education_num[is.nan(adult_lab4$education_num)] <- NA

numeric_columns <- c(
  "age", "fnlwgt", "education_num",
  "capital_gain", "capital_loss", "hours_per_week"
)

for (col in numeric_columns) {
  adult_lab4[[col]] <- median_impute(adult_lab4[[col]])
}

categorical_columns <- c(
  "workclass", "education", "marital_status", "occupation",
  "relationship", "race", "sex", "native_country", "income"
)

for (col in categorical_columns) {
  adult_lab4[[col]][is.na(adult_lab4[[col]]) | adult_lab4[[col]] == ""] <- "Unknown"
}

complete_before <- sum(complete.cases(adult_lab4))
incomplete_before <- nrow(adult_lab4) - complete_before

cat("Complete rows after treatment:", complete_before, "\n")
cat("Incomplete rows after treatment:", incomplete_before, "\n")

missing_summary_after <- data.frame(
  Variable = names(adult_lab4),
  Missing_Count = sapply(adult_lab4, function(x) sum(is.na(x))),
  Missing_Percentage = sapply(adult_lab4, function(x) mean(is.na(x)) * 100)
)

comparison <- merge(
  missing_summary_before,
  missing_summary_after,
  by = "Variable",
  suffixes = c("_Before", "_After")
)

comparison$Reduction <- comparison$Missing_Count_Before - comparison$Missing_Count_After

print(comparison)

cat("\nOverall missing values before:", sum(missing_summary_before$Missing_Count), "\n")
cat("Overall missing values after:", sum(missing_summary_after$Missing_Count), "\n")

vis_miss(adult_lab4)

skim(adult_lab4)

validation_lab4 <- list(
  impossible_age_999 = sum(adult_lab4$age == 999, na.rm = TRUE),
  selected_numeric_missing = sapply(
    adult_lab4[numeric_columns],
    function(x) sum(is.na(x) | is.nan(x))
  ),
  blank_categorical_values = sapply(
    adult_lab4[categorical_columns],
    function(x) sum(x == "", na.rm = TRUE)
  )
)

print(validation_lab4)

stopifnot(
  validation_lab4$impossible_age_999 == 0,
  all(validation_lab4$selected_numeric_missing == 0),
  all(validation_lab4$blank_categorical_values == 0)
)

cat("\nLab 4 validation passed.\n")

write.csv(
  adult_lab4,
  "cleaned_adult_data.csv",
  row.names = FALSE
)

cat("Created: cleaned_adult_data.csv\n")
