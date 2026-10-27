# LAB ASSIGNMENT 8: HIGH-PERFORMANCE BIG DATA ANALYTICS USING R
# Paste into an R cell in Google Colab or run in RStudio.
# Starts with one month to reduce memory/runtime issues.

options(repos = c(CRAN = "https://cloud.r-project.org"), scipen = 999, width = 110)
packages <- c("data.table", "arrow", "lubridate", "ggplot2", "foreach", "doParallel")
missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))

months_to_use <- c("2025-01")  # After success, try c("2025-01", "2025-02", "2025-03")
out <- "lab8_output"
plots <- file.path(out, "plots")
dir.create(plots, recursive = TRUE, showWarnings = FALSE)

cat("\n========== LAB 8: NYC TAXI BIG DATA ANALYTICS ==========\n")
cat("Months:", paste(months_to_use, collapse = ", "), "\n")

# 1. Data acquisition
url_for_month <- function(m) paste0(
  "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_", m, ".parquet"
)
read_month <- function(m) {
  dest <- file.path(out, paste0("yellow_tripdata_", m, ".parquet"))
  if (!file.exists(dest)) {
    cat("Downloading ", m, " taxi data...\n", sep = "")
    download.file(url_for_month(m), destfile = dest, mode = "wb", quiet = TRUE)
  }
  if (!file.exists(dest) || file.info(dest)$size < 10000) stop("Download failed: ", dest)
  arrow::read_parquet(dest, as_data_frame = TRUE)
}
raw <- data.table::rbindlist(lapply(months_to_use, read_month), use.names = TRUE, fill = TRUE)
cat("Raw dimensions:", nrow(raw), "rows x", ncol(raw), "columns\n")
print(names(raw))
print(summary(raw[, intersect(c("passenger_count", "trip_distance", "fare_amount",
                                "tip_amount", "total_amount"), names(raw)), drop = FALSE]))

# 2. Cleaning and feature engineering
wanted <- intersect(c("VendorID", "tpep_pickup_datetime", "tpep_dropoff_datetime",
  "passenger_count", "trip_distance", "PULocationID", "DOLocationID",
  "payment_type", "fare_amount", "tip_amount", "tolls_amount", "total_amount"), names(raw))
DT <- data.table::as.data.table(raw)[, ..wanted]
rm(raw); gc()
DT[, pickup := as.POSIXct(tpep_pickup_datetime, tz = "America/New_York")]
DT[, dropoff := as.POSIXct(tpep_dropoff_datetime, tz = "America/New_York")]
before <- nrow(DT)
duplicates <- sum(duplicated(DT))
DT <- unique(DT)
DT <- DT[!is.na(pickup) & !is.na(dropoff) & !is.na(trip_distance) &
           !is.na(fare_amount) & !is.na(total_amount) &
           trip_distance >= 0 & trip_distance <= 200 &
           fare_amount >= 0 & total_amount >= 0 & dropoff >= pickup]
DT[, trip_minutes := as.numeric(difftime(dropoff, pickup, units = "mins"))]
DT <- DT[trip_minutes <= 360]
DT[, `:=`(
  hour = lubridate::hour(pickup),
  day = lubridate::day(pickup),
  weekday = factor(lubridate::wday(pickup, label = TRUE, abbr = FALSE, week_start = 1),
                   levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
  month = format(pickup, "%Y-%m")
)]
after <- nrow(DT)
cleaning <- data.table::data.table(
  Measure = c("Rows before cleaning", "Exact duplicates detected", "Rows after cleaning", "Rows removed overall"),
  Value = c(before, duplicates, after, before - after)
)
cat("\n--- CLEANING SUMMARY ---\n"); print(cleaning)
data.table::fwrite(cleaning, file.path(out, "cleaning_summary.csv"))

# 3. Join taxi zone lookup table
zone_path <- file.path(out, "taxi_zone_lookup.csv")
if (!file.exists(zone_path)) download.file(
  "https://d37ci6vzurychx.cloudfront.net/misc/taxi_zone_lookup.csv",
  destfile = zone_path, mode = "wb", quiet = TRUE
)
zones <- data.table::fread(zone_path)
pu <- zones[, .(PULocationID = LocationID, pickup_zone = Zone, pickup_borough = Borough)]
do <- zones[, .(DOLocationID = LocationID, dropoff_zone = Zone, dropoff_borough = Borough)]
DT <- merge(DT, pu, by = "PULocationID", all.x = TRUE, sort = FALSE)
DT <- merge(DT, do, by = "DOLocationID", all.x = TRUE, sort = FALSE)
cat("Rows after zone joins:", nrow(DT), "\n")

# 4. Transportation analytics
by_hour <- DT[, .(trips = .N, revenue_USD = sum(total_amount, na.rm = TRUE),
                  average_fare_USD = mean(fare_amount, na.rm = TRUE)), by = hour][order(hour)]
by_weekday <- DT[, .(trips = .N), by = weekday][order(weekday)]
by_month <- DT[, .(trips = .N, revenue_USD = sum(total_amount, na.rm = TRUE),
                   average_fare_USD = mean(fare_amount, na.rm = TRUE)), by = month][order(month)]
route_freq <- DT[, .(trips = .N, revenue_USD = sum(total_amount, na.rm = TRUE),
                     average_fare_USD = mean(fare_amount, na.rm = TRUE)),
                 by = .(pickup_borough, pickup_zone, dropoff_borough, dropoff_zone)][order(-trips)]
top_routes <- head(route_freq, 15)
route_revenue <- DT[, .(trips = .N, revenue_USD = sum(total_amount, na.rm = TRUE),
                        average_total_USD = mean(total_amount, na.rm = TRUE)),
                    by = .(pickup_borough, pickup_zone, dropoff_borough, dropoff_zone)][order(-revenue_USD)]
top_revenue <- head(route_revenue, 15)
distance_fare <- DT[trip_distance > 0, .(
  trips = .N, average_fare_USD = mean(fare_amount, na.rm = TRUE),
  median_fare_USD = median(fare_amount, na.rm = TRUE)
), by = .(distance_band = cut(trip_distance, c(0,1,3,5,10,20,Inf),
                              labels = c("(0,1]","(1,3]","(3,5]","(5,10]","(10,20]","20+"),
                              include.lowest = TRUE))][order(distance_band)]
payment <- DT[, .(trips = .N, revenue_USD = sum(total_amount, na.rm = TRUE)),
              by = .(pickup_borough, payment_type)][order(-trips)]
top_pickups <- DT[, .(trips = .N), by = .(pickup_borough, pickup_zone)][order(-trips)][1:min(.N,15)]
top_dropoffs <- DT[, .(trips = .N), by = .(dropoff_borough, dropoff_zone)][order(-trips)][1:min(.N,15)]

tables <- list(trips_by_hour=by_hour, trips_by_weekday=by_weekday, trips_by_month=by_month,
  top_routes_by_frequency=top_routes, top_routes_by_revenue=top_revenue,
  distance_vs_fare=distance_fare, payment_by_borough=payment,
  top_pickup_zones=top_pickups, top_dropoff_zones=top_dropoffs)
for (nm in names(tables)) data.table::fwrite(tables[[nm]], file.path(out, paste0(nm, ".csv")))
cat("\n--- TRIPS BY HOUR ---\n"); print(by_hour)
cat("\n--- DEMAND BY WEEKDAY ---\n"); print(by_weekday)
cat("\n--- MONTH SUMMARY ---\n"); print(by_month)
cat("\n--- TOP FREQUENT ROUTES ---\n"); print(head(top_routes, 10))
cat("\n--- TOP REVENUE ROUTES ---\n"); print(head(top_revenue, 10))
cat("\n--- FARE BY DISTANCE BAND ---\n"); print(distance_fare)
cat("\n--- PAYMENT TYPE BY BOROUGH ---\n"); print(head(payment, 20))

# 5. Functional, vectorized and data.table comparison
x <- DT$hour
base_count <- function(z) data.frame(hour=0:23, trips=as.integer(table(factor(z, levels=0:23))))
vector_count <- function(z) data.frame(hour=0:23, trips=tabulate(z+1L, nbins=24L))
functional_count <- function(z) {
  parts <- split(z, factor(z, levels=0:23))
  data.frame(hour=0:23, trips=as.integer(vapply(parts, length, integer(1))))
}
dt_count <- function(z) {
  counts <- data.table::data.table(hour=z)[, .(trips=.N), by=hour]
  merge(data.table::data.table(hour=0:23), counts, by="hour", all.x=TRUE)[
    is.na(trips), trips := 0L][order(hour)]
}
b <- base_count(x); v <- vector_count(x); f <- functional_count(x); d <- dt_count(x)
stopifnot(identical(b$trips,v$trips), identical(b$trips,f$trips),
          identical(b$trips,as.integer(d$trips)))
timing <- function(label, fun, z, reps=3L) {
  tt <- replicate(reps, unname(system.time(invisible(fun(z)))[["elapsed"]]))
  data.table::data.table(method=label, min_seconds=min(tt),
    median_seconds=median(tt), max_seconds=max(tt), repetitions=reps)
}
bench <- data.table::rbindlist(list(
  timing("Base R table/factor", base_count, x),
  timing("Vectorized tabulate", vector_count, x),
  timing("Functional split/vapply", functional_count, x),
  timing("data.table grouping", function(z) data.table::data.table(hour=z)[,.N,by=hour], x)
))
bench[, median_ms := median_seconds*1000]
setorder(bench, median_seconds)
cat("\n--- PROGRAMMING APPROACH BENCHMARK ---\n"); print(bench)
data.table::fwrite(bench, file.path(out, "benchmark_programming_methods.csv"))

# apply() example on a bounded numeric sample
n_sample <- min(nrow(DT), 50000L)
num_mat <- as.matrix(DT[seq_len(n_sample), .(trip_distance, fare_amount, total_amount)])
cat("\n--- apply() COLUMN MEANS, SAMPLE SIZE =", n_sample, "---\n")
print(apply(num_mat, 2, mean, na.rm=TRUE))

# 6. Sequential vs parallel: partition by month
parts <- split(DT, DT$month)
month_summary <- function(z) data.table::data.table(
  month=unique(z$month)[1], trips=nrow(z),
  total_revenue_USD=sum(z$total_amount, na.rm=TRUE),
  average_fare_USD=mean(z$fare_amount, na.rm=TRUE)
)
seq_time <- system.time(seq_result <- data.table::rbindlist(lapply(parts, month_summary)))[["elapsed"]]
cores <- parallel::detectCores(logical=TRUE)
if (is.na(cores)) cores <- 2L
workers <- max(1L, min(length(parts), cores-1L))
cl <- parallel::makeCluster(workers)
doParallel::registerDoParallel(cl)
par_time <- system.time(
  par_result <- foreach::foreach(part=parts, .combine=data.table::rbindlist,
                                 .packages="data.table") %dopar% month_summary(part)
)[["elapsed"]]
parallel::stopCluster(cl); foreach::registerDoSEQ()
seq_result <- seq_result[order(month)]; par_result <- par_result[order(month)]
stopifnot(isTRUE(all.equal(as.data.frame(seq_result), as.data.frame(par_result),
                           check.attributes=FALSE)))
speedup <- if (par_time > 0) seq_time/par_time else NA_real_
parallel_bench <- data.table::data.table(
  method=c("Sequential lapply", paste0("foreach + doParallel (", workers, " worker(s))")),
  elapsed_seconds=c(seq_time,par_time), speedup_vs_sequential=c(1,speedup))
cat("\n--- SEQUENTIAL VS PARALLEL ---\n"); print(parallel_bench)
cat("Speedup = sequential elapsed time / parallel elapsed time =", round(speedup,3), "\n")
cat("With one month there is only one partition; test multiple months for a meaningful multi-worker comparison.\n")
data.table::fwrite(parallel_bench,file.path(out,"benchmark_sequential_parallel.csv"))
data.table::fwrite(seq_result,file.path(out,"monthly_summary_sequential.csv"))
data.table::fwrite(par_result,file.path(out,"monthly_summary_parallel.csv"))

# 7. Visualizations
p1 <- ggplot(by_hour,aes(hour,trips))+geom_col()+scale_x_continuous(breaks=0:23)+
  labs(title="NYC Yellow Taxi Trips by Pickup Hour",x="Pickup hour (local time)",y="Number of trips")+
  theme_minimal()
ggsave(file.path(plots,"01_trips_by_hour.png"),p1,width=9,height=5,dpi=150)

p2 <- ggplot(by_weekday,aes(weekday,trips))+geom_col()+
  labs(title="Taxi Demand by Day of Week",x="Pickup weekday",y="Number of trips")+theme_minimal()
ggsave(file.path(plots,"02_trips_by_weekday.png"),p2,width=9,height=5,dpi=150)

p3 <- ggplot(by_month,aes(month,trips,group=1))+geom_line()+geom_point(size=3)+
  labs(title="Monthly Taxi Demand",x="Pickup month",y="Number of trips")+theme_minimal()
ggsave(file.path(plots,"03_trips_by_month.png"),p3,width=8,height=5,dpi=150)

p4 <- ggplot(distance_fare,aes(distance_band,average_fare_USD))+geom_col()+
  labs(title="Average Fare by Trip Distance",x="Distance band (miles)",y="Average fare (USD)")+theme_minimal()
ggsave(file.path(plots,"04_average_fare_by_distance.png"),p4,width=9,height=5,dpi=150)

route_plot <- copy(top_routes)
route_plot[, route := paste(pickup_zone,"to",dropoff_zone)]
p5 <- ggplot(route_plot,aes(reorder(route,trips),trips))+geom_col()+coord_flip()+
  labs(title="Top 15 Most Frequent Routes",x="Pickup zone to drop-off zone",y="Number of trips")+
  theme_minimal()
ggsave(file.path(plots,"05_top_routes.png"),p5,width=10,height=7,dpi=150)

p6 <- ggplot(payment[!is.na(pickup_borough)],aes(pickup_borough,trips,fill=factor(payment_type)))+
  geom_col()+labs(title="Payment Type by Pickup Borough",x="Pickup borough",
                  y="Number of trips",fill="Payment code")+theme_minimal()
ggsave(file.path(plots,"06_payment_by_borough.png"),p6,width=10,height=5,dpi=150)

scatter <- DT[trip_distance>0 & trip_distance<=30 & fare_amount<=150]
p7 <- ggplot(scatter,aes(trip_distance,fare_amount))+geom_point(alpha=.12,size=.6)+
  labs(title="Trip Distance vs Fare Amount",subtitle="Trips up to 30 miles and fares up to $150",
       x="Trip distance (miles)",y="Fare amount (USD)")+theme_minimal()
ggsave(file.path(plots,"07_distance_vs_fare.png"),p7,width=9,height=5,dpi=150)

# 8. Save run metadata and a small sample
writeLines(c(
  paste("Months:",paste(months_to_use,collapse=", ")),
  paste("Rows before cleaning:",before),paste("Rows after cleaning:",after),
  paste("Detected logical cores:",cores),paste("Parallel workers:",workers),
  paste("Sequential seconds:",seq_time),paste("Parallel seconds:",par_time),
  paste("Speedup:",speedup),paste("R version:",R.version.string)
),file.path(out,"run_summary.txt"))
capture.output(sessionInfo(),file=file.path(out,"session_info.txt"))
data.table::fwrite(DT[1:min(.N,5000)],file.path(out,"cleaned_sample_5000.csv"))

cat("\n========== SUCCESS: SCRIPT FINISHED ==========\n")
cat("Rows analyzed:",format(nrow(DT),big.mark=","),"\n")
cat("Output folder:",normalizePath(out),"\n")
cat("Plots created:",length(list.files(plots,pattern="\\.png$")),"\n")
