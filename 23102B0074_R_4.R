required_packages <- c("readr", "jsonlite", "readxl", "dplyr",
                       "tidyr", "DBI", "RSQLite", "lubridate")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)
library(readr)
library(jsonlite)
library(readxl)
library(dplyr)
library(tidyr)
library(DBI)
library(RSQLite)
library(lubridate)
transactions <- read_csv("data/transactions.csv", show_col_types = FALSE)
products     <- fromJSON("data/products.json")
customers    <- read_excel("data/customers.xlsx")
cat("---- Structure: transactions ----\n"); str(transactions)
cat("---- Structure: products ----\n");     str(products)
cat("---- Structure: customers ----\n");    str(customers)
cat("\nDimensions before cleaning:\n")
cat("transactions:", dim(transactions), "\n")
cat("products:    ", dim(products), "\n")
cat("customers:   ", dim(customers), "\n")
cat("\nMissing values per column (transactions):\n")
print(colSums(is.na(transactions)))
cat("\nMissing values per column (products):\n")
print(colSums(is.na(products)))
cat("\nMissing values per column (customers):\n")
print(colSums(is.na(customers)))
transactions_clean <- transactions %>%
  distinct() %>%
  drop_na(CustomerID) %>%
  filter(Quantity > 0) %>%
  mutate(
    CustomerID  = as.integer(CustomerID),
    InvoiceDate = ymd_hm(InvoiceDate)
  )
cat("\nTransactions: ", nrow(transactions), "->", nrow(transactions_clean), "rows after cleaning\n")
products_clean <- products %>%
  distinct() %>%
  filter(!is.na(UnitPrice), UnitPrice > 0) %>%
  filter(!is.na(Description), Description != "") %>%
  distinct(StockCode, .keep_all = TRUE)
cat("Products: ", nrow(products), "->", nrow(products_clean), "rows after cleaning\n")
customers_clean <- customers %>%
  filter(!is.na(CustomerID), !is.na(Country)) %>%
  distinct(CustomerID, .keep_all = TRUE)
cat("Customers: ", nrow(customers), "->", nrow(customers_clean), "rows after cleaning\n")
sales_data <- transactions_clean %>%
  inner_join(products_clean, by = "StockCode") %>%
  left_join(customers_clean, by = "CustomerID") %>%
  mutate(Revenue = Quantity * UnitPrice)
cat("\nFinal integrated dataset dimensions:", dim(sales_data), "\n")
glimpse(sales_data)
unmatched_products <- transactions_clean %>%
  anti_join(products_clean, by = "StockCode")
cat("\nTransactions with no matching product record:", nrow(unmatched_products), "\n")
unmatched_customers <- sales_data %>%
  filter(is.na(Country))
cat("Transactions with no matching customer record:", nrow(unmatched_customers), "\n")
sales_data <- sales_data %>% filter(!is.na(Country))
cat("Final dataset after removing unresolved customer matches:", dim(sales_data), "\n")
total_revenue <- sum(sales_data$Revenue, na.rm = TRUE)
cat("\nTotal Sales Revenue: ", round(total_revenue, 2), "\n")
top5_products <- sales_data %>%
  group_by(StockCode, Description) %>%
  summarise(TotalRevenue = sum(Revenue), .groups = "drop") %>%
  arrange(desc(TotalRevenue)) %>%
  slice_head(n = 5)
cat("\nTop 5 Products by Revenue:\n")
print(top5_products)
top5_countries <- sales_data %>%
  group_by(Country) %>%
  summarise(TotalRevenue = sum(Revenue), .groups = "drop") %>%
  arrange(desc(TotalRevenue)) %>%
  slice_head(n = 5)
cat("\nTop 5 Countries by Revenue:\n")
print(top5_countries)
top5_customers <- sales_data %>%
  group_by(CustomerID, Country) %>%
  summarise(TotalRevenue = sum(Revenue), .groups = "drop") %>%
  arrange(desc(TotalRevenue)) %>%
  slice_head(n = 5)
cat("\nTop 5 Customers by Purchase Value:\n")
print(top5_customers)
customer_value <- sales_data %>%
  group_by(CustomerID, Country) %>%
  summarise(TotalRevenue = sum(Revenue), .groups = "drop")
q <- quantile(customer_value$TotalRevenue, probs = c(0.25, 0.5, 0.9))
cat("\nSegmentation thresholds (25th/50th/90th percentile):\n")
print(q)
customer_value <- customer_value %>%
  mutate(
    ValueSegment = case_when(
      TotalRevenue >= q[3] ~ "Premium",
      TotalRevenue >= q[2] ~ "High Value",
      TotalRevenue >= q[1] ~ "Medium Value",
      TRUE                 ~ "Low Value"
    )
  )
cat("\nCustomer count by segment:\n")
print(table(customer_value$ValueSegment))
cat("\nSample segmented customers:\n")
print(head(customer_value %>% arrange(desc(TotalRevenue)), 10))
market_perf <- sales_data %>%
  group_by(Country) %>%
  summarise(TotalRevenue = sum(Revenue),
            Orders = n_distinct(InvoiceNo),
            .groups = "drop") %>%
  arrange(desc(TotalRevenue))
best_market  <- market_perf %>% slice_head(n = 1)
worst_market <- market_perf %>% slice_tail(n = 1)
cat("\nHighest-performing market:\n"); print(best_market)
cat("Underperforming market:\n");      print(worst_market)
cat("\nInterpretation:\n")
cat(sprintf(
  "- %s generates the highest revenue (%.2f) across %d orders, suggesting strong market\n  penetration and customer volume there.\n",
  best_market$Country, best_market$TotalRevenue, best_market$Orders))
cat(sprintf(
  "- %s generates the lowest revenue (%.2f) with only %d orders, indicating limited market\n  reach or low customer adoption — a candidate for targeted marketing investment.\n",
  worst_market$Country, worst_market$TotalRevenue, worst_market$Orders))
con <- dbConnect(RSQLite::SQLite(), "retail_sales.db")
dbWriteTable(con, "retail_sales", sales_data, overwrite = TRUE)
cat("\nTable 'retail_sales' written to retail_sales.db with",
    dbGetQuery(con, "SELECT COUNT(*) AS n FROM retail_sales")$n, "rows\n")
query1 <- dbGetQuery(con, "
  SELECT CustomerID, Country, ROUND(SUM(Revenue), 2) AS TotalRevenue
  FROM retail_sales
  GROUP BY CustomerID, Country
  ORDER BY TotalRevenue DESC
  LIMIT 5;
")
cat("\nSQL Query 1 - Top 5 Customers by Revenue:\n")
print(query1)
query2 <- dbGetQuery(con, "
  SELECT Country, ROUND(SUM(Revenue), 2) AS TotalRevenue, COUNT(DISTINCT InvoiceNo) AS Orders
  FROM retail_sales
  GROUP BY Country
  ORDER BY TotalRevenue DESC;
")
cat("\nSQL Query 2 - Total Revenue by Country:\n")
print(query2)
dbDisconnect(con)
cat("\n============ BUSINESS INSIGHTS ============\n")
cat(sprintf("1. Total consolidated sales revenue across all cleaned transactions is %.2f,\n   derived from %d valid transaction line items after removing invalid/duplicate records.\n",
            total_revenue, nrow(sales_data)))
cat(sprintf("2. Revenue is concentrated in the top market (%s) and a small set of top products,\n   suggesting the business should protect and expand these high-performing segments.\n",
            best_market$Country))
cat(sprintf("3. Customer value is highly uneven: only the top segment ('Premium') contributes\n   disproportionately to revenue, so loyalty programs targeted at 'Medium Value' customers\n   could be a cost-effective way to grow revenue.\n"))
cat("=============================================\n")