##Assignment 1 
file_path <- "C:/Users/aryan/OneDrive/Desktop/Btech Study/Final Year/PRSA_Data_Aotizhongxin_20130301-20170228.csv"

air_data = tryCatch({
  read.csv(file_path)
},error = function(e){
  cat("Error Loading File: ", e$message, "\n")
  NULL
})

if(is.null(air_data)){
  head(air_data)
  str(air_data)
  dim(air_data)
  nrow(air_data)
  ncol(air_data)
  any(is.na(air_data))
  sum(is.na(air_data))
}

temperature <- c(28,30,NA,32)

missing_object <- NULL

undefined_value <- 0/0

cat(is.na(temperature), "\n")

cat(is.null(missing_object), "\n")

cat(is.nan(undefined_value), "\n")

missingSummary = function(df){
  selected = c("PM2.5","PM10","SO2","NO2","TEMP","WSPM","wd")
  summaryTable = data.frame()
  for(col in selected){
    total = nrow(df)
    missing = sum(is.na(df[[col]]))
    percent = (missing/total)*100
    summaryTable = rbind(summaryTable,
      data.frame(
        Variable = col,
        Total_Records = total,
        Missing_Values = missing, 
        Missing_Percentage = round(percent,2)
      ))
    
  }
  return(summaryTable)
}
missingSummary(air_data)

#Task 4
air_data$pollution_ratio <- air_data$PM2.5 / air_data$PM10

sum(is.na(air_data$pollution_ratio))

sum(is.nan(air_data$pollution_ratio))

sum(is.infinite(air_data$pollution_ratio))

air_data$pollution_ratio[
  is.nan(air_data$pollution_ratio) |
    is.infinite(air_data$pollution_ratio)
] <- NA

## TASK 5
numeric_variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM")

before_missing <- c()
after_missing <- c()

for(col in numeric_variables){
  
  if(col %in% names(air_data)){
    
    before <- sum(is.na(air_data[[col]]))
    before_missing <- c(before_missing, before)
    
    med <- median(air_data[[col]], na.rm = TRUE)
    
    air_data[[col]][is.na(air_data[[col]])] <- med
    
    after <- sum(is.na(air_data[[col]]))
    after_missing <- c(after_missing, after)
    
    cat("Variable:", col, "\n")
    cat("Before:", before, "\n")
    cat("Median:", med, "\n")
    cat("After:", after, "\n\n")
  }
}

## TASK 6

calculate_mode <- function(x){
  
  ux <- unique(x)
  
  ux[which.max(tabulate(match(x,ux)))]
  
}

before <- sum(is.na(air_data$wd))

mode_value <- calculate_mode(air_data$wd[!is.na(air_data$wd)])

air_data$wd[is.na(air_data$wd)] <- mode_value

after <- sum(is.na(air_data$wd))

cat(before,"\n")

cat(after,"\n")

## TASK 7
clean_variable <- function(df,var){
  
  tryCatch({
    
    if(!(var %in% names(df)))
      stop("Variable does not exist")
    
    if(!is.numeric(df[[var]]))
      stop("Variable is not numeric")
    
    if(all(is.na(df[[var]])))
      stop("All values are missing")
    
    med <- median(df[[var]],na.rm=TRUE)
    
    if(is.na(med))
      stop("Median cannot be calculated")
    
    df[[var]][is.na(df[[var]])] <- med
    
    return(df[[var]])
    
  },
  
  error=function(e){
    
    cat("Error:",e$message,"\n")
    
    return(NULL)
    
  })
  
}

# TASK 8 
comparison <- data.frame(
  
  Variable=numeric_variables,
  
  Missing_Before=before_missing,
  
  Missing_After=after_missing,
  
  Values_Replaced=before_missing-after_missing
  
)
 
comparison 


## TASK 9
before <- c(10,20,15,8,12,18,7)

after <- c(0,0,0,0,0,0,0)

data <- rbind(before,after)

barplot(
  
  data,
  
  beside=TRUE,
  
  col=c("red","green"),
  
  names.arg=c("PM2.5","PM10","SO2","NO2","TEMP","WSPM","wd"),
  
  main="Missing Values Before and After Cleaning",
  
  xlab="Variables",
  
  ylab="Missing Values",
  
  legend.text=c("Before","After")
  
)

## TASK 10 
write.csv(
  air_data,
  "C:/Users/aryan/Downloads/beijing+multi+site+air+quality+data (1)/PRSA2017_Data_20130301-20170228/PRSA_Data_20130301-20170228/cleaned_air_quality_data.csv",
  row.names = FALSE
)

list.files()