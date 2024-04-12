set.seed(447)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)

## Chicago Crash Data ##
# https://www.kaggle.com/datasets/anoopjohny/traffic-crashes-crashes?resource=download

cat("Reading Data... \n")
df <- read_excel("final_project/crash_dates_and_damages.xlsx")

cat("Processing Dates...\n")
df <- df %>%
  filter(!grepl("^\\d+\\.\\d+$", CRASH_DATE)) %>%  
  mutate(CRASH_DATE = mdy_hms(CRASH_DATE, tz = "UTC", quiet = TRUE)) %>%  
  drop_na(CRASH_DATE) %>% 
  mutate(CRASH_DATE = format(CRASH_DATE, "%m/%d/%Y"))  

cat("Aggregating... \n")
damage_levels <- unique(df$DAMAGE)
aggregated_data <- lapply(damage_levels, function(damage_level) {
  df_filtered <- df %>% filter(DAMAGE == damage_level)
  weekly_aggregation <- df_filtered %>%
    mutate(Week = floor_date(as.Date(CRASH_DATE, format = "%m/%d/%Y"), "week")) %>%
    group_by(Week) %>%
    summarise(Crash_Count = n(), .groups = 'drop') %>%
    arrange(Week)
  
  cat("Estimating Missing Values for ", damage_level, "...\n")
  average_2016 <- weekly_aggregation %>%
    filter(year(Week) == 2016) %>%
    summarise(Average_Crash = mean(Crash_Count))
  
  L14 <- average_2016$Average_Crash / 3  
  L15 <- average_2016$Average_Crash / 2
  
  weekly_aggregation <- weekly_aggregation %>%
    mutate(
      Adjustment = case_when(
        year(Week) == 2014 ~ rpois(n(), L14),  
        year(Week) == 2015 ~ rpois(n(), L15),  
        TRUE ~ 0
      ),
      Crash_Count_Adjusted = Crash_Count + Adjustment
    )
  return(weekly_aggregation)
})

# we've actually ended up creating latent categorization by 
# crash severity, which is exactly what we are looking for 
# we can think of this as "evaluated weekly crash reports"
# assuming that more severe crashes will be processed later
# or, more likely, the number of evaluated crash reports 
# declines in groups over time as severity increases
# since more severe reports take longer to evaluate
outDF = data.frame(
  crash_time = c(
    aggregated_data[[1]]$Week,
    aggregated_data[[2]]$Week,
    aggregated_data[[3]]$Week
  ),
  crash_count =  c(
    aggregated_data[[1]]$Crash_Count_Adjusted,
    aggregated_data[[2]]$Crash_Count_Adjusted,
    aggregated_data[[3]]$Crash_Count_Adjusted
  )
)

cat("Writing to File... \n")
write.csv(outDF, "final_project/cleaned_crash_data.csv", row.names = FALSE)

cat("Plotting Data... \n")
plot(outDF$crash_count)
