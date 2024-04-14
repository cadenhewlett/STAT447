library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)

#######################
### Data Processing ###
#######################

# NOTE: the aggregation is over a very large dataset, so runtime is slow

cat("Reading Data... \n")
# read Chicago Crash data from excel using `readxl`
df <- read_excel("final_project/crash_dates_and_damages.xlsx")

cat("Processing Dates...\n")
df <- df %>%
  # drop weirdly formatted excel dates
  filter(!grepl("^\\d+\\.\\d+$", CRASH_DATE)) %>%  
  # standrdize remaining dates to same timezone
  mutate(CRASH_DATE = mdy_hms(CRASH_DATE, tz = "UTC", quiet = TRUE)) %>%  
  # remove NAs
  drop_na(CRASH_DATE) %>% 
  # standardize formatting to MDY
  mutate(CRASH_DATE = format(CRASH_DATE, "%m/%d/%Y"))  

cat("Aggregating... \n")
# here, we aggregate severity counts weekly  
aggregated_data <- lapply(damage_levels, function(damage_level) {
  # we filter through each damage level
  df_filtered <- df %>% filter(DAMAGE == damage_level)
  # and aggregate by week 
  weekly_aggregation <- df_filtered %>%
    mutate(Week = floor_date(as.Date(CRASH_DATE, format = "%m/%d/%Y"), "week")) %>%
    group_by(Week) %>%
    summarise(Crash_Count = n(), .groups = 'drop') %>%
    arrange(Week)
  # use NNI for poorly-captured 2014, 2015 data
  cat("Imputing ", damage_level, "...\n")
  # get 2016 weeks
  weeks_2016 <- weekly_aggregation %>%
    filter(year(Week) == 2016) %>%
    pull(Week)
  # get exact week if present, else nearest week by euclidean distance
  weekly_aggregation <- weekly_aggregation %>%
    rowwise() %>%
    mutate(
      Nearest_2016_Week = if(year(Week) %in% c(2014, 2015)) {
        weeks_2016[which.min(abs(difftime(Week, weeks_2016, units = "weeks")))]
      } else {
        Week
      }
    ) %>%
    left_join(weekly_aggregation %>% filter(year(Week) == 2016) %>%
                select(Week, Crash_Count), by = c("Nearest_2016_Week" = "Week")) %>%
    mutate(
      # Use 2016's Crash_Count for nearest week and add to scaled 2014 count if present
      Crash_Count_Adjusted = if_else(year(Week) == 2014, Crash_Count.y + Crash_Count.x / max(Crash_Count.x) * 2, Crash_Count.x)
    ) %>%
    select(Week, Crash_Count_Adjusted)
  
  return(weekly_aggregation)
})



# the data frame we use is then the aggregated weeks & counts
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

# which we write to a csv
cat("Writing to File... \n")
write.csv(outDF, "final_project/cleaned_crash_data.csv", row.names = FALSE)

plot(outDF$crash_count)
