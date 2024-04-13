set.seed(447)
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
  df_filtered <- df %>%
    filter(DAMAGE == damage_level) %>%
    # Exclude the years 2014 and 2015 from our dataset
    filter(!year(as.Date(CRASH_DATE, format = "%m/%d/%Y")) %in% c(2014, 2015))
  
  # for the i-th damage level
  weekly_aggregation <- df_filtered %>%
    # reformat date to week
    mutate(Week = floor_date(as.Date(CRASH_DATE, format = "%m/%d/%Y"), "week")) %>%
    # group by common weeks
    group_by(Week) %>%
    # and figure out total weekly crash counts
    summarise(Crash_Count = n(), .groups = 'drop') %>%
    arrange(Week)
  
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
    aggregated_data[[1]]$Crash_Count,
    aggregated_data[[2]]$Crash_Count,
    aggregated_data[[3]]$Crash_Count
  )
)

# which we write to a csv
cat("Writing to File... \n")
write.csv(outDF, "final_project/cleaned_crash_data.csv", row.names = FALSE)

plot(outDF$crash_count)
