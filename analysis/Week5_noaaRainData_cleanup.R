# VisitToAcadia - Data Cleanup
# Created by Joy-El Talbot
# For ALY6070
# On 2021-06-24
# Modified on 2021-06-24 by Joy-El Talbot

# Purpose: Based on existing EDA, clean-up and summarize the data for the dashboard
# Summarized file: noaa_summarized_rain.csv
#   Includes year, season, dayOfYear, date, timeOfDay, station, and inchesRain
#   DOES NOT include all zero-value measurements

### Working Directory = 2021Sb_ALY6010 folder
WORKINGDIR <- "C:/Users/joyel/Documents/Roux/ALY6010/2021Sb_ALY6010/"
setwd(WORKINGDIR)

### Load Libaries
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr) # part of tidyverse to get access to the map() function

### Load data
rain <- utils::read.csv("data/multicity_rain_noaa.csv",
                        stringsAsFactors = FALSE)

### Data Cleanup
### Repeat initial cleaning from noaa_rainfall_part2.html

# Step 1 - Convert 999.99 HPCP values to NA
clean_rain <- rain
clean_rain$HPCP[clean_rain$HPCP == 999.99] <- NA

# Step 1b - rename HPCP to indicate units (inches)
clean_rain <- rename(clean_rain, inchesRain = HPCP)

# Step 2 - Convert timestamp to POSIXlt object
clean_rain$timestamp <- strptime(clean_rain$DATE, 
                                 format = "%Y%m%d %H:%M")

# Step 2b - Extract different date pieces
clean_rain$date <- strftime(clean_rain$timestamp, 
                            format = "%Y-%m-%d") %>% 
                   as.Date()

clean_rain$dayOfYear <- strftime(clean_rain$timestamp, format = "%j") %>%
                        as.numeric()

clean_rain$year <- strftime(clean_rain$timestamp, format = "%Y") %>%
                   as.numeric()

clean_rain$month <- as.numeric(strftime(clean_rain$timestamp, format = "%m"))

clean_rain$season <- sapply(clean_rain$month,
                            switch,
                            "Winter", # Jan
                            "Winter", # Feb
                            "Winter", # Mar
                            "Spring", # Apr
                            "Spring", # May
                            "Spring", # Jun
                            "Summer", # Jul
                            "Summer", # Aug
                            "Summer", # Sep
                            "Autumn", # Oct
                            "Autumn", # Nov
                            "Autumn") # Dec

clean_rain$hour <- strftime(clean_rain$timestamp, format = "%H") %>%
                   as.numeric()

clean_rain$timeOfDay <- NA
clean_rain$timeOfDay[clean_rain$hour %in% c(1, 2, 3, 4, 5, 6)] <- "early morning"
clean_rain$timeOfDay[clean_rain$hour %in% c(7, 8, 9, 10, 11, 12)] <- "morning"
clean_rain$timeOfDay[clean_rain$hour %in% c(13, 14, 15, 16, 17, 18)] <- "afternoon"
clean_rain$timeOfDay[clean_rain$hour %in% c(19, 20, 21, 22, 23, 0)] <- "night"
sum(is.na(clean_rain$timeOfDay)) # expect 0

# Step 3 - Summarize to STATION_NAME, date, dayOfYear, year, season, and timeOfDay
summary_rain <- clean_rain %>%
                group_by(year,
                         season,
                         dayOfYear,
                         date,
                         timeOfDay,
                         station = STATION_NAME) %>%
                summarize(inchesRain = sum(inchesRain))
head(summary_rain)

# NOTE!! Not all category combos have their assumed zero values represented in the data.

### Export cleaned data
write.csv(summary_rain,
          file = "data/noaa_summarized_rain.csv",
          row.names = FALSE)
