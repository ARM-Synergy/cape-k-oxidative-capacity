#clear all
rm(list = ls())

library(openair)
library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
library(latticeExtra)
library(gridExtra)
library(cowplot)
library(ggplot2)
library(patchwork)


install.packages("openairmaps")
library(openairmaps)

library(devtools)
#source_gist("https://gist.github.com/davidcarslaw/c67e33a04ff6e1be0cd7357796e4bdf5",
#            filename = "run_hysplit.R")


#Get meteorological data from the ftp link below. This fn doesn't work so just copy-paste the month into a working folder

getMet <- function(year = 2013, month = 1,
                   path_met = "c:/Users/myn003/OneDrive - CSIRO/PhD/R Studio") {
  for (i in seq_along(year)) {
    for (j in seq_along(month)) {
      download.file(
        url = paste0(
          "ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/RP",
          year[i], sprintf("%02d", month[j]), ".gbl"
        ),
        destfile = paste0(
          path_met, "RP", year[i],
          sprintf("%02d", month[j]), ".gbl"
        ),
        mode = "wb"
      )
    }
  }
}



#ONLY RUN THIS ONCE
# getMet(year = 2023, month = 9)

#ONLY RUN IF YOU KNOW WHAT YOU"RE DOING!!!!
#for next run add interval of 1 hour, change height to 100m
#Aspendale BAQS long lat -38.024326340884315, 145.1024937502337
#KCGBAPS -40.683071624774406, 144.69016268249712

# MISO RVI Data
setwd("c:\\Users\\myn003\\OneDrive - CSIRO\\PhD\\MISO\\HYSPLIT")
rvi_data <- read_csv("MISO_DateTime_Lat_Long.csv")
rvi_data <- na.omit(rvi_data)
rvi_data <- rvi_data %>%
  mutate(`Date/Time` = format(`Date/Time`, "%Y-%m-%d %H:%M"))
rvi_data
#rvi_lat <- rvi_data[["Latitude (degree_north)"]]
#rvi_lat <- as.list(rvi_lat)
#rvi_lon <- rvi_data[["Longitude (degree_east)"]]
#rvi_lon <- as.list(rvi_lon)

#coordinates <- list(
#  c(date = "2024-01-01 00:00", latitude = -40, longitude = 120),
#  c(date = "2024-01-02 01:00", latitude = -35, longitude = 125),
#  c(date = "2024-01-03 02:00", latitude = -30, longitude = 130)
#)

add_hour <- function(date_str) {
  # Parse the input date string
  date_time <- ymd_hm(date_str)
  # Add an hour
  new_date_time <- date_time + hours(1)
  # Format and return the new date string
  return(format(new_date_time, "%Y-%m-%d %H:%M"))
  }

# Create an empty list to store the output for each coordinate
data_out_list <- list()

for (i in 1:nrow(rvi_data)) {
  coord <- rvi_data[i, ]
  data_out_run <- run_hysplit(
    latitude = coord[[2]], 
    longitude = coord[[3]], 
    interval = "1 hour",
    runtime = -120, 
    start_height = 100, 
    model_height = 10000, 
    start = coord[[1]],
    end = add_hour(coord[[1]]),
    hysplit_exec = "C:/hysplit/exec", 
    hysplit_input = "c:/Users/myn003/OneDrive - CSIRO/PhD/R Studio", 
    hysplit_output = "c:/Users/myn003/OneDrive - CSIRO/PhD/R Studio/Temp",
    verbose = TRUE)
    print(head(data_out_run))
  
  # Append the output to the list
  output_directory = "c:/Users/myn003/OneDrive - CSIRO/PhD/MISO/HYSPLIT/Output"
  date_string <- substring(coord[[1]], 1, 13)
  output_filename <- file.path(output_directory, paste0(date_string, ".csv"))
  print(output_filename)
  write.csv(data_out_run, file=output_filename)

}



save(data_out_list, file="c:/Users/myn003/OneDrive - CSIRO/PhD/R Studio")

write.csv(data_out, "c:/Users/myn003/OneDrive - CSIRO/PhD/R Studio/backtraj1hrs.csv")

# Merge PTR data with traj data
#data_out_run <- left_join(data_out_run, data_all_hourly_filt, by = "date")
#head(data_out_run)

# Create a day column in data_out traj
data_out_list$day <- as.Date(data_out_list$date)
data_out_list

# For plotting 
trajPlot(data_out_run)

# For interactive plot
trajMap(data_out_run, colour="day", control="day")

# For day plot


#data_out <- read.csv(file = "C:/Users/User/Desktop/Cape Grim Backtraj stuff/backtraj1hrs.csv")

#data_out <- subset(data_out, select = -c(X))

#data_out$date <- as.POSIXct(data_out$date, format = '%Y-%m-%d')
#data_out$date2 <- as.POSIXct(data_out$date2, format = '%Y-%m-%d')
#data_out$hour.inc <- as.numeric(data_out$hour.inc)


#head(data_out)

#str(data_out_run)
#str(data_out)


#selectByDate(data_out,
#             start = "01/01/2015",
#             end = "02/01/2015"
#) %>%
#  trajPlot(
#    map.cols = openColours("hue", 10),
#    col = "grey30"
#  )

#frequency
#filter(data_out, lat > -44, lat < -32, lon > 135, lon < 155)%>%
#  trajLevel(statistic = "frequency", orientation = c(-37, 145, 0), grid.col = "transparent", map.fill = TRUE)


#potential source contribution function
#filter(data_out, lat > -44, lat < -32, lon > 135, lon < 155)%>%
#  trajLevel(statistic = "pscf", orientation = c(-37, 145, 0), grid.col = "transparent", map.fill = TRUE)


#Concentration Weighted Trajectory
#filter(data_out, lat > -44, lat < -20, lon > 130, lon < 160)%>%
#  trajLevel(statistic = "cwt", orientation = c(-37, 145, 0), grid.col = "transparent", map.fill = TRUE)

#DONT RUN!!!!!
#Simplified Quantitative Transport Bias Analysis
#filter(data_out, lat > -44, lat < -20, lon > 130, lon < 160)%>%
#  trajLevel(statistic = "sqtba", orientation = c(-37, 145, 0), grid.col = "transparent", map.fill = TRUE)



#data_out_test <- data_out_run

#trajCluster(data_out_test, method = "Angle", 
#                     n.cluster = 6, 
#                     col = "Set2",
#                     map.cols = openColours("Paired", 10))


#trajMap(data_out)

