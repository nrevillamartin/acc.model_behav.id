
#-------------------------------------------------------------------------#
# Format Ornitela accelerometer data to open it in UvaBits                #
# authors: Natalia Revilla Martín and Carolina Bravo                      #
# 8.07.2024                                                               #
#-------------------------------------------------------------------------#

library(dplyr)
library(readxl)
library(lubridate)

# Folder where raw csv (downloaded from Ornitela) are located
path_folder <- "C:/Users/natalia.revilla/OneDrive - ctfc.cat/3. Proyectos/2023 Acelerómetros/Videos y fotos sison/AA_Ornitela"

# csv file name (WITHOUT .csv)
path_raw <- paste (path_folder, "Female_ornitela/080523/Female_210990_20230508", sep = "/")
raw_acc <- read.csv2(paste (path_raw, "csv", sep = "."), sep=",", header=TRUE)


# 1. Format matrix with columns required by Uvabits -------------

acc <- raw_acc %>% 
  select(c('device_id','UTC_datetime',"acc_x","acc_y","acc_z")) %>%
  rename(
    device_info_serial = device_id,
    date_time = UTC_datetime,
    x_cal = acc_x,
    y_cal = acc_y,
    z_cal = acc_z) %>%
  mutate_at(c('x_cal', 'y_cal',"z_cal"), as.numeric)

acc$x_cal<- acc$x_cal / 1000
acc$y_cal<- acc$y_cal / 1000
acc$z_cal<- acc$z_cal / 1000

acc$speed <- 0

# Create index column. Attention: do not add columns in previous code, if you do it, you need to change column numbers in the loop
acc$date_time <- as_datetime(acc$date_time)   # format datetime

acc$date_time2 <- c(acc$date_time[1], acc$date_time[-length(acc$date_time)]) # new column moved one position
acc$diff <- difftime(acc$date_time, acc$date_time2) # difference between present and previous time

acc$index <- 1

for (i in 2:nrow(acc)) {
  if (acc[i,8] < 2){    # if diff < 2 
    index <- acc[i-1,9] + 1     # index = index from previous row +1
    acc[i,9] <- index   # write new index
  } else { 
    acc[i,9] <- 1     # if diff > o = 2, index = 0
  }
}
acc<-acc %>% 
  select(c('device_info_serial','date_time',"index", "x_cal","y_cal","z_cal","speed"))


write.csv(acc,(paste (path_raw, "formated.csv", sep = "_")), row.names=FALSE, quote = FALSE)



# 2. Cut acc data according to video  -----------------------------
# One csv file per video

acc <- read.csv2(paste (path_raw, "formated.csv", sep = "_"), sep=",", header=TRUE)

acc$date_time <- as_datetime(acc$date_time)  
str(acc)

date1 <- as.POSIXct("2023-05-08 09:55:00") # ATTENTION: check whether time is being written in UTC 
date2 <- as.POSIXct("2023-05-08 10:01:00")
int <- interval(date1, date2)

acc_cut <- acc[acc$date_time %within% int,]

# Include in wile name the initial time of the video
write.csv(acc_cut,(paste (path_raw, "cut_07.55.csv", sep = "_")), row.names=FALSE, quote = FALSE)


