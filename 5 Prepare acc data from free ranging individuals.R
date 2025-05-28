
#-------------------------------------------------------------------------#
# Prepare datafame from free rangins individuals                          #
# authors: Natalia Revilla Martín and Carolina Bravo                      #
# 19.04.2024                                                              #
#-------------------------------------------------------------------------#

# Libraries ---------------------------------------------
library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(ggplot2)

# Open files
path_folder <- "C:/Users/natalia.revilla/OneDrive - ctfc.cat/3. Proyectos/2023 Acelerómetros/Experimento acelerometro 2023/3. Analisis/data/individuos libertad 2023/new"
setwd(path_folder) 

## ONE SINGLE FILE
path_raw <- paste (path_folder, "Fletxa_222010_20230901_20231231", sep = "/")
raw_acc <- read.csv2(paste (path_raw, "csv", sep = "."), sep=",", header=TRUE)

## LIST OF FILES
files <- list.files(path_folder, pattern= "20230801_20231031")
list_raw_acc <- lapply(files, read.csv)  # read.csv cuando el separador es una coma, read.csv2 cuando es ;

list_acc <- list()
list_gps <- list()



# Format variables (like in script 2 Pivot tables) ------------------
for (i in 1:length(list_raw_acc)) {   # if you have a list of files. if you have one file go to line 36
raw_acc <- list_raw_acc[[i]]

acc <- raw_acc %>% 
  select(c('device_id','UTC_datetime',"datatype","acc_x","acc_y","acc_z", "speed_km_h")) %>%
  rename(
    device_info_serial = device_id,
    date_time = UTC_datetime,
    x = acc_x,
    y = acc_y,
    z = acc_z,
    speed = speed_km_h) %>%
  mutate_at(c('x', 'y',"z"), as.numeric)

acc$x <- acc$x / 1000
acc$y <- acc$y / 1000
acc$z <- acc$z / 1000


# Identify bursts --------------------------------------------------

# Time difference with the previous row
acc$date_time <- as_datetime(acc$date_time)

acc$date_time2 <- c(acc$date_time[1], acc$date_time[-length(acc$date_time)]) # column moved one position
acc$diff <- difftime(acc$date_time, acc$date_time2) # difference between present and previous time

# Burst_ID
# if diff > 3 it is identified as a new burst
condition <- acc$diff > 3
acc$burst_ID <- cumsum(condition)


# Sparate GPS and acc data ----------------------------
gps <- acc[!is.na(acc$speed),]

acc <- acc[is.na(acc$speed),]
acc$speed <- 0


# 1. ACCELEROMETRY --- --- --- --- --- --- --- --- --- --- ----

# 1.1. Number and order rows ----
# Time diff, only for acc data. To identify possible errors
acc$date_time3 <- c(acc$date_time[1], acc$date_time[-length(acc$date_time)]) 
acc$diff_acc <- difftime(acc$date_time, acc$date_time3) 

# Index
acc <- acc %>%
  group_by(burst_ID) %>%
  mutate(index = row_number())

# Burst_rows: number of rows in a window
burst_r <- acc %>% 
  group_by(burst_ID) %>% 
  summarise(burst_rows = n()) %>%
  as.data.frame()
acc <- left_join(acc, burst_r)

# ATTENTION: identify various settings if existent --------
table(acc$burst_rows)

# Create one object for each setting
acc1 <- acc %>%
   subset(burst_rows == 100)
 acc2 <- acc %>%                    
   subset(burst_rows == 80)

# Make sure these settings do not overlap
min1 <- min(acc1$date_time)
max1 <- max(acc1$date_time)
min2 <- min(acc2$date_time)
max2 <- max(acc2$date_time)

min1; max1; min2; max2


# ATTENTION: Indicate Ornitela settings -----------------------------
# The different settings need to be ordered separately 
# We choose one of the subsets (acc1, acc2)

Hz <- 20 # herzios  
sbt <- 4 # sensor burst time   
gbt <- 5 # gps burst time. if there are several, the smaller one 
wt <- 2 # window time: duracion del segmento en el que queremos dividir los datos

points_segm <- Hz * wt # number of acc points in a window
points_burst <- Hz * sbt # number of acc points in a burst
reps <- sbt/wt # number of windows in a burst

# segm: vector to divide burst in windows 
segm <- sort(rep(1:reps, points_segm))


# 1.2. Divide in windows --------------------------------------
acc_sel <- "acc2"  # acc1 o acc2
accw <- acc2

segm_ID <- rep(segm, times= nrow(accw)/points_burst)
accw <- accw %>% arrange(burst_ID)
accw <- cbind(accw, segm_ID = segm_ID)

# n_comp: numbers windows
accw <- accw %>%
  group_by(burst_ID, segm_ID) %>%
  mutate(n_comp = row_number())


# 1.3. Pivot table -------------------------------------------------------------
acc_xyz <- select(accw, c("device_info_serial","datatype" ,"x","y","z" ,"burst_ID", "segm_ID", "n_comp"))

acc_xyz <- pivot_wider(acc_xyz, names_from = "n_comp",
                         values_from = c("x", "y","z"), 
                         names_sep = "")

# Add date and time
f <- accw %>% 
  dplyr::select("date_time", "burst_ID", "segm_ID") %>%
  group_by(burst_ID, segm_ID) %>% 
  filter(row_number()==1) # i reatain the first date of each burst

acc_xyz <- left_join(f, acc_xyz,  by = join_by(burst_ID, segm_ID))

# Order new table 
times <- rep (1:points_segm, 3)
times <- sort(times)
xyz <- rep (c("x", "y", "z"), points_segm)
or_xyz <- paste(xyz, times, sep="")

common <- colnames(acc_xyz[1:(length(acc_xyz) - length(xyz))]) # column names before x1
common_xyz <- c(common, or_xyz)

acc_xyz <- acc_xyz[common_xyz]


# 2. EXPLORE -------------------------------------------------------------------
# Which months have data?
ggplot(acc_xyz, aes(x = as.factor(month(date_time)))) +
  geom_bar()

# 3. SAVE TABLES ----------------------------------------------------------------
# take into account:
acc_sel
min2
max2


## LIST OF FILES
list_acc <- append(list_acc, list(acc_xyz))
list_gps <- append(list_gps, list(gps2))
}


## LIST OF FILES
acc_fem <- as.data.frame(rbind(list_acc[[1]], list_acc[[3]]))
gps_fem <- as.data.frame(rbind(list_gps[[1]], list_gps[[3]]))

acc_male <- as.data.frame(rbind(list_acc[[1]], list_acc[[2]], list_acc[[3]], list_acc[[4]]))
gps_male <- as.data.frame(rbind(list_gps[[1]], list_gps[[2]], list_gps[[3]], list_gps[[4]]))

path_save <- paste (path_folder, "MALES_20230801_20231031", sep = "/")

write.csv(acc_male,(paste (path_save, "pivot_2seg_20Hz_ACC.csv", sep = "_")), row.names=FALSE, quote = FALSE)
write.csv(gps_male,(paste (path_save, "GPS.csv", sep = "_")), row.names=FALSE, quote = FALSE)


## ONE SINGLE FILE
path_save <- paste (path_folder, "Fletxa_222010_20230901_20231231", sep = "/")

write.csv(acc_xyz,(paste (path_save, "pivot_2seg.csv", sep = "_")), row.names=FALSE, quote = FALSE)
write.csv(gps2,(paste (path_save, "gps.csv", sep = "_")), row.names=FALSE, quote = FALSE)



  

