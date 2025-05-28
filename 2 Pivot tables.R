
#-------------------------------------------------------------------------#
# Pivot tables once labelled with UvaBits                                 #
# authors: Natalia Revilla Martín and Carolina Bravo                      #
# 09.04.2024                                                              #
#-------------------------------------------------------------------------#

# This script rearranges the tables so that the accelerometer bursts are divided into segments (windows)
# of a specified duration, labeled with a behavior.
# Each segment will occupy one row, and these rows are the units from which features will be extracted
# to build the models.
#
# There are two options for creating the segments:
#     - Pure behavior (DISCARDED): Each segment (each row) has a pure behavior. Minimum 2 seconds and maximum the burst duration
#     - Mixed behavior: Segments of a predetermined duration that include multiple behaviors. They are labeled with the majority behavior
#
# We have worked with data at 20 Hz. I created the 10 Hz table using Excel.

library(dplyr)
library(lubridate)
library(tidyr)
library(rabc)


# ATTENTION, PERSONALISE ------------------------------------------------
# Create vector to divide burst into segments, 
# according to your acc frequency and desired window lenght

# points: number of accelerometer points per window
# segm: vector, it needs to sum the number of acc points of a complete burst 

# In our case we had 12 seconds bursts at 20 Hz, that we wanna divide in 2 second windows
# 2 sec x 20 Hz = 40 points
# 12 sec x 20 Hz = 240 points
# 6 windows x 40 acc points = 240 acc points

points <- 40 
segm <- c(rep(1, 40), rep(2, 40), rep(3, 40), rep(4, 40), rep(5, 40), rep(6, 40))


# Another option: 3 windows x 80 rows (4 seconds)
# segm <- c(rep(1, 80), rep(2, 80), rep(3, 80))
# points <- 80


# Join labelled csv from different videos and individuals -------------------------------------------

# We had separate folders per sex, and subfolders per date
setwd("C:/Users/natalia.revilla/OneDrive - ctfc.cat/3. Proyectos/2023 Acelerómetros/Videos y fotos sison/AA_Ornitela/Female_ornitela")

# List of folders. One folder per survey day. In our case, all folders include 23 for year 2023
fold <- list.files(getwd(), pattern = "23", full.names = T)

# List of classified files (their name contain the pattern CLASIF)
file <- list.files(fold, pattern = "CLASIF", full.names = T)

# Join csv
onefile <- file %>% 
  lapply(read.csv2, sep = ",", header = F) %>% 
  bind_rows 

# Error in last column
onefile <- separate(onefile, V8, c("comp", "cod"), sep = " (?=[^ ]+$)")

# Rename columns
colnames(onefile) <- c("device", "date_time","n_burst", "x", "y", "z", "kk", "comp", "kk2")

# If saved as row.names = T it created one initial column without name
# Pay attention, when opening the file, this column is named as X
write.csv(onefile,"CLASIF_Female_Ornitela_Zoodysee.csv", row.names = T)




# Open file ----------------------------------------------------------------
path_folder <- "labelled_data"

# Female
path_raw <- paste (path_folder, "CLASIF_Female_Ornitela_Zoodysee", sep = "/")
acc <- read.csv2(paste (path_raw, "csv", sep = "."), sep=",", header=TRUE)

# Male
path_rawA <- paste (path_folder, "data_total_MaleR_Ornitela_alerta_descansa", sep = "/")
path_rawB <- paste (path_folder, "data_total_MaleV_Ornitela_alerta_descansa", sep = "/")

accA <- read.csv2(paste (path_rawA, "csv", sep = "."), sep=",", header=TRUE)
accB <- read.csv2(paste (path_rawB, "csv", sep = "."), sep=",", header=TRUE)

acc <- rbind(accA, accB)

table(acc$comp)


# Create columns needed to identify windows  -------------------------------
# Pay attention if you have added or deleted columns not included in this code
# Time difference with the previous row
acc$date_time <- as.POSIXct(x = as.character(acc$date_time), format = "%m/%d/%Y %H:%M:%S")
acc$date_time <- as_datetime(acc$date_time)   

acc$date_time2 <- c(acc$date_time[1], acc$date_time[-length(acc$date_time)])
acc$diff <- difftime(acc$date_time, acc$date_time2)

# burst_ID:
# if time difference is larger than 3 seconds, it is identified as a different burst
condition <- acc$diff > 3
acc$burst_ID <- cumsum(condition)

# burst_rows:
# divides bursts in windows
burst_r <- acc %>% 
  group_by(burst_ID) %>% 
  summarise(burst_rows = n()) %>%
  as.data.frame()
acc <- left_join(acc, burst_r)

# Delete bursts with less than 240 points (12 seconds at 20 Hz)
acc2 <- subset(acc, burst_rows == 240)

# Join row that divides in windows
segm_ID <- rep(segm, times= nrow(acc2)/240)

acc2 <- acc2 %>% arrange(burst_ID)
acc2 <- cbind(acc2, segm_ID)


# Label windows with the predominant behaviour in that window ---------------
acc2$burst_segm <- paste(acc2$burst_ID, acc2$segm_ID, sep = "_")

p <- acc2 %>% 
  group_by(burst_segm) %>% 
  count(comp)
p2 <- p %>% 
  group_by(burst_segm) %>% 
  top_n(1, n)
names(p2)[names(p2) == 'comp'] <- 'maj_comp_codi'

# If there is no predominant behaviour, classified as OTHERS (labelled with number 14 in our case)
p3 <- p2
p3[p3$n < 21, "maj_comp_codi"] <- 14
p3 <- unique(p3)

p4 <- p3[,1:2]
p4 <- as.data.frame(p4)
nrow(p4)/3  

acc3 <- merge(acc2, p4)


# Create column that identifies 2 second windows (segm_ID), and numbers rows within each windows (n_comp)
# it works like the index from script 1
acc3$segm_ID2 <- c(acc3$segm_ID[1], acc3$segm_ID[-length(acc3$segm_ID)])

acc3$n_comp <- 1
colnames(acc3)

for (i in 2:nrow(acc3)) {
  if (acc3[i,16] == acc3[i,18] ) {    # if segm_ID = segm_ID2
    n_comp <- acc3[i-1,19] + 1     #  n_comp = n_comp from previous row + 1
    acc3[i,19] <- n_comp   
  } else {
    acc3[i,19] <- 1     
  }
}


# Pivot tabla ----------------------------------------------------------------
# Pivot x
acc_subx <- dplyr::select(acc3, - c("X","y","z", "kk", "kk2", "n_burst","burst_rows","date_time2", "diff", "comp", "segm_ID2"))
acc_subx$segm_ID <- as.character(acc_subx$segm_ID)

ncolx <- ncol(acc_subx) - 1

acc_subx2 <- pivot_wider(acc_subx, names_from = "n_comp",
            values_from = "x")

colnames(acc_subx2)
new_colnames <- paste ("x", colnames(acc_subx2[ncolx:ncol(acc_subx2)]), sep = "")
colnames(acc_subx2)[ncolx:ncol(acc_subx2)] <- new_colnames


# Pivot y
acc_suby <- select(acc3, - c("X","x", "z", "kk", "kk2", "n_burst", "burst_rows","date_time2", "diff", "comp", "segm_ID2"))
acc_suby$segm_ID <- as.character(acc_suby$segm_ID)
ncoly <- ncol(acc_suby) - 1

acc_suby2 <- pivot_wider(acc_suby, names_from = "n_comp",
                         values_from = "y")

colnames(acc_suby2)
new_colnames <- paste ("y", colnames(acc_suby2[ncoly:ncol(acc_suby2)]), sep = "")
 colnames(acc_suby2)[ncoly:ncol(acc_suby2)] <- new_colnames


# Pivot z
acc_subz <- select(acc3, - c("X","x", "y", "kk", "kk2",  "n_burst","burst_rows", "date_time2",  "diff", "comp", "segm_ID2"))
acc_subz$segm_ID <- as.character(acc_subz$segm_ID)
ncolz <- ncol(acc_subz) - 1

acc_subz2 <- pivot_wider(acc_subz, names_from = "n_comp",
                         values_from = "z")

colnames(acc_subz2)
new_colnames <- paste ("z", colnames(acc_subz2[ncolz:ncol(acc_subz2)]), sep = "")
colnames(acc_subz2)[ncolz:ncol(acc_subz2)] <- new_colnames

# Join x y z tables
acc_xyz <- cbind(acc_subx2, acc_suby2[ncoly:ncol(acc_suby2)], acc_subz2[ncolz:ncol(acc_subz2)])



# Order new table -------------------------------------------------------
times <- rep (1:points, 3)
times <- times[order(times)]
xyz <- rep (c("x", "y", "z"), points)
or_xyz <- paste(xyz, times, sep="")

colnames(acc_xyz)
common <- colnames(acc_xyz[1:ncolx - 1])
common_xyz <- c(common, or_xyz)

acc_xyz <- acc_xyz[common_xyz]



# Labells from UvaBits are numbers, change them to names ---------------

# Example: 
acc_xyz$maj_comp <- acc_xyz$maj_comp_codi

acc_xyz$maj_comp[acc_xyz$maj_comp==1] <- "canto" 
acc_xyz$maj_comp[acc_xyz$maj_comp==2] <- "salto" 
acc_xyz$maj_comp[acc_xyz$maj_comp==3] <- "excitado" 
acc_xyz$maj_comp[acc_xyz$maj_comp==4] <- "caminar" 
acc_xyz$maj_comp[acc_xyz$maj_comp==5] <- "correr" 
acc_xyz$maj_comp[acc_xyz$maj_comp==6] <- "ratoneo" 
acc_xyz$maj_comp[acc_xyz$maj_comp==7] <- "picoteo" 
acc_xyz$maj_comp[acc_xyz$maj_comp==8] <- "engulle" 
acc_xyz$maj_comp[acc_xyz$maj_comp==10] <- "busca_comida" 
acc_xyz$maj_comp[acc_xyz$maj_comp==11] <- "de_pie" 
acc_xyz$maj_comp[acc_xyz$maj_comp==12] <- "tumbado" 
acc_xyz$maj_comp[acc_xyz$maj_comp==14] <- "otros" 
acc_xyz$maj_comp[acc_xyz$maj_comp==15] <- "atusa" 
acc_xyz$maj_comp[acc_xyz$maj_comp==16] <- "sacudir" 
acc_xyz$maj_comp[acc_xyz$maj_comp==17] <- "vuelo" 

acc_xyz<-acc_xyz %>%
  mutate(comp_cod=case_when(maj_comp=="canto" | maj_comp=="salto" | maj_comp=="excitado" ~ "courtship",
                            maj_comp=="caminar" ~ "locomotion_slow",
                            maj_comp=="correr" | maj_comp=="ratoneo" ~ "locomotion_fast",
                            maj_comp=="engulle" | maj_comp=="busca_comida" | maj_comp=="picoteo" ~ "foraging",
                            maj_comp=="de_pie" | maj_comp=="tumbado" | maj_comp=="atusa"  ~ "resting",
                            TRUE~"OTHER"))

acc_xyz <- acc_xyz %>% relocate(maj_comp, .after = maj_comp_codi)
acc_xyz <- acc_xyz %>% relocate(comp_cod, .after = maj_comp)

# Delete row if maj_comp = 0
acc_xyz<-acc_xyz[acc_xyz$maj_comp != 0, ]


# Summary number of rows per bevaviour ----------------------------------
acc_xyz %>%
  group_by(comp_cod) %>%
  summarise(count=n())

acc_xyz %>%
  group_by(maj_comp) %>%
  summarise(count=n())

# Check there are no  NAs
which(is.na(acc_xyz))


# Save table ----------------------------------------------------------------
write.csv(acc_xyz,(paste (path_raw, "pivot_2seg_comp_maj_sin_NA.csv", sep = "_")), row.names=FALSE, quote = FALSE)





