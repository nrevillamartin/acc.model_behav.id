
#-------------------------------------------------------------------------#
# Join tables from several individuals                                    #
# authors: Natalia Revilla Martín and Carolina Bravo                      #
# 02.04.2024                                                              #
#-------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(tibble)

# In excel I deleted kk and kk2 rows

# Path to last common folder
path_folder <- "C:/Users/natalia.revilla/OneDrive - ctfc.cat/3. Proyectos/2023 Acelerómetros/Experimento acelerometro 2023/3. Analisis/data/datos etiquetados"


# Files
path_raw <- paste (path_folder, "data_total_MaleVR_Ornitela_alerta_descansa_pivot_2seg_comp_maj_sin_NA", sep = "/")
acc_maleVR <- read.csv2(paste (path_raw, "csv", sep = "."), sep=",", header=TRUE)
acc_maleVR$sex <- 0 # add column for sex

path_raw <- paste (path_folder, "CLASIF_Female_Ornitela_Zoodysee_pivot_2seg_comp_maj_sin_NA", sep = "/")
acc_female <- read.csv2(paste (path_raw, "csv", sep = "."), sep=",", header=TRUE)
acc_female$sex <- 1

path_raw <- paste (path_folder, "CLASIF_Female_Tabernas_pivot_2seg_comp_maj_sin_NA", sep = "/")
acc_female2 <- read.csv2(paste (path_raw, "csv", sep = "."), sep=",", header=TRUE)
acc_female2$sex <- 1

acc_inds <- rbind(acc_maleVR, acc_female, acc_female2) 


# Order columns
acc_inds <-  acc_inds %>% 
  relocate(comp_cod, .before = x1) %>%
  relocate(sex, .after = device) %>%
  relocate(ind, .after = device)

acc_inds$burst_rows <- NULL

# Summary number of rows per bevaviour
acc_inds %>%
  group_by(comp_cod) %>%
  summarise(count=n())

acc_inds %>%
  group_by(maj_comp) %>%
  summarise(count=n())

# Check there are no  NAs
which(is.na(acc_inds))


write.csv(acc_inds,(paste (path_folder, "TETRAX_FR_AL_ALERTA_2seg.csv", sep = "/")), row.names=FALSE, quote = FALSE)
