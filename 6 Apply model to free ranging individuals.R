
#-------------------------------------------------------------------------#
# Apply model to free ranging individuals                                 #
# authors: Natalia Revilla Martín and Carolina Bravo                      #
# 26.11.2024                                                              #
#                                                                         #
# R version 4.2                                                           #
#-------------------------------------------------------------------------#

# Libraries -------------------------------------------------------
library(readr)
library(dplyr)
library(rabc)
library(ggplot2)
library(viridis)
library(randomForest)
library(lubridate)
library(stringr)
library(xlsx)
library(beepr)
library(gridExtra)
library(mgcv)
library(tidyr)

# Open model ------------------------------------------------------

# Write here the characteristics of the model tou are going to apply
# Sampling frequency (Hz)
samp_freq = 10

# One sex or both sexes
sexo = "female"         # options are: "femmal", "female", "male"

# Clasificar flying o no
flying = "no"         # las opciones son: "si" "no"

# With or without courtship
cortejo = "COU"       # opcions are: "COU" "noCOU"

# With or without alerta
alerta = "ALE"  # options are: "ALE" o "noALE"

# Locomotion divided in slow-fast or not
locomo = "oneLOC"  # options are: "oneLOC" or "sfLOC"

# Lying separated from standing or not
lying = "LYI" # options are: "LYI" o "noLYI"

# Min and max date (optional)
daymin <- as.POSIXct("07/07/2019 00:01:00", format = "%d/%m/%Y %H:%M", tz = "GMT") # GMT = UTC
daymax <- as.POSIXct("31/12/2023 23:59:00", format = "%d/%m/%Y %H:%M", tz = "GMT")

# Open model
if (sexo == "female") {
  model <- readRDS("results/4ind/model_rd_nob_female_10Hz_COU_oneLOC_LYI.RDS")
} else if (sexo == "male") {
  model <- readRDS("results/4ind/model_rd_nob_male_10Hz_ALE_COU.RDS")
}


# Colors -----------------------------------------------------------
# These colors come from:
# viridis(option = "viridis", alpha = 1, begin = 0.0, end = 0.95, direction = -1, discrete=T)
# print(viridis(8, option = "D")) "#440154FF" "#46337EFF" "#365C8DFF" "#277F8EFF" "#1FA187FF" "#4AC16DFF" "#9FDA3AFF" "#FDE725FF"
comps <- levels(model$predicted); comps

if (sexo == "male") {
  if (cortejo == "COU") {
    comps_ordered <- c("resting", "alert", "locomotion_slow", "locomotion_fast", "foraging", "courtship") 
    comps_wellwriten <- c("Resting", "Alert","Slow locomotion", "Fast locomotion", "Foraging", "Courtship" ) 
    colores <- c("#DEE318FF", "#62CB5FFF", "#1FA188FF", "#2C738EFF", "#424086FF", "#440154FF")
  } else if (cortejo == "noCOU") {
    comps_ordered <- c("resting", "alert", "locomotion_slow", "locomotion_fast", "foraging") 
    comps_wellwriten <- c("Resting", "Alert","Slow locomotion", "Fast locomotion", "Foraging")
    colores <- c("#DEE318FF", "#62CB5FFF", "#1FA188FF", "#2C738EFF", "#424086FF")
  }
} else if (sexo == "female" & locomo == "oneLOC" & lying == "noLYI") {
  if (cortejo == "COU") {
    comps_ordered <- c("resting", "locomotion", "foraging", "courtship") 
    comps_wellwriten <- c("Resting", "Locomotion", "Foraging", "Courtship" ) 
    colores <- c("#DEE318FF", "#1FA188FF", "#424086FF", "#440154FF")
  } else if (cortejo == "noCOU") {
    comps_ordered <- c("resting", "locomotion", "foraging") 
    comps_wellwriten <- c("Resting", "Locomotion", "Foraging")
    colores <- c("#DEE318FF", "#1FA188FF", "#424086FF")
  }
} else if (sexo == "female" & locomo == "oneLOC" & lying == "LYI") {
  if (cortejo == "COU") {
    comps_ordered <- c("resting", "lying","locomotion", "foraging", "courtship") 
    comps_wellwriten <- c("Resting", "Lying","Locomotion", "Foraging", "Courtship" ) 
    colores <- c("#DEE318FF",  "#AADC32FF", "#1FA188FF", "#424086FF", "#440154FF")
  } else if (cortejo == "noCOU") {
    comps_ordered <- c("resting", "lying", "locomotion", "foraging") 
    comps_wellwriten <- c("Resting", "Lying","Locomotion", "Foraging")
    colores <- c("#DEE318FF", "#AADC32FF","#1FA188FF", "#424086FF")
  }
}

# Make sure comps and comps_ordered have the same levels. This should be TRUE
all(comps %in% comps_ordered)

# Open free-ranging acc data ----------------------------------------
path_folder <- "C:/Users/natalia.revilla/OneDrive - ctfc.cat/3. Proyectos/2023 Acelerómetros/Experimento acelerometro 2023/3. Analisis/data/individuos libertad 2023"

# acc data
path_raw1 <- paste (path_folder, "FEMALES_2019_2023_2seg_10Hz_ACC", sep = "/")
lib <- read.csv2(paste (path_raw1, "csv", sep = "."), sep=";", header=TRUE)

# GPS data (needed to filter out speed)
# path_raw2 <- paste (path_folder, "FEMALES_2019_2023_GPS", sep = "/")
path_raw2 <- paste (path_folder, "MALES_2023_GPS", sep = "/")
gps <- read.csv2(paste (path_raw2, "csv", sep = "."), sep=",", header=TRUE)

# save table names to use them in plot titles
nombresheet <- word(path_raw1, -1, sep = "/")
nombresheet2 <- str_sub(nombresheet, start= 1 , end=  -5)


# Clean errors ----------------------------------------------------
lib_file <- word(path_raw1, -1, sep = "/")

if (lib_file == "MALES_20230301_20230417_pivot_2seg_10Hz_ACC" | lib_file == "MALES_2023_2seg_10Hz_ACC") {
  lib <- subset(lib, burst_ID != "2721" | device_info_serial != "200902")
  lib <- subset(lib, burst_ID != "1430" | device_info_serial != "221998")
} 

# Delete rows (windows) those acc data is the same for all data of one of the axis
# If this happens, the function calculate_feature_freq gives an error
xs <- lib %>%
  select(starts_with('x'))

matsub_x <- as.matrix(xs)
freq_index <- samp_freq/length(xs)
freq_x <- apply(matsub_x, 1, max_freq_amp)
positions_longer_than_3_x <- which(sapply(freq_x, function(x) length(x) > 3))

if (length(positions_longer_than_3_x != 0)) {
  lib <- lib[-positions_longer_than_3_x, ]
}

zs <- lib %>%
  select(starts_with('z'))
matsub_z <- as.matrix(zs)
freq_index <- samp_freq/length(zs)

freq_z <- apply(matsub_z, 1, max_freq_amp)
positions_longer_than_3_z <- which(sapply(freq_z, function(x) length(x) > 3))

if (length(positions_longer_than_3_z != 0)) {
  lib <- lib[-positions_longer_than_3_z, ]
}



# Order data for rabc --------------------------------------------------

# Select variables for rabc
if (samp_freq == 20) {
  lib_rabc <- lib[,6:125]
  lib_rabc <- lib_rabc  %>%
    mutate(across(1:120, as.numeric)) # convertir a numerico. ultimas columnas como character
 } else if (samp_freq == 10) {
    lib_rabc <- lib[,6:65]
    lib_rabc <- lib_rabc  %>%
     mutate(across(1:60, as.numeric)) 
  }

lib_rabc$label <- "unknown" 

# Calculate features -------------------------------------------------
lib_time <- calculate_feature_time(df_raw = lib_rabc, winlen_dba = 11)
head(lib_time, n = 2)

lib_freq <- calculate_feature_freq(df_raw = lib_rabc, samp_freq = samp_freq, axis_num = 3)  
head(lib_freq, n = 2)

lib_feature <- cbind(lib_time, lib_freq)
lib_feature$label <- "unknown" 

#write.csv(lib_feature,(paste (path_raw1, "features.csv", sep = "_")), row.names=FALSE, quote = FALSE)
#write.csv(lib,(paste (path_raw1, "features_lib.csv", sep = "_")), row.names=FALSE, quote = FALSE)



# Make predictions  ---------------------------------------
predictions <- predict(model, newdata = lib_feature)

lib2 <- lib
lib2$predictions <- predictions

#write.csv(Param_Orn_new,(paste (path_raw, "predict.csv", sep = "_")), row.names=FALSE, quote = FALSE)

# Optional: if you wanna limit the period to which the model is applied
lib2$date_time <- as.POSIXct(lib2$date_time, format = "%d/%m/%Y %H:%M", tz = "GMT")
#lib2$date_time <- as.POSIXct(lib2$date_time, format = "%Y-%m-%d %H:%M", tz = "GMT")

if (exists("daymin")) {
  lib2_total <- lib2
  lib2 <- lib2[lib2$date_time > daymin, ]
  lib2 <- lib2[lib2$date_time < daymax, ]
  }



# Relabel flights ------------------------------------------------

if (flying == "si") {
  
# If all the registered speeds in a GPS burst are higher than 25 km/h
# then we classified it as flying
gps2 <- gps %>%
  group_by(burst_ID, device_info_serial) %>%
  mutate(slowfast = case_when(any(speed <= 25) ~ "not flying",
                              TRUE ~ "flying"))
gps3 <- subset(gps2, index == 1)

libg <- merge(lib2, gps3, all = T ) 
libg <- dplyr::select(libg, c("device_info_serial","date_time","datatype","burst_ID","segm_ID","predictions","slowfast"))
libg$date_time <- as_datetime(libg$date_time)


# If a row is classified as flying, change the prediction to flying
libg2 <- libg %>%
  group_by(burst_ID, device_info_serial) %>%
  mutate(predictions = case_when(any(slowfast == "flying") ~ "flying",
                                 TRUE ~ predictions))

# Take a look at which behaviours were mixed with flying
p <- libg %>%
  group_by(burst_ID, device_info_serial) %>%
  filter(any(slowfast == "flying") )

# Retain only acc data
lib3 <- libg2 %>%
  subset(datatype == "SENSORS") %>%
  select(-slowfast)


} else if (flying == "no") {
  lib3 <- lib2
}


# SAVE feature table -----
if (exists("daymin")) {
  daymin_plain <- gsub("-", "", date(daymin))
  daymax_plain <- gsub("-", "", date(daymax))
  write.csv(lib3,(paste (path_raw1, cortejo, locomo, lying, daymin_plain, daymax_plain, "clasif.csv", sep = "_")), row.names=FALSE, quote = FALSE)
} else {
  write.csv(lib3,(paste (path_raw1, cortejo, locomo, lying, "clasif.csv", sep = "_")), row.names=FALSE, quote = FALSE)
}


# Exploration plots -----------------------------------------------------------

libp <- lib3

# Prepare table ---
# delete flying rows, we decided to not include them
libp <- libp %>% subset(predictions != "flying")

# day column
libp$date_time <- as_datetime(libp$date_time)

libp$date_timeESP <- with_tz(libp$date_time, "Europe/Madrid")
libp$hourESP <- hour(libp$date_timeESP)

libp$day <- date(libp$date_time)
libp$month <- month(libp$date_time)


# ATTENTION: personalise plot ---

# Select specific individuals
# if (sexo == "female") {
#   libp <- libp %>% subset(device_info_serial == c(222010, 222011))
# } else if (sexo == "male") {
#   libp <- libp %>% subset(device_info_serial == c(222000, 221997)) 
# }

# Limit initial and final day
inici <- ymd("2019-07-01")  
fin <-  ymd("2023-12-31")  
hinici <- 6
hfin <- 21

libp <- libp %>%
  subset(day >= inici & day <= fin)

libp <- libp %>%
  subset(hourESP >= hinici & hourESP <= hfin)

# Save plot title including individuals and dates
ind_toplot <- word(nombresheet2, 1, 1, sep = "_")
nombre_toplot <- paste(ind_toplot, inici, fin, sep = " ")

# Create summary tables to be ploted
# PER HOUR
# Count how many behaviours of each type are predicted per hour
lib_res <- libp %>%
  group_by(hourESP, predictions, device_info_serial) %>%
  summarise(count = n())

# % of behaviour types per hour
lib_res100 <- lib_res %>%
  group_by(hourESP, device_info_serial) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percent = round(count/total * 100, 2))

# Order and convert them to factor
lib_res$predictions <- factor(lib_res$predictions, levels = comps_ordered)
lib_res100$predictions <- factor(lib_res100$predictions, levels = comps_ordered)


# PER DAY
# Count how many behaviours of each type are predicted per day
lib_day <- libp %>%
  group_by(day, predictions, device_info_serial) %>%
  summarise(count = n())

# % of behaviour types per day
lib_day100 <- lib_day %>%
  group_by(day, device_info_serial) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percent = round(count/total * 100, 2))

# Order and convert them to factor
lib_day$predictions <- factor(lib_day$predictions, levels = comps_ordered)
lib_day100$predictions <- factor(lib_day100$predictions, levels = comps_ordered)


## Plots
# PLOT PER HOUR: behaviour counts per individual
ggplot() + 
  theme_minimal() +
  geom_bar(data = lib_res,
           aes(y=count, x=as.factor(hourESP), fill=predictions), position="stack", stat="identity") +
  facet_wrap(~ device_info_serial, scales = "free_x") +
  labs(title = nombre_toplot ,x = "Hour (CEST)", y = "Behaviour counts") +
  scale_fill_manual(values = colores, labels = comps_wellwriten, name = "")

# PLOT PER HOUR: behaviour % per individual
ggplot() + 
  theme_minimal() +
  geom_bar(data = lib_res100,
           aes(y=percent, x=as.factor(hourESP), fill=predictions), position="stack", stat="identity") +
  labs(title = nombre_toplot ,x = "Hour (CEST)", y = "%  Behaviour") +
  facet_wrap( ~ device_info_serial, scales = "free_x", ncol = 3, nrow = 2) +
  scale_fill_manual(values = colores, labels = comps_wellwriten, name = "")

# PLOT PER DAY: behaviour counts per individual
ggplot() + 
  theme_minimal() +
  geom_bar(data = lib_day,
           aes(y=count, x=as.factor(day), fill=predictions), position="stack", stat="identity") +
  labs(title = nombre_toplot ,x = "Day", y = "Behaviour counts") +
  facet_wrap( ~ device_info_serial, scales = "free_x") +
  scale_fill_manual(values = colores, labels = comps_wellwriten, name = "") +
  #scale_x_date(breaks = "1 month", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

# PLOT PER DAY: behaviour % per individual
ggplot() + 
  theme_minimal() +
  geom_bar(data = lib_day100,
           aes(y = percent, x = day, fill = predictions), position = "stack", stat = "identity") +
  labs(title = nombre_toplot, x = "Month", y = "% Behaviour") +
  scale_fill_manual(values = colores, labels = comps_wellwriten, name = "") +
  facet_wrap( ~ device_info_serial, scales = "free_x") +
  scale_x_date(breaks = "1 month", date_labels = "%b") +  # Show abbreviated month names on x-axis
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))


# SAVE features table with time limits -----
libp$sex <- ind_toplot
libp$limits <- nombre_toplot
# write.csv(libp,(paste (nombresheet2, "features_toplot.csv", sep = "_")), row.names=FALSE, quote = FALSE)


# Incubation -------------------------------------------------------------------

# Select acc data from days that we believe females were incubating 
# according to distance to the nest

# I wanna check whether during those days they were incubating for 
# longer time than the days that supposedly they were not incubating
incub <- read.csv("Fenologia_TT_2020-2024.csv", sep =";")

# Convert date columns in 'incub' to Date and expand date range in one step
expanded_incub <- incub %>%
  select(c("individu", "fecha_puesta_min", "fecha_eclosion_min", "Any")) %>%
  mutate(fecha_puesta_min = dmy(fecha_puesta_min),
         fecha_eclosion_min = dmy(fecha_eclosion_min)) %>%
  rowwise() %>%
  transmute(individu,  # Keep individual identifier
            all_dates = list(seq(fecha_puesta_min, fecha_eclosion_min, by = "day"))) %>%
  unnest(cols = c(all_dates))  # Unnest the list of dates directly

# Ensure 'date_time' in 'libp' is in Date format and filter based on the expanded date range
lib_incubdates <- libp %>%
  mutate(date_time = as.Date(date_time)) %>%
  inner_join(expanded_incub, by = c("device_info_serial" = "individu", "date_time" = "all_dates"))

# Table from non-incubating days in spring
levels(as.factor(lib_incubdates$month)) # months were incubatino ocurred
lib_noninc <- anti_join(libp, lib_incubdates)
lib_noninc <- lib_noninc %>% subset(month %in% c(5,6,7))

# Unir las tablas
lib_incubdates$incubation <- "Incubating"
lib_noninc$incubation <- "Not incubating"
lib_incub <- rbind(lib_incubdates, lib_noninc)

# Subset hora
lib_incub <- subset(lib_incub, hourESP >= 10)
lib_incub <- subset(lib_incub, hourESP <= 16)

# Summary incubation per day
# counts
lib_incub_day <- lib_incub %>%
  group_by(day, predictions, device_info_serial, incubation) %>%
  summarise(count = n())

# %
lib_incub_day100 <- lib_incub_day %>%
  group_by(day, device_info_serial, incubation) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percent = round(count/total * 100, 2))



# Figure S1 - time-series laying ----------
lib_incub_lyi <- lib_incub_day100 %>% subset(predictions == "lying")

# Variable combining ID and year, and filter those ID/year than include both incubation and no incubation
lib_incub_lyi <- lib_incub_lyi %>%
  mutate(year = format(day, "%Y"),  # Extract year from 'day'
         facet_var = paste(device_info_serial, year, sep = " - ")) %>% # Combine device_info_serial and year
  group_by(facet_var) %>%                                  # Group by facet_var
  filter(all(c("Incubating", "Not incubating") %in% incubation)) %>%          # Keep only those with both "yes" and "no"
  ungroup()  

coloressino <- c("#DEE318FF", "#1FA188FF")
title_incub <- "Incubating period"

Sys.setlocale("LC_TIME", "English_United States.1252")
g.incub <- ggplot() + 
  theme_minimal() +
  geom_bar(data = lib_incub_lyi,
           aes(y = percent, x = day, fill = incubation), position = "stack", stat = "identity") +
  labs(title = title_incub, x = "", y = "% Lying") +
  scale_fill_manual(values = coloressino, name = "") +
  facet_wrap(~facet_var, scales = "free_x", ncol = 2, nrow = 3) +  # Facet by combined variable
  scale_x_date(breaks = "1 month", date_labels = "%b %Y") +  # Show abbreviated month names and year
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    strip.text = element_text(size = 10)  # Adjust facet label size for readability
  )
g.incub
Sys.setlocale("LC_TIME", "Spanish_Spain.1252") 

# SAVE plot
folder <- "results/4ind/timebudget/"
file <- paste("Figure S1", ".png", sep = "")
ggsave(paste(folder, file, sep = ""), width = 7, height = 8, dpi=300) 


# Summary table
table_sum_incub <- lib_incub_lyi %>%
  group_by(incubation) %>%
  summarise(mean_percent = mean(percent, na.rm = TRUE), sd_percent = sd(percent, na.rm = TRUE), 
            min_percent = min(percent, na.rm = TRUE), max_percent = max(percent, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 3); table_sum_incub 


# ANOVA
aov_res <- aov(percent ~ incubation, lib_incub_lyi)
summary(aov_res)
TukeyHSD(aov_res)

# GLMM binomial
lib_incub_lyi$perone <- lib_incub_lyi$percent / 100
library(glmmTMB)
p <- glmmTMB(perone ~ incubation, 
             family = binomial, 
             weights = lib_incub_lyi$total,
             data = lib_incub_lyi, )
summary(p)

gg.inc <- ggplot(lib_incub_lyi, aes(x = incubation, y = percent, fill = incubation)) +
  geom_violin(width = 0.6, size = 0.3) +    
  geom_boxplot(width = 0.1, size = 0.3)
gg.inc




