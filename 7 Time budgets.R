
#-------------------------------------------------------------------------#
# Time budget                                                             #
# authors: Natalia Revilla Martín and Carolina Bravo                      #
# 17.04.2024                                                              #
#                                                                         #
# R version 4.2                                                           #
#-------------------------------------------------------------------------#

# Libraries ---------------------------------------------------------
library(ggplot2)
library(ggeffects)
library(gratia)
library(mgcv)
library(DHARMa)
library(gamm4)
library(dplyr)
library(lubridate)
library(gridExtra)

# Open data ----------------------------------------------------------------

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

# Breeding period
if (sexo == "female") {
  BR <- "Br4567"
} else if (sexo == "male"){
  BR <- "Br456"
}

# Open model 
if (sexo == "female") {
  model <- readRDS("results/4ind/model_rd_nob_female_10Hz_COU_oneLOC_LYI.RDS")
} else if (sexo == "male"){
  model <- readRDS("results/4ind/model_rd_nob_male_10Hz_ALE_COU.RDS")
}



# Colours -------------------------------------------------------------------
comps <- levels(model$predicted); comps

if (sexo == "male") {
  if (cortejo == "COU") {
    comps_ordered <- c("resting", "alert", "locomotion_slow", "locomotion_fast", "foraging", "courtship") 
    comps_wellwriten <- c("Resting", "Vigilant","Slow locomotion", "Fast locomotion", "Foraging", "Courtship" ) 
    colores <- c("#DEE318FF", "#62CB5FFF", "#1FA188FF", "#2C738EFF", "#424086FF", "#440154FF")
  } else if (cortejo == "noCOU") {
    comps_ordered <- c("resting", "alert", "locomotion_slow", "locomotion_fast", "foraging") 
    comps_wellwriten <- c("Resting", "Vigilant","Slow locomotion", "Fast locomotion", "Foraging")
    colores <- c("#DEE318FF", "#62CB5FFF", "#1FA188FF", "#2C738EFF", "#424086FF")
  }
} else if (sexo == "female" & locomo == "oneLOC" & lying == "noLYI") {
  if (cortejo == "COU") {
    comps_ordered <- c("resting", "locomotion", "foraging", "courtship") 
    comps_wellwriten <- c("Resting", "Locomotion", "Foraging", "Interaction" ) 
    colores <- c("#DEE318FF", "#1FA188FF", "#424086FF", "#440154FF")
  } else if (cortejo == "noCOU") {
    comps_ordered <- c("resting", "locomotion", "foraging") 
    comps_wellwriten <- c("Resting", "Locomotion", "Foraging")
    colores <- c("#DEE318FF", "#1FA188FF", "#424086FF")
  }
} else if (sexo == "female" & locomo == "oneLOC" & lying == "LYI") {
  if (cortejo == "COU") {
    comps_ordered <- c("resting", "lying","locomotion", "foraging", "courtship") 
    comps_wellwriten <- c("Standing", "Lying","Locomotion", "Foraging", "Interaction") 
    colores <- c("#DEE318FF",  "#AADC32FF", "#1FA188FF", "#424086FF", "#440154FF")
  } else if (cortejo == "noCOU") {
    comps_ordered <- c("resting", "lying", "locomotion", "foraging") 
    comps_wellwriten <- c("Standing", "Lying","Locomotion", "Foraging")
    colores <- c("#DEE318FF", "#AADC32FF","#1FA188FF", "#424086FF")
  }
}


# Make sure comps and comps_ordered have the same levels. This should be TRUE
all(comps %in% comps_ordered)


# Open classified acc data from free-ranging individuals ---
path_folder <- "C:/Users/natalia.revilla/OneDrive - ctfc.cat/3. Proyectos/2023 Acelerómetros/Experimento acelerometro 2023/3. Analisis/data/individuos libertad 2023"

if (sexo == "male") {
  path_raw3 <- paste (path_folder, "MALES_2023_2seg_10Hz_ACC_allCOU_clasif", sep = "/")
} 
if (sexo == "female") {
path_raw3 <- paste (path_folder, "FEMALES_2019_2023_2seg_10Hz_ACC_COU_oneLOC_LYI_clasif", sep = "/")
}

lib3 <- read.csv2(paste (path_raw3, "csv", sep = "."), sep=",", header=TRUE)

# Exclude OCTOBER from ANOVA, because it is a transition month
exclude <- "si" # options "si" "no"

libp <- lib3

libp$month <- month(libp$date_time)
libp <- libp %>%
    mutate(period = case_when(month %in% c(4,5,6,7) ~ "Breeding",
                              month %in% c(8,9) ~ "Post-breeding",
                              TRUE ~ "Wintering"))

# Males: july in postbreeding
if (sexo == "male") {
  libp <- libp %>% mutate(period = ifelse(month == 7, "Post-breeding", period))
}

# Exclude october
if (exclude == "si") {
  libp <- libp %>% mutate(period = ifelse(month == 10, "Excluded", period))
  #libp <- libp %>% filter(period != "Excluded")
}

libp$period <- as.factor(libp$period)

# Clean data ------------------------------
# Filter months/ind with too few data 
# Criteria from Masello 2023:
# We excluded the data of a particular individual during a specific month if 
# (1) the number of occurrences of one of the behaviours was less than five or 
# (2) the sum of all behaviours was < 500 

# (1) 
# How many behaviours of each type per month
lib_month1 <- libp %>%
  group_by(month, period, predictions, device_info_serial) %>%
  summarise(count = n())

# Which month:comp:ind do not match the criteria (1)
few_behav1 <- lib_month1 %>% filter(count < 5)

lib_month1 <- lib_month1 %>%
  anti_join(few_behav1, by = c("month", "device_info_serial"))


# (2)
lib_month2 <- libp %>%
  group_by(month, device_info_serial) %>%
  summarise(count = n())

# Which month:ind do not match the criteria  (2)
few_behav2 <- lib_month2 %>% filter(count < 500)

lib_month3 <- lib_month1 %>%
  anti_join(few_behav2, by = c("month", "device_info_serial"))


# % of each behaviour per month
lib_month100 <- lib_month3 %>%
  group_by(month, device_info_serial) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percent = round(count/total * 100, 2))


# Explore sample size  ---------------------------
lib_month100$predictions <- factor(lib_month100$predictions, levels = comps_ordered)
ggplot() + 
  theme_minimal() +
  geom_bar(data = lib_month100,
           aes(y=percent, x=as.factor(month), fill=predictions), position="stack", stat="identity") +
  labs(title = "nombre_toplot" ,x = "Month", y = "Behaviour counts") +
  #facet_wrap( ~ device_info_serial, scales = "free_x") +
  scale_fill_manual(values = colores, labels = comps_wellwriten, name = "") +
  #scale_x_date(breaks = "1 week", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

ggplot() + 
  theme_minimal() +
  geom_bar(data = lib_month100,
           aes(y=count, x=period, fill=predictions), position="stack", stat="identity") +
  labs(title = "nombre_toplot" ,x = "Month", y = "Behaviour counts") +
  #facet_wrap( ~ device_info_serial, scales = "free_x") +
  scale_fill_manual(values = colores, labels = comps_wellwriten, name = "") +
  #scale_x_date(breaks = "1 week", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))


# Table S4 - sample size of free individuals per month ------------
# Number of individuals per month + number of points per month
table_ind_mes <- as.data.frame.matrix(table(lib_month100$predictions, lib_month100$month))

sample_size_mes <- lib_month100 %>%
  group_by(month) %>%
  summarise(sample_size = sum(count), .groups = "drop") %>%
  { setNames(.$sample_size, .$month) } 

table_ind_mes2 <- rbind(table_ind_mes, points = sample_size_mes)

tosave8 = table_ind_mes2
nombresheet8 <- paste(sexo, "allCOU", sep = "_")

write.xlsx(as.data.frame(tosave8), file = "results/4ind/tableS3_sample_size_free.xlsx",
           sheetName = nombresheet8, append = TRUE,
           col.names=TRUE, row.names=FALSE )



# Comparison with Morales display: % display during peak hours   -----------------------------------
libp$hourUTC <- hour(libp$date_time)

libp$date_timeESP <- with_tz(libp$date_time, "Europe/Madrid")
libp$hourESP <- hour(libp$date_timeESP)

lib_hormes <- libp %>%
  group_by(hourESP, predictions, month) %>%
  summarise(count = n())

lib_hormes100 <- lib_hormes %>%
  group_by(hourESP, month) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percent = round(count/total * 100, 2))

# Compare with results from Morales 
lib_hormes_sub <- lib_hormes100 %>%     
  filter(month %in% c(4, 5)) %>%
  filter(hourESP %in% c(6,7,8,9,18,19,20,21)) %>%
  filter(predictions == "courtship")

mean(lib_hormes_sub$percent)



# GAM ---------------------------------------------------------------
lib_month100 <- as.data.frame(lib_month100)

lib_month100$perone <- lib_month100$percent / 100
lib_month100$device_info_serial <- as.factor(lib_month100$device_info_serial)
lib_month100$predictions <- as.factor(lib_month100$predictions)

# Cyclic variable
lib_month100$month <- as.numeric(lib_month100$month)
attr(lib_month100$month, "knots") <- list(month = c(1, 12))

M2 <- gamm4(perone ~ s(month, by = predictions, bs = "cc") + predictions,
            family = binomial,
            weights = lib_month100$total,
            random = ~ (1 | device_info_serial),
            data = lib_month100)


# plot effects
par(mfrow = c(3,2), mar = c(5,5,2,2))
plot(M2$gam, scale = FALSE, cex.lab = 1.5)
gratia::draw(M2$gam)

# validate mdoel
gam.check(M2$gam)
gratia::appraise(M2$gam)
#simM2 <- simulateResiduals(fittedModel = M2$mer, n = 10000, plot = TRUE)

# plot predictions
respuesta <- predict_response(M2$gam, terms = c("month", "predictions"))
plot(respuesta, show_ci = TRUE)


# Figure 2 - time budget per month ----------------------------------------------------
ds <- data_slice(M2$gam, month = evenly(month, n = 100), predictions = evenly(predictions))
fv <- fitted_values(M2$gam, data = ds)


g.ann <- ggplot(fv, aes(x = month, y = .fitted, colour = predictions)) +
  geom_line(size = 0.7) +
  geom_ribbon(aes(x = month, ymin = .lower_ci, ymax = .upper_ci, fill = predictions),
              inherit.aes = FALSE, alpha = 0.2, show.legend = FALSE) +
  labs(#title = "Estimated Smoothing Curve of Month with 95% Confidence Interval",
       x = "", #Month
       y = "% of time") + 
  scale_color_manual(
    values = colores,  # Set line colors from 'colores'
    name = "Behaviours",  # Set the legend title
    #labels = c("Resting", "Vigilant", "Locomotion slow","Locomotion fast", "Foraging", "Courtship")  # Males
    labels = c("Standing", "Lying", "Locomotion", "Foraging", "Interaction")  # Females
  ) +
  scale_fill_manual(
    values = colores  # Match ribbon fill colors to 'colores'
  ) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) + 
  scale_y_continuous(limits = c(0, 1)) + 
  theme_minimal()
g.ann

# Save male and female separately
if (sexo == "female") {
  g.ann.fem <- g.ann
} else if (sexo == "male") {
  g.ann.mas <- g.ann
}

g.ann.mas <- g.ann.mas + labs(title = "Males")
g.ann.fem <- g.ann.fem + labs(title = "Females")

# Plot female and male toguether
# ggplots to gtables
g1 <- ggplotGrob(g.ann.fem)
g2 <- ggplotGrob(g.ann.mas)

# Align the widths of the plotting area (panels) using gtable
g1$widths <- g2$widths

# Arrange the plots vertically with separate legends but aligned panels
g3 <- grid.arrange(g1, g2, nrow = 2)


# SAVE FIgure 2
ggsave("results/4ind/timebudget/Figure 2.png", plot = g3, width = 7, height = 8, dpi=300)


# SAVE Table S9 and S10 - COEF TABLE 
gam.sum <- summary(M2$gam)

tosave.s <- gam.sum$s.table
nombresheet.s <- paste("GAMM", "smooth", sexo, "allCOU", lying, sep = "_")
tosave.p <- gam.sum$p.table
nombresheet.p <- paste("GAMM", "param", sexo, "allCOU", lying, sep = "_")

write.xlsx(as.data.frame(tosave.s), file = "results/4ind/tableS8_smooth.xlsx",
           sheetName = nombresheet.s, append = TRUE,
           col.names=TRUE, row.names=TRUE )

write.xlsx(as.data.frame(tosave.p), file = "results/4ind/tableS8_smooth.xlsx",
           sheetName = nombresheet.p, append = TRUE,
           col.names=TRUE, row.names=TRUE )


# ANOVA -------------------------------------------

# Raw data. Behaviour frequency. To mention these means in the text
# Table mean, sd per behaviour
table_sum_comp <- lib_month100 %>%
  group_by(predictions) %>%
  summarise(mean_percent = mean(percent, na.rm = TRUE), sd_percent = sd(percent, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 3) # round
colnames(table_sum_comp) <- c("Period", "Behaviour", "Mean", "Sd")

# Table mean, sd per behaviour and period
table_sum_per <- lib_month100 %>%
  group_by(period, predictions) %>%
  summarise(mean_percent = mean(percent, na.rm = TRUE), sd_percent = sd(percent, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 3) # round
colnames(table_sum_per) <- c("Period", "Behaviour", "Mean", "Sd")

table_sum_per$Behaviour <- factor(table_sum_per$Behaviour, levels = comps_ordered,
                                  labels = comps_wellwriten)

# SAVE table mean sd
tosave5 = table_sum_per
nombresheet5 <- paste(sexo, "allCOU", BR, "Exl10", sep = "_")

write.xlsx(as.data.frame(tosave5), file = "results/4ind/tableSX_periods.xlsx",
           sheetName = nombresheet5, append = TRUE,
           col.names=TRUE, row.names=FALSE )


# Prepare data for ANOVA
lib_month100_exc <- lib_month100 %>% filter(period != "Excluded")

lib_month_res <- lib_month100_exc %>% filter(predictions == "resting")
lib_month_ale <- lib_month100_exc %>% filter(predictions == "alert")
lib_month_slo <- lib_month100_exc %>% filter(predictions == "locomotion_slow")
lib_month_fas <- lib_month100_exc %>% filter(predictions == "locomotion_fast")
lib_month_for <- lib_month100_exc %>% filter(predictions == "foraging")
lib_month_cou <- lib_month100_exc %>% filter(predictions == "courtship")
lib_month_loc <- lib_month100_exc %>% filter(predictions == "locomotion")
lib_month_lyi <- lib_month100_exc %>% filter(predictions == "lying")

# ANOVA
aov_res <- aov(percent ~ period, lib_month_res)
summary(aov_res)
TukeyHSD(aov_res)

aov_ale <- aov(percent ~ period, lib_month_ale)
summary(aov_ale)
TukeyHSD(aov_ale)

aov_fas <- aov(percent ~ period, lib_month_fas)
summary(aov_fas)
TukeyHSD(aov_fas)

aov_slo <- aov(percent ~ period, lib_month_slo)
summary(aov_slo)
TukeyHSD(aov_slo)

aov_for <- aov(percent ~ period, lib_month_for)
summary(aov_for)
TukeyHSD(aov_for)

aov_cou <- aov(percent ~ period, lib_month_cou)
summary(aov_cou)
TukeyHSD(aov_cou)

aov_loc <- aov(percent ~ period, lib_month_loc)
summary(aov_loc)
TukeyHSD(aov_loc)

aov_lyi <- aov(percent ~ period, lib_month_lyi)
summary(aov_lyi)
TukeyHSD(aov_lyi)

# Table S5 and S6 - coeficients ANOVA -----
a_res <- as.data.frame(summary(aov_cou)[[1]])
a_res$behav <- "resting"
a_lyi <- as.data.frame(summary(aov_lyi)[[1]])
a_lyi$behav <- "lying"
a_ale <- as.data.frame(summary(aov_ale)[[1]])
a_ale$behav <- "alert"
a_loc <- as.data.frame(summary(aov_loc)[[1]])
a_loc$behav <- "locomotion"
a_fas <- as.data.frame(summary(aov_fas)[[1]])
a_fas$behav <- "locomotion_fast"
a_slo <- as.data.frame(summary(aov_slo)[[1]])
a_slo$behav <- "locomotion_slow"
a_for <- as.data.frame(summary(aov_for)[[1]])
a_for$behav <- "foraging"
a_cou <- as.data.frame(summary(aov_cou)[[1]])
a_cou$behav <- "courtship"

coef_anova$behav <- factor(coef_anova$behav, levels = comps_ordered,
                                        labels = comps_wellwriten)

# female
coef_anova <- rbind(a_res, a_lyi, a_loc, a_for, a_cou) 
coef_anova$behav <- factor(coef_anova$behav, levels = comps_ordered,
                           labels = comps_wellwriten)
coef_anova <- coef_anova %>%
  mutate_if(is.numeric, round, 3) %>% # round
  select(behav, everything()) %>%  # Move 'behav' to the first position
  rename(Behavior = behav)

# male
coef_anova <- rbind(a_res, a_ale, a_slo, a_fas, a_for, a_cou) 
coef_anova$behav <- factor(coef_anova$behav, levels = comps_ordered,
                           labels = comps_wellwriten)
coef_anova <- coef_anova %>%
  mutate_if(is.numeric, round, 3) %>% # round
  select(behav, everything()) %>%  # Move 'behav' to the first position
  rename(Behavior = behav)

# SAVE S5 and S6 ANOVA table
tosave4 = coef_anova

nombresheet <- paste(sexo, "allCOU", BR, "Exl10", sep = "_")

write.xlsx(as.data.frame(tosave4), file = "results/4ind/tableS4_anova.xlsx",
           sheetName = nombresheet, append = TRUE,
           col.names=TRUE, row.names=TRUE )


# Table S7 and S8 - coeficients tukey -----------------------------------
c_res <- as.data.frame(TukeyHSD(aov_res)$period)
c_res$behav <- "resting"
c_lyi <- as.data.frame(TukeyHSD(aov_lyi)$period)
c_lyi$behav <- "lying"
c_ale <- as.data.frame(TukeyHSD(aov_ale)$period)
c_ale$behav <- "alert"
c_loc <- as.data.frame(TukeyHSD(aov_loc)$period)
c_loc$behav <- "locomotion"
c_fas <- as.data.frame(TukeyHSD(aov_fas)$period)
c_fas$behav <- "locomotion_fast"
c_slo <- as.data.frame(TukeyHSD(aov_slo)$period)
c_slo$behav <- "locomotion_slow"
c_for <- as.data.frame(TukeyHSD(aov_for)$period)
c_for$behav <- "foraging"
c_cou <- as.data.frame(TukeyHSD(aov_cou)$period)
c_cou$behav <- "courtship"

#female
coef_tukey <- rbind(c_res, c_lyi, c_loc, c_for, c_cou) 
coef_tukey$behav <- factor(coef_tukey$behav, levels = comps_ordered,
                           labels = comps_wellwriten)
coef_tukey <- coef_tukey %>%
  mutate_if(is.numeric, round, 3) %>% # round
  select(behav, everything()) %>%  # Move 'behav' to the first position
  rename(Behavior = behav)

#male
coef_tukey <- rbind(c_res, c_ale, c_slo, c_fas, c_for, c_cou) 
coef_tukey$behav <- factor(coef_tukey$behav, levels = comps_ordered,
                           labels = comps_wellwriten)
coef_tukey <- coef_tukey %>%
  mutate_if(is.numeric, round, 3) %>% # round
  select(behav, everything()) %>%  # Move 'behav' to the first position
  rename(Behavior = behav)

# SAVE table S7 and S8 - Tukey table
tosave4 = coef_tukey

nombresheet <- paste(sexo, "allCOU", BR, "Exl10", sep = "_")

write.xlsx(as.data.frame(tosave4), file = "results/4ind/tableS6_tukey.xlsx",
           sheetName = nombresheet, append = TRUE,
           col.names=TRUE, row.names=TRUE )


# Figure 1 - time budget per periods ----------
lib_month100_plot <- lib_month100_exc
lib_month100_plot$predictions <- factor(lib_month100_plot$predictions, 
                                        levels = comps_ordered,
                                        labels = comps_wellwriten)
# do the same for both sexes
write.csv(lib_month100_plot, "results/4ind/timebudget/TOPLOT_anova_table_female.csv")

lib_month100_plot$sex <- NULL

# Join male and female data
p.box.mal <- read.csv("results/4ind/timebudget/TOPLOT_anova_table_male.csv")
p.box.mal$sex <- "Male"
comps_wellwriten_male <- c("Resting", "Vigilant","Slow locomotion", "Fast locomotion", "Foraging", "Courtship")
p.box.mal$predictions <- factor(p.box.mal$predictions, 
                                 levels = comps_wellwriten_male,
                                 labels = comps_wellwriten_male)

p.box.fem <- read.csv("results/4ind/timebudget/TOPLOT_anova_table_female.csv")
p.box.fem$sex <- "Female"
comps_wellwriten_female <- c("Standing", "Lying","Locomotion", "Foraging", "Interaction")
p.box.fem$predictions <- factor(p.box.fem$predictions, 
                                levels = comps_wellwriten_female,
                                labels = comps_wellwriten_female)

# Summarize Slow and Fast Locomotion counts into a new "Locomotion" category
locomotion_data <- p.box.mal %>%
  filter(predictions %in% c("Slow locomotion", "Fast locomotion")) %>%
  group_by(month, device_info_serial, period, total, sex) %>%  # Keep common variables for new rows
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%      # Sum counts
  mutate(predictions = "Locomotion") %>% # Assign the new prediction category
  mutate(percent = round((count / total) * 100, 2), 
         X = seq(from = max(p.box.mal$X) + 1, length.out = n(), by = 1))  # add X column
  
# Combine the new locomotion rows with the original dataset
p.box.mal2 <- bind_rows(p.box.mal, locomotion_data)


# Check range of values
range_by_group <- p.box.fem %>%
  group_by(predictions) %>%               # Group by 'predictions'
  summarise(
    min_percent = min(percent, na.rm = TRUE),   # Minimum of 'percent'
    max_percent = max(percent, na.rm = TRUE),   # Maximum of 'percent'
    range_percent = max_percent - min_percent   # Range of 'percent'
  ) ; range_by_group

# Estabish range of values
p.box.mal2 <- p.box.mal2 %>%
  mutate(y_limit = case_when(
    predictions == "Courtship" ~ 5,
    predictions == "Foraging" ~ 50,
    predictions == "Fast locomotion" ~ 30,
    predictions == "Slow locomotion" ~ 30,
    predictions == "Locomotion" ~ 30,
    predictions == "Vigilant" ~ 50,
    predictions == "Resting" ~ 80,
    TRUE ~ NA_real_  ))  # If no specific limit, keep default

p.box.fem <- p.box.fem %>%
  mutate(y_limit = case_when(
    predictions == "Interaction" ~ 5,
    predictions == "Foraging" ~ 50,
    predictions == "Locomotion" ~ 30,
    predictions == "Standing" ~ 80,
    predictions == "Lying" ~ 50,
    TRUE ~ NA_real_   ))  # If no specific limit, keep default


# Change female behavior names just to represent them with more space
p.box.fem <- p.box.fem %>%
  mutate(predictions_unames = case_when(
    predictions == "Interaction" ~ "Courtship",
    predictions == "Foraging" ~ "Foraging",
    predictions == "Locomotion" ~ "Locomotion",
    predictions == "Standing" ~ "Resting",
    predictions == "Lying" ~ "Vigilant",
    TRUE ~ NA_character_  # If no specific limit, keep default
  ))

p.box.mal2$predictions_unames <- p.box.mal2$prediction

# Join the two tables
p.box.dos <- rbind(p.box.mal2, p.box.fem)

p.box.dos$sex <- factor(p.box.dos$sex, levels = c("Male", "Female"))

# Delete fast and slow locomotion levels, to plot
p.box.dos <- p.box.dos %>%
  subset(!(predictions %in% c("Slow locomotion", "Fast locomotion")))


# trial: only one sex
g2 <- ggplot(p.box.fem, aes(x = period, y = percent, fill = period)) +
  geom_violin(width = 0.6, size = 0.3) +    
  geom_boxplot(width = 0.1, size = 0.3) +
  theme_classic() + 
  facet_wrap( ~ predictions, ncol=1, scales = "free") +
  geom_blank(aes(y = y_limit))
g2


# both sexes
g4 <- ggplot(p.box.dos, aes(x = period, y = percent, fill = period)) +
  geom_violin(width = 0.5, size = 0.3) +    
  geom_boxplot(width = 0.1, size = 0.3) +
  labs(title = "", x = "", y = " % time") +
  theme_classic() +                               
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10),
    strip.text = element_text(size = 9),
    strip.background = element_blank(),
    axis.line = element_line(size = 0.3),
    panel.border = element_rect(color = "black", linewidth = 0.3, fill = NA)) +
  facet_grid(predictions_unames ~ sex, scales = "free") +
  geom_blank(aes(y = y_limit)) +
  scale_fill_manual(values = c("Breeding" = "#00BA38",       # Green
                               "Post-breeding" = "#F8766D",   # Red
                               "Wintering" = "#619CFF"))          # Blue
g4

# Add individual titles
g5 <- g4 +
  geom_text(aes(x = 0.5, y = max(percent, na.rm = TRUE), label = predictions),
            hjust = 0, vjust = 1.8, size = 3, inherit.aes = FALSE)
g5


# SAVE Figure 1
folder <- "results/4ind/timebudget/"
file <- paste("Figure 1_v2_names", ".png", sep = "")
ggsave(paste(folder, file, sep = ""), width = 6, height = 7, dpi=300) 

