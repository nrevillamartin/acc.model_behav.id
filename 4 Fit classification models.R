
#-------------------------------------------------------------------------#
# Fit classification models                                               #
# authors: Natalia Revilla Martín and Carolina Bravo                      #
# 08.10.2024                                                              #
# run in R version 4.2.1                                                  #
#-------------------------------------------------------------------------#


library(rabc) # browseVignettes('rabc')
library(dplyr)
library(caret) 
library(lubridate)

library(tidyr)
library(purrr)
library(stringr)
library(xlsx)

## 1. Oepn acc table and define behaviours -------------------------------
path_folder <- "C:/Users/natalia.revilla/OneDrive - ctfc.cat/3. Proyectos/2023 Acelerómetros/Experimento acelerometro 2023/3. Analisis/data/datos etiquetados"

path_raw <- paste (path_folder, "TETRAX_FR_AL_ALERTA_2seg", sep = "/")

acc_raw <- read.csv2(paste (path_raw, "csv", sep = "."), sep=",", header=TRUE) # el segundo sep quizás es ; 
acc <- acc_raw


## ATTENTION: personalise ---- ---- --- 
# Sampling frequency (Hz)
samp_freq = 20

# Split method
modelo = "random"     # options are:  "random"  "LOIO"   "random+LOIO"

# ONe sex or both sexes
sexo = "femmal"         # options are: "femmal", "female", "male"

# With or without courtship
cortejo = "COU"       # opcions are: "COU" "noCOU"

# With or without alerta
alerta = "ALE"  # options are: "ALE" o "noALE"
 
## ---- ---- --- 


# Execute personalised options
# Delete OTHER
acc <- subset(acc, comp_cod != "OTHER")

# Sex
if (sexo == "male") {
  acc <- subset(acc, sex == 0)
} else if (sexo == "female"){
  acc <- subset(acc, sex == 1)
}

# Courtship
if (cortejo == "noCOU") {
  acc <- subset(acc, comp_cod != "courtship")
}

# Alert
if (alerta == "noALE") {
  acc$comp_cod[acc$comp_cod == "alert"] <- "resting"
}

# Females: join slow and fast locomotion
acc <- acc %>%
  mutate(comp_cod = case_when(
    (sex == 1 & (comp_cod == "locomotion_slow" | comp_cod == "locomotion_fast")) ~ "locomotion",
    TRUE ~ comp_cod
  ))

# Females: separate lying and standing
acc <- acc %>%
  mutate(comp_cod = case_when(
    (sex == 1 & maj_comp == "tumbado") ~ "lying",
    TRUE ~ comp_cod
  ))


# Check if behavious are what was expected
c <- acc %>%
  group_by(sex, maj_comp) %>%
  summarise(count=n())

# write.xlsx(as.data.frame(acc), file = "results/4ind/tableXXX.xlsx",
#            sheetName = nombresheet2, append = TRUE,
#            col.names=TRUE, row.names=FALSE )



## 2. Clean data table  ----------------------------------------------

# Delete rows including NA
acc <- acc[complete.cases(acc),]
levels(as.factor(acc$comp_cod))

# Delete rows (windows) those acc data is the same for all data of one of the axis
# If this happens, the function calculate_feature_freq gives an error
xs <- acc %>%
  select(starts_with('x'))

matsub_x <- as.matrix(xs)
freq_index <- samp_freq/length(xs)
freq_x <- apply(matsub_x, 1, max_freq_amp)
positions_longer_than_3_x <- which(sapply(freq_x, function(x) length(x) > 3))

if (length(positions_longer_than_3_x != 0)) {
acc <- acc[-positions_longer_than_3_x, ]
}

zs <- acc %>%
  select(starts_with('z'))
matsub_z <- as.matrix(zs)
freq_index <- samp_freq/length(zs)

freq_z <- apply(matsub_z, 1, max_freq_amp)
positions_longer_than_3_z <- which(sapply(freq_z, function(x) length(x) > 3))

if (length(positions_longer_than_3_z != 0)) {
acc <- acc[-positions_longer_than_3_z, ]
}



## 3. Order data for rabc --------------------------------------------------

# Sort data by behaviour
acc_sorted <- dplyr::arrange(acc, comp_cod)

# comp_cod to last column
acc_sorted <- acc_sorted %>% select(-comp_cod,comp_cod)

comp_cod <- acc_sorted$comp_cod
sex <- acc_sorted$sex
ind <- acc_sorted$ind

# Select variables for rabc
start_col_index <- which(names(acc_sorted) == 'x1') # find the index of column 'x1'
acc_rabc <- acc_sorted[, start_col_index:length(acc_sorted)] # select columns from 'x1' to the last column

# Convert to numeric. Last column as character
acc_rabc <- acc_rabc  %>%
  mutate(across(1:length(acc_rabc)-1, as.numeric))



## 4. Calculate features -------------------------------------------------------
df_time <- calculate_feature_time(df_raw = acc_rabc, winlen_dba = 11)
head(df_time, n = 2)

df_freq <- calculate_feature_freq(df_raw = acc_rabc, samp_freq = samp_freq, axis_num = 3)  
head(df_freq, n = 2)

df_feature <- cbind(df_time, df_freq)


## 5. Data frame including one sex or both sexes ----------------------------

# females + males
df_femmal_IDs <- cbind(df_feature, comp_cod, ind, sex)
# only females
df_female_IDs <- df_femmal_IDs %>% subset(sex == 1)
# only males
df_male_IDs <- df_femmal_IDs %>% subset(sex == 0)


if (sexo == "femmal") {
  df_list <- list(df_femmal_IDs, df_female_IDs, df_male_IDs)
  names(df_list) <- c("femmal", "female", "male")
} else if (sexo == "female") {
  df_list <- list(df_female_IDs)
  names(df_list) <- c("female")
} else if (sexo == "male") {
  df_list <- list(df_male_IDs)
  names(df_list) <- c("male")
}



## 6. Fit classification models (random forest loop) ------------------------

# ---- Random split -------------------------
if (grepl("random", modelo)) {

# ---- 1. Balancing levels ----
i = 1 # set i to 1

for (j in df_list) {  
  
  ## Train and test data frames
  train_indices <- createDataPartition(j$comp_cod, p = 0.7, list = FALSE)
  
  Train_p <- j[train_indices,]
  Train <- Train_p %>%
    select(-c(#fold, 
              ind, sex))
  
  Test_p <- j[-train_indices,]
  Test <- Test_p %>%
    select(-c(#fold, 
              ind, sex))
  
  
  ## Balancing data
  # table(Train$comp_cod)
  
  # A) Undersampling 
  set.seed(123)
  traindown <- downSample(x = Train[,-ncol(Train)],
                          y = as.factor(Train$comp_cod))
  colnames(traindown)[which(names(traindown) == "Class")] <- "comp_cod"
  
  # B) Upsample 
  trainup <- upSample(x = Train[,-ncol(Train)],
                      y = as.factor(Train$comp_cod))
  colnames(trainup)[which(names(trainup) == "Class")] <- "comp_cod"
  
  # C) Upsample to 50% of the frequency of the most common behaviour
  train50_1 <- Train %>%
    filter(comp_cod == "resting") # most common behaviour
  train50_1 <- train50_1 %>%
    sample_n(nrow(train50_1)/2)
  
  train50_2 <- Train %>%
    filter(comp_cod != "resting")
  
  train50_3 <- rbind(train50_1, train50_2)
  
  train50 <- upSample(x = train50_3[,-ncol(train50_3)],
                      y = as.factor(train50_3$comp_cod))
  
  colnames(train50)[which(names(train50) == "Class")] <- "comp_cod"
  
  
  # Save group name to save dat frames with a unique name
  if (sexo == "femmal") {
    if (i == 1) {
      group_name <- "femmal"
    } else if (i == 2) {
      group_name <- "female"
    } else if (i == 3) {
      group_name <- "male"
    }
  } else if (sexo == "female") {
    group_name <- "female"
  } else if (sexo == "male") {
    group_name <- "male"
  }
  
  # Save test
  test_name <- paste("Test_rd", group_name, sep = "_")
  assign(test_name, Test)
  
  # Create trains with unique name (Train_rd_balancing_femmal)
  train_name <- paste("Train_rd_nob", group_name, sep = "_")
  assign(train_name, Train) 
  train_name <- paste("Train_rd_down", group_name, sep = "_")
  assign(train_name, traindown) 
  train_name <- paste("Train_rd_up", group_name, sep = "_")
  assign(train_name, trainup) 
  train_name <- paste("Train_rd_50", group_name, sep = "_")
  assign(train_name, train50) 

  i = i + 1
}

# Save train and test in lists
train_rd_list <- list(mget(ls(pattern = "Train_rd")))[[1]]
test_rd_list <- list(mget(ls(pattern = "Test_rd")))[[1]]



# ---- 2. Train random-split models  ----

############################## 
# If you just wanna run some of the models, list them here
# train_rd_list <- list(train_rd_list$Train_rd_nob_femmal)
# names(train_rd_list) <- c("Train_rd_nob_femmal")
#########################################

# Create an emplty list to save models
store_models_rd <- vector("list", length(train_rd_list)) 

o = 1

for (m in train_rd_list) {  
  # Create folds 
  # The results in folds can be used as inputs into the index argument of the trainControl function.
  # trainControl(index = folds)
  folds_random <- caret::createFolds(m$comp_cod, k =10, list = T)
  grid_mtry <- expand.grid(.mtry = c(4,5,6,8))
  # ntrees <- c(250, 500) # we've checked both, it does not change the results
  
  # Train model
  model_random <- train(
    comp_cod ~ ., 
    data = m, 
    method = "rf",
    metric = "Accuracy", 
    tuneGrid = grid_mtry,# evaluatino metrics: precision
    trControl = trainControl(index = folds_random)
  )
  model_random
  model_final <- model_random$finalModel
  
  
  store_models_rd[[o]] <- model_final
  
  o = o + 1
  
  }

names(store_models_rd) <- names(train_rd_list)


# ---- 3. Test random-split models  ---------------------------
# I train and test the models with the same group (female-female, male-male, femmal-femmal),
# because behaviours were different among sexes

# Create an empty list to save confusion matrices
store_conf_matrix_rd <- vector("list", length(store_models_rd) * length(test_rd_list)) 
store_title_rd <- vector("list", length(store_models_rd) * length(test_rd_list))

o = 1

for (i in 1:length(store_models_rd)) {
  
  name_model <- names(store_models_rd[i])
  ind_train <- word(name_model, 4, sep = "_")
  
  for (j in 1:length(test_rd_list)) {
    
    name_test <- names(test_rd_list[j]) 
    ind_test <- word(name_test, 3, sep = "_")
    
    if (ind_train == ind_test) { # female-female, male-male, femmal-femmal
      
      predictions <- predict(store_models_rd[[i]], newdata = test_rd_list[[j]])
      conf_matrix <- confusionMatrix(predictions, as.factor(test_rd_list[[j]]$comp_cod))
      
      title <- paste("Model", name_model, "tested on", name_test)
      
      store_conf_matrix_rd[[o]] <- conf_matrix
      store_title_rd[[o]] <- title
      
      o = o + 1
    }
  }
}

store_title_rd <- unlist(store_title_rd)
names(store_conf_matrix_rd) <- store_title_rd

# show confusion matrix
store_conf_matrix_rd[[3]]$table


# Delete empty list elements
store_conf_matrix_rd <- store_conf_matrix_rd[lengths(store_conf_matrix_rd) != 0]


# Close random-split models
}

# ---- LOIO -----------------------------------------

if (grepl("LOIO", modelo)) {
  
  
# ---- 1. Create data frames from the different groups ----

# Create train sets
if (sexo == "femmal") {
  inds <- levels(as.factor(df_femmal_IDs$ind))
  for (i in inds) {
    df_LOIO <- subset(df_femmal_IDs, ind != i)
    df_LOIO <- df_LOIO %>%
      select(-c(
                #fold, 
                ind, sex))
    df_name <- paste("Train_LOIO_femmal", i, sep = "_")
    assign(df_name, df_LOIO) 
}
  }

inds <- levels(as.factor(df_female_IDs$ind))
for (i in inds) {
  df_LOIO <- subset(df_female_IDs, ind != i)
  df_LOIO <- df_LOIO %>%
    select(-c( #fold, 
              ind, sex))
  df_name <- paste("Train_LOIO_female", i, sep = "_")
  assign(df_name, df_LOIO) 
}

inds <- levels(as.factor(df_male_IDs$ind))
for (i in inds) {
  df_LOIO <- subset(df_male_IDs, ind != i)
  df_LOIO <- df_LOIO %>%
    select(-c(#fold, 
              ind, sex))
  df_name <- paste("Train_LOIO_male", i, sep = "_")
  assign(df_name, df_LOIO) 
}


# Create test sets
for (i in inds) {
  df_LOIO <- subset(df_femmal_IDs, ind == i)
  df_LOIO <- df_LOIO %>%
    select(-c(#fold, 
              ind, sex))
  df_name <- paste("Test_LOIO_femmal", i, sep = "_")
  assign(df_name, df_LOIO) 
  
}

# Check if folds are well compensated
#table(df_LOIO_femmal_maleV$comp_cod, df_LOIO_femmal_maleV$fold)

# Save train and test tables in lists 
train_LOIO_list <- list(mget(ls(pattern = "Train_LOIO")))[[1]]
test_LOIO_list <- list(mget(ls(pattern = "Test_LOIO_femmal")))[[1]]


# ---- 2. Train LOIO models ----
store_models_LOIO <- vector("list", length(train_LOIO_list)) 

o = 1

for (m in train_LOIO_list) {  
  
  folds_random <- caret::createFolds(m$comp_cod, k =10, list = T)
  grid_mtry <- expand.grid(.mtry = c(4,5))
  # ntrees <- c(250, 500)
  
  model_random <- train(
    comp_cod ~ ., 
    data = m, 
    method = "rf",
    metric = "Accuracy", 
    tuneGrid = grid_mtry,
    trControl = trainControl(index = folds_random)
  )
  model_random
  model_final <- model_random$finalModel
  
  store_models_LOIO[[o]] <- model_final
  
  o = o + 1
  
}

names(store_models_LOIO) <- names(train_LOIO_list)


# ---- 3. Test LOIO models----
store_conf_matrix_LOIO <- vector("list", length(store_models_LOIO) * length(test_LOIO_list)) 
store_title_LOIO <- vector("list", length(store_models_LOIO) * length(test_LOIO_list))

o = 1

for (i in 1:length(store_models_LOIO)) {
  name_model <- names(store_models_LOIO[i])
  indOUT <- word(name_model, 4, sep = "_")
  
  for (j in 1:length(test_LOIO_list)) {
    name_test <- names(test_LOIO_list[j])
    ind_test <- word(name_test, 4, sep = "_")
    
    # if test dataset contains all behaviours
    if (indOUT == ind_test & 
        length(levels(store_models_LOIO[[i]]$predicted)) == length(levels(as.factor(test_LOIO_list[[j]]$comp_cod)))) {  
      predictions <- predict(store_models_LOIO[[i]], newdata = test_LOIO_list[[j]])
      conf_matrix <- confusionMatrix(predictions, as.factor(test_LOIO_list[[j]]$comp_cod))
      #name_model <- names(store_models_LOIO[i])
      #name_test <- names(test_LOIO_list[j]) 
      title <- paste("Model", name_model, "tested on", name_test)
      
      store_conf_matrix_LOIO[[o]] <- conf_matrix
      store_title_LOIO[[o]] <- title
      
      o = o + 1
    }
  }
}

store_title_LOIO <- unlist(store_title_LOIO)
names(store_conf_matrix_LOIO) <- store_title_LOIO

store_conf_matrix_LOIO <- store_conf_matrix_LOIO[lengths(store_conf_matrix_LOIO) != 0]

# Close LOIO models
}

# Show confusion matrix
#store_conf_matrix_LOIO[[2]]$table


## 7. Calculate evaluation metrics for each class ----------------------------

# Calculate TP, TN, FP, FN for each class:

# TP (True Positives): The intersection of the reference class and the predicted class.
# TN (True Negatives): The sum of all values in the matrix, except for the row and column of the reference class.
# FP (False Positives): The sum of the values in the column of the predicted class, excluding the row of the reference class.
# FN (False Negatives): The sum of the values in the row of the reference class, excluding the column of the predicted class.

if (modelo == "random") {
  store_conf_matrix <- store_conf_matrix_rd
  store_title <- store_title_rd 
} else if (modelo == "LOIO")  {
  store_conf_matrix <- store_conf_matrix_LOIO
  store_title <- store_title_LOIO 
} else if (modelo == "random+LOIO") {
  store_conf_matrix <- append(store_conf_matrix_rd, store_conf_matrix_LOIO)
  store_title <- c(store_title_rd, store_title_LOIO)
}

store_eval_metrics <- vector("list", length(store_conf_matrix))
  
for (j in 1:length(store_conf_matrix)) {
  
  conf_table <- store_conf_matrix[[j]]$table
  conf_name <- names(store_conf_matrix[j])
  
  classes <- rownames(conf_table)
  
  results <- data.frame(Class = character(length(classes)),
                        Recall_Sensitivity = numeric(length(classes)),
                        Precision = numeric(length(classes)),
                        Accuracy = numeric(length(classes)),
                        Specificity = numeric(length(classes)),
                        F1_score = numeric(length(classes)))
  
  
  for (i in 1:length(classes)) {
    cls <- classes[i]
    TP <- conf_table[cls, cls]
    TN <- sum(conf_table) - rowSums(conf_table)[cls] - colSums(conf_table)[cls] + TP
    FP <- sum(conf_table[, cls]) - TP
    FN <- sum(conf_table[cls, ]) - TP
    
    recall <- TP / (TP + FN)
    precision <- TP / (TP + FP)
    accuracy <- (TP + TN) / sum(conf_table)
    specificity <- TN / (TN + FP)
    f1_score <- 2 * (precision * recall) / (precision + recall)
    
    results[i, ] <- c(cls, recall, precision, accuracy, specificity, f1_score)
  }
  
  results <- results %>% 
    mutate_at(c("Recall_Sensitivity", "Precision", "Accuracy", "Specificity", "F1_score"), as.numeric) %>%
    mutate_if(is.numeric, ~round(., 4))
  
  average <- results %>%
    summarise_if(is.numeric, mean)
  average2 <- cbind(Class = "MEAN", average)
  
  results2 <- rbind(results, average2)

  store_eval_metrics[[j]] <- results2
  
}

names(store_eval_metrics) <- store_title




## 8. Tablas ------------------------------------------------------

# ----Table 2 and table S1: ----
# Precision, sensitivity and classification performance (F1-score) per model
sum_eval_metrics <- data.frame()

for (i in 1:length(store_eval_metrics)) {
  mod <- word(store_title[[i]],2) # model
  method <- word(mod, 2, sep = "_")
  sex_model <- word(mod, 4, sep = "_")
  balan <- word(mod, 3, sep = "_")
  test <- word(store_title[[i]],5) # test
  test <- word(test, 3, sep = "_")
  
  # ncomp: number of behaviours included in the model. Precision[ncomp + 1] because it also includes the mean
  ncomp = length(store_eval_metrics[[i]]$Class)
  
  pre <- store_eval_metrics[[i]]$Precision[ncomp] # mean precision
  sen <- store_eval_metrics[[i]]$Recall_Sensitivity[ncomp] # mean sensitivity
  fi <- store_eval_metrics[[i]]$F1_score[ncomp] # mean f1 score
  
  row_complete <- c(method, sex_model, balan, test, pre, sen, fi)
  
  sum_eval_metrics <- rbind(sum_eval_metrics, row_complete)

}

colnames(sum_eval_metrics) <- c("Method", "Sex_model", "Balancing", "Test", "Precision", "Sensitivity", "F1-score")

# Random-split models: 
sum_eval_metrics_rd <- sum_eval_metrics %>%
  filter(Method == "rd") %>%
  pivot_wider(names_from = Sex_model, values_from = c(Precision, Sensitivity, `F1-score`), 
              names_vary = "slowest") %>% 
  arrange(factor(Balancing, levels = c("nob", "down", "50", "up"))) %>%
  arrange(factor(Test, levels = c("femmal", "female", "male")))

# LOIO models:
sum_eval_metrics_LOIO <- sum_eval_metrics %>%
                            filter(Method == "LOIO")
colnames(sum_eval_metrics_LOIO) <- c("Method", "Ind_out", "Sex_model", "Test", "Precision", "Sensitivity", "F1-score")
sum_eval_metrics_LOIO <- select(sum_eval_metrics_LOIO, -Test)


# SAVE
tosave = sum_eval_metrics_rd # options: sum_eval_metrics_rd o sum_eval_metrics_LOIO

methodtosave  <- tosave$Method[1]
nombresheet <- paste(methodtosave, "LYI","oneLOC", cortejo, alerta, samp_freq, sep = "_")

write.xlsx(as.data.frame(tosave), file = "results/4ind/table2.xlsx",
           sheetName = nombresheet, append = TRUE,
           col.names=TRUE, row.names=FALSE )



# ----Table 3 ----
# F1-score per behaviour

# ATTENTION: choose balancing level
balan = "nob"  #  "nob"   "up"   "down"   "50"


comp_max <- levels(as.factor(acc$comp_cod))

# random-split

sum_eval_beha <-  data.frame(matrix(ncol = length(comp_max) + 5, nrow= 0))
colnames(sum_eval_beha) <- c("Method", "Sex_model", "Balancing", "Test", comp_max, "MEAN")


for (i in 1:length(store_eval_metrics)) {
  if (grepl("rd", store_title[[i]])) { 
    if (grepl(balan, store_title[[i]])) {      
      mod <- word(store_title[[i]],2) # model
      method <- word(mod, 2, sep = "_")
      sex_model <- word(mod, 4, sep = "_")
      balan <- word(mod, 3, sep = "_")
      test <- word(store_title[[i]],5) # test
      test <- word(test, 3, sep = "_")
      
      row_complete <- c(method, sex_model, balan, test, store_eval_metrics[[i]]$F1_score)
      colnames_complete <- c("Method", "Sex_model", "Balancing", "Test", store_eval_metrics[[i]]$Class)
      
      sum_eval_met_sub <- data.frame(matrix(row_complete, nrow = 1, byrow = TRUE))
      colnames(sum_eval_met_sub) <- colnames_complete
      
      sum_eval_beha <- merge(sum_eval_beha, sum_eval_met_sub, all = T)
      
    }
  }
}

sum_eval_beha_rd <- sum_eval_beha %>%
  arrange(factor(Test, levels = c("femmal", "female", "male"))) %>%
  arrange(factor(Sex_model, levels = c("femmal", "female", "male"))) 


if (alerta == "ALE") {
  sum_eval_beha_rd <- sum_eval_beha_rd %>% dplyr::relocate(alert, .after = Test) 
}



# LOIO
sum_eval_beha <- data.frame()

for (i in 1:length(store_eval_metrics)) {
  if (grepl("LOIO", store_title[[i]])) { 
    mod <- word(store_title[[i]],2) # model
    model <- word(mod, 2, sep = "_")
    method <- word(mod, 3, sep = "_")
    test <- word(store_title[[i]],5) # test
    test <- word(test, 4, sep = "_")
    
    fi <- store_eval_metrics[[i]]$F1_score
    
    row_complete <- c(method, model, test, fi)
    
    sum_eval_beha <- rbind(sum_eval_beha, row_complete)
  }
}

colnames(sum_eval_beha) <- c("Model","Method", "Test", store_eval_metrics[[i]]$Class)

sum_eval_beha_LOIO <- subset(sum_eval_beha, resting != 1)



# SAVE
tosave2 = sum_eval_beha_rd # options: sum_eval_beha_LOIO or  sum_eval_beha_rd

methodtosave2  <- tosave2$Method[1]
nombresheet2 <- paste(methodtosave2, "LYI", "oneLOC", cortejo, alerta, samp_freq, sep = "_")


write.xlsx(as.data.frame(tosave2), file = "results/4ind/table3.xlsx",
           sheetName = nombresheet2, append = TRUE,
           col.names=TRUE, row.names=FALSE )

# Table S2 ----------------------------
# confusion matrices

for (j in 1:length(store_conf_matrix)) {
  
  conf_table <- store_conf_matrix[[j]]$table
  conf_name <- names(store_conf_matrix[j])
  
  mod <- word(conf_name,2, sep ="_")
  sex_model <- word(conf_name, 3, sep = "_")
  test <- word(conf_name, 7, sep = "_")
  
  nombresheet <- paste(mod, sex_model, test, "fsLOC", cortejo, alerta, samp_freq, sep = "_")
  
  
  write.xlsx(as.data.frame(conf_table), file = "results/4ind/conf_matrices.xlsx",
             sheetName = nombresheet, append = TRUE,
             col.names=TRUE, row.names=FALSE )
  
}


# ---- Table S3 ----
muestra <- acc %>%
  group_by(sex, ind, comp_cod) %>%
  summarise(count=n()) %>%
  ungroup()

# SAVE
write.xlsx(as.data.frame(muestra), file = "results/4ind/tableS3.xlsx",
           sheetName = XXXX, append = TRUE,
           col.names=TRUE, row.names=FALSE )


## Save models --------------
model_tosave <- store_models_rd$Train_rd_nob_female
saveRDS(model_tosave, file = "results/4ind/model_rd_nob_female_20Hz_noCOU_oneLOC_LYI.RDS") 



