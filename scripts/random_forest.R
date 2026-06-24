############################## loading libraries ##############################

if (!require("tidyverse")) install.packages("tidyverse"); require("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("randomForest")) install.packages("randomForest"); library("randomForest")
if (!require("caret")) install.packages("caret"); library("caret")
if (!require("pROC")) install.packages("pROC"); library("pROC")
if (!require("Metrics")) install.packages("Metrics"); library("Metrics")

################################ loading dataset ###############################

### read dataset
ds = read.table(
  "0_data/life_history_dataset.csv", 
   sep= ",", 
   header= T, 
   fill= T
)

################################# my functions #################################

### helper function to find the most common value (mode)
get_mode = function(x) {
  # Remove NAs so they aren't counted as a category
  clean_x <- x[!is.na(x)]
  # If the entire group is NA, return NA or a default value
  if (length(clean_x) == 0) return(NA)
  # Create a frequency table and find the name of the highest count
  freq_table <- table(clean_x)
  names(freq_table)[which.max(freq_table)]
}

### function to set train and test
set_train_test = function(data){
  list = list()
  ind = sample(2, size = nrow(data), replace = T, prob = c(0.6, 0.4))
  list$train = data[ind==1,]
  list$test = data[ind==2,]
  return(list)
}

### null model function
null_model = function(train, test, target_name){
  values = train[,target_name]
  pred = values[sample(nrow(test), replace= F)]
  return(pred)
}

################################ processing dataset ###########################

### set species as row names
rownames(ds) = ds[,"species"]
ds = ds[,-1]

### setting target
target_name = "target"

### changing target to factor
ds[[target_name]] = as.factor(ds[[target_name]])

### all classes
all_classes = levels(ds[[target_name]])
n_classes = length(all_classes)

### getting dataset with target values
tds = ds[!is.na(ds[[target_name]]),]

### setting features
feature_names = colnames(tds)
feature_names = feature_names[!feature_names == target_name]
feature_names 

### counting NAs
na_sum = c()
for(one_feature in feature_names){
  na_sum = c( na_sum, sum(is.na(tds[[one_feature]])) )
}
nadf = data.frame(feature_names, na_sum)
nadf

### replacing NAs
for(one_feature in feature_names){
  ftype = class(tds[[one_feature]])
  na_sum = sum(is.na(tds[[one_feature]])) 
  ### if categorical feature
  if(na_sum > 0 & ftype == "character"){
    tds[[one_feature]] = ifelse(
      is.na(tds[[one_feature]]), 
      ave(tds[[one_feature]], tds[[target_name]], FUN = get_mode), 
      tds[[one_feature]]
    )
  }
  ### if non-categorical feature
  if(na_sum > 0 & ftype != "character"){
    tds[[one_feature]] = ifelse(
      is.na(tds[[one_feature]]), 
      ave(tds[[one_feature]], tds[[target_name]], FUN = function(x) median(x, na.rm = TRUE)), 
      tds[[one_feature]]
    )
  }
}

################################### a single RF ################################

### separating train and test
list = set_train_test(data = tds)
train = list$train
test = list$test

### training model 
rf = randomForest(
  x = train[,feature_names],
  y = train[,target_name],
  xtest= test[,feature_names],
  ytest= test[,target_name],
  ntree = 100,
  mtry = 2
) 

### confusion matrix
conmat = confusionMatrix(
  data = test[,target_name], 
  reference = rf$test$predicted
)

### performance metrics
performance = c(
  "accuracy" = conmat$overall[["Accuracy"]],
  "kappa" = conmat$overall[["Kappa"]],
  "sensitivity" = conmat$byClass[,"Sensitivity"],
  "specificity" = conmat$byClass[,"Specificity"],
  "tss"= conmat$byClass[,"Sensitivity"] + conmat$byClass[,"Specificity"] - 1
)
### show metrics
performance

### importance
round(importance(rf), 2)

### improtance plot
varImpPlot(
  rf,
  sort = F,
  main = paste0("Feature Importance")
)

############################### optimizing RF model #############################

### grid of parameters
grid = expand.grid(
  "ntree" = c(100, 500), 
  "mtry" = c(2, 10),
  "maxnodes" = 4
)

### dataframes to save performances
all_rf_perf = c()
all_null_perf = c()

### repeats
repeats = 10
### initial repeat
n = 1
### loop until N repeats
while(n <= repeats){
  ### separating train and test
  list = set_train_test(data = tds)
  train = list$train
  test = list$test
  ### check if train and test have all classes
  train_check = sum(all_classes %in% unique(train[,target_name]))
  test_check = sum(all_classes %in% unique(test[,target_name]))
  ### if train and test ok, measure performance
  if(train_check == n_classes & test_check == n_classes){
    for(i in 1:nrow(grid) ){
      ### training model 
      rf = randomForest(
        x = train[,feature_names],
        y = train[,target_name],
        xtest= test[,feature_names],
        ytest= test[,target_name],
        ntree = grid$ntree[i],
        mtry = grid$mtry[i]
      ) 
      ### confusion matrix
      rf_conmat = confusionMatrix(
        data = test[,target_name], 
        reference = rf$test$predicted
      )
      ### performance metrics
      rf_perf = c(
        "repeat" = n,
        "grid_line" = i,
        "ntree" = grid$ntree[i],
        "mtry" = grid$mtry[i],
        "accuracy" = rf_conmat$overall[["Accuracy"]],
        "kappa" = rf_conmat$overall[["Kappa"]],
        "sensitivity" = rf_conmat$byClass[,"Sensitivity"],
        "specificity" = rf_conmat$byClass[,"Specificity"],
        "tss"= rf_conmat$byClass[,"Sensitivity"] + rf_conmat$byClass[,"Specificity"] - 1
      )
      ### add performance
      all_rf_perf = rbind(all_rf_perf, rf_perf)
      ### check
      print(paste0("Training done! repeat: ", n, ", grid line: ", i) )
    }
    ### null predictions
    nullpred = null_model(
      train = train, 
      test = test, 
      target_name = target_name 
    )
    ### null confusion matrix
    null_conmat = confusionMatrix(
      data = test[,target_name], 
      reference = nullpred
    )
    ## performance metrics
    null_perf = c(
      "repeat" = n,
      "accuracy" = null_conmat$overall[["Accuracy"]],
      "kappa" = null_conmat$overall[["Kappa"]],
      "sensitivity" = null_conmat$byClass[,"Sensitivity"],
      "specificity" = null_conmat$byClass[,"Specificity"],
      "tss"= null_conmat$byClass[,"Sensitivity"] + null_conmat$byClass[,"Specificity"] - 1
    )
    ### add performance
    all_null_perf = rbind(all_null_perf, null_perf)
    ### update repeat
    n = n + 1
  }
}

### convert to dataframe
all_rf_perf = data.frame(all_rf_perf)
### export
saveRDS(
  all_rf_perf,
  "2_model_performance/all_rf_perf.RDS"
)

### convert to dataframe
all_null_perf = data.frame(all_null_perf)
### export
saveRDS(
  all_null_perf,
  "2_model_performance/all_null_perf.RDS"
)

############################## evaluating performance ##########################

### read performance metrics
all_rf_perf = readRDS("2_model_performance/all_rf_perf.RDS")

### ignore these columns
cols_to_ignore = c("repeat.", "grid_line")

### summarise
rf_perf_mean = all_rf_perf %>% 
  group_by((grid_line)) %>% 
  summarise(
    across(
      where(is.numeric) & !all_of(cols_to_ignore), 
      list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  ) %>% 
  rename("grid_line" = "(grid_line)")

### plotting
ggplot(
  rf_perf_mean,
  aes(x = grid_line, 
      y = accuracy_mean) 
  )+
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    x = "grid line",
    y = "accuracy"
  )

### getting tss columns
col_names = colnames(rf_perf_mean)
col_tss_names = col_names[grepl("tss.", col_names)]

### make 'long' format for TSS
rf_tss_mean = rf_perf_mean %>%
  pivot_longer(
    cols = col_tss_names, 
    names_to = c("class", ".value"), 
    names_pattern = "(.*)_(mean|sd)"
  ) %>% 
  mutate(class = case_when(
    grepl("annual", class) ~ "annual",
    grepl("early", class) ~ "early",
    grepl("late", class) ~ "late"
  ))

### plotting
ggplot(
  rf_tss_mean,
  aes(x = grid_line, 
      y = mean, 
      group = class, 
      color = class)
  ) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    x = "grid line",
    y = "TSS"
  )
