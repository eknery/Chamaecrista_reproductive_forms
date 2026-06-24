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
  list$train = data[ind==2,]
  list$test = data[ind==1,]
  return(list)
}

### null model function
null_model = function(train, test, target_name){
  values = train[[target_name]]
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

### getting performance metrics
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
varImpPlot(rf,
           sort = F,
           main = paste0("Feature Importance ", target))


############################### optimizing RF model #############################

### repeats
repeats = 10

### grid of parameters
grid = expand.grid(
  "ntree" = c(100, 500), 
  "mtry" = c(2, 10),
  "maxnodes" = 4
)

### dataframe to save performances
all_performances = c()

### loop
for(n in 1:repeats){
  ### separating train and test
  list = set_train_test(data = tds)
  train = list$train
  test = list$test
  ### check if train and test have all classes
  train_check = sum(all_classes %in% unique(train[,target_name]))
  test_check = sum(all_classes %in% unique(test[,target_name]))
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
      conmat = confusionMatrix(
        data = test[,target_name], 
        reference = rf$test$predicted
      )
      ### getting results
      performance = c(
        "repeat" = n,
        "grid_line" = i,
        "ntree" = grid$ntree[i],
        "mtry" = grid$mtry[i],
        "accuracy" = conmat$overall[["Accuracy"]],
        "kappa" = conmat$overall[["Kappa"]],
        "sensitivity" = conmat$byClass[,"Sensitivity"],
        "specificity" = conmat$byClass[,"Specificity"],
        "tss"= conmat$byClass[,"Sensitivity"] + conmat$byClass[,"Specificity"] - 1
      )
      ### add performance
      all_performances = rbind(all_performances, performance)
      ### check
      print(paste0("Training done! repeat: ", n, ", grid line: ", i) )
    }
  }
}

### convert to dataframe
all_performances = data.frame(all_performances)

############################# plotting ps predictions ############################

### load flower pc scores
pred_df = read.table("1_flower_analyses/pred_class_ps.csv", sep=",", h=T)

### testing differences
tab = table(pred_df$geo_state, pred_df$my_pred)
chisq.test(tab)

### summarizing
pred_df= pred_df %>% 
  group_by(geo_state) %>% 
  reframe(generalist = sum(my_pred == "generalist"),
         specialist = sum(my_pred == "specialist"),
         ) %>% 
  pivot_longer(cols = c(generalist, specialist), names_to = "pollination" ) %>% 
  group_by(geo_state) %>% 
  mutate(perc = 100* value/sum(value) )

### plot param
axis_title_size = 8
x_text_size = 7
y_text_size = 7
legend_text_size = 6
legend_key_size = 0.4

### plotting
pred_plot = ggplot(data = pred_df) + 
  
  geom_bar(aes(x = geo_state, 
               y = perc, 
               fill = pollination,
               color = geo_state),
           stat = "identity", 
           width = 0.9,
           linewidth = 0.8,
           alpha = 0.75) +
  
  scale_x_discrete(labels=c("AF" = "AF-endemic", 
                            "other" = "non-endemic"))+
  
  scale_fill_manual(values = c("gray", "black") )+
  
  scale_color_manual(values = c("#1E88E5","#D81B60") )+
  
  xlab("geographic distribution") +
  
  ylab("relative frequency (%)\n of pollination systems") +
  
  guides(fill=guide_legend(title="")) +
  guides(color = "none")+
  
  theme(panel.background=element_rect(fill="white"),
        panel.grid=element_line(colour=NULL),
        panel.border=element_rect(fill=NA,colour="black"),
        axis.title=element_text(size=axis_title_size, face="bold"),
        axis.text.x= element_text(size= x_text_size),
        axis.text.y = element_text(size=y_text_size, angle = 0),
        legend.position = "bottom",
        legend.text = element_text(size= legend_text_size),
        legend.key = element_blank(),
        legend.key.size = unit(legend_key_size, 'cm'))

# export plot
tiff("3_graphs/pred_plot_ps.tiff", units="cm", width=7, height=6.5, res=600)
  print(pred_plot)
dev.off()

############################# plotting ms predictions ############################

### load flower pc scores
pred_df = read.table("1_flower_analyses/pred_class_ms1.csv", sep=",", h=T)

### exploring differences
pred_df %>% 
  group_by(geo_state) %>% 
  reframe(mean(my_pred), sd(my_pred), median(my_pred))

### testing differences
my_pred = pred_df$my_pred
geo_state = pred_df$geo_state
names(my_pred) = pred_df$species
names(geo_state) = pred_df$species

paov = phylANOVA(tree = mcc_phylo, 
          x = geo_state ,
          y = my_pred,
          posthoc = F)

### plot param
axis_title_size = 8
x_text_size = 7
y_text_size = 7
legend_text_size = 6
legend_key_size = 0.4

### plotting
pred_plot = ggplot(data = pred_df,
                   aes(x=geo_state, 
                       y=my_pred, 
                       fill=geo_state)) + 
  
  geom_point(aes(color=geo_state),
             position = position_jitter(width = 0.07), 
             size = 1.2, 
             alpha = 0.65) +
  
  geom_boxplot(width = 0.5, 
               outlier.shape = NA,
               alpha = 0.25)+
  
  scale_fill_manual(values=c("#1E88E5","#D81B60"))+
  scale_colour_manual(values=c("#1E88E5","#D81B60"))+
  scale_x_discrete(labels=c("AF" = "AF-endemic", 
                            "other" = "non-endemic")) +
  
  xlab("geographic distribution") +
  
  ylab("relative frequency (%)\n of self pollination") +
  
  guides(fill=guide_legend(title="")) +
  guides(color = "none")+
  
  theme(panel.background=element_rect(fill="white"),
        panel.grid=element_line(colour=NULL),
        panel.border=element_rect(fill=NA,colour="black"),
        axis.title=element_text(size=axis_title_size, face="bold"),
        axis.text.x= element_text(size= x_text_size),
        axis.text.y = element_text(size=y_text_size, angle = 0),
        legend.position = "bottom",
        legend.text = element_text(size= legend_text_size),
        legend.key = element_blank(),
        legend.key.size = unit(legend_key_size, 'cm'))

# export plot
tiff("3_graphs/pred_plot_ms.tiff", units="cm", width=7, height=6.5, res=600)
  print(pred_plot)
dev.off()