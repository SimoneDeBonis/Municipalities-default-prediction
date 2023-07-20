rm(list=ls())
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
lapply(c('ggplot2', 'GGally','DMwR2', 'rstudioapi','nnet','e1071','zoo','dplyr','themis',
         'tidymodels','tidyverse','caret','readxl','rpart','rattle',
         'rpart.plot','caTools','DataExplorer','MASS','klaR',
         'cluster','randomForest', 'factoextra','ROSE'), library, character.only=TRUE)

data = read.csv("Datasets/data_final.csv", sep = ",")
#################################################
# DESCRIZIONE DATASET:
#
# Stato : 2 = Dissesto, 1 = Deficit, 0 = Altro
# Sesso : 1 = Maschio, 0 = Femmina
# Nord : 1 = Nord, 0 = Altro
#
#################################################

ds_regr = data[, -c(13,16,19,22,24)]
ds_regr = ds_regr[-c(4901),]
##### PREPROCESSING ######

#Comuni della provincia di Napoli vengono letti come Na
ds_regr$Sigla.Provincia[is.na(ds_regr$Sigla.Provincia)] <- "NA"

#Controllo se rimangono NA 
colSums(is.na(ds_regr))

ds_regr$Stato = as.factor(ds_regr$Stato)
ds_regr$TITOLO_DI_STUDIO = as.integer(ds_regr$TITOLO_DI_STUDIO)
ds_smote = na.aggregate(ds_regr[,])  
#Rimpiazzo na con media provinciale
ds_smote <- ds_smote %>%
  mutate_at(vars(3:21), as.numeric)
for (i in 1:nrow(ds_smote)){
  for (j in 1:ncol(ds_smote)){
    if(is.na(ds_smote[i,j]==T)){
      ds_smote[i,j]= mean(ds_smote[ds_smote$Sigla.Provincia==ds_smote$Sigla.Provincia[i],j], na.rm=T)
    }
    if(is.na(ds_regr[i,j]==T)){
      ds_smote[i,j]= mean(ds_smote[,j], na.rm=T)
    }
  }
}

ds_regr$SESSO = round(ds_regr$SESSO)
ds_regr$eta_sindaco = round(ds_regr$eta_sindaco)

####    Train Test Split

ds <- sort(sample(nrow(ds_smote), nrow(ds_smote)*.8)) #Training (80%)
ds_train <- ds_smote[ ds , ]  #Training
ds_test <- ds_smote[ -ds , ]  #Test

minority_data <- ds_train[ds_train$Stato %in% c(2, 1), ]
majority_data <- ds_train[ds_train$Stato == 0, ]

#COSA FA STA ROBA?
majority_data <- majority_data[complete.cases(majority_data$grado.di.autonomia.impositiva), ]

# NA
colSums(is.na(majority_data))

###   Undersampling con Kmeans su classe maggioritaria

# k-means clustering sulla classe maggioritaria
k_values <- 2:10  
wss <- sapply(k_values, function(k) kmeans(majority_data[, -c(1, 15,22)], centers = k)$tot.withinss)

# Plotto the within-cluster sum of squares (wss) 
plot(k_values, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares")

# silhouette 
sil_width <- sapply(k_values, function(k) {
  clust <- kmeans(majority_data[, -c(1, 15,16)], centers = k)
  s <- silhouette(clust$cluster, dist(majority_data[, -c(1, 15)]))
  mean(s[, "sil_width"])  # Compute average silhouette width
})

# Plot silhouette 
plot(k_values, sil_width, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Average Silhouette Width")


# k-means con il k ottimale
#clusters <- kmeans(majority_data[, -c(1, 15,16)], centers = 3)

#majority_data$cluster <- clusters$cluster

#percent_to_select <- 0.5  # prendiamo il 50%

#undersampled_majority <- majority_data %>%
#  group_by(cluster) %>%
#  sample_frac(percent_to_select) %>%
#  ungroup()

#undersampled_majority <- undersampled_majority[,-17]


#####    Oversampling con SMOTE classe minoritaria   #####
majority_data <- majority_data[, -c(1,15,22)]
minority_data <- minority_data[, -c(1,15,22)]


# SMOTE  
#formula <- as.formula("Stato ~ .")
#synthetic_samples <- ROSE(formula, data = minority_data, N = 1000)$data
#minority_smote <- rbind(minority_data, synthetic_samples)
#undersampled_majority <- undersampled_majority[, -c(1, 15)]

#sampled_data = rbind(minority_smote, undersampled_majority)
#sampled_data <- sampled_data %>%
#  mutate_at(vars(2:13), as.numeric)



####    funzione di calcolo della f_measure    ####
calculate_f_measure <- function(predictions, actual) {
  class_count <- length(unique(actual))  # Number of classes
  
  f_measures <- numeric(class_count)
  
  for (i in 1:class_count) {
    class_label <- i
    
    tp <- sum(predictions == class_label & actual == class_label)
    fp <- sum(predictions != class_label & actual == class_label)
    fn <- sum(predictions == class_label & actual != class_label)
    
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    
    f_measures[i] <- 2 * precision * recall / (precision + recall)
  }
  
  average_f_measure <- mean(f_measures, na.rm = TRUE)
  
  return(average_f_measure)
}


#######         CERCA I PARAMETRI IGLIORI PER LA MULTINOMIALE       #############

undersampling_percentages <- c(0.25, 0.4, 0.5,0.75,0.8)
smote_sample_numbers <- c(75,380,580, 1080, 1810, 2300)

# Create an empty list to store the result rows
result_df <- data.frame(percentual = numeric(),
                        number = numeric(),
                        f_measure = numeric(),
                        stringsAsFactors = FALSE)

for (percentage in undersampling_percentages) {
  
  # Faccio undersampling
  clusters <- kmeans(majority_data[, -c(1, 15,16)], centers = 3)
  majority_data$cluster <- clusters$cluster
  undersampled_majority <- majority_data %>%
  group_by(cluster) %>%
  sample_frac(percentage) %>%
  ungroup()
  undersampled_majority <- undersampled_majority[, -c(14)]
  
  # Faccio SMOTE
  for (number in smote_sample_numbers) {
    
    formula <- as.formula("Stato ~ .")
    synthetic_samples <- ROSE(formula, data = minority_data, N = number)$data
    minority_smote <- rbind(minority_data, synthetic_samples)
    
    sampled_data <- rbind(minority_smote, undersampled_majority)
    
    # Train della multinomiale 
    model <- multinom(Stato ~ . , data = sampled_data)
    predictions <- predict(model, newdata = ds_test, type = "class")
    f_1 <- calculate_f_measure(predictions, ds_test$Stato)
    
    # Creo riga con i parametri 
    result_row <- list(
      percentual = percentage,
      number = number,
      f_measure = f_1
    )
    result_df <- rbind(result_df, result_row)
  }
}
# print del risultato
print(result_df)



####    REGRESSIONE MULTIVARIATA   ####
clusters <- kmeans(majority_data[, -c(1, 15,16)], centers = 3)
majority_data$cluster <- clusters$cluster
percent_to_select <- 0.25  # prendiamo il 50%
under_majority <- majority_data %>%
  group_by(cluster) %>%
  sample_frac(percent_to_select) %>%
  ungroup()
under_majority <- under_majority[,-14]

formula <- as.formula("Stato ~ .")
over_samples <- ROSE(formula, data = minority_data, N = 1080)$data
over_smote <- rbind(minority_data, over_samples)

sampled_data <- rbind(over_smote, under_majority)

regressione = multinom(Stato ~ . , data = sampled_data)
conf = predict(regressione, ds_test)
summary(conf)

conf_matr = table(conf, ds_test$Stato)
conf_matr

f_1 <- calculate_f_measure(predictions, ds_test$Stato)
print(f_1)



##### Trovare i parametri migliori per la SVM

undersampling_percentages <- c(0.25, 0.4, 0.5, 0.75, 0.8)
smote_sample_numbers <- c(75, 380, 580, 1080, 1810, 2300)

svm_types <- c('linear', 'rbf', 'polynomial')
svm_parameters <- list(
  linear = list(C = c(0.01, 0.1,0.5, 1, 10)),
  rbf = list(C = c(0.01, 0.1,0.5, 1, 10), gamma = c(0.001,0.01, 0.1,0.4,0.8, 1)),
  polynomial = list(C = c(0.01, 0.1,0.5, 1, 10), degree = c(2, 3, 4,5,6))
)

# Creo dataset
result_df <- data.frame(
  percentual = numeric(),
  number = numeric(),
  svm_type = character(),
  C = numeric(),
  gamma = numeric(),
  degree = numeric(),
  f_measure = numeric(),
  stringsAsFactors = FALSE
)

for (percentage in undersampling_percentages) {
  # Faccio undersampling
  clusters <- kmeans(majority_data[, -c(1, 15, 16)], centers = 3)
  majority_data$cluster <- clusters$cluster
  undersampled_majority <- majority_data %>%
    group_by(cluster) %>%
    sample_frac(percentage) %>%
    ungroup()
  undersampled_majority <- undersampled_majority[, -c(14)]
  
  # Faccio SMOTE
  for (number in smote_sample_numbers) {
    formula <- as.formula("Stato ~ .")
    synthetic_samples <- ROSE(formula, data = minority_data, N = number)$data
    minority_smote <- rbind(minority_data, synthetic_samples)
    
    sampled_data <- rbind(minority_smote, undersampled_majority)
    
    # Train di SVM 
    for (svm_type in svm_types) {
      parameters <- svm_parameters[[svm_type]]
      for (C in parameters$C) {
        if (svm_type == 'linear') {
          model <- svm(
            Stato ~ .,
            data = sampled_data,
            type = "C-classification",
            kernel = 'linear',
            cost = C
          )
          result_row <- list(
            percentual = percentage,
            number = number,
            svm_type = svm_type,
            C = C,
            gamma = NA,
            degree = NA,
            f_measure = calculate_f_measure(predict(model, newdata = ds_test), ds_test$Stato)
          )
          result_df <- rbind(result_df, result_row)
        } else if (svm_type == 'rbf') {
          for (gamma in parameters$gamma) {
            model <- svm(
              Stato ~ .,
              data = sampled_data,
              type = "C-classification",
              kernel = 'radial',
              cost = C,
              gamma = gamma
            )
            result_row <- list(
              percentual = percentage,
              number = number,
              svm_type = svm_type,
              C = C,
              gamma = gamma,
              degree = NA,
              f_measure = calculate_f_measure(predict(model, newdata = ds_test), ds_test$Stato)
            )
            result_df <- rbind(result_df, result_row)
          }
        } else if (svm_type == 'polynomial') {
          for (degree in parameters$degree) {
            model <- svm(
              Stato ~ .,
              data = sampled_data,
              type = "C-classification",
              kernel = 'polynomial',
              cost = C,
              degree = degree
            )
            result_row <- list(
              percentual = percentage,
              number = number,
              svm_type = svm_type,
              C = C,
              gamma = NA,
              degree = degree,
              f_measure = calculate_f_measure(predict(model, newdata = ds_test), ds_test$Stato)
            )
            result_df <- rbind(result_df, result_row)
          }
        }
      }
    }
  }
}

# Print dei risultati
print(result_df)

# massima f_measure
max_row_index <- which.max(result_df$f_measure)
row_with_max_f_measure <- result_df[max_row_index, ]
print(row_with_max_f_measure)


####  SUPPORT VECTOR MACHINE   ##### 

clusters <- kmeans(majority_data[, -c(1, 15,16)], centers = 3)
majority_data$cluster <- clusters$cluster
percent_to_select <- 0.75  # prendiamo il 50%
under_majority <- majority_data %>%
  group_by(cluster) %>%
  sample_frac(percent_to_select) %>%
  ungroup()
under_majority <- under_majority[,-14]

formula <- as.formula("Stato ~ .")
over_samples <- ROSE(formula, data = minority_data, N = 75)$data
over_smote <- rbind(minority_data, over_samples)

sampled_data <- rbind(over_smote, under_majority)

svm = svm(Stato ~ ., data = sampled_data, method="C-classification", kernel="radial", gamma=0.4, cost=10)

prediction <- predict(svm, ds_test[,-c(1,2,15,16)])
summary(prediction)

xtab <- table(prediction, ds_test$Stato)
xtab

conf_matrsvm = confusionMatrix(prediction, as.factor(ds_test$Stato))
conf_matrsvm





####     ALBERO DECISIONALE     ####

# parametri della cross-validation
impurity_measures <- c("gini", "accuracy", "gain", "information")
tree_depths <- c(5, 10, 20,30)

result_decisiontree <- data.frame(percentual = numeric(),
                        number = numeric(),
                        impurity_measure = character(), 
                        tree_depth = numeric(),
                        f_measure = numeric(),
                        stringsAsFactors = FALSE)

for (percentage in undersampling_percentages) {
  
  # Faccio undersampling
  clusters <- kmeans(majority_data[, -c(1, 15, 16)], centers = 3)
  majority_data$cluster <- clusters$cluster
  undersampled_majority <- majority_data %>%
    group_by(cluster) %>%
    sample_frac(percentage) %>%
    ungroup()
  undersampled_majority <- undersampled_majority[, -c(14)]
  
  # Faccio SMOTE
  for (number in smote_sample_numbers) {
    
    formula <- as.formula("Stato ~ .")
    synthetic_samples <- ROSE(formula, data = minority_data, N = number)$data
    minority_smote <- rbind(minority_data, synthetic_samples)
    
    sampled_data <- rbind(minority_smote, undersampled_majority)
    
    for (impurity_measure in impurity_measures) {
      for (tree_depth in tree_depths) {
        
        # Train del decision tree
        model <- rpart(Stato ~ ., data = sampled_data,
                       method = "class",
                       control = rpart.control(cp = 0.0001),
                       parms = list(split = impurity_measure),
                       maxdepth = tree_depth)
        
        # predictions
        predictions <- predict(model, newdata = ds_test, type = "class")
        
        f_1 <- calculate_f_measure(predictions, ds_test$Stato)
        
        result_row <- list(
          percentual = percentage,
          number = number,
          impurity_measure = impurity_measure,
          tree_depth = tree_depth,
          f_measure = f_1
        )
        
        result_decisiontree<- rbind(result_decisiontree, result_row)
      }
    }
  }
}

print(result_decisiontree)

# parametri migliori
max_f_measure_rows <- aggregate(f_measure ~ impurity_measure, data = result_decisiontree, FUN = max)
result_df_max <- merge(result_decisiontree, max_f_measure_rows, by = c("impurity_measure", "f_measure"))
print(result_df_max)


######      Modello Albero Decisionale      ######

clusters <- kmeans(majority_data[, -c(1, 15,16)], centers = 3)
majority_data$cluster <- clusters$cluster
percent_to_select <- 0.75  # prendiamo il 50%
under_majority <- majority_data %>%
  group_by(cluster) %>%
  sample_frac(percent_to_select) %>%
  ungroup()
under_majority <- under_majority[,-14]

formula <- as.formula("Stato ~ .")
over_samples <- ROSE(formula, data = minority_data, N = 1080)$data
over_smote <- rbind(minority_data, over_samples)

sampled_data <- rbind(over_smote, under_majority)

tree <- rpart(Stato ~. , data = sampled_data, control = rpart.control(cp = 0.0001), parms = list(split = "gini"), method = "class",  maxdepth = 10)

#oppure:
#fit <- ds_smote %>% train(Stato ~. , data=. , method="rpart", control = rpart.control(cp=0.0001), trControl = trainControl(method = "cv", number = 10), 
#                          tuneLength = 6)
#fit

#plotcp(tree)
#printcp(tree)

#bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
#bestcp 

#tree.pruned <- prune(tree, cp = bestcp) 
#tree.pruned 

#plot(tree.pruned)
#text(tree.pruned, cex=0.8, use.n=TRUE, xpd=TRUE)
#prp(tree.pruned, faclen=0, cex=0.8, extra=1)

# Confusion Matrix
predictions <- predict(tree, newdata = ds_test, type="class")
confusion_matrix <- table( predictions, ds_test$Stato)
confusion_matrix






####      RANDOMFOREST       ####



impurity_measures <- c("gini", "accuracy", "gain", "information")
tree_depths <- c(5, 10, 20, 30)
num_trees <- c(50, 100, 200)  # numbero di alberi nella cross-validation

result_randomforest <- data.frame(
  percentual = numeric(),
  number = numeric(),
  impurity_measure = character(),
  tree_depth = numeric(),
  num_trees = numeric(),
  f_measure = numeric(),
  stringsAsFactors = FALSE
)

for (percentage in undersampling_percentages) {
  
  # Faccio undersampling
  clusters <- kmeans(majority_data[, -c(1, 15, 16)], centers = 3)
  majority_data$cluster <- clusters$cluster
  undersampled_majority <- majority_data %>%
    group_by(cluster) %>%
    sample_frac(percentage) %>%
    ungroup()
  undersampled_majority <- undersampled_majority[, -c(14)]
  
  # Faccio SMOTE
  for (number in smote_sample_numbers) {
    
    formula <- as.formula("Stato ~ .")
    synthetic_samples <- ROSE(formula, data = minority_data, N = number)$data
    minority_smote <- rbind(minority_data, synthetic_samples)
    
    sampled_data <- rbind(minority_smote, undersampled_majority)
    
    for (impurity_measure in impurity_measures) {
      for (tree_depth in tree_depths) {
        for (num_tree in num_trees) {
          
          # Train modello Random Forest
          model <- randomForest(
            Stato ~ .,
            data = sampled_data,
            ntree = num_tree,
            mtry = sqrt(ncol(sampled_data) - 1),  # Number of variables to randomly consider at each split
            importance = TRUE,  # Calculate variable importance
            nodesize = 1,  # Minimum size of terminal nodes
            replace = FALSE,  # Bootstrap sampling with replacement
            splitrule = impurity_measure,  # Impurity measure for splitting
            maxdepth = tree_depth  # Maximum depth of each tree
          )
          
          # Prediction
          predictions <- predict(model, newdata = ds_test[,-c(1,2,15,16)], type = "class")
          
          f_1 <- calculate_f_measure(predictions, ds_test$Stato)
          
          result_row <- list(
            percentual = percentage,
            number = number,
            impurity_measure = impurity_measure,
            tree_depth = tree_depth,
            num_trees = num_tree,
            f_measure = f_1
          )
          
          result_randomforest <- rbind(result_randomforest, result_row)
        }
      }
    }
  }
}

print(result_randomforest)

## Parametri migliori
max_f_measure_rows <- aggregate(f_measure ~ impurity_measure, data = result_randomforest, FUN = max)
result_df_max <- merge(result_randomforest, max_f_measure_rows, by = c("impurity_measure", "f_measure"))
print(result_df_max)



######      Modello RandomForest        ######

clusters <- kmeans(majority_data[, -c(1, 15,16)], centers = 3)
majority_data$cluster <- clusters$cluster
percent_to_select <- 0.5  
under_majority <- majority_data %>%
  group_by(cluster) %>%
  sample_frac(percent_to_select) %>%
  ungroup()
under_majority <- under_majority[,-14]

formula <- as.formula("Stato ~ .")
over_samples <- ROSE(formula, data = minority_data, N = 75)$data
over_smote <- rbind(minority_data, over_samples)

sampled_data <- rbind(over_smote, under_majority)

modelrf<-model <- randomForest(
  Stato ~ .,
  data = sampled_data,
  ntree = 50,
  mtry = sqrt(ncol(sampled_data) - 1),  # Number of variables to randomly consider at each split
  importance = TRUE,  # Calculate variable importance
  nodesize = 1,  # Minimum size of terminal nodes
  replace = FALSE,  # Bootstrap sampling with replacement
  splitrule = 'information',  # Impurity measure for splitting
  maxdepth = 30  # Maximum depth of each tree
)

##previsioni
pred = predict(modelrf, newdata = ds_test, type="class") 
confronto=table(pred, ds_test$Stato)
confronto 





## LDA
linear<-lda(Stato~., ds_smote)
linear

attributes(linear)

p<-predict(linear,ds_smote)
dev.new()
ldahist(data= p$x[,1], g=ds_smote$Stato)
ldahist(data= p$x[,2], g=ds_smote$Stato)

#Partition plot
dev.new()
partimat(Stato~., data=ds_smote, method="lda")

## Matrice di confusione e accuracy
##training set
p1<-predict(linear,ds_smote)$class
tab<-table(Predicted=p1, Actual=ds_smote$Stato)
tab
sum(diag(tab))/sum(tab)

##testing set 

p2<-predict(linear, ds_test)$class
tab1<- table(Predicted=p2, Actual=ds_test$Stato)
tab1
sum(diag(tab1))/sum(tab1)
