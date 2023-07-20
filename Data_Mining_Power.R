rm(list=ls())
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
lapply(c('ggplot2', 'GGally', 'rstudioapi','nnet','e1071','zoo','themis',
         'tidymodels','tidyverse','caret','readxl','rpart','rattle',
         'rpart.plot','caTools','DataExplorer','MASS','klaR',
         'cluster','randomForest'), library, character.only=TRUE)

data = read.csv("Datasets/data_final.csv", sep = ",")

#################################################
# DESCRIZIONE DATASET:
# DESCRIZIONE DATASET:
#
# Stato : 1 = Deficit o Dissesto, 0 = Altro
# Sesso : 1 = Maschio, 0 = Femmina
# Nord  : 1 = Nord, 0 = Altro
#
#################################################

# ORDINE DEGLI EVENT:
# - trattamento degli na 

ds_regr = data[, -c(13,16,22,24)]
ds_regr$Sigla.Provincia = as.character(ds_regr$Sigla.Provincia)

#Rimpiazzo na con media di colonna
for (i in 1:nrow(ds_regr)){
  for (j in 1:ncol(ds_regr)){
    if(is.na(ds_regr[i,j]==T)){
      ds_regr[i,j]= mean(ds_regr[ds_regr$Sigla.Provincia==ds_regr$Sigla.Provincia[i],j], na.rm=T)
    }
  }
}

ds_regr$SESSO = round(ds_regr$SESSO)
ds_regr$eta_sindaco = round(ds_regr$eta_sindaco)

#sostituisco NA con media di colonna E
ds_regr$Stato = as.integer(ds_regr$Stato)
ds_regr$TITOLO_DI_STUDIO = as.integer(ds_regr$TITOLO_DI_STUDIO)
ds_smote = na.aggregate(ds_regr[,])  


ds_smote$Stato[ds_smote$Stato==2] = 1
ds_smote$Stato = as.factor(ds_smote$Stato)
# Oversampling con SMOTE classi minoritarie 
ds_smote = ds_smote[,-c(17,23)]
ds_smote[, -c(1,2)] <- sapply(ds_smote[, -c(1,2)], as.numeric)

library(dplyr)
library(tidyr)
ds_smote = ds_smote %>% 
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))

ds_svm <- smote(ds_smote[,-1], var="Stato", k = 5, over_ratio = 1)
write.csv(ds_smote, "ds_smote.csv", row.names = FALSE)


svm = svm(Stato ~ ., data = ds_svm, method="C-classification", kernel="radial", 
          gamma=0.99, cost=0.1)

prediction <- predict(svm, ds_smote[,-1])
summary(prediction)

xtab <- table(prediction, ds_smote$Stato)
xtab

data_new = data.frame(ds_smote$denominazione,prediction)
write.csv(data_new, "previsione.csv", row.names = FALSE)
                  
conf_matrsvm = confusionMatrix(prediction, as.factor(ds_test$Stato))
conf_matrsvm