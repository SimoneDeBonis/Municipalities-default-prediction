library(readxl)
library(tidyverse)
current_path<-getActiveDocumentContext()$path
setwd("~/Universit? DSEI/ANNO 1/METODOLOGIE STATISTICHE PER BIG DATA/Metodologie/Metodologie/DATASETS")

df_target1 <- read_excel("comuni dissesto.xlsx", sheet = "Table 2")
df_target2 <- read_excel("comuni dissesto.xlsx", sheet = "Table 3")
df_target= rbind(df_target1[,c(1,2,5)],df_target2[,c(1,2,6)])
df_target2=df_target2[,-4]
diferenta=setdiff(df_target1,df_target2)
diferenza2=setdiff(df_target2,df_target1)
View(df_target)

comuni <- read_excel("Classificazioni statistiche-e-dimensione-dei-comuni_31_12_2018.xls")
View(comuni)

comuni <- comuni %>% 
  rename("Ente" = `Denominazione (italiana e straniera)`)
comuni$Ente = toupper(comuni$Ente)

# Aggiungo i comuni non in deficit/dissesto
df_target <- df_target %>%
  right_join(comuni, by = c("Ente" = "Ente"))

# inserisco 0 nella variabile target sane finanziariamente
df_target$Stato <- df_target$Stato %>% 
  replace_na("0")

# Sostituisco con 1 i comuni in deficit e con 2 quelli in dissesto
df_target$Stato <- str_replace_all(df_target$Stato, c("Deficitario" = "1", "Dissestato" = "2"))

# trasformo Stato in fattore
df_target$Stato <- as.factor(df_target$Stato)
View(df_target)
### Stato: Dissestato = 2
###        Deficitario = 1
###        Altro = 0

# Otteniamo uniformità nella scrittura delle parole
library(Hmisc)
df_target$Ente = tolower(df_target$Ente)
for (i in 1:length(df_target$Ente)){
  df_target$Ente[i] = capitalize(df_target$Ente[i])
}

View(df_target)
