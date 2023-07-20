rm(list=ls())
library(rstudioapi)
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
library(readxl)
library(dplyr)
library(tidyverse)
library(Hmisc)
library(stringi)

# AGGIUNGO SINDACI
sindaci <- read.csv("sindaci_regioni.csv" , sep =',')

# Seleziona solo i Sindaci ed alcuni loro attributi esenziali
# Attributo Nord 1, Sud 0 per indicare l'apparteneza geografica 
sindaci = sindaci %>% filter (sindaci$DESCRIZIONE_CARICA=='Sindaco') %>% 
  select (DESCRIZIONE_COMUNE, ISTAT_CODICE_COMUNE, TITOLO_DI_STUDIO, SESSO , DATA_NASCITA, PARTITO_LISTA_COALIZIONE,nord)
sindaci= sindaci %>%filter(!duplicated(sindaci[,1]))
names(sindaci)[1]='denominazione'

#Prbolem with string formatting
sindaci$denominazione = iconv(sindaci$denominazione, from = 'ISO-8859-1', to = 'utf8')
sindaci$denominazione = tolower(sindaci$denominazione)

#Uniformo caratteri per il join
for (i in 1:length(sindaci$denominazione)){
  sindaci$denominazione[i] = stri_trans_general(sindaci$denominazione[i], "Latin-ASCII")
  sindaci$denominazione[i] = stri_replace_all_regex(sindaci$denominazione[i], "'", "")
  sindaci$denominazione[i] = capitalize(sindaci$denominazione[i])
}
names(sindaci) [2] ='codice Istat'


#POPOLAZIONE censimento anni 2011 e 2018
popolazione2011 = read_excel("Classificazioni statistiche-e-dimensione-dei-comuni_31_12_2011.xls")
popolazione2018 = read_excel("Classificazioni statistiche-e-dimensione-dei-comuni_31_12_2018.xls")

names(popolazione2011)[3] = 'denominazione'
names(popolazione2018)[4] = 'denominazione'

# comuni fusi o incorporati
merged_municipalities = read_excel("Merged_Municiplaities.xlsx")

# uniformo i caratteri delle denominazioni
merged_municipalities$`COMUNI DI RIFERIMENTO`= tolower(merged_municipalities$`COMUNI DI RIFERIMENTO`)
merged_municipalities$`DENOMINAZIONE FUSIONE/FUSIONE PER INCORPORAZIONE` = tolower(merged_municipalities$`DENOMINAZIONE FUSIONE/FUSIONE PER INCORPORAZIONE`)
popolazione2011$denominazione = tolower(popolazione2011$denominazione)
popolazione2018$denominazione = tolower(popolazione2018$denominazione)
names(merged_municipalities)[10] = 'denominazione'
popolazione2011 = popolazione2011[,3:4]
#nel dataset sindaci sono presenti alcuni comuni che poi si sono fusi
sindaci = right_join(merged_municipalities, sindaci, by="denominazione")
for (i in 1:nrow(sindaci)){
  if(is.na(sindaci$PROVINCIA[i])==FALSE){
    sindaci$denominazione[i] = sindaci$`DENOMINAZIONE FUSIONE/FUSIONE PER INCORPORAZIONE`[i]
  }
}
sindaci = sindaci[,-c(1:9)]


#popolazione al 2011 dei comuni fondati dopo
temp = inner_join(merged_municipalities,popolazione2011, by='denominazione')
merged_municipalities = data.frame(distinct(temp,temp$`DENOMINAZIONE FUSIONE/FUSIONE PER INCORPORAZIONE`))
names(merged_municipalities)[1] = "nuova_denominazione"

### ciclo calcola la popolazione dei nuovi comuni risalente al 2011  
for (i in 1:nrow(merged_municipalities)){
  merged_municipalities$popolazione[i] = sum(temp[temp$`DENOMINAZIONE FUSIONE/FUSIONE PER INCORPORAZIONE`==merged_municipalities$nuova_denominazione[i],]$`Popolazione residente al 31/12/2011`)
}

names(merged_municipalities) = names(popolazione2011)
popolazione2011 = data.frame(rbind(popolazione2011,merged_municipalities))

# Join tra popolazione 2011 e popolazione 2018
popolazione = inner_join(popolazione2011,popolazione2018, by='denominazione')


#selezioniamo colonne:
#codice ISTAT del comune
#denominazione
#Popolazione residente al 31/12/2011
#Popolazione residente al 31/12/2018
popolazione = popolazione[,c(1,2,5,7)]
# Media popolazione e differenza 2011 / 2018
popolazione$media = (popolazione$Popolazione.residente.al.31.12.2011 + popolazione$`Popolazione residente al 31/12/2018`)/2
popolazione$diff = (popolazione$`Popolazione residente al 31/12/2018` - popolazione$Popolazione.residente.al.31.12.2011)

# Uniformo caratteri per join con sindaci
popolazione$denominazione = tolower(popolazione$denominazione)
for (i in 1:length(popolazione$denominazione)){
  popolazione$denominazione[i] = stri_trans_general(popolazione$denominazione[i], "Latin-ASCII")
  popolazione$denominazione[i] = stri_replace_all_regex(popolazione$denominazione[i], "'", "")
  popolazione$denominazione[i] = capitalize(popolazione$denominazione[i])
}

names(popolazione)[3] ='codice Istat'
popolazione$`codice Istat` = as.integer(popolazione$`codice Istat`)


# Join tra dataset popolazione e sindaci
df = inner_join(popolazione,sindaci, by='denominazione')

df$DATA_NASCITA = as.Date(df$DATA_NASCITA, format="%Y-%m-%d")
df$data = as.Date('06/06/2017', format = '%d/%m/%Y')
df$eta_sindaco = as.integer(difftime(df$data, df$DATA_NASCITA, units = "weeks")/52.25)

df1 = df[,-c(3,4,10,13)]
names(df1)[5] = "codice Istat"

# Dataset indicatori finanziari
df = read.csv("finanza_comuni.csv", sep=';')
names(df)=c("Ente","Indicatori","TIME","Value")
# restringo il campo ai soli comuni
df$Ente = substring(df$Ente,8)
df = df %>% filter(substr(df$Ente,1,1)=='C')
df$Ente = substring(df$Ente,11)
df$Value=as.numeric(df$Value)

#sostituisco i valori assenti con NA
df$Value[df$Value=='']=NA

## Controlliamo gli indicatori con presenza massiva di zeri
zeri = unique(df$Indicatori)

#conto gli indicatori con valori zeri
for (i in 1:25){
  print (zeri[i])
  print(nrow(df[df$Indicatori==zeri[i]  &df$Value==0 & !is.na(df$Value), ]))
}
zeri = zeri[-25][-23][-20][-19][-17][-16][-15][-14][-11][-10][-9][-7][-4][-3][-2]

#sostituisco gli attributi (zeri) con NA poché non possono essere 0
for (i in 1:10){
  df[df$Indicatori==zeri[i],]$Value = 
    replace(df[df$Indicatori==zeri[i],]$Value,df[df$Indicatori==zeri[i],]$Value==0.000,NA)
}

# elimino duplicati e i dati relativi all'anno 2018
df = df %>% filter (df$TIME!=2018)
df =df %>% filter (!duplicated(df[,1:4]))

#dataset finale con il nome degli Enti, gli indicatori e i valori
df2= data.frame(matrix(ncol = 26, nrow = 0))
for (i in unique(df$Ente)){
  names(df2) = c('Ente', c(unique(df$Indicatori)))
  b=c(i)
  for(j in names(df2[2:26])){
    a = mean(df[df$Ente==i & df$Indicatori==j,]$Value, na.rm=T)
    b=cbind(b,a)
  }
  df2 = data.frame(rbind(df2,b[1,]))
}

#quanti NA ci sono nel df2? 
for (i in 2:ncol(df2)){
  print(names(df2)[i])
  print(nrow(df2 %>% filter (df2[,i]=='NaN')))
}

# selezionati gli indicatori più essenziali per l'analisi
# - grado di autonomia impositiva
# - grado di dipendenza da contributi e trasferimento correnti
# - grado di finanzamento interno
# - grado di autonomia finanziaria
# - rigidità della spesa
# - incidenza spesa personale su spese correnti
# - grado di dipendenza da finanziamento esterno
# - incidenza spese personale su entrate correnti
# - spese per rimborso prestiti in relazione alle entrate correnti
# - indice di consistenza iniziale dei residui passivi
# - capacirà di spesa
df2 = df2[,c(1,2,3,4,5,6,7,8,9,10,11,14)]
df2[,c(2,3,4,5,6,7,8,9,10,11,12)] = lapply (df2[,c(2,3,4,5,6,7,8,9,10,11,12)],as.numeric)

names(df2)[1] = 'denominazione'
df2$denominazione = tolower(df2$denominazione)
for (i in 1:length(df2$denominazione)){
  df2$denominazione[i] = stri_trans_general(df2$denominazione[i], "Latin-ASCII")
  df2$denominazione[i] = stri_replace_all_regex(df2$denominazione[i], "'", "")
  df2$denominazione[i] = capitalize(df2$denominazione[i])
}

regressori = data.frame(inner_join (df2, df1, by='denominazione'))


### Aggiungo Dataset comuni in dissesto/default
df_target <- read_excel("comuni dissesto.xlsx", sheet = "Table 2")

# selezioni solo i comuni
df_target <- df_target %>%
  rename("Tipo" = "Tipo ente", "Stato" = "Dissesto e/ deficitario") %>%
  filter(Tipo == "Comune")


# si uniformano i caratteri
df_target$Ente = tolower(df_target$Ente)
for (i in 1:length(df_target$Ente)){
  df_target$Ente[i] = stri_trans_general(df_target$Ente[i], "Latin-ASCII")
  df_target$Ente[i] = stri_replace_all_regex(df_target$Ente[i], "'", "")
  df_target$Ente[i] = capitalize(df_target$Ente[i])
}
df_target = df_target[, c(2,5)]
names(df_target)[1]='denominazione'



# Dataset con regressori + label class
dataset = df_target %>%
  full_join(regressori, by = "denominazione")


# inserisco 0 nella variabile target:
dataset$Stato = dataset$Stato %>%
  replace_na("0")

# traduco e raggruppo variabili categoriche in variabili numeriche
### Stato: Dissestato = 2
### Deficitario = 1
### Altro = 0
### M =  1
### F =  0
dataset$Stato = str_replace_all(dataset$Stato, c("Deficitario" = "1", "Dissestato" = "2"))
dataset$SESSO = str_replace_all(dataset$SESSO, c("M" = "1", "F" = "0"))
dataset$TITOLO_DI_STUDIO = str_replace_all(dataset$TITOLO_DI_STUDIO,c("NESSUN TITOLO DI STUDIO POSSEDUTO"  = "0",   
                                                                      "LICENZA ELEMENTARE"  = "1",
                                                                      "TITOLI O DIPLOMI PROFESSIONALI POST LIC.ELEMENTARE" = "2",
                                                                      "LICENZA MEDIA INFERIORE"     = "2",
                                                                      "LICENZA DI SCUOLA MEDIA INF. O TITOLI EQUIPOLLENTI" = "2",
                                                                      "LICENZA DI SCUOLA MEDIA SUP. O TITOLI EQUIPOLLENTI" = "3",
                                                                      "LICENZA MEDIA SUPERIORE"   ="3",
                                                                      "TITOLI O DIPLOMI PROFESSIONALI POST MEDIA INFER."   = "3",
                                                                      "TITOLI O DIPLOMI PROFESSIONALI POST MEDIA SUPER."  = "3",
                                                                      "LAUREA  BREVE"      = "4",
                                                                      "LAUREA"          = "4",
                                                                      "SPECIALIZZAZIONE POST LAUREA / DOTTORATO DI RICERCA" = "5",
                                                                      "ALTRI TITOLI  POST-LAUREA"  = "5"))
dataset$SESSO = as.factor(dataset$SESSO)
dataset$Stato = as.factor(dataset$Stato)
dataset$TITOLO_DI_STUDIO = as.factor(dataset$TITOLO_DI_STUDIO)
dataset$nord = as.factor(dataset$nord)


# elimino duplicati
dataset= dataset %>%filter(!duplicated(dataset[,c(1)]))


### Aggiungo FSC E IRPEF:
# importo dataset per fondo di solidariet� comunale:
dati = read.csv("2017_VAR_FSC_1_2022.csv",  header=TRUE, sep=";", dec=",", stringsAsFactors=FALSE)
meta1 = read_excel("2017_Metadati_FSC_1_2022.xlsx")
meta2 = read_excel("Metadati_Enti_FSC_2022.xlsx")

# creo pivot con la variabile Fondo di Solidarietà 
dati1 = dati %>% pivot_wider(names_from = VAR_FSC_NAME, values_from = VAR_FSC_VAL)
dati2 = dati1 %>% inner_join(meta2, by = "USERNAME") %>% filter(ENTE_TIPOLOGIA == "COMUNE")
# seleziono attributi esenziali
dati2$denominazione = tolower(dati2$ENTE)
dati2 = dati2[, -c(27:32,34:38)]

# si uniformano i caratteri

for (i in 1:nrow(dati2)){
  dati2$denominazione[i] = stri_trans_general(dati2$denominazione[i], "Latin-ASCII")
  dati2$denominazione[i] = stri_replace_all_regex(dati2$denominazione[i], "'", "")
  dati2$denominazione[i] = capitalize(dati2$denominazione[i])
}

# filtro colonne di interesse:
# solo FONDO STORICO (per mantenere inalterate risorse storiche dal passaggio ICI a IMU/TARI/TASI)
# e FONDO PEREQUATIVO (Fabbisogno standard - Capacit� fiscale, per assicurare risorse a tutti i comuni)
dati2$"codice istat" = as.integer(dati2$COMUNE_ISTAT_COD)
dati2 = dati2 %>% select(`codice istat`, denominazione, FONDO_STORICO, FONDO_PEREQUATIVO)

dataset$"codiceistat" = as.integer(dataset$codice.Istat)

## IRPEF come proxy per povert� dei cittadini del comune:

irpef = read_excel("Irpef.xlsx")

# si uniformano i caratteri
irpef$denominazione = tolower(irpef$denominazione)
for (i in 1:nrow(irpef)){
  irpef$denominazione[i] = stri_trans_general(irpef$denominazione[i], "Latin-ASCII")
  irpef$denominazione[i] = stri_replace_all_regex(irpef$denominazione[i], "'", "")
  irpef$denominazione[i] = capitalize(irpef$denominazione[i])
}

# seleziono l'attributo Reddito Imponibile individuale
irpef = irpef %>% select(denominazione, "Sigla Provincia","Reddito imponibile individuale")
dati3 = inner_join(dataset, irpef, by = "denominazione")

dati3=dati3[,-23]

# mettiamo insieme i dataset con info sociali ed economici 
data_final = inner_join(dati3, dati2, by = "denominazione")
data_finals = inner_join(dati2, dati3, by = "denominazione")
data_finalss = full_join(dati2, dati3, by = "denominazione")
# eliminiamo i duplicati usciti dal join
data_final = data_final %>% filter(!duplicated(data_final[,1]))
data_final=data_final[,-6] #elimino colonna duplicata codice.istat

## sostituisco gli na delle province con valori reali di data_final.csv

data_final$Sigla.Provincia = data_final$`Sigla Provincia` %>% replace_na("NA")
# DATASET DEFINITIVO CON TUTTE LE VARIABILI:
write.csv(data_final, "data_final.csv", row.names = FALSE)
write.csv(data_finalss, "data_finalss.csv", row.names = FALSE)





