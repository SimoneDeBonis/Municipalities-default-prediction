# importo dataset per fondo di solidariet? comunale:
dati = read.csv("2017_VAR_FSC_1_2022.csv", sep = ";")
meta1 = read_excel("2017_Metadati_FSC_1_2022.xlsx")
meta2 = read_excel("Metadati_Enti_FSC_2022.xlsx")

unique(dati$VAR_FSC_NAME)
length(unique(dati$USERNAME))


dati1 = dati %>% pivot_wider(names_from = VAR_FSC_NAME, values_from = VAR_FSC_VAL)
dati2 = dati1 %>% inner_join(meta2, by = "USERNAME") %>% filter(ENTE_TIPOLOGIA == "COMUNE")
dati2 = dati2[, -c(34:38)]
dati2 = dati2[, -c(30:32)]
dati2 = dati2[, -c(27:28)]

dati2$denominazione = tolower(dati2$ENTE)
for (i in 1:nrow(dati2)){
  dati2$denominazione[i] = stri_trans_general(dati2$denominazione[i], "Latin-ASCII")
  dati2$denominazione[i] = stri_replace_all_regex(dati2$denominazione[i], "'", "")
  dati2$denominazione[i] = capitalize(dati2$denominazione[i])
}

# filtro colonne di interesse:
# solo FONDO STORICO (per mantenere inalterate risorse storiche dal passaggio ICI a IMU/TARI/TASI)
# e FONDO PEREQUATIVO (Fabbisogno standard - Capacit? fiscale, per assicurare risorse a tutti i comuni)
dati2$"codice istat" = as.integer(dati2$COMUNE_ISTAT_COD)
dati2 = dati2 %>% select(`codice istat`, denominazione, FONDO_STORICO, FONDO_PEREQUATIVO)

## importo dataset con tutti gli altri dati:
dataset = read.csv("dataset.csv", sep = ",", check.names = FALSE)[-1]

dataset$"codice istat" = as.integer(dataset$codice.istat)

join = left_join(dataset, dati2, by = "denominazione")

# sostituisco NA con 0 per i comuni senza fondo di solidariet?
join$FONDO_STORICO = join$FONDO_STORICO %>%
  replace_na(0)
join$FONDO_PEREQUATIVO = join$FONDO_PEREQUATIVO %>%
  replace_na(0)

## IRPEF come proxy per povert? dei cittadini del comune:

irpef = read.csv("Irpef.csv", sep = ";", check.names = FALSE)

irpef$denominazione = tolower(irpef$denominazione)
for (i in 1:nrow(irpef)){
  irpef$denominazione[i] = stri_trans_general(irpef$denominazione[i], "Latin-ASCII")
  irpef$denominazione[i] = stri_replace_all_regex(irpef$denominazione[i], "'", "")
  irpef$denominazione[i] = capitalize(irpef$denominazione[i])
}

irpef = irpef %>% select(denominazione, "Reddito imponibile individuale")

data = inner_join(dataset, irpef, by = "denominazione")

data = data %>% filter(!duplicated(data[,1]))

write.csv(data, "data.csv", row.names = FALSE)
