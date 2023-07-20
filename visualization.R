rm(list=ls())

library(rstudioapi)
library(tidyverse)
library(scales)
library(ggcorrplot)
library(GGally)
library(statsr)
library(sf)
library(Hmisc)
library(stringi)
library(giscoR)
library(Rmisc)
library(ggpubr)
library(sf)

# set working directory
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
print(current_path)

#import data
data = read.csv("dataset.csv", sep = ",")

# transform variables in factors
data$status = factor(data$status, labels = c("other", "deficit", "default"))
data$gender = factor(data$gender, labels = c("F", "M"))
data$degree = factor(data$degree, labels = c("no degree", "elementary", "middle", "high school", "university", "master"))

# compute chi-square test to check variables dependence (sex/status)
inference(y = status, x = sex, data = data, statistic = "proportion", type = "ht", 
          alternative = "greater", method = "theoretical", show_inf_plot = FALSE)

# dataframe with proportion between gender and status, excluding "other" status to plot 
gender_prop = data %>% select(gender, status) %>%  table() %>% prop.table(margin = 1) %>% data.frame() %>% filter(status != "other") %>% print()

# define theme for plots
thm = list(theme_clean(), scale_fill_manual(values = c("#ececff", "#ffa31a", "#456A85")),
           theme(axis.title.y=element_blank()))

# plot relative frequency about gender of the mayor and status of the town, excluding "other"
gender_prop %>%
  ggplot(aes(fill = gender)) +
  geom_col(aes(x= status, y = Freq), position = "dodge") +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Gender of the mayor and Status of the Municipality",
       subtitle = "Given 100 male or female mayors, how much administer a default or deficit municipality (%)?",
       caption = "Source: Ministero dell'Interno") + thm

# dataframe with proportion between degree and status, excluding "other" status to plot 
degree_prop = data %>% select(degree, status) %>% drop_na(degree) %>% table() %>% prop.table(margin = 1) %>% data.frame() %>% filter(status != "other") %>% print()

# plot relative frequency about degree of the mayor and status of the town, excluding "other"
degree_prop %>%
  ggplot(aes(fill = status)) +
  geom_col(aes(x= degree, y = Freq), position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Degree of the mayor and Status of the Municipality",
       subtitle = "Given 100 mayors with a specific degree, how much administer a default or deficit municipality (%)?",
       caption = "Source: Ministero dell'Interno") + thm

# compute chi-square test to check variables dependence (degree/status)
inference(y = status, x = degree, data = data, statistic = "proportion", type = "ht", 
          alternative = "greater", method = "theoretical", show_inf_plot = FALSE)

###  MAKE SENSE?
#eta = data %>%
#  drop_na(eta_sindaco) %>%
#  ggplot(aes(x = status, y = eta_sindaco, fill = status)) + 
#  geom_boxplot(aes(fill = status)) +
#  theme(panel.background = element_rect(fill = '#eff4f4'),
#        plot.background = element_rect(fill = "#eff4f4"))
#eta

# conditional mean of citizens given status
aggregate(data$citizen, list(data$status), FUN = mean, na.rm = TRUE)

# plot conditional mean 
data %>%
  ggplot(aes(x = status, y = citizen, fill=status)) +
  geom_boxplot(outlier.shape = NA)  +
  coord_cartesian(ylim = quantile(data$media, c(0, 0.97), na.rm = TRUE))+
  labs(title = "Citizens per town, given its Status",
       subtitle = "(Plot considers only smaller municipalities, excluding big cities)",
       caption = "Source: Istat") +
  thm

# conditional mean of individual income given status
aggregate(data$individual_income, list(data$status), FUN = mean, na.rm = TRUE)

data %>%
  ggplot(aes(x = individual_income, fill=status)) +
  geom_density(alpha = 0.7) +
  labs(title = "Individual Income in mean per town, given Status",
       subtitle = "Deficit and Default Municipalities are poorer or whealtier, in mean?",
       caption = "Source: Ministero dell'Economia e delle Finanze") + thm

# correlation matrix and plot between financial and demographic variables
corr = data %>% select_if(is.numeric) %>% select(-c(1,3,4,5,17)) %>% drop_na %>% as.matrix() %>% cor()

corr %>% ggcorrplot(type = "full", ggtheme = ggthemes::theme_clean, lab = TRUE,
                   colors = c("#ffa31a", "#CFDDE8", "#456A85"), lab_col = "black",  
                   lab_size = 2.5, outline.color = "white", tl.cex = 10) +
  labs(title = "Correlation between Financial and Demographic Variables", 
       caption = "Sources: Istat, Ministero dell'Economia e delle Finanze") + 
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))


### GGPAIR tra status ed INDICATORI FINANZIARI!!! ###

# a = data[, c(2, 11,12,13,14,15,16,17,18,19,20,21)]
# ggpairs(a, aes(color = status))

## variabili interessanti:
# 1) Rigidit? della spesa (6)
# 2) Grado di autonomia finanziaria (5)
# 3) Dipendenza da finanziamenti esterni (8)
# 4) Spese per rimborso prestiti in relazione alle entrate correnti (10)
# 5) Capacit? di spesa (12)

# compare 4 relevant financial indicators per status

str_rig = data %>% drop_na(rigiditÃ..della.spesa) %>%
  ggplot(aes(x=rigiditÃ..della.spesa, fill = status)) +
  geom_density(alpha=0.7) +
  labs(title = "Degree of Structural Rigidity per Status",
       subtitle = "Degree of Structural Rigidity = (Personnel Expenses + Loan Repayments) / Current Revenue",
       caption = "Source: Istat") + thm +
  theme(plot.subtitle=element_text(size=11))
str_rig

dep_extfin = data %>% drop_na(grado.di.dipendenza.da.finanziamento.esterno) %>%
  ggplot(aes(x=grado.di.dipendenza.da.finanziamento.esterno, fill = status)) +
  geom_density(alpha=0.7) +
  labs(title = "Degree of Dependence on External Financing per Status",
       subtitle = "D. of D. on E. F. = Current Transfers / (Current Revenue + Current Transfers + Non Tributary Revenue)",
       caption = "Source: Istat") + thm +
  theme(plot.subtitle=element_text(size=11))
dep_extfin

cap_exp = data %>% drop_na(capacitÃ..di.spesa) %>%
  ggplot(aes(x = capacitÃ..di.spesa , fill= status)) +
  geom_density(alpha=0.7) +
  labs(title = "Capacity of Expense per Status",
       subtitle = "Capacity of Expense = Payments in Accrual Account / Commitments",
       caption = "Source: Istat") + thm +
  theme(plot.subtitle=element_text(size=11))
cap_exp

loan_rep = data %>% drop_na(spese.per.rimborso.prestiti.in.relazione.alle.entrate.correnti) %>%
  ggplot(aes(x=spese.per.rimborso.prestiti.in.relazione.alle.entrate.correnti, fill = status)) +
  geom_density(alpha=0.7) +
  labs(title = "Loan Repayments on Current Revenue per Status",
       subtitle = "L. R. on C. R. = Loan Repayments / (Current Revenue + Current Transfers + Non Tributary Revenue)",
       caption = "Source: Istat") + thm +
  theme(plot.subtitle=element_text(size=11))
loan_rep

ggarrange(str_rig, dep_extfin, cap_exp, loan_rep, ncol=2, nrow=2, common.legend = TRUE, legend="top")

# import geographical data to create a map
ita = read_sf("Com01012016_g_WGS84.shp")

colnames(ita)[7] = "istat.code"
colnames(ita)[8] = "denomination" #### column in "data" should have the same name!!! ####

# prepare "ita" dataframe to join with "data"
ita$denomination = tolower(ita$denomination)
for (i in 1:nrow(ita)){
  ita$denomination[i] = stri_trans_general(ita$denomination[i], "Latin-ASCII")
  ita$denomination[i] = stri_replace_all_regex(ita$denomination[i], "'", "")
  ita$denomination[i] = capitalize(ita$denomination[i])
}

#
#data$denominazione = tolower(data$denominazione)
#for (i in 1:nrow(ita)){
#  data$denominazione[i] = stri_trans_general(data$denominazione[i], "Latin-ASCII")
#  data$denominazione[i] = stri_replace_all_regex(data$denominazione[i], "'", "")
#  data$denominazione[i] = capitalize(data$denominazione[i])
#}

# temporary dataframe with italian geographic data
temp = left_join(ita, data, by = "denomination")
temp$Stato = temp$Stato %>% replace_na(0) %>% cut(breaks = 3, labels=c("Altro", "Deficit", "Dissesto"))

# import italian region boundaries and transform coordinates in european coordinate reference system (crs = 3035)
nuts2 = gisco_get_nuts(year = 2016, resolution = 20, country = c("Italy"), nuts_level = 2) %>%
  select(NUTS_ID, NAME_LATN, geometry) 
nuts2_3035 = st_transform(nuts2, 3035)

# import also non-italian countries for the background of the map
backg = gisco_get_countries(year = 2016,resolution = 20) %>%
  st_transform(3035)

# transform coordinates in "temp" dataframe in european coordinates (crs = 3035)
data_map = st_transform(temp, 3035) 

ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA) +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = status), color = NA) +
  scale_fill_manual(values = c("#ececff", "#ffa31a", "#456A85")) +
  geom_sf(data = nuts2_3035, fill = NA) +
  theme_void() +
  labs(title = "Italian Municipalities by Status",
       caption = "Sources: Istat, Ministero dell'Interno") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(legend.position = c(0.85, 0.87),
        legend.title = element_text(size = 12, face = "bold"),
        legend.background = element_rect(fill = "white", colour = "white"),
        legend.key.size = unit(0.6, units = "cm"),
        legend.box.background = element_rect(colour = "black", size = 1),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.key=element_rect(colour="black"),
        legend.text = element_text(size = 10),
        legend.spacing.y = unit(0.2, 'lines'))
