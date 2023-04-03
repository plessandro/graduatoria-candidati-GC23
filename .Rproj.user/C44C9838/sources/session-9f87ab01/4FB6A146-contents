# test Panachage elezioni 2023 GC

rm(list = ls())

library(tidyverse)
library(rvest)
library(colorspace)


url <- paste0(getwd(), "/Candidati.htm")

tabelle<-read_html(url) %>% 
  html_table()

lista<-list()

for (i in 1:14) {


capre<-tabelle[[i]]

capre<-replace(capre, capre=='', NA)


names(capre)<-capre[1,1:ncol(capre)]

names(capre)[22]<-"TOT. ALTRE LISTE"

capre<-capre[2:nrow(capre),]


# capire partito

nomi_partiti <- names(capre)[5:21]

capre$PARTITO<-nomi_partiti[i]


# rimuovere tutti i Totali

capre<-capre %>% select(everything(), -c(4,22,23)) %>% 
  filter(! row_number() == n()) %>% 
  relocate(PARTITO, .after = "NOMINATIVO")


capre_long<-reshape2::melt(capre, id.var=c("NOMINATIVO", "PARTITO"))

capre_long<-as.data.frame(capre_long)

capre_long$value<-str_replace(capre_long$value, "'","")

capre_long$value<-as.numeric(capre_long$value)

capre_long <- capre_long %>% 
  group_by(NOMINATIVO) %>% 
  mutate(value_perc = value / sum(value, na.rm = T)) %>% 
  ungroup()


# colori di partito

info_partiti<-capre_long %>% 
  select(variable) %>% 
  distinct() %>% 
  mutate(colori = c(
    "#666666", # BASE
    "#a0a0a0", # PREF.
    "#0E52A0", # PLR
    "#cf2e2e", # MPS
    "#b4dc00", # Verdi Liberali
    "#0693e3", # Più Donne
    "#dd3333", # PC
    "#e5be01", # AVanti con Ticino & Lavoro
    "#ff6900", # DIE FRITTE
    "#0d730d", # HelvEthica
    "#009f4f", # UDC
    "#e52e32", # PS-GISO
    "#7200cc", # LEGA
    "#84b414", # VERDI
    "#511401", # Dignità ai pentsionati
    "#198754", # Montagna Viva
    "black" # SSI
  ))


# aggiungere colori

capre_long <- capre_long %>% 
  left_join(., info_partiti, by="variable")


lista[[i]] <- capre_long


rm(capre_long, capre)


}


# unire elementi lista in un unico df


df_full<- do.call("rbind", lista)


# creare variabile che informa su candidato E partito

df_full<-df_full %>% 
  mutate(candidato_e_partito = paste(NOMINATIVO, PARTITO, sep= " – ")) %>% 
  relocate(candidato_e_partito, .after = "PARTITO")


# export (per usare poi in shiny)

save(df_full, file = "df_full.Rdata")



