rm(list=ls())
library(tidyverse)
library(brazilmaps)
library(readr)
library(readxl)
library(sf)

CODIGOS <- read_csv("CODIGOS.csv") 

dados<-read_csv("dados_2018_2turno_BRANCOSeNULOS.csv") %>% 
  mutate(PROP_PSL=`JAIR BOLSONARO`/VOTOS_NOMINAIS) %>%
  mutate(PROP_PSL_BN=`JAIR BOLSONARO`/(VOTOS_NOMINAIS + QT_VOTOS_BRANCOS + QT_VOTOS_NULOS)) %>%
  mutate(des2018 =  case_when(0 < PROP_PSL  & PROP_PSL < .4999999 ~ "Fernando Haddad",
                              .5 < PROP_PSL  & PROP_PSL  < 1 ~ "Jair Bolsonaro")) 

BR<-get_brmap("City")

map2018 <- BR %>% left_join(dados, c("City" = "codigo_ibge")) %>%
  ggplot() +
  geom_sf(aes(fill = des2018), size = 0.00000001) +  
  scale_fill_manual(name = "Winning candidate", values = c("blue","orange")) +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

map2018


