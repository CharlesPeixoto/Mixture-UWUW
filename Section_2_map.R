rm(list=ls())
library(tidyverse)
library(brazilmaps)
library(readr)
setwd("~/rrg1@de.ufpe.br/Bruce/R/Dados Brutos/pre processamento 2018")



library(readxl)
CODIGOS <- read_excel("CODIGOS.xlsx") 

### mapa dos municipios onde o bolonaro ganha
dados<-read_csv("dados_2018_2turno_BRANCOSeNULOS.csv") %>% 
  mutate(PROP_PSL=`JAIR BOLSONARO`/VOTOS_NOMINAIS) %>%
  mutate(PROP_PSL_BN=`JAIR BOLSONARO`/(VOTOS_NOMINAIS+QT_VOTOS_BRANCOS+QT_VOTOS_NULOS)) %>%
  mutate(des2018 =  case_when(0 < PROP_PSL  & PROP_PSL < .4999999 ~ "Votos PT",
                              .5 < PROP_PSL  & PROP_PSL  < 1 ~ "Votos PSL")) 


BR<-get_brmap("City")

BR %>% left_join(dados, c("City" = "codigo_ibge")) %>%
  ggplot() +
  geom_sf(aes(fill = des2018), size = 0.01) +  
  scale_fill_manual(name = "% Votos PSL",values = c("lightblue","red")) +
  theme(panel.background = element_rect(fill = "white", colour = "white"))
