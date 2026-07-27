
# Figure 2


rm(list=ls())
options(scipen=999)

library(tidyverse)
library(openxlsx)
library(ggbreak)
library(ggtern)
library(ggbreak)
library(patchwork)
 

# Import and process data

real1            <- read.xlsx("df_decomposicao.xlsx")
rotulo_traduzido <- read.xlsx("CodigoCID10.xlsx", sheet = "Ingles")   


total <- real1 %>% 
  filter(GRUPO_IDADE == "Total") %>% 
  pivot_longer(names_to = "YEAR", values_to = "CHANGE", cols= -c("LOCAL","SEXO", "CAUSA", "GRUPO_IDADE")) %>% 
  rename(REFERENCIA = GRUPO_IDADE)


grupos <- real1 %>% 
  filter(GRUPO_IDADE != "Total") %>% 
  mutate(GRUPO_IDADE = as.numeric(GRUPO_IDADE)) %>% 
  pivot_longer(names_to = "YEAR", values_to = "CHANGE", cols= -c("LOCAL","SEXO", "CAUSA", "GRUPO_IDADE")) %>% 
  mutate(REFERENCIA = ifelse(GRUPO_IDADE < 15, "0-14", ifelse(GRUPO_IDADE > 64, "65+", "15-64"))) %>% 
  group_by(LOCAL, SEXO, CAUSA, YEAR, REFERENCIA) %>% 
  summarize(CHANGE = sum(CHANGE)) %>% 
  ungroup()

todos <- rbind(total,grupos)


# Plot COVID-19 

graf_covid <- todos %>% 
  filter(CAUSA == "COVID-19", REFERENCIA == "Total" & SEXO == "Ambos" & YEAR %in% c(2020,2021)) %>% 
  filter(!LOCAL %in% c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
  group_by(LOCAL) %>% 
  summarize(CHANGE = sum(CHANGE)) %>% 
  mutate(
    LOCAL = case_when(
      LOCAL == "Brasil" ~ "Brazil",
      LOCAL == "Sul" ~ "South",
      LOCAL == "Sudeste" ~ "Southeast",
      LOCAL == "Centro-Oeste" ~ "Central-West",
      LOCAL == "Norte" ~ "North",
      LOCAL == "Nordeste" ~ "Northeast",
      TRUE ~ LOCAL
    ),
    cor_brasil = ifelse(LOCAL == "Brazil", "Brazil", "Other"),
    cor_regiao = case_when(
      LOCAL %in% c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins") ~ "North",
      LOCAL %in% c("Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe","Bahia") ~ "Northeast",
      LOCAL %in% c("Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo") ~ "Southeast",
      LOCAL %in% c("Paraná","Santa Catarina","Rio Grande do Sul") ~ "South",
      LOCAL %in% c("Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal") ~ "Central-West",
      TRUE ~ "Brazil"
    ),
    rotulo = paste0(LOCAL, ", ", format(round(CHANGE, 1), nsmall = 1), " years")
  ) %>% 
  
  ggplot(aes(x = reorder(rotulo, CHANGE), y = CHANGE, fill = cor_regiao)) +
  
  geom_bar(stat = "identity", width = 0.9) +
  geom_text(aes(label = format(round(CHANGE, 1), nsmall = 1)), hjust = 1.3, size = 3) +
  coord_flip()+
  theme_classic()+
  labs(y = "Change in life expectancy (in years)", 
       x = "")+
  scale_y_continuous(limits = c(-6, 0), breaks = seq(-6,0,1))+
  scale_fill_manual(values = c(
    "North"        = "#F4A261",  # warm peach
    "Northeast"    = "#E9C46A",  # golden sand
    "Southeast"    = "#90BE6D",  # fresh green
    "South"        = "#4ECDC4",  # aqua blue
    "Central-West" = "#F4978E",  # coral rose
    "Brazil"       = "#264653"   # deep blue for highlight
  ))+
  
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(color = "grey"),
    axis.line.x = element_line(color = "grey"),
    axis.line.y = element_line(color = "grey"),
    axis.title.x = element_text(size = 8.5),
    axis.text.x = element_text(size = 8.5, color = "grey15"),
    axis.text.y = element_text(size = 7.5)
  )
graf_covid

ggsave(file = "Fig2.pdf", plot = graf_covid, 
      width = 250, height = 150, units = "mm", dpi = 400)


# Pneumonia - 2022

graf_pneumonia_2022 <- todos %>% 
  filter(CAUSA == "Pneumonia", REFERENCIA == "Total" & SEXO == "Ambos" & YEAR %in% c(2022)) %>% 
  filter(!LOCAL %in% c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
  group_by(LOCAL) %>% 
  summarize(CHANGE = sum(CHANGE)) %>% 
  mutate(
    LOCAL = case_when(
      LOCAL == "Brasil" ~ "Brazil",
      LOCAL == "Sul" ~ "South",
      LOCAL == "Sudeste" ~ "Southeast",
      LOCAL == "Centro-Oeste" ~ "Central-West",
      LOCAL == "Norte" ~ "North",
      LOCAL == "Nordeste" ~ "Northeast",
      TRUE ~ LOCAL
    ),
    cor_brasil = ifelse(LOCAL == "Brazil", "Brazil", "Other"),
    cor_regiao = case_when(
      LOCAL %in% c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins") ~ "North",
      LOCAL %in% c("Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe","Bahia") ~ "Northeast",
      LOCAL %in% c("Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo") ~ "Southeast",
      LOCAL %in% c("Paraná","Santa Catarina","Rio Grande do Sul") ~ "South",
      LOCAL %in% c("Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal") ~ "Central-West",
      TRUE ~ "Brazil"
    ),
    CHANGE = round(CHANGE, 3),
    rotulo = paste0(LOCAL, ", ", format(round(CHANGE, 1), nsmall = 1), " years")
  ) %>% 
  
  ggplot(aes(x = reorder(rotulo, CHANGE), y = CHANGE, fill = cor_regiao)) +
  
  geom_bar(stat = "identity", width = 0.9) +
  geom_text(aes(label = format(round(CHANGE, 3), nsmall = 1)), hjust = 1.3, size = 3) +
  coord_flip()+
  theme_classic()+
  labs(y = "Change in life expectancy (in years)", 
       x = "")+
  scale_y_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.3,0.3,0.1),
                     labels = scales::label_number(accuracy = 0.1))+
  scale_fill_manual(values = c(
    "North"        = "#F4A261",  # warm peach
    "Northeast"    = "#E9C46A",  # golden sand
    "Southeast"    = "#90BE6D",  # fresh green
    "South"        = "#4ECDC4",  # aqua blue
    "Central-West" = "#F4978E",  # coral rose
    "Brazil"       = "#264653"   # deep blue for highlight
  ))+
  
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(color = "grey"),
    axis.line.x = element_line(color = "grey"),
    axis.line.y = element_line(color = "grey"),
    axis.title.x = element_text(size = 8.5),
    axis.text.x = element_text(size = 8.5, color = "grey15"),
    axis.text.y = element_text(size = 7.5)
  )
graf_pneumonia_2022


ggsave(file = "Fig4a.pdf", plot = graf_pneumonia_2022, 
       width = 250, height = 150, units = "mm", dpi = 400)

# Pneumonia - 2024

graf_pneumonia_2024 <- todos %>% 
  filter(CAUSA == "Pneumonia", REFERENCIA == "Total" & SEXO == "Ambos" & YEAR %in% c(2024)) %>% 
  filter(!LOCAL %in% c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
  group_by(LOCAL) %>% 
  summarize(CHANGE = sum(CHANGE)) %>% 
  mutate(
    LOCAL = case_when(
      LOCAL == "Brasil" ~ "Brazil",
      LOCAL == "Sul" ~ "South",
      LOCAL == "Sudeste" ~ "Southeast",
      LOCAL == "Centro-Oeste" ~ "Central-West",
      LOCAL == "Norte" ~ "North",
      LOCAL == "Nordeste" ~ "Northeast",
      TRUE ~ LOCAL
    ),
    cor_brasil = ifelse(LOCAL == "Brazil", "Brazil", "Other"),
    cor_regiao = case_when(
      LOCAL %in% c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins") ~ "North",
      LOCAL %in% c("Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe","Bahia") ~ "Northeast",
      LOCAL %in% c("Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo") ~ "Southeast",
      LOCAL %in% c("Paraná","Santa Catarina","Rio Grande do Sul") ~ "South",
      LOCAL %in% c("Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal") ~ "Central-West",
      TRUE ~ "Brazil"
    ),
    CHANGE = round(CHANGE, 3),
    rotulo = paste0(LOCAL, ", ", format(round(CHANGE, 1), nsmall = 1), " years")
  ) %>% 
  
  ggplot(aes(x = reorder(rotulo, CHANGE), y = CHANGE, fill = cor_regiao)) +
  
  geom_bar(stat = "identity", width = 0.9) +
  geom_text(aes(label = format(round(CHANGE, 3), nsmall = 1)), hjust = 1.3, size = 3) +
  coord_flip()+
  theme_classic()+
  labs(y = "Change in life expectancy (in years)", 
       x = "")+
  scale_y_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.3,0.3,0.1),
                     labels = scales::label_number(accuracy = 0.1))+
  scale_fill_manual(values = c(
    "North"        = "#F4A261",  # warm peach
    "Northeast"    = "#E9C46A",  # golden sand
    "Southeast"    = "#90BE6D",  # fresh green
    "South"        = "#4ECDC4",  # aqua blue
    "Central-West" = "#F4978E",  # coral rose
    "Brazil"       = "#264653"   # deep blue for highlight
  ))+
  
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(color = "grey"),
    axis.line.x = element_line(color = "grey"),
    axis.line.y = element_line(color = "grey"),
    axis.title.x = element_text(size = 8.5),
    axis.text.x = element_text(size = 8.5, color = "grey15"),
    axis.text.y = element_text(size = 7.5)
  )
graf_pneumonia_2024

ggsave(file = "Fig4b.pdf", plot = graf_pneumonia_2024, 
       width = 250, height = 150, units = "mm", dpi = 400)