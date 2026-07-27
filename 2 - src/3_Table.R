
# Table 1

rm(list=ls())

library(tidyverse)
library(openxlsx)

# Import files from previous steps

real1            <- read.xlsx("df_decomposicao.xlsx")
rotulo_traduzido <- read.xlsx("CodigoCID10.xlsx", sheet = "Ingles")  


total <- real1 %>% 
  filter(GRUPO_IDADE == "Total") %>% 
  pivot_longer(names_to = "YEAR", values_to = "CHANGE", cols= -c("SEXO","LOCAL", "CAUSA", "GRUPO_IDADE")) %>% 
  rename(REFERENCIA = GRUPO_IDADE)


grupos <- real1 %>% 
  filter(GRUPO_IDADE != "Total") %>% 
  mutate(GRUPO_IDADE = as.numeric(GRUPO_IDADE)) %>% 
  pivot_longer(names_to = "YEAR", values_to = "CHANGE", cols= -c("SEXO","LOCAL", "CAUSA", "GRUPO_IDADE")) %>% 
  mutate(REFERENCIA = ifelse(GRUPO_IDADE < 15, "0-14", ifelse(GRUPO_IDADE > 64, "65+", "15-64"))) %>% 
  group_by(SEXO,LOCAL, CAUSA, YEAR, REFERENCIA) %>% 
  summarize(CHANGE = sum(CHANGE)) %>% 
  ungroup()

todos <- rbind(total,grupos) %>% filter(LOCAL == "Brasil", SEXO == "Ambos")


## Life expectancy changes


base1 <- todos %>% 
  pivot_wider(names_from = "YEAR", values_from = "CHANGE") %>% 
  left_join(rotulo_traduzido, by = c("CAUSA" = "Categoria.CID")) %>% 
  select(SEXO,LOCAL, CAUSA, REFERENCIA, Category, `2001`: `2024`) %>% 
  unique() %>%                            
  pivot_longer(names_to = "YEAR", values_to = "CHANGE", cols = -c("SEXO","LOCAL", "CAUSA", "Category", "REFERENCIA")) %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  select(REFERENCIA, SEXO, LOCAL,  Category, YEAR, CHANGE) %>% 
  group_by(REFERENCIA, SEXO,LOCAL, Category) %>% 
  summarize(CHANGE = sum(CHANGE)) %>% 
  ungroup() %>% 
  select(REFERENCIA:LOCAL,everything()) %>% 
  arrange(LOCAL,REFERENCIA, desc(CHANGE)) 


## Top causes of death

top7 <- base1 %>% 
  group_by(LOCAL,REFERENCIA, SEXO) %>% 
  top_n(7) %>% 
  ungroup()

menos_top7 <- base1 %>%
  anti_join(top7, by = c("LOCAL", "REFERENCIA", "SEXO", "Category", "CHANGE")) %>% 
  group_by(LOCAL, REFERENCIA, SEXO) %>%
  summarise(
    Category = "Others",
    CHANGE = sum(CHANGE),
    .groups = "drop"
  ) 

top7_others <- rbind(top7, menos_top7) %>% 
  arrange(REFERENCIA, SEXO, LOCAL) %>% 
  group_by(REFERENCIA, SEXO, LOCAL) %>% 
  mutate(PERC = CHANGE / sum(CHANGE))


total <- top7_others %>% 
  group_by(REFERENCIA, SEXO, LOCAL) %>% 
  summarize(CHANGE = sum(CHANGE)) %>% 
  arrange(REFERENCIA, SEXO, LOCAL) %>% 
  mutate(
    GRUPO = "Total",
    CODIGO = 99,
    Category = "Total",
    PERC = 1
  ) %>% 
  select(REFERENCIA, SEXO, LOCAL, Category, CHANGE, PERC)

total_top7_others <- rbind(total, top7_others) %>% 
  arrange(REFERENCIA, SEXO, LOCAL,  desc(PERC)) 


# Export table 1

write.xlsx(total_top7_others, "Table1.xlsx")

