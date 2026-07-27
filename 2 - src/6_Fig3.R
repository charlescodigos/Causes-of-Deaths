
# Figure 3

rm(list=ls())

library(tidyverse)
library(openxlsx)
library(ggbreak)
library(ggtern)
library(patchwork)


real1            <- read.xlsx("df_decomposicao.xlsx")
rotulo_traduzido <- read.xlsx("CodigoCID10.xlsx", sheet = "Ingles")   
rotulo           <- read.xlsx("CodigoCID10.xlsx", sheet = "Grupo")   


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


#######################################
#         LE at birth                 #
#######################################


base1 <- todos %>% 
  filter(REFERENCIA == "Total" & SEXO == "Ambos" & LOCAL == "Brasil") %>% 
  pivot_wider(names_from = "YEAR", values_from = "CHANGE") %>% 
  left_join(rotulo_traduzido, by = c("CAUSA" = "Categoria.CID")) %>% 
  left_join(rotulo, by = c("Codigo.Categoria.CID")) %>% 
  
  select(LOCAL,GRUPO, SEXO, CAUSA, Category, `2001`: `2024`) %>% 
  filter(GRUPO == "Communicable") %>% 
  unique() %>%                             
  
  pivot_longer(names_to = "YEAR", values_to = "CHANGE", cols = -c("LOCAL","GRUPO", "SEXO", "CAUSA", "Category")) %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  arrange(YEAR, CHANGE)


#### Gráfico de barras

graf_birth <- base1 %>% 
  
  mutate(Category = ifelse(Category %in% c("Dengue","Dengue [classical dengue]",
                                           "Dengue hemorrhagic fever"), "Dengue", Category)) %>% 
  mutate(Category = ifelse(Category %in% c("Newborn affected by maternal factors and by complications of pregnancy, labor, and delivery"), 
                           "Newborn affected by maternal factors", Category)) %>% 
  
  mutate(Category = factor(ifelse(Category %in% 
                                    c("Dengue",
                                      "Pneumonia",
                                      "Influenza",
                                      "Newborn affected by maternal factors",
                                      "COVID-19"), 
                                  Category, "Others"),
                           levels = c("Dengue",
                                      "COVID-19", 
                                      "Pneumonia",
                                      "Influenza",
                                      "Newborn affected by maternal factors",
                                      "Others"))) %>%
  
  ggplot(aes(x = factor(YEAR), y = CHANGE, fill = Category)) +
  
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "gray") +
  scale_x_discrete(breaks = seq(1996, 2024, by = 2)) + 
  scale_fill_manual(
    values = c(
      "Dengue"  = "#BE3D2A",
      "COVID-19" = "#E89CA0",  
      "Pneumonia" = "#FFA55D",
      "Influenza" = "#ACC572",
      "Newborn affected by maternal factors" = "#A76545",
      "Others" = "lightgray"
    )
  ) + 
  
  scale_y_break(c(0.22, 0.5), scales = 0.5, space = 0.1)+
  scale_y_break(c(-0.22, -1.7), scales = 5, space = 0.1)+

  theme(
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x.top = element_blank(),
    axis.title.y = element_text(colour = "gray50", angle = 90),
    axis.ticks.x = element_line(color = "gray50"), 
    axis.ticks.x.top = element_blank(), 
    panel.grid = element_blank(),
    axis.line.x.top = element_blank(),
    axis.line = element_line(color = "gray50")
  )

graf_birth


ggsave(file = "FIG3a.pdf", plot = graf_birth, width = 250, height = 150, units = "mm", dpi = 600)


#######################################
#         0-14 age group              #
#######################################

base1 <- todos %>% 
  filter(REFERENCIA == "0-14" & SEXO == "Ambos" & LOCAL == "Brasil") %>% 
  pivot_wider(names_from = "YEAR", values_from = "CHANGE") %>% 
  left_join(rotulo_traduzido, by = c("CAUSA" = "Categoria.CID")) %>% 
  left_join(rotulo, by = c("Codigo.Categoria.CID")) %>% 
  
  select(LOCAL, GRUPO, SEXO, CAUSA, Category, `2001`: `2024`) %>% 
  filter(GRUPO == "Communicable") %>% 
  unique() %>%                            
  
  pivot_longer(names_to = "YEAR", values_to = "CHANGE", cols = -c("LOCAL","GRUPO", "SEXO", "CAUSA", "Category")) %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  arrange(YEAR, CHANGE)


#### Plot

graf_014 <- base1 %>% 
  
  mutate(Category = ifelse(Category %in% c("Dengue","Dengue [classical dengue]",
                                           "Dengue hemorrhagic fever"), "Dengue", Category)) %>% 
  mutate(Category = ifelse(Category %in% c("Disorders of newborn related to fetal growth, fetal malnutrition, short gestation, and low birth weight"), 
                           "Disorders of newborn", Category)) %>% 
  mutate(Category = ifelse(Category %in% c("Newborn affected by maternal factors and by complications of pregnancy, labor, and delivery"), 
                           "Newborn affected by maternal factors", Category)) %>% 
  
  mutate(Category = factor(ifelse(Category %in% c("Dengue",
                                                  "Pneumonia",
                                                  "Influenza",
                                                  "Newborn affected by maternal factors", 
                                                  "Disorders of newborn", 
                                                  "COVID-19", 
                                                  "Pneumonia"), 
                                  Category, "Others"),
                           levels = c("Pneumonia",
                                      "Dengue",
                                      "Septicemia",
                                      "Influenza",
                                      "Newborn affected by maternal factors",
                                      "Disorders of newborn", 
                                      "COVID-19", 
                                      "Others"))) %>%
  
  ggplot(aes(x = factor(YEAR), y = CHANGE, fill = Category)) +
  
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "gray") + 
  scale_x_discrete(breaks = seq(2002, 2024, by = 2)) + 
  scale_fill_manual(
    values = c(
      "Newborn affected by maternal factors" = "#A76545",
      "Disorders of newborn" = "#6F826A",
      "Influenza" = "#ACC572",
      "Congenital viral and parasitic diseases" = "orange",
      "COVID-19" = "#E89CA0",   
      "Pneumonia" = "#FFA55D",
      "Dengue [classical dengue]" = "#BE3D2A",
      "Dengue hemorrhagic fever" = "#BE3D2A",
      "Dengue" = "#BE3D2A", 
      "Others" = "lightgray"
    )
  ) +
  
  theme(
    panel.background = element_rect(fill = "gray98", color = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x.top = element_blank(),
    axis.title.y = element_text(colour = "gray50", angle = 90),
    axis.ticks.x = element_line(color = "gray50"), 
    axis.ticks.x.top = element_blank(), 
    panel.grid = element_blank(),
    axis.line.x.top = element_blank(),
    axis.line = element_line(color = "gray50")
  )

graf_014


ggsave(file = "Fig3b.pdf", plot = graf_014, width = 250, height = 150, units = "mm", dpi = 600)



#######################################
#         15-64 age group             #
#######################################

base1 <- todos %>% 
  filter(REFERENCIA == "15-64" & SEXO == "Ambos" & LOCAL == "Brasil") %>% 
  pivot_wider(names_from = "YEAR", values_from = "CHANGE") %>% 
  left_join(rotulo_traduzido, by = c("CAUSA" = "Categoria.CID")) %>% 
  left_join(rotulo, by = c("Codigo.Categoria.CID")) %>% 
  
  select(LOCAL, GRUPO, SEXO, CAUSA, Category, `2001`: `2024`) %>% 
  filter(GRUPO == "Communicable") %>% 
  unique() %>%                            
  
  pivot_longer(names_to = "YEAR", values_to = "CHANGE", cols = -c("LOCAL","GRUPO", "SEXO", "CAUSA", "Category")) %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  arrange(YEAR, CHANGE)



#### Plot

graf_1564 <- base1 %>% 
  
  mutate(Category = ifelse(Category %in% c("Dengue","Dengue [classical dengue]",
                                           "Dengue hemorrhagic fever"), "Dengue", Category)) %>% 
  mutate(Category = factor(ifelse(Category %in% c("COVID-19", 
                                                  "Pneumonia", "Influenza", "Dengue"), 
                                  Category, "Others"),
                           levels = c("COVID-19","Influenza", "Pneumonia", "Dengue", "Others"))) %>%
  
  
  ggplot(aes(x = factor(YEAR), y = CHANGE, fill = Category)) +
  
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "gray") + 
  scale_x_discrete(breaks = seq(2002, 2024, by = 2)) + 
  scale_fill_manual(
    values = c(
      "Pneumonia" = "#FFA55D",
      "Influenza" = "#C3C84C",
      "COVID-19" = "#E89CA0",
      "Others" = "lightgray",
      "Dengue"  = "#BE3D2A"
    )
  ) +

  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  scale_y_break(c(0.07, 0.2), scales = 0.5, space = 0.1)+
  scale_y_break(c(-0.065, -0.22), scales = 5, space = 0.1)+
  
  theme(
    panel.background = element_rect(fill = "gray98", color = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x.top = element_blank(),
    axis.title.y = element_text(colour = "gray50", angle = 90),
    axis.ticks.x = element_line(color = "gray50"), 
    axis.ticks.x.top = element_blank(), 
    panel.grid = element_blank(),
    axis.text.y.right = element_blank(),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line = element_line(color = "gray50")
  )


graf_1564

ggsave(file = "Fig3c.pdf", plot = graf_1564, width = 250, height = 150, units = "mm", dpi = 600)

#######################################
#         65+ age group               #
#######################################

base1 <- todos %>% 
  filter(REFERENCIA == "65+" & SEXO == "Ambos" & LOCAL == "Brasil") %>% 
  pivot_wider(names_from = "YEAR", values_from = "CHANGE") %>% 
  left_join(rotulo_traduzido, by = c("CAUSA" = "Categoria.CID")) %>% 
  left_join(rotulo, by = c("Codigo.Categoria.CID")) %>% 
  
  select(LOCAL, GRUPO, SEXO, CAUSA, Category, `2001`: `2024`) %>% 
  filter(GRUPO == "Communicable") %>% 
  unique() %>%                             
  
  pivot_longer(names_to = "YEAR", values_to = "CHANGE", cols = -c("LOCAL","GRUPO", "SEXO", "CAUSA", "Category")) %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  arrange(YEAR, CHANGE)


#### Plot

graf_65 <- base1 %>% 
  
  mutate(Category = ifelse(Category %in% c("Dengue","Dengue [classical dengue]",
                                           "Dengue hemorrhagic fever"), "Dengue", Category)) %>% 
  mutate(Category = factor(ifelse(Category %in% c("Influenza","Dengue","Pneumonia", "COVID-19"), 
                                  Category, "Others"),
                           levels = c("Influenza","Dengue","COVID-19", "Pneumonia", "Others"))) %>%
  
  
  ggplot(aes(x = factor(YEAR), y = CHANGE, fill = Category)) +
  
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "gray") + 
  scale_x_discrete(breaks = seq(1996, 2024, by = 2)) + 
  
  scale_fill_manual(
    values = c(
      "Pneumonia" = "#FFA55D",
      "COVID-19" = "#E89CA0",  
      "Influenza" = "#C3C84C",
      "Dengue" = "#BE3D2A",
      "Others" = "lightgray"
    )
  ) +
  scale_y_break(c(0.18, 0.3), scales = 0.9, space = 0.1)+
  scale_y_break(c(-0.12, -0.4), scales = 5, space = 0.1)+
  
  theme(
    panel.background = element_rect(fill = "gray98", color = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x.top = element_blank(),
    axis.title.y = element_text(colour = "gray50", angle = 90),
    axis.ticks.x = element_line(color = "gray50"), 
    axis.ticks.x.top = element_blank(), 
    panel.grid = element_blank(),
    axis.line.x.top = element_blank(),
    axis.line = element_line(color = "gray50")
  )


graf_65


ggsave(file = "Fig3d.pdf", plot = graf_65, width = 250, height = 150, units = "mm", dpi = 600)

