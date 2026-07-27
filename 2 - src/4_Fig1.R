
rm(list=ls())

# SIM

library(tidyverse)
library(openxlsx)
library(ggbreak)
library(ggtern)
library(ggbreak)
library(ggrepel)
library(patchwork)

### Import and process data

real1            <- read.xlsx("df_decomposicao.xlsx")
rotulo_traduzido <- read.xlsx("CodigoCID10.xlsx", sheet = "Ingles")
rotulo           <- read.xlsx("CodigoCID10.xlsx", sheet = "Grupo")   


total <- real1 %>% 
  filter(GRUPO_IDADE == "Total") %>% 
  pivot_longer(names_to = "YEAR", values_to = "CHANGE", cols= -c("SEXO", "CAUSA", "GRUPO_IDADE", "LOCAL")) %>% 
  rename(REFERENCIA = GRUPO_IDADE) %>% 
  mutate(
    SEXO = ifelse(SEXO == "Feminino", "Women", ifelse(SEXO == "Masculino", "Men", "Both sexes")),
    LOCAL = case_when(
      LOCAL == "Brasil" ~ "Brazil",
      LOCAL == "Sul" ~ "South",
      LOCAL == "Sudeste" ~ "Southeast",
      LOCAL == "Centro-Oeste" ~ "Central-West",
      LOCAL == "Norte" ~ "North",
      LOCAL == "Nordeste" ~ "Northeast",
      TRUE ~ LOCAL
    )
  )


grupos <- real1 %>% 
  filter(GRUPO_IDADE != "Total") %>% 
  mutate(GRUPO_IDADE = as.numeric(GRUPO_IDADE)) %>% 
  pivot_longer(names_to = "YEAR", values_to = "CHANGE", cols= -c("SEXO", "CAUSA", "GRUPO_IDADE", "LOCAL")) %>% 
  mutate(REFERENCIA = ifelse(GRUPO_IDADE < 15, "0-14", ifelse(GRUPO_IDADE > 64, "65+", "15-64"))) %>% 
  group_by(SEXO, LOCAL, CAUSA, YEAR, REFERENCIA) %>% 
  summarize(CHANGE = sum(CHANGE)) %>% 
  ungroup() %>% 
  mutate(
    SEXO = ifelse(SEXO == "Feminino", "Women", ifelse(SEXO == "Masculino", "Men", "Both sexes")),
    LOCAL = case_when(
      LOCAL == "Brasil" ~ "Brazil",
      LOCAL == "Sul" ~ "South",
      LOCAL == "Sudeste" ~ "Southeast",
      LOCAL == "Centro-Oeste" ~ "Central-West",
      LOCAL == "Norte" ~ "North",
      LOCAL == "Nordeste" ~ "Northeast",
      TRUE ~ LOCAL
    )
  )

todos <- rbind(total,grupos) 


############ Regions

regioes <- c("Brazil", "South", "Southeast", "Central-West",  "Northeast", "North")

base1 <- todos %>% 
  filter(
    REFERENCIA == "Total",
    SEXO == "Both sexes",
    LOCAL %in% regioes
  ) %>% 
  pivot_wider(names_from = "YEAR", values_from = "CHANGE") %>% 
  left_join(rotulo_traduzido, by = c("CAUSA" = "Categoria.CID")) %>% 
  left_join(rotulo, by = c("Codigo.Categoria.CID")) %>% 
  select(LOCAL, GRUPO, SEXO, CAUSA, Category, `2001`:`2024`) %>% 
  unique() %>%                             
  pivot_longer(
    names_to = "YEAR",
    values_to = "CHANGE",
    cols = -c(LOCAL, GRUPO, SEXO, CAUSA, Category)
  ) %>% 
  mutate(
    YEAR = as.numeric(YEAR),
    GRUPO = ifelse(
      GRUPO == "Communicable",
      "Communicable, maternal, perinatal, and nutritional conditions",
      GRUPO
    ),
    GRUPO = factor(
      GRUPO,
      levels = c(
        "Non-communicable",
        "Ill-defined",
        "Injuries",
        "Communicable, maternal, perinatal, and nutritional conditions"
      )
    ),
    LOCAL = factor(LOCAL, levels = regioes)
  )

base_total <- base1 %>% 
  group_by(LOCAL, YEAR) %>% 
  summarize(valor = sum(CHANGE), .groups = "drop")


###################### Plot

cores_grupo <- c(
  "Non-communicable" = "#CBA3E3",
  "Ill-defined"      = "#F5B48A",
  "Injuries"         = "#E0E08C",
  "Communicable, maternal, perinatal, and nutritional conditions" = "#A8D18D"
)

locais <- c(
  "Brazil", "South",
  "Southeast", "Central-West",
  "Northeast", "North"
)

fazer_grafico <- function(local_nome, eixo_y = TRUE) {
  
  base_local <- base1 %>%
    filter(LOCAL == local_nome)
  
  total_local <- base_total %>%
    filter(LOCAL == local_nome)
  
  p <- ggplot(base_local, aes(x = YEAR, y = CHANGE)) +
    geom_col(aes(fill = GRUPO), width = 1) +
    geom_hline(yintercept = 0, color = "white", linewidth = 0.2) +
    geom_line(
      data = total_local,
      aes(x = YEAR, y = valor),
      col = "black",
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    labs(title = local_nome, x = NULL, y = NULL) +
    scale_y_continuous(
      limits = c(-3.5, 4.2),
      breaks = c(-3, -2, -1, 0, 1, 2, 4),
      minor_breaks = NULL
    )+
    scale_y_break(c(1.7, 1.9), scales = 1, space = 0.08) +
    scale_y_break(c(-1.1, -1.3), scales = 4, space = 0.08) +
    scale_x_continuous(breaks = seq(2000, 2024, by = 3)) +
    scale_fill_manual(values = cores_grupo, drop = TRUE) +
    guides(fill = guide_legend(nrow = 2)) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5),
      legend.position = "none",
      legend.title = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = "gray98", color = NA),
      plot.margin = margin(4, 4, 4, 4),
      
      axis.text.y.right  = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right  = element_blank()
    )+
    annotate("text", label = "Annual change", x = 2018, y = 1.4, size = 3)
  
  if (!eixo_y) {
    p <- p +
      theme(
        axis.text.y.left  = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.line.y.left  = element_blank()
      )
  }
  
  p
}

g1 <- fazer_grafico("Brazil", eixo_y = TRUE)
g2 <- fazer_grafico("South", eixo_y = FALSE)
g3 <- fazer_grafico("Southeast", eixo_y = TRUE)
g4 <- fazer_grafico("Central-West", eixo_y = FALSE)
g5 <- fazer_grafico("Northeast", eixo_y = TRUE)
g6 <- fazer_grafico("North", eixo_y = FALSE)

graf_painel <-
  (
    (g1 + g2) /
      (g3 + g4) /
      (g5 + g6)
  ) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

graf_painel

# Export 

ggsave(
  filename = "Fig1.pdf",
  plot = graf_painel,
  device = cairo_pdf,
  width = 180,
  height = 219,
  units = "mm"
)
