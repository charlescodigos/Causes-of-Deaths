
# Specific Mortality Rates

rm(list=ls())
options(scipen=999) 

# SIM

library(tidyverse)
library(openxlsx)
library(stringr)

####################################
#   Importação dos rótulos do CID  #
####################################

rotulo_ingles <- read.xlsx("CodigoCID10.xlsx", sheet = "Ingles")
rotulo        <- read.xlsx("CodigoCID10.xlsx", sheet = "CID") %>% left_join(rotulo_ingles)

################################################
#   Importação e tratamento dos dados do IBGE  #
################################################

# nMx

tabua <- read.xlsx("G://Meu Drive//Pesquisa//Epidemiological transition//Dados//IBGE_Rev2024_Inputs//projecoes_2024_tab5_tabuas_mortalidade.xlsx", 
                   startRow = 6) %>% 
  select(ANO, LOCAL, SEXO, GRUPO_IDADE = IDADE, nMx) %>% 
  mutate(SEXO = ifelse(SEXO == "Mulheres", "Feminino", ifelse(SEXO == "Homens", "Masculino", SEXO))) %>% 
  mutate(
    ANO = as.factor(ANO),
    LOCAL = as.factor(LOCAL),
    SEXO = as.factor(SEXO),
    GRUPO_IDADE = as.factor(GRUPO_IDADE)
  )

# população

pop <- read.xlsx("G://Meu Drive//Pesquisa//Epidemiological transition//Dados//IBGE_Rev2024_Inputs//projecoes_2024_tab1_idade_simples.xlsx",
                 startRow = 6) %>% 
  select(-CÓD., -SIGLA) %>% 
  pivot_longer(names_to = "ANO", values_to = "POPULACAO", cols = -c("IDADE", "SEXO", "LOCAL")) %>% 
  
  mutate(GRUPO_IDADE = case_when(
    IDADE == 0 ~ 0,
    IDADE >= 1 & IDADE <= 4 ~ 1,
    IDADE >= 5 & IDADE <= 9 ~ 5,
    IDADE >= 10 & IDADE <= 14 ~ 10,
    IDADE >= 15 & IDADE <= 19 ~ 15,
    IDADE >= 20 & IDADE <= 24 ~ 20,
    IDADE >= 25 & IDADE <= 29 ~ 25,
    IDADE >= 30 & IDADE <= 34 ~ 30,
    IDADE >= 35 & IDADE <= 39 ~ 35,
    IDADE >= 40 & IDADE <= 44 ~ 40,
    IDADE >= 45 & IDADE <= 49 ~ 45,
    IDADE >= 50 & IDADE <= 54 ~ 50,
    IDADE >= 55 & IDADE <= 59 ~ 55,
    IDADE >= 60 & IDADE <= 64 ~ 60,
    IDADE >= 65 & IDADE <= 69 ~ 65,
    IDADE >= 70 & IDADE <= 74 ~ 70,
    IDADE >= 75 & IDADE <= 79 ~ 75,
    IDADE >= 80 & IDADE <= 84 ~ 80,
    IDADE >= 85 & IDADE <= 89 ~ 85,
    IDADE >= 90 ~ 90
  ))%>%
  mutate(GRUPO_IDADE = factor(GRUPO_IDADE, levels = c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90))) %>% 
  mutate(SEXO = ifelse(SEXO == "Mulheres", "Feminino", ifelse(SEXO == "Homens", "Masculino", SEXO))) %>% 
  
  group_by(ANO, LOCAL, SEXO, GRUPO_IDADE) %>% 
  summarize(POPULACAO = sum(POPULACAO)) %>% 
  ungroup() %>% 
  
  mutate(
    ANO = as.factor(ANO),
    LOCAL = as.factor(LOCAL),
    SEXO = as.factor(SEXO),
    GRUPO_IDADE = as.factor(GRUPO_IDADE)
  )

# Mortes e população no IBGE

ibge <- left_join(tabua, pop) %>% 
  mutate(MORTES_IBGE = nMx*POPULACAO) %>% 
  select(-nMx)


################################################
#   Importação e tratamento dos dados do SIM   #
################################################

tabela <- list()

anos <- seq(2000, 2024, by = 1)

t = Sys.time()   # Geralmente esse loop demora uns 4min

for (n in anos){
  
  setwd("..")                           # Return to previous folder
  setwd(".\\SIM_Inputs")                # Go to SIM folder
  
  nome <- paste0("SIM_", n, ".rds")
  df   <- readRDS(nome, refhook = NULL)
  
  setwd("..")                           # Return to previous folder
  setwd(".\\Regional")                  # Go to Regional folder
  
  
  # Base com mortes por sexo
  
  base <- df %>% 
    
    select(IDADE, SEXO, CODMUNRES, CAUSABAS) %>%   
    mutate(
      ANO = n,
      MORTES = 1,
      
      UF_codigo     = substr(CODMUNRES, 1, 2),
      LOCAL = case_when(
        UF_codigo == "11" ~ "Rondônia",
        UF_codigo == "12" ~ "Acre",
        UF_codigo == "13" ~ "Amazonas",
        UF_codigo == "14" ~ "Roraima",
        UF_codigo == "15" ~ "Pará",
        UF_codigo == "16" ~ "Amapá",
        UF_codigo == "17" ~ "Tocantins",
        UF_codigo == "21" ~ "Maranhão",
        UF_codigo == "22" ~ "Piauí",
        UF_codigo == "23" ~ "Ceará",
        UF_codigo == "24" ~ "Rio Grande do Norte",
        UF_codigo == "25" ~ "Paraíba",
        UF_codigo == "26" ~ "Pernambuco",
        UF_codigo == "27" ~ "Alagoas",
        UF_codigo == "28" ~ "Sergipe",
        UF_codigo == "29" ~ "Bahia",
        UF_codigo == "31" ~ "Minas Gerais",
        UF_codigo == "32" ~ "Espírito Santo",
        UF_codigo == "33" ~ "Rio de Janeiro",
        UF_codigo == "35" ~ "São Paulo",
        UF_codigo == "41" ~ "Paraná",
        UF_codigo == "42" ~ "Santa Catarina",
        UF_codigo == "43" ~ "Rio Grande do Sul",
        UF_codigo == "50" ~ "Mato Grosso do Sul",
        UF_codigo == "51" ~ "Mato Grosso",
        UF_codigo == "52" ~ "Goiás",
        UF_codigo == "53" ~ "Distrito Federal",
        TRUE ~ "Erro"
      ),
      
      IDADE = str_pad(IDADE, width=3, pad="0"),
      CAMPO1_IDADE = as.numeric(str_sub(IDADE,1,1)),
      CAMPO2_IDADE = as.numeric(str_sub(IDADE,2,3)),
      
      IDADE = case_when(
        CAMPO1_IDADE %in% c(0,1,2,3) ~ 0,
        CAMPO1_IDADE == 4 ~ CAMPO2_IDADE,
        CAMPO1_IDADE == 5 ~ 100,
        CAMPO1_IDADE == 9 ~ as.numeric(NA)),
      
      SEXO = ifelse(SEXO == 1, "Masculino", ifelse(SEXO == 2, "Feminino", "Ignorado"))       
    ) %>% 
    
    filter(!is.na(IDADE)) %>%         
    filter(SEXO != "Ignorado") %>%  
    
    mutate(
      GRUPO_IDADE = 
        case_when(
          IDADE == 0 ~ 0,
          IDADE >= 1 & IDADE < 5 ~ 1,
          IDADE > 4  & IDADE < 10 ~ 5,
          IDADE > 9  & IDADE < 15 ~ 10,
          IDADE > 14 & IDADE < 20 ~ 15,
          IDADE > 19 & IDADE < 25 ~ 20,
          IDADE > 24 & IDADE < 30 ~ 25,
          IDADE > 29 & IDADE < 35 ~ 30,
          IDADE > 34 & IDADE < 40 ~ 35,
          IDADE > 39 & IDADE < 45 ~ 40,
          IDADE > 44 & IDADE < 50 ~ 45,
          IDADE > 49 & IDADE < 55 ~ 50,
          IDADE > 54 & IDADE < 60 ~ 55,
          IDADE > 59 & IDADE < 65 ~ 60,
          IDADE > 64 & IDADE < 70 ~ 65,
          IDADE > 69 & IDADE < 75 ~ 70,
          IDADE > 74 & IDADE < 80 ~ 75,
          IDADE > 79 & IDADE < 85 ~ 80,
          IDADE > 84 & IDADE < 90 ~ 85,
          IDADE > 89 & IDADE < 95 ~ 90,
          IDADE >= 90 ~ 90)
    ) %>% 
    
    mutate(CID_LETRA   = str_to_upper(str_sub(CAUSABAS, 1,1)),
           CID_NUM     = as.numeric(str_sub(CAUSABAS, 2,3)),
           CID_NUM1    = str_sub(CAUSABAS, 2,3),
           CID         = case_when(
             (CAUSABAS == "B342" & ANO >= 2020) ~ "B342",
             TRUE ~ paste0(CID_LETRA, CID_NUM1)
           ),
    ) %>% 
    
    mutate(
      CID          = as.factor(CID),
      SEXO         = as.factor(SEXO)
    ) %>% 
    
    select(ANO, LOCAL, GRUPO_IDADE, SEXO, CID, MORTES) %>% 
    
    group_by(ANO, LOCAL, GRUPO_IDADE, SEXO, CID) %>% 
    summarize(MORTES = sum(MORTES)) %>% 
    ungroup() 
  
  ### Existem CIDs fora da classificação CID-10 nos dados preliminares de 2024 - Agosto
  ### São 15 observações/mortes na base
  
  cids_excluir <- c("A11", "D79", "D96", "I18", "I56", "I57", "J25", 
                    "J48", "J89", "K39", "K99", "O93", "P33", "R65", "Y95")
  
  n_removidas <- base %>% filter(CID %in% cids_excluir)
  
  base <- base %>% filter(!CID %in% cids_excluir)
  
  # Criação da base com ambos os sexos
  
  base_temp <- base %>% 
    group_by(ANO, LOCAL, GRUPO_IDADE, CID) %>% 
    summarize(MORTES = sum(MORTES)) %>% 
    ungroup() %>% 
    mutate(SEXO = "Ambos")
  
  
  # Juntar "ambos" na primeira base e fazer tratamentos
  
  base1 <- rbind(base, base_temp) %>% 
    left_join(rotulo, by = "CID") %>% 
    select(ANO, LOCAL, SEXO, CID, GRUPO_IDADE, Categoria.CID, Category, MORTES)
  
  # Checagem de CIDs do SIM sem correspondência no tabela do MS
  # Se pular o bloco acima de cids_excluir, aqui aparecerá a lista de CIDs sem correspondência
  # Se rodar o bloco, a checagem deverá ser igual a zero porque teremos tirado esses casos
  
  checagem <- base1 %>% filter(is.na(Category))
  
  ##################
  # Mortes totais 
  
  mortes_total <- base1 %>%     # PODE ACONTECER DE NÃO TER TODOS GRUPOS ETÁRIOS PARA ALGUMAS MORTES
    
    group_by(ANO, SEXO, GRUPO_IDADE, CID, Categoria.CID, Category) %>% 
    summarize(MORTES = sum(MORTES)) %>% 
    ungroup() %>% 
    mutate(LOCAL = "Brasil")
  
  
  ##################
  # Mortes regionais 
  
  mortes_regiao <- base1 %>%     # PODE ACONTECER DE NÃO TER TODOS GRUPOS ETÁRIOS PARA ALGUMAS MORTES
    mutate(REGIAO = case_when(
      LOCAL %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul") ~ "Sul",
      LOCAL %in% c("São Paulo", "Rio de Janeiro", "Minas Gerais", "Espírito Santo") ~ "Sudeste",
      LOCAL %in% c("Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul") ~ "Centro-Oeste",
      LOCAL %in% c("Bahia", "Sergipe", "Alagoas", "Pernambuco", "Paraíba",
                   "Rio Grande do Norte", "Ceará", "Piauí", "Maranhão") ~ "Nordeste",
      LOCAL %in% c("Tocantins", "Pará", "Amapá", "Amazonas", "Roraima", "Rondônia", "Acre") ~ "Norte",
      TRUE ~ "ERRO"
    )) %>% 
    group_by(ANO, REGIAO, SEXO, GRUPO_IDADE, CID, Categoria.CID, Category) %>% 
    summarize(MORTES = sum(MORTES)) %>% 
    ungroup() %>% 
    rename(LOCAL = REGIAO) 
  
  mortes <- rbind(base1, mortes_total, mortes_regiao) %>% 
    group_by(ANO, LOCAL, SEXO, GRUPO_IDADE, Categoria.CID, Category) %>% 
    summarize(MORTES = sum(MORTES)) %>% 
    ungroup() %>% 
    mutate(
      ANO = as.factor(ANO),
      LOCAL = as.factor(LOCAL),
      GRUPO_IDADE = as.factor(GRUPO_IDADE)
    )
  
  tabela <- rbind(tabela, mortes)       
  
}

Sys.time() - t


# Completar a tabela com mortes = 0 para todos os grupos etários e cids faltantes

cid_rotulos <- tabela %>%
  select(Categoria.CID, Category) %>%
  distinct()

tabela_completa <- tabela %>%
  select(-Category) %>% 
  complete(
    ANO, LOCAL, SEXO, 
    Categoria.CID = unique(tabela$Categoria.CID),
    GRUPO_IDADE = unique(tabela$GRUPO_IDADE),
    fill = list(MORTES = 0)
  ) %>% 
  rename(MORTES_i = MORTES) %>% 
  group_by(ANO,LOCAL,SEXO,GRUPO_IDADE) %>% 
  mutate(MORTES = sum(MORTES_i)) %>% 
  ungroup()

tabela_completa <- tabela_completa %>%
  left_join(cid_rotulos, by = "Categoria.CID")

###################################
#   Join de IBGE e SIM            #
###################################

tabela_final <- left_join(tabela_completa, ibge, by = c("ANO", "SEXO", "LOCAL", "GRUPO_IDADE")) %>% 
  mutate(
    ANO              = droplevels(ANO),
    fator_correcao   = MORTES_IBGE / MORTES,
    MORTES_i_ADJ     = MORTES_i*fator_correcao,
    TAXA_MORTALIDADE = MORTES_i_ADJ / POPULACAO
  ) %>% 
  
  group_by(ANO,LOCAL,SEXO,GRUPO_IDADE) %>% 
  mutate(MORTES_ADJ = sum(MORTES_i_ADJ)) %>% 
  ungroup() %>% 
  
  select(ANO, LOCAL, SEXO, GRUPO_IDADE, Categoria.CID, Category, 
         MORTES_i, MORTES, MORTES_IBGE, fator_correcao, MORTES_i_ADJ, MORTES_ADJ, 
         POPULACAO, TAXA_MORTALIDADE)

# Geralmente esse print abaixo dá 278 categorias 

print(paste0("Número de Categorias CID: ", nlevels(as.factor(tabela_final$Categoria.CID))))

niveis <- data.frame(
  Category = unique(tabela_final$Category)
) %>% 
  left_join(rotulo_ingles) %>% 
  select(Code = Codigo.Categoria.CID, Category) %>% 
  arrange(Code) %>% 
  mutate(Row = row_number()) %>% 
  select(Row, everything())

write.xlsx(niveis, "Table_SM_278causes.xlsx")

#########################################################
#   Exportar as taxas de mortalidade por causa          #
#########################################################

saveRDS(tabela_final,"taxas.rds")
