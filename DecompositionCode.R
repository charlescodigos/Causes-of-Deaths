
# Decomposição de Horiuchi

rm(list=ls())
options(scipen=999) 

library(tidyverse)
library(openxlsx)
library(DemoDecomp)


## Tabela com todas as taxas de mortalidade por causa

ordem_ibge <- c(
  "Brasil",
  "Norte",
  "Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins",
  "Nordeste",
  "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba",
  "Pernambuco", "Alagoas", "Sergipe", "Bahia",
  "Sudeste",
  "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo",
  "Sul",
  "Paraná", "Santa Catarina", "Rio Grande do Sul",
  "Centro-Oeste",
  "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"
)


tabua    <- readRDS("taxas.rds", refhook = NULL) %>% 
  mutate(ANO = as.numeric(as.character(ANO)),
         LOCAL = factor(LOCAL, levels = ordem_ibge))


anos <- sort(unique(tabua$ANO))
anos <- anos[anos < max(anos)]  # evita último (ano_base + 1) inexistente
sexos <- unique(tabua$SEXO)
local <- unique(tabua$LOCAL)


#####################################################################################
########## Loop de cálculo da decomposição (demora 1 dia e meio para rodar)       ###
#####################################################################################


lista_resultado_idade <- list()
lista_resultado_total <- list()


contador <- 1  # índice para preencher as listas

t1 <- Sys.time()
t1

for (n in anos){
  for (z in local){
    for (i in sexos){
      
      ano_base <- as.numeric(as.character(n))
      
      message("Processando ANO = ", ano_base,", LOCAL = ", z, ", SEXO = ", i)
      print(Sys.time())
      
      # Filtrar os dois anos de comparação para o sexo e local específico
      
      tabua_ano1 <- tabua %>% filter(LOCAL == z & SEXO == i & ANO == ano_base)     %>% select(LOCAL, SEXO, GRUPO_IDADE, Categoria.CID, taxa_morte_i1 = TAXA_MORTALIDADE)
      tabua_ano2 <- tabua %>% filter(LOCAL == z & SEXO == i & ANO == ano_base + 1) %>% select(LOCAL, SEXO, GRUPO_IDADE, Categoria.CID, taxa_morte_i2 = TAXA_MORTALIDADE)
      
      # Coletar as causas específicas de morte nesses dois anos
      
      tabua_temp <- tabua_ano1 %>%           # garantir que os vetores tem a mesma sequencia de causas
        left_join(tabua_ano2, by = c("LOCAL", "SEXO", "GRUPO_IDADE", "Categoria.CID"))
      
      pars1 <- as.vector(tabua_temp$taxa_morte_i1) # Vetor de causas especificas no ano 1
      pars2 <- as.vector(tabua_temp$taxa_morte_i2) # Vetor de causas especificas no ano 2
      
      
      # Função que calcula e0 a partir de vetor nMx concatenado (para o loop em horiuchi)
      
      e0_from_causeMx <- function(pars) {
        
        nMx_mat <- matrix(pars, ncol = 20, byrow = TRUE)  # causas com 20 grupos etários cada (0,1,5,10,..,85,90)
        nMx_total <- colSums(nMx_mat)                     # soma as causas por idade
        
        lt <- LTabr(Mx = nMx_total, 
                    Age = c(0, 1, cumsum(rep(5, length(nMx_total) - 2))), 
                    radix = 1e5)
        
        return(lt)    # expectativa de vida ao nascer
        
      }
      
      # Decomposição de Horiuchi
      
      contrib_total <- horiuchi(func = e0_from_causeMx,
                                pars1 = pars1,
                                pars2 = pars2,
                                N = 20)
      
      # Resultados
      
      resultado_idade <- data.frame(
        ANO         = ano_base +1,
        LOCAL       = z,
        SEXO        = i,
        CAUSA       = tabua_temp$Categoria.CID,
        GRUPO_IDADE = tabua_temp$GRUPO_IDADE,
        VALOR       = contrib_total
      )
      
      resultado_total <- resultado_idade %>% 
        group_by(ANO, LOCAL, SEXO, CAUSA) %>% 
        summarize(VALOR = sum(VALOR)) %>% 
        ungroup()
      
      lista_resultado_idade[[contador]] <- resultado_idade
      lista_resultado_total[[contador]] <- resultado_total
      
      contador <- contador + 1
      
      print(Sys.time())
      
    }
  }
}

t2 <- Sys.time()
t2 - t1


df_idade <- bind_rows(lista_resultado_idade)
df_total <- bind_rows(lista_resultado_total) %>% mutate(GRUPO_IDADE = "Total")


df_completo <- rbind(df_idade, df_total) %>% 
  pivot_wider(names_from = "ANO", values_from = "VALOR") %>% 
  mutate(LOCAL = factor(LOCAL, levels = ordem_ibge)) %>% 
  arrange(SEXO, LOCAL, CAUSA) 

write.xlsx(df_completo,"Results.xlsx")

