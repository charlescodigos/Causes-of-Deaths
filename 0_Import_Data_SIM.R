
# Import SIM data 
# It may take a few hours!

# The data on the Ministry of Health website may be subject to updates, especially in recent years.

# https://dados.gov.br/dados/conjuntos-dados/sim-1979-2019

rm(list=ls())

library(tidyverse)
library(data.table)


# Sequence of years

anos <- seq(2000, 2024, by = 1)

setwd("G:\\Meu Drive\\Pesquisa\\Epidemiological transition\\Dados\\SIM_Inputs")


for (n in anos){
  
  if (n < 2021){
    link <- paste0("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_", n,".csv")
  } else if (n == 2021){
    link <- paste0("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2021.csv")
  } else if (n == 2022){
    link <- paste0("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO22OPEN.csv")
  }else if (n == 2023){
    link <- paste0("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO23OPEN.csv")
  }else if (n == 2024){
    link <- paste0("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN.csv")
  }
  
  df <- fread(link)
  
  nome_do_arquivo <- paste0("SIM_", n, ".rds")
  
  saveRDS(df, nome_do_arquivo)
  
} 



