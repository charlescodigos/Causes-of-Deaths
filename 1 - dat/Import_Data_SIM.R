
# Download cause-specific mortality data from the 
# Brazilian Ministry of Health's Mortality Information System (SIM)

# The data on the Ministry of Health website may be subject to updates, especially in recent years.

# It may take a few hours!

# https://dados.gov.br/dados/conjuntos-dados/sim-1979-2019


rm(list=ls())

library(tidyverse)
library(data.table)

# Choose a folder to save your data files:

setwd("G:\\...\\dat")


# Download files year by year

anos <- seq(2000, 2024, by = 1)

for (n in anos){
  
  if (n < 2021){
    link <- paste0("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_", n,".csv")
  } else if (n == 2021){
    link <- paste0("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2021_csv.zip")
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


# Note: The 2021 CSV file must be unzipped, since it is the only file in a compressed format.
