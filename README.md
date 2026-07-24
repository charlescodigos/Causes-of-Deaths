# Stalled Life Expectancy Gains and a Divergent Epidemiologic Transition in Brazil

(Manuscript submitted for publication)

## Description

Data sources:  
•	Total deaths and population estimates from the IBGE 2024 Population Revision are publicly available at: https://www.ibge.gov.br/estatisticas/sociais/populacao/9109-projecao-da-populacao.html?=&t=resultados  
•	Deaths by cause from the Brazilian Mortality Information System (SIM) are publicly available at: https://opendatasus.saude.gov.br/dataset/sim 

The Methods section of the article describes the data processing procedures used to generate an aggregated dataset of mortality rates by age, sex, geographic region, year, and cause of death. The resulting dataset comprises **approximately 14 million observations**.

The file 'DecompositionCode.R' reads the processed dataset and decomposes annual changes in life expectancy at birth by cause of death between 2000 and 2024 using the continuous-change decomposition method of Horiuchi, Wilmoth, and Pletcher (2008), implemented through the DemoDecomp R package (Riffe, 2024). Running the full decomposition requires **approximately 36 hours**.

The file 'SupplementalData.xlsx' associates cause of death with its respective ICD-10 code.


## Authors

Charles H Correa, PhD in Economics, Central Bank of Brazil  
Gerson F M Pereira (in memoriam), PhD in Public Health, Brazil's Ministry of Health  
Marília R Nepomuceno, PhD in Demography, Max Planck Institute for Demographic Research


## References

Horiuchi S, Wilmoth JR, Pletcher SD. A decomposition method based on a model of continuous change. Demography. 2008 Nov 1;45(4):785–801. doi:10.1353/dem.0.0033

Riffe T. DemoDecomp: Decompose Demographic Functions. R package version 1.14.1. 2024. Available from: https://CRAN.R-project.org/package=DemoDecomp
