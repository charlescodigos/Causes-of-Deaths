# Stalled Life Expectancy Gains and a Divergent Epidemiologic Transition in Brazil

(Manuscript submitted for publication)

## Description

The code repository is organized into a sequence of steps that:  

-	Download cause-specific mortality data from the Brazilian Ministry of Health's Mortality Information System (SIM);  
- Process the raw data and calculate mortality rates using IBGE population estimates and life tables;  
- Perform life expectancy decomposition from Horiuchi, Wilmoth and Pletcher (2008) in R package DemoDecomp by Riffe (2024);  
- Create table and figures.

Running the decomposition is computationally intensive and may take **approximately 36 hours** to complete.

## Structure

1 - dat: data (cause-specific counts, total population, life tables, ICD-10 labels)    
2 -	src: code to replicate data processing, decomposition, analysis, and visualization  
3 - out: output figures and table  

## Data availability

Data from the IBGE 2024 Population Revision are provided in the `dat/` directory and are publicly available from the Brazilian Institute of Geography and Statistics (IBGE): 

https://www.ibge.gov.br/estatisticas/sociais/populacao/9109-projecao-da-populacao.html?=&t=resultados

Cause-specific mortality data from the Mortality Information System (SIM) are publicly available from the Brazilian Ministry of Health: 

https://opendatasus.saude.gov.br/dataset/sim

SIM microdata is not included in this repository because of their large size and the large number of files required. Nevertheless, the provided script `dat/Import_DATA_SIM.R` automatically download the original data directly from the Brazilian Ministry of Health's official API.
 

## Authors

Charles H Correa, PhD in Economics, Central Bank of Brazil  
Gerson F M Pereira (in memoriam), PhD in Public Health, Brazil's Ministry of Health  
Marília R Nepomuceno, PhD in Demography, Max Planck Institute for Demographic Research


## References

Horiuchi S, Wilmoth JR, Pletcher SD. A decomposition method based on a model of continuous change. Demography. 2008 Nov 1;45(4):785–801. doi:10.1353/dem.0.0033

Riffe T. DemoDecomp: Decompose Demographic Functions. R package version 1.14.1. 2024. Available from: https://CRAN.R-project.org/package=DemoDecomp
