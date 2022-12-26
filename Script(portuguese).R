# Roteiro de cálculo para o Potencial de Consumo

library(readxl)
library(tidyverse)
library(writexl)


# Reajuste de salário
## Importação de dados e ajuste do banco de dados

despesa_0A <- read_excel("53DF.xls.xlsx", sheet = 1, range = "A9:I61", col_names = FALSE, na = "-")
despesa_0B <- read_excel("53DF.xls.xlsx", sheet = 1, range = "A72:I111", col_names = FALSE, na = "-")

rendimento_0 <- read_excel("53DF.xls.xlsx", sheet = 5, range = "A11:i28", col_names = FALSE, na="-")

despesa_0 <- bind_rows(despesa_0A,despesa_0B)
despesa_0 <- despesa_0 %>% rename(item = ...1, total = ...2, ate2 = ...3, m2a3 = ...4, m3a6 = ...5, m6a10 = ...6, m10a15 = ...7, m15a25 = ...8, m25 = ...9)


INPC_2019 <- read_excel("Tabela 1100 - INPC.xlsx", sheet = 1, range = "L5:U22", col_names = TRUE, na = "-")
INPC_2020 <- read_excel("Tabela 7063 - INPC.xlsx", sheet = 1, range = "B5:K22", col_names = TRUE, na = "-")
INPC_2021 <- read_excel("Tabela 7063 - INPC.xlsx", sheet = 1, range = "L5:U22", col_names = TRUE, na = "-")
INPC_2022 <- read_excel("Tabela 7063 - INPC.xlsx", sheet = 1, range = "V5:AE22", col_names = TRUE, na = "-")

regiao <- read_excel("Tabela 1100 - INPC.xlsx", sheet = 1, range = "A5:A22", col_names = TRUE)
regiao
IPCA_2019 <- read_excel("Tabela 1419 - IPCA.xlsx", sheet = 1, range = "L5:U22", col_names = TRUE, na="-")
IPCA_2020 <- read_excel("Tabela 7060 - IPCA.xlsx", sheet = 1, range = "B5:K22", col_names = TRUE, na="-")
IPCA_2021 <- read_excel("Tabela 7060 - IPCA.xlsx", sheet = 1, range = "L5:U22", col_names = TRUE, na="-")
IPCA_2022 <- read_excel("Tabela 7060 - IPCA.xlsx", sheet = 1, range = "V5:AE22", col_names = TRUE, na="-")



## ajuste do dados do INPC
INPC_2019 <- bind_cols(INPC_2019, regiao)
INPC_2019 <- column_to_rownames(INPC_2019, var = "...11")
INPC_2020 <- bind_cols(INPC_2020, regiao)
INPC_2020 <- column_to_rownames(INPC_2020, var = "...11")
INPC_2021 <- bind_cols(INPC_2021, regiao)
INPC_2021 <- column_to_rownames(INPC_2021, var = "...11")
INPC_2022 <- bind_cols(INPC_2022, regiao)
INPC_2022 <- column_to_rownames(INPC_2022, var = "...11")

## Ajustando os valores dos índices

INPC_2019 <- INPC_2019/100+1
INPC_2020 <- INPC_2020/100+1
INPC_2021 <- INPC_2021/100+1
INPC_2022 <- INPC_2022/100+1

IPCA_2019 <- IPCA_2019/100+1
IPCA_2020 <- IPCA_2020/100+1
IPCA_2021 <- IPCA_2021/100+1
IPCA_2022 <- IPCA_2022/100+1


INPC_2019 
INPC_2020
INPC_2021 
INPC_2022 

IPCA_2019
IPCA_2020
IPCA_2021 
IPCA_2022

## Calculado os multiplicadores de reajuste das cestas

INPC_t <- INPC_2019*INPC_2020*INPC_2021*INPC_2022
IPCA_t <- IPCA_2019*IPCA_2020*IPCA_2021*IPCA_2022


indice_medio <- (2*INPC_t+IPCA_t)/3

INPC_t
IPCA_t
indice_medio 
## Construindo o vetor de reajuste de cesta

vetor_total_inpc <- INPC_t[17,]
vetor_total_ipca <- IPCA_t[17,]
vetor_total_medio <- indice_medio[17,]


vetor_mult_inpc <- seq(1:93)
vetor_mult_ipca <- seq(1:93)
vetor_mult_medio <- seq(1:93)

vetor_mult_inpc[1]=1 
vetor_mult_inpc[2]=1
vetor_mult_inpc[3]=1
vetor_mult_inpc[4]=vetor_total_inpc[2]
vetor_mult_inpc[5]=vetor_total_inpc[3]
vetor_mult_inpc[6]=vetor_total_inpc[3]
vetor_mult_inpc[7]=vetor_total_inpc[3]
vetor_mult_inpc[8]=vetor_total_inpc[3]
vetor_mult_inpc[9]=vetor_total_inpc[3]
vetor_mult_inpc[10]=vetor_total_inpc[3]
vetor_mult_inpc[11]=vetor_total_inpc[3]
vetor_mult_inpc[12]=vetor_total_inpc[10]
vetor_mult_inpc[13]=vetor_total_inpc[10]
vetor_mult_inpc[14]=vetor_total_inpc[10]
vetor_mult_inpc[15]=vetor_total_inpc[3]
vetor_mult_inpc[16]=vetor_total_inpc[3]
vetor_mult_inpc[17]=vetor_total_inpc[3]
vetor_mult_inpc[18]=vetor_total_inpc[3]
vetor_mult_inpc[19]=vetor_total_inpc[3]
vetor_mult_inpc[20]=vetor_total_inpc[4]
vetor_mult_inpc[21]=vetor_total_inpc[4]
vetor_mult_inpc[22]=vetor_total_inpc[4]
vetor_mult_inpc[23]=vetor_total_inpc[5]
vetor_mult_inpc[24]=vetor_total_inpc[5]
vetor_mult_inpc[25]=vetor_total_inpc[5]
vetor_mult_inpc[26]=vetor_total_inpc[5]
vetor_mult_inpc[27]=vetor_total_inpc[5]
vetor_mult_inpc[28]=vetor_total_inpc[5]
vetor_mult_inpc[29]=vetor_total_inpc[5]
vetor_mult_inpc[30]=vetor_total_inpc[6] 
vetor_mult_inpc[31]=vetor_total_inpc[6] 
vetor_mult_inpc[32]=vetor_total_inpc[6] 
vetor_mult_inpc[33]=vetor_total_inpc[6]
vetor_mult_inpc[34]=vetor_total_inpc[6]
vetor_mult_inpc[35]=vetor_total_inpc[6]
vetor_mult_inpc[36]=vetor_total_inpc[8]
vetor_mult_inpc[37]=vetor_total_inpc[7]
vetor_mult_inpc[38]=vetor_total_inpc[7]
vetor_mult_inpc[39]=vetor_total_inpc[7]
vetor_mult_inpc[40]=vetor_total_inpc[7]
vetor_mult_inpc[41]=vetor_total_inpc[7]
vetor_mult_inpc[42]=vetor_total_inpc[7]
vetor_mult_inpc[43]=vetor_total_inpc[7]
vetor_mult_inpc[44]=vetor_total_inpc[7]
vetor_mult_inpc[45]=vetor_total_inpc[7]
vetor_mult_inpc[46]=vetor_total_inpc[7]
vetor_mult_inpc[47]=vetor_total_inpc[7]
vetor_mult_inpc[48]=vetor_total_inpc[7]
vetor_mult_inpc[49]=vetor_total_inpc[7]
vetor_mult_inpc[50]=vetor_total_inpc[7]
vetor_mult_inpc[51]=vetor_total_inpc[7]
vetor_mult_inpc[52]=vetor_total_inpc[7]
vetor_mult_inpc[53]=vetor_total_inpc[7]
vetor_mult_inpc[54]=vetor_total_inpc[9]
vetor_mult_inpc[55]=vetor_total_inpc[9]
vetor_mult_inpc[56]=vetor_total_inpc[9]
vetor_mult_inpc[57]=vetor_total_inpc[9]
vetor_mult_inpc[58]=vetor_total_inpc[9]
vetor_mult_inpc[59]=vetor_total_inpc[9]
vetor_mult_inpc[60]=vetor_total_inpc[9]
vetor_mult_inpc[61]=vetor_total_inpc[8]
vetor_mult_inpc[62]=vetor_total_inpc[8]
vetor_mult_inpc[63]=vetor_total_inpc[8]
vetor_mult_inpc[64]=vetor_total_inpc[8]
vetor_mult_inpc[65]=vetor_total_inpc[8]
vetor_mult_inpc[66]=vetor_total_inpc[8]
vetor_mult_inpc[67]=vetor_total_inpc[8]
vetor_mult_inpc[68]=vetor_total_inpc[8]
vetor_mult_inpc[69]=vetor_total_inpc[8]
vetor_mult_inpc[70]=vetor_total_inpc[8]
vetor_mult_inpc[71]=vetor_total_inpc[8]
vetor_mult_inpc[72]=vetor_total_inpc[8]
vetor_mult_inpc[73]=vetor_total_inpc[8]
vetor_mult_inpc[74]=vetor_total_inpc[8]
vetor_mult_inpc[75]=vetor_total_inpc[10]
vetor_mult_inpc[76]=vetor_total_inpc[8]
vetor_mult_inpc[77]=vetor_total_inpc[8]
vetor_mult_inpc[78]=vetor_total_inpc[8]
vetor_mult_inpc[79]=vetor_total_inpc[8]
vetor_mult_inpc[80]=vetor_total_inpc[1]
vetor_mult_inpc[81]=vetor_total_inpc[1]
vetor_mult_inpc[82]=vetor_total_inpc[1]
vetor_mult_inpc[83]=vetor_total_inpc[1]
vetor_mult_inpc[84]=vetor_total_inpc[1]
vetor_mult_inpc[85]=vetor_total_inpc[1]
vetor_mult_inpc[86]=vetor_total_inpc[1]
vetor_mult_inpc[87]=vetor_total_inpc[1]
vetor_mult_inpc[88]=vetor_total_inpc[1]
vetor_mult_inpc[89]=vetor_total_inpc[1]
vetor_mult_inpc[90]=vetor_total_inpc[1]
vetor_mult_inpc[91]=vetor_total_inpc[1]
vetor_mult_inpc[92]=vetor_total_inpc[1]
vetor_mult_inpc[93]=vetor_total_inpc[1]

vetor_mult_inpc <- as.numeric(vetor_mult_inpc)

## IPCA
vetor_mult_ipca[1]=1 
vetor_mult_ipca[2]=1
vetor_mult_ipca[3]=1
vetor_mult_ipca[4]=vetor_total_ipca[2]
vetor_mult_ipca[5]=vetor_total_ipca[3]
vetor_mult_ipca[6]=vetor_total_ipca[3]
vetor_mult_ipca[7]=vetor_total_ipca[3]
vetor_mult_ipca[8]=vetor_total_ipca[3]
vetor_mult_ipca[9]=vetor_total_ipca[3]
vetor_mult_ipca[10]=vetor_total_ipca[3]
vetor_mult_ipca[11]=vetor_total_ipca[3]
vetor_mult_ipca[12]=vetor_total_ipca[10]
vetor_mult_ipca[13]=vetor_total_ipca[10]
vetor_mult_ipca[14]=vetor_total_ipca[10]
vetor_mult_ipca[15]=vetor_total_ipca[3]
vetor_mult_ipca[16]=vetor_total_ipca[3]
vetor_mult_ipca[17]=vetor_total_ipca[3]
vetor_mult_ipca[18]=vetor_total_ipca[3]
vetor_mult_ipca[19]=vetor_total_ipca[3]
vetor_mult_ipca[20]=vetor_total_ipca[4]
vetor_mult_ipca[21]=vetor_total_ipca[4]
vetor_mult_ipca[22]=vetor_total_ipca[4]
vetor_mult_ipca[23]=vetor_total_ipca[5]
vetor_mult_ipca[24]=vetor_total_ipca[5]
vetor_mult_ipca[25]=vetor_total_ipca[5]
vetor_mult_ipca[26]=vetor_total_ipca[5]
vetor_mult_ipca[27]=vetor_total_ipca[5]
vetor_mult_ipca[28]=vetor_total_ipca[5]
vetor_mult_ipca[29]=vetor_total_ipca[5]
vetor_mult_ipca[30]=vetor_total_ipca[6] 
vetor_mult_ipca[31]=vetor_total_ipca[6] 
vetor_mult_ipca[32]=vetor_total_ipca[6] 
vetor_mult_ipca[33]=vetor_total_ipca[6]
vetor_mult_ipca[34]=vetor_total_ipca[6]
vetor_mult_ipca[35]=vetor_total_ipca[6]
vetor_mult_ipca[36]=vetor_total_ipca[8]
vetor_mult_ipca[37]=vetor_total_ipca[7]
vetor_mult_ipca[38]=vetor_total_ipca[7]
vetor_mult_ipca[39]=vetor_total_ipca[7]
vetor_mult_ipca[40]=vetor_total_ipca[7]
vetor_mult_ipca[41]=vetor_total_ipca[7]
vetor_mult_ipca[42]=vetor_total_ipca[7]
vetor_mult_ipca[43]=vetor_total_ipca[7]
vetor_mult_ipca[44]=vetor_total_ipca[7]
vetor_mult_ipca[45]=vetor_total_ipca[7]
vetor_mult_ipca[46]=vetor_total_ipca[7]
vetor_mult_ipca[47]=vetor_total_ipca[7]
vetor_mult_ipca[48]=vetor_total_ipca[7]
vetor_mult_ipca[49]=vetor_total_ipca[7]
vetor_mult_ipca[50]=vetor_total_ipca[7]
vetor_mult_ipca[51]=vetor_total_ipca[7]
vetor_mult_ipca[52]=vetor_total_ipca[7]
vetor_mult_ipca[53]=vetor_total_ipca[7]
vetor_mult_ipca[54]=vetor_total_ipca[9]
vetor_mult_ipca[55]=vetor_total_ipca[9]
vetor_mult_ipca[56]=vetor_total_ipca[9]
vetor_mult_ipca[57]=vetor_total_ipca[9]
vetor_mult_ipca[58]=vetor_total_ipca[9]
vetor_mult_ipca[59]=vetor_total_ipca[9]
vetor_mult_ipca[60]=vetor_total_ipca[9]
vetor_mult_ipca[61]=vetor_total_ipca[8]
vetor_mult_ipca[62]=vetor_total_ipca[8]
vetor_mult_ipca[63]=vetor_total_ipca[8]
vetor_mult_ipca[64]=vetor_total_ipca[8]
vetor_mult_ipca[65]=vetor_total_ipca[8]
vetor_mult_ipca[66]=vetor_total_ipca[8]
vetor_mult_ipca[67]=vetor_total_ipca[8]
vetor_mult_ipca[68]=vetor_total_ipca[8]
vetor_mult_ipca[69]=vetor_total_ipca[8]
vetor_mult_ipca[70]=vetor_total_ipca[8]
vetor_mult_ipca[71]=vetor_total_ipca[8]
vetor_mult_ipca[72]=vetor_total_ipca[8]
vetor_mult_ipca[73]=vetor_total_ipca[8]
vetor_mult_ipca[74]=vetor_total_ipca[8]
vetor_mult_ipca[75]=vetor_total_ipca[10]
vetor_mult_ipca[76]=vetor_total_ipca[8]
vetor_mult_ipca[77]=vetor_total_ipca[8]
vetor_mult_ipca[78]=vetor_total_ipca[8]
vetor_mult_ipca[79]=vetor_total_ipca[8]
vetor_mult_ipca[80]=vetor_total_ipca[1]
vetor_mult_ipca[81]=vetor_total_ipca[1]
vetor_mult_ipca[82]=vetor_total_ipca[1]
vetor_mult_ipca[83]=vetor_total_ipca[1]
vetor_mult_ipca[84]=vetor_total_ipca[1]
vetor_mult_ipca[85]=vetor_total_ipca[1]
vetor_mult_ipca[86]=vetor_total_ipca[1]
vetor_mult_ipca[87]=vetor_total_ipca[1]
vetor_mult_ipca[88]=vetor_total_ipca[1]
vetor_mult_ipca[89]=vetor_total_ipca[1]
vetor_mult_ipca[90]=vetor_total_ipca[1]
vetor_mult_ipca[91]=vetor_total_ipca[1]
vetor_mult_ipca[92]=vetor_total_ipca[1]
vetor_mult_ipca[93]=vetor_total_ipca[1]

vetor_mult_ipca <- as.numeric(vetor_mult_ipca)


## médio

vetor_mult_medio[1]=1 
vetor_mult_medio[2]=1
vetor_mult_medio[3]=1
vetor_mult_medio[4]=vetor_total_medio[2]
vetor_mult_medio[5]=vetor_total_medio[3]
vetor_mult_medio[6]=vetor_total_medio[3]
vetor_mult_medio[7]=vetor_total_medio[3]
vetor_mult_medio[8]=vetor_total_medio[3]
vetor_mult_medio[9]=vetor_total_medio[3]
vetor_mult_medio[10]=vetor_total_medio[3]
vetor_mult_medio[11]=vetor_total_medio[3]
vetor_mult_medio[12]=vetor_total_medio[10]
vetor_mult_medio[13]=vetor_total_medio[10]
vetor_mult_medio[14]=vetor_total_medio[10]
vetor_mult_medio[15]=vetor_total_medio[3]
vetor_mult_medio[16]=vetor_total_medio[3]
vetor_mult_medio[17]=vetor_total_medio[3]
vetor_mult_medio[18]=vetor_total_medio[3]
vetor_mult_medio[19]=vetor_total_medio[3]
vetor_mult_medio[20]=vetor_total_medio[4]
vetor_mult_medio[21]=vetor_total_medio[4]
vetor_mult_medio[22]=vetor_total_medio[4]
vetor_mult_medio[23]=vetor_total_medio[5]
vetor_mult_medio[24]=vetor_total_medio[5]
vetor_mult_medio[25]=vetor_total_medio[5]
vetor_mult_medio[26]=vetor_total_medio[5]
vetor_mult_medio[27]=vetor_total_medio[5]
vetor_mult_medio[28]=vetor_total_medio[5]
vetor_mult_medio[29]=vetor_total_medio[5]
vetor_mult_medio[30]=vetor_total_medio[6] 
vetor_mult_medio[31]=vetor_total_medio[6] 
vetor_mult_medio[32]=vetor_total_medio[6] 
vetor_mult_medio[33]=vetor_total_medio[6]
vetor_mult_medio[34]=vetor_total_medio[6]
vetor_mult_medio[35]=vetor_total_medio[6]
vetor_mult_medio[36]=vetor_total_medio[8]
vetor_mult_medio[37]=vetor_total_medio[7]
vetor_mult_medio[38]=vetor_total_medio[7]
vetor_mult_medio[39]=vetor_total_medio[7]
vetor_mult_medio[40]=vetor_total_medio[7]
vetor_mult_medio[41]=vetor_total_medio[7]
vetor_mult_medio[42]=vetor_total_medio[7]
vetor_mult_medio[43]=vetor_total_medio[7]
vetor_mult_medio[44]=vetor_total_medio[7]
vetor_mult_medio[45]=vetor_total_medio[7]
vetor_mult_medio[46]=vetor_total_medio[7]
vetor_mult_medio[47]=vetor_total_medio[7]
vetor_mult_medio[48]=vetor_total_medio[7]
vetor_mult_medio[49]=vetor_total_medio[7]
vetor_mult_medio[50]=vetor_total_medio[7]
vetor_mult_medio[51]=vetor_total_medio[7]
vetor_mult_medio[52]=vetor_total_medio[7]
vetor_mult_medio[53]=vetor_total_medio[7]
vetor_mult_medio[54]=vetor_total_medio[9]
vetor_mult_medio[55]=vetor_total_medio[9]
vetor_mult_medio[56]=vetor_total_medio[9]
vetor_mult_medio[57]=vetor_total_medio[9]
vetor_mult_medio[58]=vetor_total_medio[9]
vetor_mult_medio[59]=vetor_total_medio[9]
vetor_mult_medio[60]=vetor_total_medio[9]
vetor_mult_medio[61]=vetor_total_medio[8]
vetor_mult_medio[62]=vetor_total_medio[8]
vetor_mult_medio[63]=vetor_total_medio[8]
vetor_mult_medio[64]=vetor_total_medio[8]
vetor_mult_medio[65]=vetor_total_medio[8]
vetor_mult_medio[66]=vetor_total_medio[8]
vetor_mult_medio[67]=vetor_total_medio[8]
vetor_mult_medio[68]=vetor_total_medio[8]
vetor_mult_medio[69]=vetor_total_medio[8]
vetor_mult_medio[70]=vetor_total_medio[8]
vetor_mult_medio[71]=vetor_total_medio[8]
vetor_mult_medio[72]=vetor_total_medio[8]
vetor_mult_medio[73]=vetor_total_medio[8]
vetor_mult_medio[74]=vetor_total_medio[8]
vetor_mult_medio[75]=vetor_total_medio[10]
vetor_mult_medio[76]=vetor_total_medio[8]
vetor_mult_medio[77]=vetor_total_medio[8]
vetor_mult_medio[78]=vetor_total_medio[8]
vetor_mult_medio[79]=vetor_total_medio[8]
vetor_mult_medio[80]=vetor_total_medio[1]
vetor_mult_medio[81]=vetor_total_medio[1]
vetor_mult_medio[82]=vetor_total_medio[1]
vetor_mult_medio[83]=vetor_total_medio[1]
vetor_mult_medio[84]=vetor_total_medio[1]
vetor_mult_medio[85]=vetor_total_medio[1]
vetor_mult_medio[86]=vetor_total_medio[1]
vetor_mult_medio[87]=vetor_total_medio[1]
vetor_mult_medio[88]=vetor_total_medio[1]
vetor_mult_medio[89]=vetor_total_medio[1]
vetor_mult_medio[90]=vetor_total_medio[1]
vetor_mult_medio[91]=vetor_total_medio[1]
vetor_mult_medio[92]=vetor_total_medio[1]
vetor_mult_medio[93]=vetor_total_medio[1]

vetor_mult_medio <- as.numeric(vetor_mult_medio)

## reajuste da despesa

despesa_reajustada <- despesa_0 %>% mutate(ate2=ate2*vetor_mult_inpc, m2a3=m2a3*vetor_mult_inpc, m3a6=m3a6*vetor_mult_medio, m6a10=m6a10*vetor_mult_ipca, m10a15=m10a15*vetor_mult_ipca, m15a25=m15a25*vetor_mult_ipca, m25=m25*vetor_mult_ipca)
despesa_reajustada


##Somando as despesas agregadas

### Despesa de consumo
### Despesa de consumo = Alimentação + Habitação + Vestuário + Transporte + 
### +Higiene e cuidados pessoais + Assistência a saúde + Educação + Recreação e cultura
### + Fumo + Serviços pessoais + Despesas diversas

despesa_consumo_reajust <- despesa_reajustada %>% slice(4,5,23,30,38,43,54,61,67,68,73) %>% select(-1) %>% colSums(na.rm = TRUE)
despesa_reajustada[3,2]=despesa_consumo_reajust[1]
despesa_reajustada[3,3]=despesa_consumo_reajust[2]
despesa_reajustada[3,4]=despesa_consumo_reajust[3]
despesa_reajustada[3,5]=despesa_consumo_reajust[4]
despesa_reajustada[3,6]=despesa_consumo_reajust[5]
despesa_reajustada[3,7]=despesa_consumo_reajust[6]
despesa_reajustada[3,8]=despesa_consumo_reajust[7]
despesa_reajustada[3,9]=despesa_consumo_reajust[8]

despesa_consumo_reajust

### Despesas correntes
### Despesas corresntes = Despesas de consumo + outras despesas correntes

despesa_corrente_reajust <- despesa_reajustada %>% slice(3,80) %>% select(-1) %>% colSums()
despesa_corrente_reajust
despesa_reajustada[2,2]=despesa_corrente_reajust[1]
despesa_reajustada[2,3]=despesa_corrente_reajust[2]
despesa_reajustada[2,4]=despesa_corrente_reajust[3]
despesa_reajustada[2,5]=despesa_corrente_reajust[4]
despesa_reajustada[2,6]=despesa_corrente_reajust[5]
despesa_reajustada[2,7]=despesa_corrente_reajust[6]
despesa_reajustada[2,8]=despesa_corrente_reajust[7]
despesa_reajustada[2,9]=despesa_corrente_reajust[8]

### Despesa total
### Despesa total = Despesa corrente + Aumento do Ativo = Redução do passivo

despesa_total_reajust <- despesa_reajustada %>% slice(2,87,91) %>% select(-1) %>% colSums()
despesa_total_reajust
despesa_reajustada[1,2]=despesa_total_reajust[1]
despesa_reajustada[1,3]=despesa_total_reajust[2]
despesa_reajustada[1,4]=despesa_total_reajust[3]
despesa_reajustada[1,5]=despesa_total_reajust[4]
despesa_reajustada[1,6]=despesa_total_reajust[5]
despesa_reajustada[1,7]=despesa_total_reajust[6]
despesa_reajustada[1,8]=despesa_total_reajust[7]
despesa_reajustada[1,9]=despesa_total_reajust[8]



##Extração da despesa total e cálculo da proporção por item
despesa_reajustada_prop <- despesa_reajustada

#Total
despesatotal_total <- as.numeric(despesa_reajustada[1,2])
despesatotal_total
despesa_reajustada_prop <- despesa_reajustada_prop %>% mutate(despesa_reajustada_prop, total = total/despesatotal_total)
despesa_reajustada_prop

#Até 2 salários mínimos;
despesatotal_ate2 <- as.numeric(despesa_reajustada[1,3])
despesatotal_ate2
despesa_reajustada_prop <- despesa_reajustada_prop %>% mutate(despesa_reajustada_prop, ate2 = ate2/despesatotal_ate2)


#Mais que 2 a 3 salários mínimos;
despesatotal_m2a3 <- as.numeric(despesa_reajustada[1,4])
despesatotal_m2a3
despesa_reajustada_prop <- despesa_reajustada_prop %>% mutate(despesa_reajustada_prop, m2a3 = m2a3/despesatotal_m2a3)

#Mais que 3 a 6 salários mínimos;
despesatotal_m3a6 <- as.numeric(despesa_reajustada[1,5])
despesatotal_m3a6
despesa_reajustada_prop <- despesa_reajustada_prop %>% mutate(despesa_reajustada_prop, m3a6 = m3a6/despesatotal_m3a6)

#Mais que 6 a 10 salários mínimos;
despesatotal_m6a10 <- as.numeric(despesa_reajustada[1,6])
despesatotal_m6a10
despesa_reajustada_prop <- despesa_reajustada_prop %>% mutate(despesa_reajustada_prop, m6a10 = m6a10/despesatotal_m6a10)

#Mais que 10 a 15 salários mínimos;
despesatotal_m10a15 <- as.numeric(despesa_reajustada[1,7])
despesatotal_m10a15
despesa_reajustada_prop <- despesa_reajustada_prop %>% mutate(despesa_reajustada_prop, m10a15 = m10a15/despesatotal_m10a15)

#\item Mais que 15 a 25 salários mínimos;
despesatotal_m15a25 <- as.numeric(despesa_reajustada[1,8])
despesatotal_m15a25
despesa_reajustada_prop <- despesa_reajustada_prop %>% mutate(despesa_reajustada_prop, m15a25 = m15a25/despesatotal_m15a25)

#Mais que 25 salários mínimos.
despesatotal_m25 <- as.numeric(despesa_reajustada[1,9])
despesatotal_m25
despesa_reajustada_prop <- despesa_reajustada_prop %>% mutate(despesa_reajustada_prop, m25 = m25/despesatotal_m25)

## Após distribuir o peso de cada item na despesa total por faixa de salário mínimo, o próximo passo é 
## encontrar o valor de cada cesta com o resjuste
## Para isso iremos encontrar a proporção a renda média de cada faixa e a proporção da despesa total por cada renda média.
## Como a planilha 1 da POF é despesa monetária e não monetária, utilizaremos o rendimento total (monetário e não monetário)
## para distribuir o valores conforme a proporção obtida na planilha despesa_reajustada_prop
## 
rendimento_0 <- rendimento_0 %>% slice(c(-2,-4))
rendimento_0 <- column_to_rownames(rendimento_0, var = "...1")
rendimento_0 <- rendimento_0 %>% rename(total = ...2, ate2 = ...3, m2a3 = ...4, m3a6 = ...5, m6a10 = ...6, m10a15 = ...7, m15a25 = ...8, m25 = ...9)
rendimento_0

## Extração do rendimento 
rendimento_total <- slice(rendimento_0, c(2))
rendimento_total


#vetor linha de rendimento monetário em salários mínimos
rend_salmin <- (rendimento_total)/954
rend_salmin
rendimento_total_2022 <- rend_salmin*1212
rendimento_total_2022

# Razão entre despesa total e rendiento total

#POF
razao_despe_t_rend_t <- (slice(despesa_0, 1) %>% select(-1))/rendimento_total

# Despesa total 2022 e por faixa salarial
despesa_total_2022 <- razao_despe_t_rend_t*rendimento_total_2022
despesa_total_2022

despesa_total_2022_total <- as.numeric(despesa_total_2022[1])
despesa_total_2022_ate2 <- as.numeric(despesa_total_2022[2])
despesa_total_2022_m2a3 <- as.numeric(despesa_total_2022[3])
despesa_total_2022_m3a6 <- as.numeric(despesa_total_2022[4])
despesa_total_2022_m6a10 <- as.numeric(despesa_total_2022[5])
despesa_total_2022_m10a15 <- as.numeric(despesa_total_2022[6])
despesa_total_2022_m15a25 <- as.numeric(despesa_total_2022[7])
despesa_total_2022_m25 <- as.numeric(despesa_total_2022[8])


# Nova POF reajustada

despesa_reajustada <- mutate(despesa_reajustada_prop, total = total*despesa_total_2022_total, ate2 = ate2*despesa_total_2022_ate2, m2a3=m2a3*despesa_total_2022_m2a3, m3a6=m3a6*despesa_total_2022_m3a6,m6a10=m6a10*despesa_total_2022_m6a10, m10a15=m10a15*despesa_total_2022_m10a15, m15a25=m15a25*despesa_total_2022_m15a25, m25=m25*despesa_total_2022_m25)
despesa_reajustada
despesa_reajustada <- despesa_reajustada %>% rename("ate dois salarios" = ate2, "Maior que 2 salarios ate 3 salarios" = m2a3, "Maior que 3 salarios ate 6 salarios" = m3a6, "Maior que 6 salarios ate 10 salarios" = m6a10, "Maior que 10 salarios ate 15 salarios" = m10a15, "Maior que 15 salarios ate 25 salarios"=m15a25, "Maior que 25 salarios"=m25)
despesa_reajustada 

write_xlsx(despesa_reajustada, "Reajustada - 53DF.xls.xlsx", col_names = TRUE)
