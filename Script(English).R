# Calculation script for Consumption Potential

library(readxl)
library(tidyverse)
library(readxl)


# Salary readjustment
## Data import and database tuning

expense_0A <- read_excel("53DF.xls.xlsx", sheet = 1, range = "A9:I61", col_names = FALSE, na = "-")
expense_0B <- read_excel("53DF.xls.xlsx", sheet = 1, range = "A72:I111", col_names = FALSE, na = "-")

yield_0 <- read_excel("53DF.xls.xlsx", sheet = 5, range = "A11:i28", col_names = FALSE, na="-")

expense_0 <- bind_rows(expense_0A, expense_0B)
expense_0 <- expense_0 %>% rename(item = ...1, total = ...2, up to2 = ...3, m2a3 = ...4, m3a6 = ...5, m6a10 = ...6 , m10a15 = ...7, m15a25 = ...8, m25 = ...9)


INPC_2019 <- read_excel("Table 1100 - INPC.xlsx", sheet = 1, range = "L5:U22", col_names = TRUE, na = "-")
INPC_2020 <- read_excel("Table 7063 - INPC.xlsx", sheet = 1, range = "B5:K22", col_names = TRUE, na = "-")
INPC_2021 <- read_excel("Table 7063 - INPC.xlsx", sheet = 1, range = "L5:U22", col_names = TRUE, na = "-")
INPC_2022 <- read_excel("Table 7063 - INPC.xlsx", sheet = 1, range = "V5:AE22", col_names = TRUE, na = "-")

region <- read_excel("Table 1100 - INPC.xlsx", sheet = 1, range = "A5:A22", col_names = TRUE)
region
IPCA_2019 <- read_excel("Table 1419 - IPCA.xlsx", sheet = 1, range = "L5:U22", col_names = TRUE, na="-")
IPCA_2020 <- read_excel("Table 7060 - IPCA.xlsx", sheet = 1, range = "B5:K22", col_names = TRUE, na="-")
IPCA_2021 <- read_excel("Table 7060 - IPCA.xlsx", sheet = 1, range = "L5:U22", col_names = TRUE, na="-")
IPCA_2022 <- read_excel("Table 7060 - IPCA.xlsx", sheet = 1, range = "V5:AE22", col_names = TRUE, na="-")



## INPC data adjustment
INPC_2019 <- bind_cols(INPC_2019, region)
INPC_2019 <- column_to_rownames(INPC_2019, var = "...11")
INPC_2020 <- bind_cols(INPC_2020, region)
INPC_2020 <- column_to_rownames(INPC_2020, var = "...11")
INPC_2021 <- bind_cols(INPC_2021, region)
INPC_2021 <- column_to_rownames(INPC_2021, var = "...11")
INPC_2022 <- bind_cols(INPC_2022, region)
INPC_2022 <- column_to_rownames(INPC_2022, var = "...11")

## Adjusting index values

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

## Calculated the basket readjustment multipliers

INPC_t <- INPC_2019*INPC_2020*INPC_2021*INPC_2022
IPCA_t <- IPCA_2019*IPCA_2020*IPCA_2021*IPCA_2022


average_index <- (2*INPC_t+IPCA_t)/3

INPC_t
IPCA_t
average_index
## Building the basket readjustment vector

vector_total_inpc <- INPC_t[17,]
vector_total_ipca <- IPCA_t[17,]
average_total_vector <- average_index[17,]


mult_inpc_vector <- seq(1:93)
mult_ipca_vector <- seq(1:93)
medium_mult_vector <- seq(1:93)

vector_mult_inpc[1]=1
vector_mult_inpc[2]=1
vector_mult_inpc[3]=1
mult_inpc_vector[4]=total_inpc_vector[2]
mult_inpc_vector[5]=total_inpc_vector[3]
mult_inpc_vector[6]=total_inpc_vector[3]
mult_inpc_vector[7]=total_inpc_vector[3]
mult_inpc_vector[8]=total_inpc_vector[3]
mult_inpc_vector[9]=total_inpc_vector[3]
mult_inpc_vector[10]=total_inpc_vector[3]
mult_inpc_vector[11]=total_inpc_vector[3]
mult_inpc_vector[12]=total_inpc_vector[10]
mult_inpc_vector[13]=total_inpc_vector[10]
mult_inpc_vector[14]=total_inpc_vector[10]
mult_inpc_vector[15]=total_inpc_vector[3]
mult_inpc_vector[16]=total_inpc_vector[3]
mult_inpc_vector[17]=total_inpc_vector[3]
mult_inpc_vector[18]=total_inpc_vector[3]
mult_inpc_vector[19]=total_inpc_vector[3]
mult_inpc_vector[20]=total_inpc_vector[4]
mult_inpc_vector[21]=total_inpc_vector[4]
mult_inpc_vector[22]=total_inpc_vector[4]
mult_inpc_vector[23]=total_inpc_vector[5]
mult_inpc_vector[24]=total_inpc_vector[5]
mult_inpc_vector[25]=total_inpc_vector[5]
mult_inpc_vector[26]=total_inpc_vector[5]
mult_inpc_vector[27]=total_inpc_vector[5]
mult_inpc_vector[28]=total_inpc_vector[5]
mult_inpc_vector[29]=total_inpc_vector[5]
mult_inpc_vector[30]=total_inpc_vector[6]
mult_inpc_vector[31]=total_inpc_vector[6]
mult_inpc_vector[32]=total_inpc_vector[6]
mult_inpc_vector[33]=total_inpc_vector[6]
mult_inpc_vector[34]=total_inpc_vector[6]
mult_inpc_vector[35]=total_inpc_vector[6]
mult_inpc_vector[36]=total_inpc_vector[8]
mult_inpc_vector[37]=total_inpc_vector[7]
mult_inpc_vector[38]=total_inpc_vector[7]
mult_inpc_vector[39]=total_inpc_vector[7]
mult_inpc_vector[40]=total_inpc_vector[7]
mult_inpc_vector[41]=total_inpc_vector[7]
mult_inpc_vector[42]=total_inpc_vector[7]
mult_inpc_vector[43]=total_inpc_vector[7]
mult_inpc_vector[44]=total_inpc_vector[7]
mult_inpc_vector[45]=total_inpc_vector[7]
mult_inpc_vector[46]=total_inpc_vector[7]
mult_inpc_vector[47]=total_inpc_vector[7]
mult_inpc_vector[48]=total_inpc_vector[7]
mult_inpc_vector[49]=total_inpc_vector[7]
mult_inpc_vector[50]=total_inpc_vector[7]
mult_inpc_vector[51]=total_inpc_vector[7]
mult_inpc_vector[52]=total_inpc_vector[7]
mult_inpc_vector[53]=total_inpc_vector[7]
mult_inpc_vector[54]=total_inpc_vector[9]
🇧🇷
mult_inpc_vector[55]=total_inpc_vector[9]
mult_inpc_vector[56]=total_inpc_vector[9]
mult_inpc_vector[57]=total_inpc_vector[9]
mult_inpc_vector[58]=total_inpc_vector[9]
mult_inpc_vector[59]=total_inpc_vector[9]
mult_inpc_vector[60]=total_inpc_vector[9]
mult_inpc_vector[61]=total_inpc_vector[8]
mult_inpc_vector[62]=total_inpc_vector[8]
mult_inpc_vector[63]=total_inpc_vector[8]
mult_inpc_vector[64]=total_inpc_vector[8]
mult_inpc_vector[65]=total_inpc_vector[8]
mult_inpc_vector[66]=total_inpc_vector[8]
mult_inpc_vector[67]=total_inpc_vector[8]
mult_inpc_vector[68]=total_inpc_vector[8]
mult_inpc_vector[69]=total_inpc_vector[8]
mult_inpc_vector[70]=total_inpc_vector[8]
mult_inpc_vector[71]=total_inpc_vector[8]
mult_inpc_vector[72]=total_inpc_vector[8]
mult_inpc_vector[73]=total_inpc_vector[8]
mult_inpc_vector[74]=total_inpc_vector[8]
mult_inpc_vector[75]=total_inpc_vector[10]
mult_inpc_vector[76]=total_inpc_vector[8]
mult_inpc_vector[77]=total_inpc_vector[8]
mult_inpc_vector[78]=total_inpc_vector[8]
mult_inpc_vector[79]=total_inpc_vector[8]
mult_inpc_vector[80]=total_inpc_vector[1]
mult_inpc_vector[81]=total_inpc_vector[1]
mult_inpc_vector[82]=total_inpc_vector[1]
mult_inpc_vector[83]=total_inpc_vector[1]
mult_inpc_vector[84]=total_inpc_vector[1]
mult_inpc_vector[85]=total_inpc_vector[1]
mult_inpc_vector[86]=total_inpc_vector[1]
mult_inpc_vector[87]=total_inpc_vector[1]
mult_inpc_vector[88]=total_inpc_vector[1]
mult_inpc_vector[89]=total_inpc_vector[1]
mult_inpc_vector[90]=total_inpc_vector[1]
mult_inpc_vector[91]=total_inpc_vector[1]
mult_inpc_vector[92]=total_inpc_vector[1]
mult_inpc_vector[93]=total_inpc_vector[1]

mult_inpc_vector <- as.numeric(mult_inpc_vector)

## IPCA
vector_mult_ipca[1]=1
vector_mult_ipca[2]=1
vector_mult_ipca[3]=1
mult_ipca_vector[4]=total_ipca_vector[2]
mult_ipca_vector[5]=total_ipca_vector[3]
mult_ipca_vector[6]=total_ipca_vector[3]
mult_ipca_vector[7]=total_ipca_vector[3]
mult_ipca_vector[8]=total_ipca_vector[3]
mult_ipca_vector[9]=total_ipca_vector[3]
mult_ipca_vector[10]=total_ipca_vector[3]
mult_ipca_vector[11]=total_ipca_vector[3]
mult_ipca_vector[12]=total_ipca_vector[10]
mult_ipca_vector[13]=total_ipca_vector[10]
mult_ipca_vector[14]=total_ipca_vector[10]
mult_ipca_vector[15]=total_ipca_vector[3]
mult_ipca_vector[16]=total_ipca_vector[3]
mult_ipca_vector[17]=total_ipca_vector[3]
mult_ipca_vector[18]=total_ipca_vector[3]
mult_ipca_vector[19]=total_ipca_vector[3]
mult_ipca_vector[20]=total_ipca_vector[4]
mult_ipca_vector[21]=total_ipca_vector[4]
mult_ipca_vector[22]=total_ipca_vector[4]
mult_ipca_vector[23]=total_ipca_vector[5]
mult_ipca_vector[24]=total_ipca_vector[5]
mult_ipca_vector[25]=total_ipca_vector[5]
mult_ipca_vector[26]=total_ipca_vector[5]
mult_ipca_vector[27]=total_ipca_vector[5]
mult_ipca_vector[28]=total_ipca_vector[5]
mult_ipca_vector[29]=total_ipca_vector[5]
mult_ipca_vector[30]=total_ipca_vector[6]
mult_ipca_vector[31]=total_ipca_vector[6]
mult_ipca_vector[32]=total_ipca_vector[6]
mult_ipca_vector[33]=total_ipca_vector[6]
mult_ipca_vector[34]=total_ipca_vector[6]
mult_ipca_vector[35]=total_ipca_vector[6]
mult_ipca_vector[36]=total_ipca_vector[8]
mult_ipca_vector[37]=total_ipca_vector[7]
mult_ipca_vector[38]=total_ipca_vector[7]
mult_ipca_vector[39]=total_ipca_vector[7]
mult_ipca_vector[40]=total_ipca_vector[7]
mult_ipca_vector[41]=total_ipca_vector[7]
mult_ipca_vector[42]=total_ipca_vector[7]
mult_ipca_vector[43]=total_ipca_vector[7]
mult_ipca_vector[44]=total_ipca_vector[7]
mult_ipca_vector[45]=total_ipca_vector[7]
mult_ipca_vector[46]=total_ipca_vector[7]
mult_ipca_vector[47]=total_ipca_vector[7]
mult_ipca_vector[48]=total_ipca_vector[7]
mult_ipca_vector[49]=total_ipca_vector[7]
mult_ipca_vector[50]=total_ipca_vector[7]
mult_ipca_vector[51]=total_ipca_vector[7]
mult_ipca_vector[52]=total_ipca_vector[7]
mult_ipca_vector[53]=total_ipca_vector[7]
mult_ipca_vector[54]=total_ipca_vector[9]
mult_ipca_vector[55]=total_ipca_vector[9]
mult_ipca_vector[56]=total_ipca_vector[9]
mult_ipca_vector[57]=total_ipca_vector[9]
mult_ipca_vector[58]=total_ipca_vector[9]
mult_ipca_vector[59]=total_ipca_vector[9]
mult_ipca_vector[60]=total_ipca_vector[9]
mult_ipca_vector[61]=total_ipca_vector[8]
mult_ipca_vector[62]=total_ipca_vector[8]
mult_ipca_vector[63]=total_ipca_vector[8]
mult_ipca_vector[64]=total_ipca_vector[8]
mult_ipca_vector[65]=total_ipca_vector[8]
mult_ipca_vector[66]=total_ipca_vector[8]
mult_ipca_vector[67]=total_ipca_vector[8]
mult_ipca_vector[68]=total_ipca_vector[8]
mult_ipca_vector[69]=total_ipca_vector[8]
mult_ipca_vector[70]=total_ipca_vector[8]
mult_ipca_vector[71]=total_ipca_vector[8]
mult_ipca_vector[72]=total_ipca_vector[8]
mult_ipca_vector[73]=total_ipca_vector[8]
mult_ipca_vector[74]=total_ipca_vector[8]
mult_ipca_vector[75]=total_ipca_vector[10]
mult_ipca_vector[76]=total_ipca_vector[8]
mult_ipca_vector[77]=total_ipca_vector[8]
mult_ipca_vector[78]=total_ipca_vector[8]
mult_ipca_vector[79]=total_ipca_vector[8]
mult_ipca_vector[80]=total_ipca_vector[1]
mult_ipca_vector[81]=total_ipca_vector[1]
mult_ipca_vector[82]=total_ipca_vector[1]
mult_ipca_vector[83]=total_ipca_vector[1]
mult_ipca_vector[84]=total_ipca_vector[1]
mult_ipca_vector[85]=total_ipca_vector[1]
mult_ipca_vector[86]=total_ipc_vectorto 1]
mult_ipca_vector[87]=total_ipca_vector[1]
mult_ipca_vector[88]=total_ipca_vector[1]
mult_ipca_vector[89]=total_ipca_vector[1]
mult_ipca_vector[90]=total_ipca_vector[1]
mult_ipca_vector[91]=total_ipca_vector[1]
mult_ipca_vector[92]=total_ipca_vector[1]
mult_ipca_vector[93]=total_ipca_vector[1]

mult_ipca_vector <- as.numeric(mult_ipca_vector)


## average

vector_mult_medium[1]=1
vector_mult_medium[2]=1
vector_mult_medium[3]=1
average_mult_vector[4]=total_average_vector[2]
average_mult_vector[5]=total_average_vector[3]
average_mult_vector[6]=total_average_vector[3]
medium_mult_vector[7]=total_medium_vector[3]
average_mult_vector[8]=total_average_vector[3]
average_mult_vector[9]=total_average_vector[3]
average_mult_vector[10]=total_average_vector[3]
average_mult_vector[11]=total_average_vector[3]
average_mult_vector[12]=total_average_vector[10]
average_mult_vector[13]=total_average_vector[10]
average_mult_vector[14]=total_average_vector[10]
average_mult_vector[15]=total_average_vector[3]
average_mult_vector[16]=total_average_vector[3]
medium_mult_vector[17]=total_medium_vector[3]
average_mult_vector[18]=total_average_vector[3]
average_mult_vector[19]=total_average_vector[3]
average_mult_vector[20]=total_average_vector[4]
medium_mult_vector[21]=total_medium_vector[4]
average_mult_vector[22]=total_average_vector[4]
average_mult_vector[23]=total_average_vector[5]
average_mult_vector[24]=total_average_vector[5]
average_mult_vector[25]=total_average_vector[5]
average_mult_vector[26]=total_average_vector[5]
average_mult_vector[27]=total_average_vector[5]
average_mult_vector[28]=total_average_vector[5]
average_mult_vector[29]=total_average_vector[5]
average_mult_vector[30]=total_average_vector[6]
medium_mult_vector[31]=total_medium_vector[6]
average_mult_vector[32]=total_average_vector[6]
average_mult_vector[33]=total_average_vector[6]
average_mult_vector[34]=total_average_vector[6]
average_mult_vector[35]=total_average_vector[6]
average_mult_vector[36]=total_average_vector[8]
average_mult_vector[37]=total_average_vector[7]
average_mult_vector[38]=total_average_vector[7]
average_mult_vector[39]=total_average_vector[7]
average_mult_vector[40]=total_average_vector[7]
average_mult_vector[41]=total_average_vector[7]
average_mult_vector[42]=total_average_vector[7]
average_mult_vector[43]=total_average_vector[7]
average_mult_vector[44]=total_average_vector[7]
average_mult_vector[45]=total_average_vector[7]
average_mult_vector[46]=total_average_vector[7]
average_mult_vector[47]=total_average_vector[7]
average_mult_vector[48]=total_average_vector[7]
average_mult_vector[49]=total_average_vector[7]
average_mult_vector[50]=total_average_vector[7]
average_mult_vector[51]=total_average_vector[7]
average_mult_vector[52]=total_average_vector[7]
average_mult_vector[53]=total_average_vector[7]
average_mult_vector[54]=total_average_vector[9]
average_mult_vector[55]=total_average_vector[9]
average_mult_vector[56]=total_average_vector[9]
average_mult_vector[57]=total_average_vector[9]
average_mult_vector[58]=total_average_vector[9]
average_mult_vector[59]=total_average_vector[9]
average_mult_vector[60]=total_average_vector[9]
average_mult_vector[61]=total_average_vector[8]
average_mult_vector[62]=total_average_vector[8]
average_mult_vector[63]=total_average_vector[8]
average_mult_vector[64]=total_average_vector[8]
average_mult_vector[65]=total_average_vector[8]
average_mult_vector[66]=total_average_vector[8]
average_mult_vector[67]=total_average_vector[8]
average_mult_vector[68]=total_average_vector[8]
average_mult_vector[69]=total_average_vector[8]
average_mult_vector[70]=total_average_vector[8]
average_mult_vector[71]=total_average_vector[8]
average_mult_vector[72]=total_average_vector[8]
average_mult_vector[73]=total_average_vector[8]
average_mult_vector[74]=total_average_vector[8]
average_mult_vector[75]=total_average_vector[10]
average_mult_vector[76]=total_average_vector[8]
average_mult_vector[77]=total_average_vector[8]
average_mult_vector[78]=total_average_vector[8]
average_mult_vector[79]=total_average_vector[8]
average_mult_vector[80]=total_average_vector[1]
average_mult_vector[81]=total_average_vector[1]
average_mult_vector[82]=total_average_vector[1]
average_mult_vector[83]=total_average_vector[1]
average_mult_vector[84]=total_average_vector[1]
average_mult_vector[85]=total_average_vector[1]
average_mult_vector[86]=total_average_vector[1]
average_mult_vector[87]=total_average_vector[1]
average_mult_vector[88]=total_average_vector[1]
average_mult_vector[89]=total_average_vector[1]
average_mult_vector[90]=total_average_vector[1]
average_mult_vector[91]=total_average_vector[1]
average_mult_vector[92]=total_average_vector[1]
average_mult_vector[93]=total_average_vector[1]

mult_vector <- as.numeric(mult_vector)

## expense readjustment

)
readjusted_expense


##Adding aggregate expenses

### Consumption expense
### Consumption expenditure = Food + Housing + Clothing + Transportation +
### +Hygiene and personal care + Health care + Education + Recreation and culture
### + Tobacco + Personal services + Miscellaneous expenses

readjust_consumer_expense <- readjusted_expense %>% slice(4,5,23,30,38,43,54,61,67,68,73) %>% select(-1) %>% colSums(na.rm = TRUE)
expense_readjusted[3,2]=expense_consumo_readjust[1]
readjusted_expensea[3,3]=expenses_consumo_readjust[2]
expense_readjusted[3,4]=expense_consumo_readjust[3]
expense_readjusted[3,5]=expense_consumo_readjust[4]
expense_readjusted[3,6]=expense_consumo_readjust[5]
expense_readjusted[3,7]=expense_consumo_readjust[6]
expense_readjusted[3,8]=expense_consumo_readjust[7]
expense_readjusted[3,9]=expense_consumo_readjust[8]

expense_consumption_readjust

### Current expenses
### Current expenses = Consumer expenses + other current expenses

current_expense_readjust <- expense_readjusted %>% slice(3.80) %>% select(-1) %>% colSums()
current_expense_adjustment
readjusted_expense[2,2]=readjusted_current_expense[1]
readjusted_expense[2,3]=readjusted_current_expense[2]
readjusted_expense[2,4]=readjusted_current_expense[3]
readjusted_expense[2,5]=readjusted_current_expense[4]
readjusted_expense[2,6]=readjusted_current_expense[5]
readjusted_expense[2,7]=readjusted_current_expense[6]
readjusted_expense[2,8]=readjusted_current_expense[7]
readjusted_expense[2,9]=readjusted_current_expense[8]

### Total expense
### Total Expenses = Current Expenses + Increase in Assets = Decrease in Liabilities

readjust_total_expense <- readjusted_expense %>% slice(2,87,91) %>% select(-1) %>% colSums()
expense_total_adjustment
readjusted_expense[1,2]=readjusted_total_expense[1]
readjusted_expense[1,3]=readjusted_total_expense[2]
readjusted_expense[1,4]=readjusted_total_expense[3]
readjusted_expense[1,5]=readjusted_total_expense[4]
readjusted_expense[1,6]=readjusted_total_expense[5]
readjusted_expense[1,7]=readjusted_total_expense[6]
readjusted_expense[1,8]=readjusted_total_expense[7]
readjusted_expense[1,9]=readjusted_total_expense[8]



##Extraction of the total expense and calculation of the proportion per item
expense_readjusted_prop <- expense_readjusted

#Total
total_totalexpense <- as.numeric(readjusted_expense[1,2])
total_total expense
expense_readjusted_prop <- expense_readjusted_prop %>% mutate(expense_readjusted_prop, total = total/expensetotal_total)
expense_adjusted_prop

#Up to 2 minimum wages;
expensetotal_up to2 <- as.numeric(adjusted_expense[1,3])
expensetotal_ate2
expense_readjusted_prop <- expense_readjusted_prop %>% mutate(expense_readjusted_prop, up to2 = up to2/expensetotal_up2)


#More than 2 to 3 minimum wages;
expensetotal_m2a3 <- as.numeric(adjusted_expense[1,4])
total expense_m2a3
expense_readjusted_prop <- expense_readjusted_prop %>% mutate(expense_readjusted_prop, m2a3 = m2a3/expensetotal_m2a3)

#More than 3 to 6 minimum wages;
expensetotal_m3a6 <- as.numeric(adjusted_expense[1,5])
total expense_m3a6
expense_readjusted_prop <- expense_readjusted_prop %>% mutate(expense_readjusted_prop, m3a6 = m3a6/expensetotal_m3a6)

#More than 6 to 10 minimum wages;
expensetotal_m6a10 <- as.numeric(adjusted_expense[1,6])
total expense_m6a10
expense_readjusted_prop <- expense_readjusted_prop %>% mutate(expense_readjusted_prop, m6a10 = m6a10/expensetotal_m6a10)

#More than 10 to 15 minimum wages;
expensetotal_m10a15 <- as.numeric(adjusted_expense[1,7])
total expense_m10a15
expense_readjusted_prop <- expense_readjusted_prop %>% mutate(expense_readjusted_prop, m10a15 = m10a15/expensetotal_m10a15)

#\item More than 15 to 25 minimum wages;
expensetotal_m15a25 <- as.numeric(adjusted_expense[1,8])
total expense_m15a25
expense_readjusted_prop <- expense_readjusted_prop %>% mutate(expense_readjusted_prop, m15a25 = m15a25/expensetotal_m15a25)

#More than 25 minimum wages.
expensetotal_m25 <- as.numeric(adjusted_expense[1,9])
total expense_m25
expense_readjusted_prop <- expense_readjusted_prop %>% mutate(expense_readjusted_prop, m25 = m25/expensetotal_m25)

## After distributing the weight of each item in the total expenditure by minimum wage range, the next step is
## find the value of each basket with the reset
## For this we will find the proportion of the average income of each range and the proportion of total expenditure for each average income.
## As worksheet 1 of the POF is a monetary and non-monetary expense, we will use the total income (monetary and non-monetary)
## to distribute the values according to the proportion obtained in the expense_reajustada_prop worksheet
🇧🇷
yield_0 <- yield_0 %>% slice(c(-2,-4))
income_0 <- column_to_rownames(income_0, var = "...1")
yield_0 <- yield_0 %>% rename(total = ...2, up to2 = ...3, m2a3 = ...4, m3a6 = ...5, m6a10 = ...6, m10a15 = ...7 , m15a25 = ...8, m25 = ...9)
yield_0

## Yield extraction
total_yield <- slice(yield_0, c(2))
total_income


#line vector of monetary income in minimum wages
income_salmin <- (total_income)/954
yield_salmin
income_total_2022 <- income_salmin*1212
income_total_2022

# Ratio between total expenditure and total income

#POF
reason_expense_t_rend_t <- (slice(expense_0, 1) %>% select(-1))/total_income

# Total expense 2022 and by salary range
expense_total_2022 <- razao_despe_t_rend_t*rendimento_total_2022
expense_total_2022

total_expenses_2022_total <- as.numeric(total_expenses_2022[1])
expense_total_2022_up to2 <- as.numeric(expense_total_2022[2])
expense_total_2022_m2a3 <- as.numeric(total_expense_2022[3])
expense_total_2022_m3a6 <- as.numeric(expense_total_2022[4])
expense_total_2022_m6a10 <- as.numeric(expense_total_2022[5])
expense_total_2022_m10a15 <- as.numeric(expense_total_2022[6])
expense_total_2022_m15a25 <- as.numeric(expense_total_2022[7])
expense_total_2022_m25 <- as.numeric(expense_total_2022[8])


# New readjusted POF

=m25*expense_total_2022_m25)
readjusted_expense
expense_reajustada <- expense_readjustada %>% rename("up to two salaries" = ate2, "Greater than 2 salaries up to 3 salaries" = m2a3, "Greater than 3 salaries up to 6 salaries" = m3a6, "Greater than 6 salaries up to 10 salaries" = m6a10, "Greater than 10 salaries up to 15 salaries" = m10a15, "Greater than 15 salaries up to 25 salaries"=m15a25, "Greater than 25 salaries"=m25)
readjusted_expense

write_xlsx(readjusted_expense, "Readjusted - 53DF.xls.xlsx", col_names = TRUE)