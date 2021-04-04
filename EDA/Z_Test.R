
setwd('/Users/raghavawasthi/Desktop/COVID19/MentalHealth/Analysis/Cleand_Data/')
library(dplyr)
library(tidyr)
df = read.csv('Aprl_May_DataforBN.csv')

########## Gender ############ 
### Hypotheis is Female are more stressed than Men 

Tbl = data.frame(table(df$GENDER,df$SOC5A))
table(df$GENDER)
res <- prop.test(x = c(5434,5629), n = c(9868 ,7886))
print(res)

################# Age group 
Tbl = data.frame(table(df$AGE4,df$SOC5A))
Tbl = dplyr::filter(Tbl,Tbl$Var2 == '(4) 5-7 days')
Tbl$Var2 = NULL
tb= data.frame(table(df$AGE4))
res <- prop.test(x =Tbl$Freq[-5],tb$Freq[-5])
linearTrend = prop.trend.test(x =Tbl$Freq[-5],tb$Freq[-5])
print(res)
