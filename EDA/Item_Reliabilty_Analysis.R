
setwd('/Users/raghavawasthi/Desktop/COVID19/MentalHealth/Analysis/Cleand_Data/')
df = read.csv('Encoded_Aprl_May_DataforBN.csv')

df$X = NULL
View(names(df))
df = df[,c(27:31)]   ##### only mental Health Variable 
library(psych)
View(names(df))
df = 3-df 
alpha.test = alpha(df)

result = alpha.test$item.stats

result$var = rownames(result)
result = result[c(2,8)]
rownames(result) = NULL
names(result)[1] = 'Cronbach’s α'
write.csv(result,file='MentalHelathItemReliabiltyAnalysis.csv',row.names = F)
library(ggplot2)
p<-ggplot(data=result, aes(x= var, y= raw.r)) +
  geom_bar(stat="identity")+xlab('Mental Health Variable') + 
  ylab('Cronbach’s α')
p

# Horizontal bar plot
p + coord_flip()
ggsave('Item_Reliabilty_Analysis.png',dpi = 300)
