setwd('/Users/raghavawasthi/Desktop/COVID19/MentalHealth/Analysis/Cleand_Data/')

library(bnlearn)
library(gRain)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
load('101BootPrunedBIC_WithAGe_BN.RData')
data =  read.csv('Aprl_May_DataforBN.csv')
data$PHYS2_REF = NULL
data$PHYS7_REF = NULL
data$PHYS7_DK = NULL
data = data.frame(lapply(data,as.factor))


j =1
YesNo_col = c()

for( i in 1:ncol(data)){
  lev = levels(as.factor(data[,i]))
  if('(1) Yes' %in% lev){
    YesNo_col[j] = names(data)[i]
    j = j+1
  }
}
# bn.hc.boot.fit <- bn.fit(bn.hc.boot.average,data[,names(bn.hc.boot.average$nodes)],method = "bayes")
bn.hc.boot.average <- cextend(averaged.network(bn.hc.boot.pruned))
bn.hc.boot.fit <- bn.fit(bn.hc.boot.average,data[,names(bn.hc.boot.average$nodes)],method = "bayes")

# SortInterval<-function(x){
#   y=gsub(pattern = '\\[|\\)|\\]',replacement = "",x)
#   z=gsub(pattern = '\\,',replacement = "-",y)
#   t = readr::parse_number(z)
#   x = as.data.frame(x)
#   x$num = t
#   xx = x[order(x$num),]
#   xx = xx[,1]
#   return(xx)
# }  

Infr_df = data.frame(matrix(-9,nrow = 2*length(YesNo_col),ncol = 6))
vars_Yes = paste('Yes',YesNo_col,sep = '_')
vars_No = paste('No',YesNo_col,sep = '_')

names(Infr_df) = c('Variables','Variables_1','Levels','Mean_Inference','Lower','Upper')
Infr_df$Variables_1 = c(vars_Yes,vars_No)
Infr_df$Variables = c(YesNo_col,YesNo_col)
n= nrow(Infr_df)/2
for(i in  1:nrow(Infr_df)){
  print(i)
  if(i<=n){
    
    Infr_df[i,3] = '(1) Yes'
  }
  else{
    Infr_df[i,3] = '(2) No'
  }
  
}


for(i in 1:nrow(Infr_df)){
  Boot = data.frame(matrix(-99,nrow = 10,ncol = 2))
  names(Boot) = c('epoch','Inference')
  for(epochs in 1:10){
    x = (noquote(Infr_df$Variables[i]))
    infr= prop.table(table(cpdist(bn.hc.boot.fit,nodes = c('SOC5A'),evidence = c(eval(parse(text = x))== Infr_df$Levels[i]))))
    infr = data.frame(infr)
    print(infr)
    Boot$epoch[epochs]=epochs
    Boot$Inference[epochs]=infr[1,2]
  }
  Infr_df$Mean_Inference[i] = mean(Boot$Inference)
  Infr_df$Lower[i] = mean(Boot$Inference) - 1.96*sd(Boot$Inference)
  Infr_df$Upper[i] = mean(Boot$Inference) + 1.96*sd(Boot$Inference)
  
}

# infr= prop.table(table(cpdist(bn.hc.boot.fit,nodes = c('Tot.Cases_per_1M_pop'),evidence = c(GDP_nominal_per_capita == levels(data$GDP_nominal_per_capita)[1]))))
# x = (noquote(Infr_df$Variables[1]))
# infr= prop.table(table(cpdist(bn.hc.boot.fit,nodes = c('Tot.Cases_per_1M_pop'),evidence = c(eval(parse(text = x)) == Infr_df$Levels[1]))))
# infr = data.frame(infr)
# infr


Infr_df = na.omit(Infr_df)


plot_df = Infr_df[,c('Variables','Mean_Inference','Lower','Upper')]
plot_df$Group_var = NA
plot_df$Group_var[c(1:n)] = 'Yes'
plot_df$Group_var[c(n:nrow(Infr_df))] = 'No'

names(plot_df) = c('Covariate','prob_less_Than1day_Stress' ,'Lower',
                   'Upper','Covariate_Levels')

# ggplot(plot_df,aes(Covariate,
#                   y=probability_of_Deaths_per_1M_pop_High,ymin= Lower,ymax= Upper,fill= Covariate_Levels))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   geom_pointrange(aes(shape= Covariate_Levels))
# ggsave('High_Deaths_per_1M_pop_1M_pop.png',dpi=300)
# 

plot_df = plot_df[c(1:10,101:110),]   #### visual plot 
p = ggplot(plot_df, aes(x= Covariate, y= prob_less_Than1day_Stress, fill=Covariate_Levels)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_errorbar(aes(ymin= Lower, ymax=Upper), width=.2,position=position_dodge(.9))

p

# ggsave('plot.png',dpi=300)


