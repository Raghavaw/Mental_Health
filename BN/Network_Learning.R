
setwd('/Users/raghavawasthi/Desktop/COVID19/MentalHealth/Analysis/Cleand_Data/')

library(bnlearn)
set.seed(123)

df = readxl::read_xlsx('V1.0_Aprl_May_Data.xlsx')
df = df[,-c(1:6)]

## Romoved NA Values by Not_Given
df = data.frame(lapply(df,as.character))
for(i in 1:ncol(df)){
  levels(df[,i]) = c(levels(df[,i]),'Not_Given')
}
df[is.na(df)] = 'Not_Given'

# df$AGE4 = NULL
# df$AGE7  = NULL
# df$AGE_BANNER = NULL

########## Remove Columns Which have Only 1 Level
df = df[, sapply(df, function(col) length(unique(col))) > 1]

###### Defining Network Parameter
bootstrap_replicates = 101
sample_size = 0.51
edgeStrength = 0.51
directionStrength = 0.1
algorithm = "hc"
score = "aic"
train_test_smp_size = 0.80

smp_size <- floor(train_test_smp_size * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size =smp_size)
train_data <- df[train_ind, ]
test_data <- df[-train_ind, ]

#------------------------------------------ DO STRUCTURE LEARNING ---------------------------------------------------------------------------


cl <<- parallel::makeCluster(4, type="SOCK")
print("Learning structure and parameters")

if(bootstrap_replicates > 1)
{
  # bn.hc.boot <<- boot.strength(data = train_data, R = bootstrap_replicates, algorithm = algorithm, algorithm.args=list(score = score,iss=input$iss,exp = INTvar),cluster = cl,
  #                              algorithm.args=list(blacklist=bl))
  bn.hc.boot <<- boot.strength(data = train_data, R = bootstrap_replicates, algorithm =algorithm, algorithm.args = list(score=score), cluster = cl)
  # save(bn.hc.boot, file = bn_hc_boot_filename)
  print("Structure learning done")
  bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength > edgeStrength & bn.hc.boot$direction > directionStrength,]
  # save(bn.hc.boot.pruned, file = bn_hc_boot_prunned_filename)
  bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
  # save(bn.hc.boot.average, file = bn_hc_boot_average_filename)
  bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,train_data[,names(bn.hc.boot.average$nodes)],method = "bayes")
  # save(bn.hc.boot.fit,file = bn_hc_boot_fit_filename)
}else{
  print('oneTime')
  bn.hc.boot.average <<- cextend(bnlearn::hc(train_data,cluster = cl,algorithm =algorithm, algorithm.args = list(score=score,blacklist=bl)))
  # save(bn.hc.boot.average, file = bn_hc_boot_average_filename)
  bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,train_data[,names(bn.hc.boot.average$nodes)],method = "bayes")
  # save(bn.hc.boot.fit,file = bn_hc_boot_fit_filename)
}
print("Parameter learning done")

# save(bn.hc.boot.average,file='otBIC_Staphylococcus_CompNetwork.RData')
# save(bn.hc.boot,file='101BootBIC_Middle_DiffiEnterobacterCloa.RData')
# save(bn.hc.boot.pruned,file='101BootBIC_High_StaphylococcusDiff.RData')
save(bn.hc.boot.pruned,file='101BootPrunedBIC_WithoutBlackListing_BN.RData')

# # # # # # 

write.csv(df,'Aprl_May_DataforBN.csv',row.names = F)


