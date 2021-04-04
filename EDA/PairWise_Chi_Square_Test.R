
library(pheatmap)
setwd('/Users/ASUS/3D Objects/2020/HomelssNess/V1.20_work/Association/')
data = read.csv('Discretised_imputed_rm_10PerMissing_GBD_HI_Homelessness.csv')

lapply(data,typeof)

data = data.frame(sapply(data,as.factor))

Homeless_vars = colnames(data)[121:162]
GBD_HI = colnames(data)[c(1:120)]

# tbl = table(data[,1],data[,44])
# tst=chisq.test(tbl) 
# tst$p.value


#pValue_df = data.frame()
pValue_df = data.frame(matrix(-9,nrow = 120,ncol = 42))
names(pValue_df)=Homeless_vars
rownames(pValue_df)=GBD_HI

for(i in c(1:42))
{
  for(j in c(1:120))
  {
    tbl = table(data[,i+120],data[,j])
    tst = chisq.test(tbl)
    print(tst$statistic)
    pValue_df[j,i] = tst$p.value
    
  }
  
}
pValue_df = -log10(pValue_df)
write.csv(pValue_df,file='pValue.csv')
pheatmap(t(pValue_df),cellwidth = 9,cellheight = 9,cluster_rows = F,cluster_cols = F,legend = F)


