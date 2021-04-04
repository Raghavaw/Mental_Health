
setwd('/Users/raghavawasthi/Desktop/COVID19/MentalHealth/Analysis/')

aprl30_df = read.csv('/Users/raghavawasthi/Desktop/COVID19/MentalHealth/Data/associatedpress-covid-impact-survey-public-data/01_April_30_covid_impact_survey.csv')
may12_df = read.csv('/Users/raghavawasthi/Desktop/COVID19/MentalHealth/Data/associatedpress-covid-impact-survey-public-data/02_May_12_covid_impact_survey.csv')

intersect(aprl30_df$SU_ID,may12_df$SU_ID) ##### 313 ids are cooman may be survey done twice for the same 

setdiff(names(may12_df),names(aprl30_df))  # "NAT_WGT_COMB_POP" "REG_WGT_COMB_POP" "MARITAL"  "LGBT"  four extra variabels in may 12 data 

## removing extra vars 
may12_df = may12_df[which(names(may12_df) %in% names(aprl30_df))]


####### Combined data 
combined_df = rbind(aprl30_df,may12_df)
combined_df$X = NULL


DataExplorer::plot_missing(data = combined_df,missing_only = T)

write.csv(combined_df,file='apr_may_Combined_Df.csv',row.names=F)


############# 
config <- list(
  "introduce" = list(),
  "plot_intro" = list(),
  "plot_str" = list(
    "type" = "diagonal",
    "fontSize" = 6,
    "width" = 1000,
    "margin" = list("left" = 350, "right" = 250)
  ),
  "plot_missing" = list("missing_only" = 'T'),
  "plot_histogram" = list(),
  "plot_density" = list(),
  "plot_qq" = list(sampled_rows = 1000L),
  "plot_bar" = list()
  
  )
create_report(combined_df[c(3:173)], config = config,report_title = 'Impact Survey Analysis ')

