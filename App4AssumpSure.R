# Check test assumptions with AssumpSure
# run this script
# export df file to CSV format
setwd("G:/My Drive/Paul/Box/scripts/workinginR/workinginR3/data")
load("paddataClean.RData")
df<-as.data.frame(paddata)
library(writexl)
write.csv(df,"paddataclean.csv")  # Assumpsure needs csv file
library(AssumpSure)

launch_app()
  
# In the app, load the CSV file "df2.csv" and check the assumptions for your data.