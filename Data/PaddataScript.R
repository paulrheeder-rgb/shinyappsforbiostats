
#import data
library(haven)
library(readstata13)
library(summarytools)
library(dplyr)
library(compareGroups)
library(gtsummary)
library(editData)
library(flextable)
library(conflicted)

# import with readstata13
setwd("G:/My Drive/Paul/Box/scripts/workinginR/workinginR3/data")

df<- as.data.frame(read.dta13("padR4.dta"))

conflicts_prefer(dplyr::select)

# Easier just to fix 999 and 99 and -99 not in id var
df[ , names(df) != "id"] <- lapply(df[ , names(df) != "id"], function(x) {
  replace(x, x %in% c(999, 99, -99), NA)
})


#One group

desired_order <- c("age","race","sex","smo", "dmtype", 
                   "hyp", "pad", "b2m", "trgl","creat",
                   "ldl","tbir","tbil")
df2 <- df %>%
  select(all_of(desired_order), -id)

# Convert the first six columns to factors
df2$pad <- factor(df2$pad, levels = c(0,1),
                  labels = c("no PAD","PAD"))
df2$race <- factor(df2$race, levels = c(1:4),
                   labels = c("Black", "White", "Coloured", "Indian"))

df2$sex <- factor(df2$sex, levels = c(0,1), 
                  labels = c("Male", "Female") ) 

df2$smo  <- factor(df2$smo, levels = c(0,1,2), 
                   labels = c("Non Smoker", "Ex Smoker", "Current Smoker"))

df2$hyp <- factor(df2$hyp, levels = c(0,1), 
                  labels = c("non HT", "HT"))

df2$dmtype <- factor(df2$dmtype, levels = c( 1,2), 
                     labels = c(" Type 1", "Type 2"))

# Assign label names to each variable in df2

# easier way to do labels
library(expss)

df2 = apply_labels(df2,
                   pad = "PAD",
                   race = "Race",
                   sex = "Sex",
                   smo = "Smoking status",
                   dmtype= "Diabetes Type",
                   hyp = "Hypertension",
                   age = "Age",
                   b2m = "B2 microglobulin",
                   creat = "Creatinine",
                   ldl = "LDL",
                   trgl = "Triglycerides",
                   tbir = "TBI right",
                   tbil = "TBI left"  )
                   
 # show labels
sjPlot::view_df(df2)

          
