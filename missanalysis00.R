# DATA ANALYSIS
# Tabula Rasa
rm(list = ls())

# Dependencies
if (!require('tidyverse'))
  install.packages('tidyverse', dependencies = TRUE)
if (!require('kableExtra'))
  install.packages('kableExtra', dependencies = TRUE)

# Load Data
load('misspatt00.Rdata')

# Recode Ns
misspatt$n <- recode_factor(misspatt$n, `1`='100', `2`='250', `3`='1000')

# Create Summary Variables
misspatt2 <- misspatt %>%
  mutate(pctparan=nparan==f,
         pctkaiser=nkaiser==f,
         pctjolliffe=njolliffe==f,
         pctproflik=nproflik==f,
         pctekc=nekc==f)

# Turn into Large Table
bigtable <- 
  misspatt2 %>%
  group_by(n, f, ipc, pmiss, corrtype) %>%
  summarize(mean(pctparan),
            mean(pctkaiser),
            mean(pctjolliffe),
            mean(pctproflik),
            mean(pctekc), na.rm=TRUE)

# Write Output
write_excel_csv(bigtable, path='./bigtable.csv')