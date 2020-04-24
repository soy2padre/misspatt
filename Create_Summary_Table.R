library(tidyverse)
library(openxlsx)
load('misspatt/misspatt00.Rdata')
colnames(misspatt)[2] <- 'n'

myout <- misspatt %>%
  group_by(n, f, ipc, pmiss, corrtype, ptype) %>%
  summarize(nreps=n(),
            pctparan = mean(f==nparan),
            pctkaiser = mean(f==nkaiser),
            pctjolliffe = mean(f==njolliffe),
            pctproflik = mean(f==nproflik),
            pctekc = mean(f==nekc))

write.xlsx(myout, file='summary_table.xlsx')