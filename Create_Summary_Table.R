library(tidyverse)
library(openxlsx)
load('misspatt00.Rdata')
colnames(misspatt)[2] <- 'n'

myout <- misspatt %>% 
  mutate_at(.vars=c(1:5, 8:12), as.numeric)%>% 
  group_by(n, f, ipc, pmiss, corrtype, ptype) %>%
  summarize(nreps=n(),
            pctparan = mean(abs(f-nparan)<=1),
            pctkaiser = mean(abs(f-nkaiser)<=1),
            pctjolliffe = mean(abs(f-njolliffe)<=1),
            pctproflik = mean(abs(f-nproflik)<=1),
            pctekc = mean(abs(f-nekc)<=1))

write.xlsx(myout, file='summary_table.xlsx')