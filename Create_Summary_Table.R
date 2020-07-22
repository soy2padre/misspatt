library(tidyverse)
library(openxlsx)
load('misspatt00.Rdata')
colnames(misspatt)[2] <- 'n'

myout <- misspatt %>% 
  mutate_at(.vars=c(1:5, 8:12), as.numeric)%>% 
  group_by(n, f, ipc, pmiss, corrtype, ptype) %>%
  summarize(nreps=n(),
            pctparan = mean(abs(f-nparan)==0),
            pctkaiser = mean(abs(f-nkaiser)==0),
            pctjolliffe = mean(abs(f-njolliffe)==0),
            pctproflik = mean(abs(f-nproflik)==0),
            pctekc = mean(abs(f-nekc)==0))

write.xlsx(myout, file='summary_table.xlsx')

newdat <- misspatt %>% filter(corrtype=="pearson", ptype=='pca')
outdata <- summary(aov(nparan~(n*f*ipc*pmiss), data=newdat))

smallout <- newdat %>%
  mutate_at(.vars=c(1:5, 8:12), as.numeric)%>% 
  group_by(n, f, ipc, pmiss, corrtype, ptype) %>%
  summarize(nreps=n(),
            pctparan = mean(abs(f-nparan)==0),
            pctkaiser = mean(abs(f-nkaiser)==0),
            pctproflik = mean(abs(f-nproflik)==0),
            pctekc = mean(abs(f-nekc)==0))

write.xlsx(smallout, file='smaller_summary_table.xlsx')
