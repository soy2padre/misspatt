if(!require('ggplot2')) install.packages('ggplot2', dependencies=TRUE)
setwd("C:/Users/Ting/Dropbox/RDshared")
data <- read.csv("Simulation_results_10-05-2013_restructured.csv")
for (i in 1:9) {
d<-subset(data[(40*(i-1)+1):(40*i),])
d$pmiss
g <- ggplot(d, aes(x=Method, y = Correct))
#+ facet_wrap(~ type + ipc + ncomp, nrow=2) #+ geom_bar(aes(fill=method)) #+ scale_fill_grey() #stat=data$correct, 
p <- g+theme_bw()+theme(axis.line=element_line(color="black"), panel.border=element_rect(color="white"), strip.background=element_rect(fill="white",color="white"))+ggtitle(sprintf("Proportion Correct Components Recovered\nN=%d, %d%% Missing",d$nobs[1],d$pmiss[1])) + geom_bar(stat="identity",aes(fill=Method), color="black")+facet_grid(ipc+ncomp~type,FALSE)+ scale_fill_grey(start=1, end=0.2)+geom_abline(linetype="dashed",color="black",lwd=.3, intercept=.9,slope=0) + theme(legend.position="none")
pdf(paste("Excellent_Figure_",d$nobs[1],"_",d$pmiss[1],".pdf",sep=""))
print(p)
dev.off()
}
