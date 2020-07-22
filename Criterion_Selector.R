# Recursive partitioning of output
# Used to guide selection of method based on data characteristics
library(tidyverse)
library(rpart)
library(ramify)

colMeans(smallout[,8:12])
# Use below to provide more conditions where parallel analysis is not tied for best
#myout$best <- argmax(myout[,c(10, 11, 9, 12, 8)], rows=TRUE)
# Favors parallel analysis because it's first
#myout$best <- argmax(myout[,8:12], rows=TRUE)
# Favors best overall
smallout$best <- argmax(smallout[,c(8, 12, 9, 11)], rows=TRUE)

# Cross-validated recursive partitioning
cfit <- rpart(best ~ n + f + ipc + pmiss, data=smallout, method="class", control = rpart.control(xval = 100, minbucket = 5, cp = 0))
printcp(cfit)
fit <- prune(cfit, cp = 0.02)

# Show output
ppi <- 300
tiff(filename="Decision_Tree.tiff", width=6*dpi, height=6*dpi, res=ppi)
par(mar=rep(0.1,4))
plot(fit,  branch = 0.3, compress = TRUE)
text(fit)
dev.off()
summary(fit)