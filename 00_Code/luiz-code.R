library(ggplot2)
library(dplyr)
library(readr)
Luiz <- read_csv("https://raw.githubusercontent.com/DragonflyStats/MCS-code/master/ArchiveData/Luiz.csv")

LuizTemp <- cbind(Luiz[1:17,],Luiz[19:35,],Luiz[36:52,])
xtable(LuizTemp)

Method1 <- luiz$Urography 
Method2 <- luiz$Tomography 
Diff = Method1-Method2 
mean(Diff)

Method1 <- Method1 - mean(Method1)
Method2 <- Method2 - mean(Method2)
Quant <- c(0:50/100,49:0/100)
Method1 <- quantile(Method1,0:100/100)
Method2 <- quantile(Method2,0:100/100)
method <- c(rep("Method1",length(Method1)), rep("Method2",length(Method2))) %>% factor()
myMountainPlot <- data.frame(value=c(Method1,Method2),quant =c(Quant,Quant),method)
ggplot(data=myMountainPlot,aes(x=value,y=quant,col=method)) + geom_step() + theme_bw() + geom_vline(xintercept=0,col="red",lty=2) + geom_hline(yintercept=0)
ggplot(data=myMountainPlot,aes(x=value,y=quant,col=method)) + geom_step(lwd=1.2) + theme_bw() + geom_vline(xintercept=0,col="red",lty=2) + geom_hline(yintercept=0)+ scale_color_manual(values=c("#CC6666", "#9999CC"))

