X <- sample(770:790,20,replace=TRUE)
Y <- sample(770:790,20,replace=TRUE)
D <- X-Y
A <- (X+Y)*0.5
myCor <- cor(D,A)

while( myCor > -0.825){
  X <- sample(770:790,25,replace=TRUE)
  Y <- sample(770:790,25,replace=TRUE)
  D <- X-Y
  A <- (X+Y)*0.5
  myCor <- cor(D,A)
}


myBAdf <- data.frame(X,Y,D,A)



MCS_plot_2 <-  ggplot(myBAdf, aes(x = A, y = D)) +
  geom_point(pch=17,colour="blue",alpha = 0.75,size=3) +
  geom_hline(yintercept = mean(myBAdf$D), colour = "#9999CC", lwd=1.25) +
  #geom_hline(yintercept = mean(BAplotDF$D) - (1.96 * sd(BAplotDF$D)), colour = "#CC6666", lwd=1.25, lty=2) +
  #geom_hline(yintercept = mean(BAplotDF$D) + (1.96 * sd(BAplotDF$D)), colour = "#CC6666", lwd=1.25,lty=2) +
  geom_hline(yintercept = 0, colour = "black", lty=3, size = 0.5) +
  ylab("Casewise Differences") +
  xlab("Casewise Averages")  + theme_bw() + theme_bw() + ggtitle("Simulated Data: Bland-Altman Plot",subtitle="Indication of Proportional Bias") +theme(
    axis.title.y = element_text(colour="grey20",size=14,face="bold"),
    axis.text.x = element_text(colour="grey20",size=14,face="bold"),
    axis.text.y = element_text(colour="grey20",size=14,face="bold"),  
    axis.title.x = element_text(colour="grey20",size=14,face="bold"),
    plot.title = element_text(colour="grey20",size=16, face = "bold"),
    plot.subtitle = element_text(colour="grey20",size=14, face = "bold"))



ggsave(filename = "PropBias.png", MCS_plot_2 ,      width = 10, height = 6, dpi = 300, units = "in", device='png')

