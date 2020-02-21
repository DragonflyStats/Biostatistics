library(ggplot2)

Method1 <- grubbs$Fotobalk
Method2 <- grubbs$Counter

Dif = Method1-Method2 
Avg = 0.5*(Method1+Method2)
BAplotDF = data.frame(Method1,Method2,Avg,Dif)

MCS_plot_1 <- ggplot(BAplotDF, aes(x = Method1, y = Method2)) +
  geom_point(pch=17,col="red",alpha = 0.75,size=2.5) +
  geom_abline(intercept=0,slope=1, colour = "#9999CC", lwd=1,lty=2)+
#  stat_smooth(method="lm",se=FALSE, colour = "blue", lwd=1,lty=3) + 
  geom_point(pch=17,col="red",alpha = 0.75,size=2.5) +
  ylab("Counter ") +
  xlab("Fotobalk")  + theme_bw() +  ggtitle("Grubbs Artillery Data",
                                               subtitle = "Scatterplot of Fotobalk vs Counter") +theme(
                                                 axis.title.y = element_text(colour="grey20",size=14,face="bold"),
                                                 axis.text.x = element_text(colour="grey20",size=14,face="bold"),
                                                 axis.text.y = element_text(colour="grey20",size=14,face="bold"),  
                                                 axis.title.x = element_text(colour="grey20",size=14,face="bold"),
                                                 plot.title = element_text(size = 16, face = "bold"),
                                                 plot.subtitle = element_text(size = 14, face = "bold"))
ggsave(filename = "GrubbsScatter.png", MCS_plot_2,      width = 10, height = 8, dpi = 300, units = "in", device='png')



MCS_plot_2 <-  ggplot(BAplotDF, aes(x = Avg, y = Dif)) +
  geom_point(pch=17,col="red",alpha = 0.75,size=2.5) +
  geom_hline(yintercept = mean(BAplotDF$Dif), colour = "#9999CC", lwd=1.25) +
  #geom_hline(yintercept = mean(BAplotDF$Dif) - (1.96 * sd(BAplotDF$Dif)), colour = "#CC6666", lwd=1.25, lty=2) +
  #geom_hline(yintercept = mean(BAplotDF$Dif) + (1.96 * sd(BAplotDF$Dif)), colour = "#CC6666", lwd=1.25,lty=2) +
  geom_hline(yintercept = 0, colour = "black", lty=3, lwd=1.25) +
  geom_point(pch=17,col="red",alpha = 0.75,size=2.5) +
  ylab("Casewise Differences") +
  xlab("Casewise Averages")  + theme_bw()  + ggtitle("Grubbs Artillery Data",
                                                                 subtitle = "Bland-Altman Plot: Fotobalk vs Counter") +theme(
    axis.title.y = element_text(colour="grey20",size=14,face="bold"),
    axis.text.x = element_text(colour="grey20",size=14,face="bold"),
    axis.text.y = element_text(colour="grey20",size=14,face="bold"),  
    axis.title.x = element_text(colour="grey20",size=14,face="bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold")) + ylim(-1,0.15)

ggsave(filename = "GrubbsBAplot-noLOA.png", MCS_plot_2,      width = 10, height = 8, dpi = 300, units = "in", device='png')


BA_FC_plot <-  ggplot(BAplotDF, aes(x = Avg, y = Dif)) +
  geom_point(pch=17,col="red",alpha = 0.75,size=2.5) +
  geom_hline(yintercept = mean(BAplotDF$Dif), colour = "#9999CC", lwd=1.25) +
  geom_hline(yintercept = mean(BAplotDF$Dif) - (1.96 * sd(BAplotDF$Dif)), colour = "#9999CC", lwd=1.25, lty=2) +
  geom_hline(yintercept = mean(BAplotDF$Dif) + (1.96 * sd(BAplotDF$Dif)), colour = "#9999CC", lwd=1.25,lty=2) +
  geom_hline(yintercept = 0, colour = "black", lty=3, lwd=1.25) +
  geom_point(pch=17,col="red",alpha = 0.75,size=2.5) +
  ylim(-1.3,0.25) + 
  ylab("Casewise Differences") +
  xlab("Casewise Averages")  + theme_bw() + theme_bw() + ggtitle("Grubbs Artillery Data",subtitle = "Bland-Altman Plot: Fotobalk vs Counter") +theme(
    axis.title.y = element_text(colour="grey20",size=14,face="bold"),
    axis.text.x = element_text(colour="grey20",size=14,face="bold"),
    axis.text.y = element_text(colour="grey20",size=14,face="bold"),  
    axis.title.x = element_text(colour="grey20",size=14,face="bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"))

ggsave(filename = "GrubbsBAplot-LOA.png", BA_FC_plot,      width = 10, height = 8, dpi = 300, units = "in", device='png')


MCS_plot_2 <-  ggplot(BAplotDF, aes(x = Avg, y = Dif)) +
  geom_point(pch=17,col="red",alpha = 0.75,size=2.5) +
  geom_hline(yintercept = mean(BAplotDF$Dif), colour = "#9999CC", lwd=1.25) +
  geom_hline(yintercept = mean(BAplotDF$Dif) - (1.96 * sd(BAplotDF$Dif)), colour = "#9999CC", lwd=0.75, lty=2) +
  geom_hline(yintercept = mean(BAplotDF$Dif) + (1.96 * sd(BAplotDF$Dif)), colour = "#9999CC", lwd=0.75,lty=2) +
  geom_hline(yintercept = 0, colour = "black", lty=3, lwd=1.25) +
  geom_point(pch=17,col="red",alpha = 0.75,size=2.5) +
  stat_ellipse(lwd=1.0,colour = "#CC6666") + 
  ylim(-1.3,0.25) + 
  ylab("Casewise Differences") +
  xlab("Casewise Averages")  + theme_bw() + theme_bw() + ggtitle("Grubbs Artillery Data",subtitle = "Bland-Altman Plot: Fotobalk vs Counter - with Ellipse") +theme(
    axis.title.y = element_text(colour="grey20",size=14,face="bold"),
    axis.text.x = element_text(colour="grey20",size=14,face="bold"),
    axis.text.y = element_text(colour="grey20",size=14,face="bold"),  
    axis.title.x = element_text(colour="grey20",size=14,face="bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"))

ggsave(filename = "GrubbsBartko.png", MCS_plot_2,      width = 10, height = 8, dpi = 300, units = "in", device='png')




BAplotDF2 <- BAplotDF %>% bind_rows( data.frame(  Method1 = 782, Method2 =782,    Avg  =786,Dif= -0.6083333))
BAplotDF3 <- data.frame(  Method1 = 782, Method2 =782,    Avg  =786,Dif= -0.6083333)

MCS_plot_3 <-  ggplot(BAplotDF2, aes(x = Avg, y = Dif)) +
  geom_point(pch=17,col="red",alpha = 0.75,size=3.2) +
  geom_hline(yintercept = mean(BAplotDF$Dif), colour = "#9999CC", lwd=1.25) +
  geom_hline(yintercept = mean(BAplotDF$Dif) - (1.96 * sd(BAplotDF$Dif)), colour = "#9999CC", lwd=0.75, lty=2) +
  geom_hline(yintercept = mean(BAplotDF$Dif) + (1.96 * sd(BAplotDF$Dif)), colour = "#9999CC", lwd=0.75,lty=2) +
  geom_hline(yintercept = 0, colour = "black", lty=3, lwd=1.25) +
  geom_point(pch=17,col="red",alpha = 0.75,size=3.2) +
  stat_ellipse(lwd=1.0,colour = "#CC6666") + 
  ylim(-1.3,0.25) + 
  ylab("Casewise Differences") +
  xlab("Casewise Averages")  + theme_bw() + theme_bw() + ggtitle("Grubbs Artillery Data",subtitle = "Bland-Altman Plot: Fotobalk vs Counter - with Ellipse") +theme(
    axis.title.y = element_text(colour="grey20",size=14,face="bold"),
    axis.text.x = element_text(colour="grey20",size=14,face="bold"),
    axis.text.y = element_text(colour="grey20",size=14,face="bold"),  
    axis.title.x = element_text(colour="grey20",size=14,face="bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold")) + 
  geom_point(data=BAplotDF3,aes(x = Avg, y = Dif),pch=17,col="black",alpha = 0.75,size=3.2)

ggsave(filename = "GrubbsBartko2.png", MCS_plot_3,      width = 10, height = 8, dpi = 300, units = "in", device='png')

##################



Method1 <- grubbs$Fotobalk
Method2 <- grubbs$Counter

Dif = Method1-Method2 
Avg = 0.5*(Method1+Method2)
BAplotDF = data.frame(Method1,Method2,Avg,Dif)

BA_FC_plot <-  ggplot(BAplotDF, aes(x = Avg, y = Dif)) +
  geom_point(pch=17,col="red",alpha = 0.75,size=2.5) +
  geom_hline(yintercept = mean(BAplotDF$Dif), colour = "#9999CC", lwd=1.25) +
  geom_hline(yintercept = mean(BAplotDF$Dif) - (1.96 * sd(BAplotDF$Dif)), colour = "#9999CC", lwd=1.25, lty=2) +
  geom_hline(yintercept = mean(BAplotDF$Dif) + (1.96 * sd(BAplotDF$Dif)), colour = "#9999CC", lwd=1.25,lty=2) +
  geom_hline(yintercept = 0, colour = "darkgreen", lty=3, lwd=1.25) +
  geom_point(pch=17,col="red",alpha = 0.75,size=2.75) +
  ylim(-1.5,1.45) + 
  xlim(787.5,796) +
  ylab("Casewise Differences") +
  xlab("Casewise Averages")  + theme_bw() + theme_bw() + ggtitle("Fotobalk vs Counter") +theme(
    axis.title.y = element_text(colour="grey20",size=14,face="bold"),
    axis.text.x = element_text(colour="grey20",size=14,face="bold"),
    axis.text.y = element_text(colour="grey20",size=14,face="bold"),  
    axis.title.x = element_text(colour="grey20",size=14,face="bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"))


Method1 <- grubbs$Fotobalk
Method2 <- grubbs$Terma

Dif = Method1-Method2 
Avg = 0.5*(Method1+Method2)
BAplotDF = data.frame(Method1,Method2,Avg,Dif)
BA_FT_plot <-  ggplot(BAplotDF, aes(x = Avg, y = Dif)) +
  geom_point(pch=17,col="red",alpha = 0.75,size=2.75) +
  geom_hline(yintercept = mean(BAplotDF$Dif), colour = "#9999CC", lwd=1.25) +
  geom_hline(yintercept = mean(BAplotDF$Dif) - (1.96 * sd(BAplotDF$Dif)), colour = "#9999CC", lwd=1.25, lty=2) +
  geom_hline(yintercept = mean(BAplotDF$Dif) + (1.96 * sd(BAplotDF$Dif)), colour = "#9999CC", lwd=1.25,lty=2) +
  geom_hline(yintercept = 0, colour = "darkgreen", lty=3, lwd=1.25) +
  geom_point(pch=17,col="red",alpha = 0.75,size=2.5) +
  ylim(-1.5,1.45) + 
  xlim(787.5,796) + 
  ylab("Casewise Differences") +
  xlab("Casewise Averages")  + theme_bw() + theme_bw() + ggtitle("Fotobalk vs Terma") +theme(
    axis.title.y = element_text(colour="white",size=10,face="bold"), 
    axis.text.x = element_text(colour="grey20",size=14,face="bold"),
    axis.text.y = element_text(colour="white",size=10,face="bold"), 
    axis.title.x = element_text(colour="grey20",size=14,face="bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"))


Combi_MCS <- grid.arrange( BA_FC_plot ,BA_FT_plot , ncol=2)

ggsave(filename = "GrubbsDataTwoBAplots.png", Combi_MCS,      width = 12, height = 6, dpi = 300, units = "in", device='png')
