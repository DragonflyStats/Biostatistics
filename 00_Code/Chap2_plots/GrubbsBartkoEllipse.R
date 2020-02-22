Round <- 1:12
Fotobalk <- c(793.8, 793.1, 792.4, 794, 791.4, 792.4, 791.7, 792.3, 789.6,   794.4, 790.9, 793.5)
Counter <- c(794.6, 793.9, 793.2, 794, 792.2, 793.1, 792.4, 792.8, 790.2,  795, 791.6, 793.8)
Terma <- c(793.2, 793.3, 792.6, 793.8, 791.6, 791.6, 791.6, 792.4, 788.5,  794.7, 791.3, 793.5)

grubbs <- data.frame(Round, Fotobalk,Counter, Terma)


Method1 <- grubbs$Fotobalk
Method2 <- grubbs$Counter

Dif = Method1-Method2 
Avg = 0.5*(Method1+Method2)
BAplotDF = data.frame(Method1,Method2,Avg,Dif)

BAplotDF2 <- BAplotDF %>% bind_rows( data.frame(  Method1 = 782, Method2 =782,    Avg  =786,Dif= -0.6083333))
BAplotDF3 <- data.frame(  Method1 = 782, Method2 =782,    Avg  =786,Dif= -0.6083333)

MCS_plot_3 <-  ggplot(BAplotDF2, aes(x = Avg, y = Dif)) +
  geom_point(pch=17,col="red",alpha = 0.75,size=3.2) +
  geom_hline(yintercept = mean(BAplotDF$Dif), colour = "#9999CC", lwd=1.25) +
  geom_hline(yintercept = mean(BAplotDF$Dif) - (1.96 * sd(BAplotDF$Dif)), colour = "#CC6666", lwd=0.75, lty=2) +
  geom_hline(yintercept = mean(BAplotDF$Dif) + (1.96 * sd(BAplotDF$Dif)), colour = "#CC6666", lwd=0.75,lty=2) +
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
