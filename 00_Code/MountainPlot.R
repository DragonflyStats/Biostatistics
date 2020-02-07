
library(mountainplot)

dmice <- data.frame(
  albumen=c(156,282,197,297,116,127,119,29,253,122,349,110,143,64,26,86,122,455,655,14,
            391,46,469,86,174,133,13,499,168,62,127,276,176,146,108,276,50,73,
            82,100,98,150,243,68,228,131,73,18,20,100,72,133,465,40,46,34, 44),
  group=c(rep('normal',20), rep('alloxan', 18), rep('insulin', 19))
)

mountainplot(~albumen, data=dmice, group=group, auto.key=list(columns=3),
             main="Diabetic mice", xlab="Nitrogen-bound bovine serum albumen")

Round <- 1:12
Fotobalk <- c(793.8, 793.1, 792.4, 794, 791.4, 792.4, 791.7, 792.3, 789.6,   794.4, 790.9, 793.5)
Counter <- c(794.6, 793.9, 793.2, 794, 792.2, 793.1, 792.4, 792.8, 790.2,  795, 791.6, 793.8)
Terma <- c(793.2, 793.3, 792.6, 793.8, 791.6, 791.6, 791.6, 792.4, 788.5,  794.7, 791.3, 793.5)

myData <- data.frame(Round, Fotobalk,Counter, Terma)

Grubbs <- data.frame( Round = rep(Round,3))


mountainplot(~Speed, data=Grubbs, lty=c(1,2,3), group=Method, auto.key=list(columns=3),
main="Grubbs's Artillery Data", xlab="Mountain Plot")

library(tidyr)

Grubbs <- myData %>% gather("Method","Speed",Fotobalk:Terma)

mountainplot(~y, data=sbp, group=meth,auto.key=list(columns=3),
             main="Systolic Blood Pressure", xlab="Mountain Plot")

plvol3 <- plvol
plvol3$y[plvol3$meth=="Nadler"] = plvol3$y[plvol3$meth=="Nadler"]-98.5
plvol3$y[plvol3$meth=="Hurley"] = plvol3$y[plvol3$meth=="Hurley"]-89.2

mountainplot(~y, data=plvol, group=meth,auto.key=list(columns=2),
             main="Plasma Volume Data ", xlab="Plasma Volume (%) ")


mountainplot(~y, data=plvol2, group=meth,auto.key=list(columns=2),
             main="Plasma Volume Data (Adjusted for Bias)", xlab="Plasma Volume (%) ")

mountainplot(~y, data=plvol3, group=meth,auto.key=list(columns=2),
             main="Plasma Volume Data (Mean Centred)", xlab="Plasma Volume (%) ") 
vline(x=0)
