library(Hmisc)
library(ggplot2)
##----------------------
#Plots second submission: Figure 1 and 2 the same, figure 3 new; old figure 3 left out.
#neuer Versuch von Plot der Vorhersage mit Konfidenzintervallen in ggplot2
load("poll_for_figures.Rda")
poll2013.df <- poll.df[poll.df$election==2013,]
dodge <- position_dodge(width=1)
pred.total.p <- ggplot(poll2013.df, aes(daystoelec, pred.total, group=interaction(daystoelec,institute)))+
  scale_x_reverse()+
  geom_point(aes(x = daystoelec, y = pred.total), position = dodge)+
  geom_errorbar(aes(ymin=KI.U, ymax=KI.O), position = dodge, width = 0.25)+
  geom_hline(yintercept = 49.3, linetype = 2)+
  geom_text(mapping=aes(x=25, y=49.2, label="Fundamental variable prediction"), size=3, family="Times")+
  geom_hline(yintercept = 46.3)+
  geom_text(mapping=aes(x=25, y=46.2, label="Vote outcome"), size=3, family="Times")+
  theme_classic()+
  xlab("days to the elections")+
  ylab("Predicted vote share")

ggsave(filename = "figure3.pdf", plot= pred.total.p, path = "C:/Users/User/Documents/Uni_KN/BA_Arbeit/submission_german_politics/2_submission", width=9.5, height=5)
ggsave(filename = "figure3.eps", plot= pred.total.p, path = "C:/Users/User/Documents/Uni_KN/BA_Arbeit/submission_german_politics/2_submission", family="Times")

##-----------------------
# Plots

# Poll-Vohersagen auf Days to election
pdf(file="C:/Users/User/Documents/Uni/BA_Arbeit/Arbeit/poll_pred.pdf",width=9,height=6)
plot(poll.df$daystoelec[poll.df$election==j.p], 
     poll.df$poll.pred[poll.df$election==j.p], 
     type="p", 
     pch=20, 
     cex=1, 
     bty="n", 
     las=1, 
     #xlim=c(0,30), 
     ylim=c(44, 48), 
     xlab=c("days to the elections"), 
     ylab=c("vote share in %"))
grid()
dev.off()

# Poll-Vohersagen auf Days to election für Publikation
setEPS()
postscript
pdf(file="C:/Users/User/Documents/Uni_KN/BA_Arbeit/submission_german_politics/figure1.pdf",family="serif", width=9,height=6)
par(mar=c(12,4,4,2))
plot(poll.df$daystoelec[poll.df$election==j.p], 
     poll.df$poll.pred[poll.df$election==j.p], 
     type="p", 
     pch=20, 
     cex=1, 
     bty="n", 
     las=1, 
     xlim=c(30,0), 
     ylim=c(44, 48), 
     xlab=c("days to the elections"), 
     ylab=c("vote share in %"))
abline(a=46.3, b=0)
dev.off()

## plot als postscript
setEPS()
postscript(file="C:/Users/User/Documents/Uni_KN/BA_Arbeit/submission_german_politics/figure1.eps",family="serif", width=9,height=6)
par(mar=c(12,4,4,2))
plot(poll.df$daystoelec[poll.df$election==j.p], 
     poll.df$poll.pred[poll.df$election==j.p], 
     type="p", 
     pch=20, 
     cex=1, 
     bty="n", 
     las=1, 
     xlim=c(30,0), 
     ylim=c(44, 48), 
     xlab=c("days to the elections"), 
     ylab=c("vote share in %"))
abline(a=46.3, b=0)
dev.off()

# Poll-Gewicht auf Days to election
weight.poll <- c(NA)
for (i in 1:length(poll.df$daystoelec[poll.df$election==j.p])) {
  help <- ng.error/(predict(res2.lm, poll.df[poll.df$election==j.p,][i,])+ng.error)
  weight.poll <- c(weight.poll, help)
}

plot(poll.df$daystoelec[poll.df$election==j.p], 
     weight.poll[-1], 
     type="p", 
     pch=20, 
     cex=1, 
     bty="n", 
     las=1, 
     #xlim=c(0,30),
     ylim=c(.2,1),
     xlab=c("days to the elections"),     
     ylab=c("weight of the poll model"), 
     main=c("poll-based model"))

# Versuch2
#varinaz für polls bei gegebenem institut für 0 bis 30 tage bis zur Wahl
AfD.var <- (0.14+5.72)+(0:30*0.14)
TNS.var <- (0.14+2.06)+(0:30*0.14)
forsa.var <- (0.14+1.63)+(0:30*0.14)
FGW.var <- 0.14+(0:30*0.14)
GMS.var <- (0.14+0.56)+(0:30*0.14)
dimap.var <- (0.14+1.13)+(0:30*0.14)

weight.poll.2.func <- function(var.poll, var.ng){
  weight.poll.2 <- var.ng/(var.ng+var.poll)
  return(weight.poll.2)
}

pdf(file="C:/Users/User/Documents/Uni/BA_Arbeit/Arbeit/poll_weight.pdf",width=9,height=6, family="URWTimes")
plot(c(0:30), 
     weight.poll.2.func(AfD.var, ng.error),
     xlim=c(0,30),
     ylim=c(0, 1),
     ylab=c("weight of the poll model"), #expression(hat(V))
     xlab=c("days to the elections"),
     type="l",
      
     cex=1, bty="n", 
     las=1
)
lines(c(0:30), weight.poll.2.func(TNS.var, ng.error), col="red")
lines(c(0:30), weight.poll.2.func(forsa.var, ng.error), col="blue")
lines(c(0:30), weight.poll.2.func(FGW.var, ng.error), col="green")
lines(c(0:30), weight.poll.2.func(GMS.var, ng.error), col="purple")
lines(c(0:30), weight.poll.2.func(dimap.var, ng.error), col="orange")
legend("topright", legend=c("Die Forschungsgruppe Wahlen", "GMS", 
                            "Infratest dimap", "Forsa", "TNS Emnid",
                            "IfD Allensbach"),
       col=c("green", "purple", "orange", "blue", "red", "black"), 
       lty = 1,
       cex=0.9)
dev.off()

#abbildung für publikation als eps
setEPS()
postscript(file="C:/Users/User/Documents/Uni_KN/BA_Arbeit/submission_german_politics/figure2.eps",width=9,height=6, family="URWTimes")
plot(c(0:30), 
     weight.poll.2.func(AfD.var, ng.error),
     xlim=c(30,0),
     ylim=c(0, 1),
     ylab=c("weight of the poll model"), #expression(hat(V))
     xlab=c("days to the elections"),
     type="l",
     
     cex=1, bty="n", 
     las=1
)
lines(c(0:30), weight.poll.2.func(TNS.var, ng.error), lty=2)
lines(c(0:30), weight.poll.2.func(forsa.var, ng.error), lty=3)
lines(c(0:30), weight.poll.2.func(FGW.var, ng.error), lty=4)
lines(c(0:30), weight.poll.2.func(GMS.var, ng.error), lty=5)
lines(c(0:30), weight.poll.2.func(dimap.var, ng.error), lty=6)
legend("topleft", legend=c("Die Forschungsgruppe Wahlen", "GMS", 
                            "Infratest dimap", "Forsa", "TNS Emnid",
                            "IfD Allensbach"),
       #col=c("green", "purple", "orange", "blue", "red", "black"), 
       lty = c(4,5,6,3,2,1),
       cex=0.9)
dev.off()

#abbildung für publikation als pdf
pdf(file="C:/Users/User/Documents/Uni_KN/BA_Arbeit/submission_german_politics/figure2.pdf",width=9,height=6, family="URWTimes")
plot(c(0:30), 
     weight.poll.2.func(AfD.var, ng.error),
     xlim=c(30,0),
     ylim=c(0, 1),
     ylab=c("weight of the poll model"), #expression(hat(V))
     xlab=c("days to the elections"),
     type="l",
     
     cex=1, bty="n", 
     las=1
)
lines(c(0:30), weight.poll.2.func(TNS.var, ng.error), lty=2)
lines(c(0:30), weight.poll.2.func(forsa.var, ng.error), lty=3)
lines(c(0:30), weight.poll.2.func(FGW.var, ng.error), lty=4)
lines(c(0:30), weight.poll.2.func(GMS.var, ng.error), lty=5)
lines(c(0:30), weight.poll.2.func(dimap.var, ng.error), lty=6)
legend("topleft", legend=c("Die Forschungsgruppe Wahlen", "GMS", 
                            "Infratest dimap", "Forsa", "TNS Emnid",
                            "IfD Allensbach"),
       #col=c("green", "purple", "orange", "blue", "red", "black"), 
       lty = c(4,5,6,3,2,1),
       cex=0.9)
dev.off()

# gleiche Abbildung mit umgedrehter x-achse

pdf(file="C:/Users/User/Documents/Uni/BA_Arbeit/München_Konf/Präsentation/poll_weight.pdf",width=9,height=6, family="URWTimes")
plot(c(0:30), 
     weight.poll.2.func(AfD.var, ng.error),
     xlim=c(30,0),
     ylim=c(0, 1),
     ylab=c("weight of the poll model"), #expression(hat(V))
     xlab=c("days to the elections"),
     type="l",
     
     cex=1, bty="n", 
     las=1
)
lines(c(0:30), weight.poll.2.func(TNS.var, ng.error), col="red")
lines(c(0:30), weight.poll.2.func(forsa.var, ng.error), col="blue")
lines(c(0:30), weight.poll.2.func(FGW.var, ng.error), col="green")
lines(c(0:30), weight.poll.2.func(GMS.var, ng.error), col="purple")
lines(c(0:30), weight.poll.2.func(dimap.var, ng.error), col="orange")
legend("topleft", legend=c("Die Forschungsgruppe Wahlen", "GMS", 
                            "Infratest dimap", "Forsa", "TNS Emnid",
                            "IfD Allensbach"),
       col=c("green", "purple", "orange", "blue", "red", "black"), 
       lty = 1,
       cex=0.9,
       bty="n",
       fill="white")
dev.off()

# Shrinkage estimator predicitons
pdf(file="C:/Users/User/Documents/Uni/BA_Arbeit/Arbeit/total_pred.pdf",width=9,height=6, family="URWTimes")
plot(poll.df$daystoelec[poll.df$election==j.p], 
     poll.df$pred.total[poll.df$election==j.p],
     #poll.df$KI.U[poll.df$election==j.p],
     xlim=c(0,30),
     ylim=c(45, 50),
     ylab=c("Predicted vote share"), #expression(hat(V))
     xlab=c("days to the elections"),
     type="p",
     pch=20, 
     cex=1, bty="n", 
     las=1
)
grid(lwd = c(2))
abline(a=49.3, b=0, lty=2)
abline(a=46.3, b=0)
dev.off()

# Shrinkage estimator predicitons für publikation
setEPS()
postscript(file="C:/Users/User/Documents/Uni_KN/BA_Arbeit/submission_german_politics/figure3.eps",width=9,height=6, family="URWTimes")
plot(poll.df$daystoelec[poll.df$election==j.p], 
     poll.df$pred.total[poll.df$election==j.p],
     #poll.df$KI.U[poll.df$election==j.p],
     xlim=c(30,0),
     ylim=c(45, 50),
     ylab=c("Predicted vote share"), #expression(hat(V))
     xlab=c("days to the elections"),
     type="p",
     pch=20, 
     cex=1, bty="n", 
     las=1
)
#grid(lwd = c(2))
abline(a=49.3, b=0, lty=2)
abline(a=46.3, b=0)
dev.off()

# Shrinkage estimator predicitons für publikation als pdf
pdf(file="C:/Users/User/Documents/Uni_KN/BA_Arbeit/submission_german_politics/figure3.pdf",width=9,height=6, family="URWTimes")
plot(poll.df$daystoelec[poll.df$election==j.p], 
     poll.df$pred.total[poll.df$election==j.p],
     #poll.df$KI.U[poll.df$election==j.p],
     xlim=c(30,0),
     ylim=c(45, 50),
     ylab=c("Predicted vote share"), #expression(hat(V))
     xlab=c("days to the elections"),
     type="p",
     pch=20, 
     cex=1, bty="n", 
     las=1
)
#grid(lwd = c(2))
abline(a=49.3, b=0, lty=2)
abline(a=46.3, b=0)
dev.off()

#das gleiche nochmal mit umgedrehter x-achse
pdf(file="C:/Users/User/Documents/Uni/BA_Arbeit/München_Konf/Präsentation/total_pred.pdf",width=9,height=6, family="URWTimes")
plot(poll.df$daystoelec[poll.df$election==j.p & poll.df$institute=="Forsa"], 
     poll.df$pred.total[poll.df$election==j.p & poll.df$institute=="Forsa"],
     #poll.df$KI.U[poll.df$election==j.p],
     col="blue",
     xlim=c(30,0),
     ylim=c(45,51),
     ylab=c("predicted vote share"), #expression(hat(V))
     xlab=c("days to the elections"),
     type="p",
     pch=20, 
     cex=1, bty="n", 
     las=1
)
#grid(lwd = c(2))
abline(a=49.3, b=0, lty=2)
abline(a=46.3, b=0)
points(poll.df$daystoelec[poll.df$election==j.p & poll.df$institute=="F'gruppe Wahlen"], 
       poll.df$pred.total[poll.df$election==j.p & poll.df$institute=="F'gruppe Wahlen"],
       col="green", pch=20, cex=1)
points(poll.df$daystoelec[poll.df$election==j.p & poll.df$institute=="GMS"], 
       poll.df$pred.total[poll.df$election==j.p & poll.df$institute=="GMS"],
       col="purple", pch=20, cex=1)
points(poll.df$daystoelec[poll.df$election==j.p & poll.df$institute=="Infratest Dimap"], 
       poll.df$pred.total[poll.df$election==j.p & poll.df$institute=="Infratest Dimap"],
       col="orange", pch=20, cex=1)
points(poll.df$daystoelec[poll.df$election==j.p & poll.df$institute=="TNS Emnid"], 
       poll.df$pred.total[poll.df$election==j.p & poll.df$institute=="TNS Emnid"],
       col="red", pch=20, cex=1)
points(poll.df$daystoelec[poll.df$election==j.p & poll.df$institute=="IfD Allensbach"], 
       poll.df$pred.total[poll.df$election==j.p & poll.df$institute=="IfD Allensbach"],
       col="black", pch=20, cex=1)
legend("top", legend=c("F'gruppe Wahlen", "GMS", 
                           "Infratest dimap", "Forsa", "TNS Emnid",
                           "IfD Allensbach"),
       col=c("green", "purple", "orange", "blue", "red", "black"), 
       ncol= 3,
       bty="n",
       pch=20,
       cex=0.8, 
       fill="white")
text(x=15, y=46, labels="vote share of the gov. coalition after the elections")
text(x=15, y=49.6, labels="prediction solely based on fundamental variables")
dev.off()

  # total_pred mit Konf_Intervall-Untergrenze
plot(poll.df$daystoelec[poll.df$election==j.p], 
     poll.df$pred.total[poll.df$election==j.p],
     xlim=c(0,30),
     ylim=c(45, 50),
     ylab=c("Predicted vote share"), #expression(hat(V))
     xlab=c("days to the elections"),
     type="p",
     pch=20, 
     cex=1, bty="n", 
     las=1
)
grid(lwd = c(2))
abline(a=49.3, b=0, lty=2)
abline(a=46.3, b=0)
points(poll.df$daystoelec[poll.df$election==j.p],poll.df$KI.U[poll.df$election==j.p], col="red")



# Shrinkage estimator prediction with confidence interval
errbar(x = poll.df$daystoelec[poll.df$election==j.p], 
     y = poll.df$pred.total[poll.df$election==j.p],
     yplus = poll.df$KI.O[poll.df$election==j.p],
     yminus = poll.df$KI.U[poll.df$election==j.p],
     ylab=c("Predicted vote share"), #expression(hat(V))
     xlab=c("days to the elections"),
     col = rep(c('forestgreen','blue', 'red')),
     errbar.col = rep(c('forestgreen','blue', 'red')) )
    
     
     #poll.df$KI.U[poll.df$election==j.p],
     xlim=c(0,30),
     ylim=c(45, 50),
     ylab=c("Predicted vote share"), #expression(hat(V))
     xlab=c("days to the elections"),
     type="p",
     pch=20, 
     cex=1, bty="n", 
     las=1
)
grid()
abline(a=49.3, b=0, lty=2)
abline(a=46.3, b=0)


#Plott: Entwicklung der Gewichte des Modells
poll.error.year <- sapply(poll.df$daystoelec[poll.df$election==j.p], poll.error)
weight.poll <- poll.error.year/(poll.error.year+ng.error)
weight.ng <- ng.error/(poll.error.year+ng.error)

# par(mfrow=c(1,2))
# plot(poll.df$daystoelec[poll.df$election==j.p], 
#      weight.ng, 
#      type="p", 
#      pch=20, 
#      cex=1, 
#      bty="n", 
#      las=1, 
#      xlim=c(0,30), 
#      xlab=c("Tage bis zur Wahl d"),      
#      ylab=c(""), 
#      main=c("fundamental variable-based model"))
plot(poll.df$daystoelec[poll.df$election==j.p], 
     weight.poll, 
     type="p", 
     pch=20, 
     cex=1, 
     bty="n", 
     las=1, 
     xlim=c(0,30),
     ylim=c(.5,.95),
     xlab=c("days to the elections"),     
     ylab=c(""), 
     main=c("poll-based model"))

# quadrierte residuen auf tage bis zur wahl
##einfaches model
res2.lm.2 <- lm(res2~daystoelec, data= poll.df)

pdf(file="C:/Users/User/Documents/Uni/BA_Arbeit/Arbeit/poll_resid.pdf",width=9,height=6, family="URWTimes")
plot(poll.df$daystoelec[poll.df$election<=j.p], 
     poll.df$res2[poll.df$election<=j.p], 
     type="p", 
     pch=20, 
     cex=1, 
     bty="n", 
     las=1, 
     xlim=c(0,35), 
     ylim=c(0,30), 
     xlab=c("days to the elections"), 
     ylab=c("squared residuals"))
grid(lwd = c(2))
abline(res2.lm.2)
dev.off()

# Prognose basierend auf Umfragen
plot(poll.df$daystoelec[poll.df$election==j.p], 
     poll.df$poll.pred[poll.df$election==j.p], 
     type="p", 
     pch=20, 
     cex=1, 
     bty="n", 
     las=1, 
     #xlim=c(0,35), 
     ylim=c(44, 49), 
     xlab=c("days to the elections"), 
     ylab=c("%"))
grid()

# Versuch alles in einem:

plot(c(poll.df$daystoelec[poll.df$election==j.p], poll.df$daystoelec[poll.df$election==j.p]), 
     c(poll.df$KI.U[poll.df$election==j.p], poll.df$pred.total[poll.df$election==j.p]), 
     col = rep(c('forestgreen','blue')),
     type="p", 
     pch=20, 
     cex=1, 
     bty="n", 
     las=1, 
     #xlim=c(0,30), 
     ylim=c(44, 50), 
     xlab=c("days to the elections"), 
     ylab=c("vote share in %"))
abline(a=49.3, b=0, lty=2)
abline(a=46.3, b=0)
grid()

# Konfidenzintervall-Differenz auf Tage vor elecetion
plot(poll.df$daystoelec[poll.df$election==j.p], 
     poll.df$konf.dif[poll.df$election==j.p], 
     type="p", 
     pch=20, 
     cex=1, 
     bty="n", 
     las=1, 
     #xlim=c(0,30), 
     #ylim=c(44, 48), 
     xlab=c("days to the elections"), 
     ylab=c("Konf Dif"))
grid()