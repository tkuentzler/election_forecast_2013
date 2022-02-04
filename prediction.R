########################################################################

# packages
library(foreign)
library(reshape)
library(memisc)
########################################################################

# Laden von ger.df data frame -> Daten von Simon aus dem Workshop
ger.df <- read.dta("ger_bundestag.dta", convert.underscore = TRUE )

# Abk?rzung Gr?ne ?ndern:
ger.df <-reshape::rename(ger.df, c("gru.proz" ="grue.proz"))

# Entfernen der unn?tigen Variablen
ger.df <- subset(ger.df, select= -c(cdsu.sitze, spd.sitze, fdp.sitze, gru.sitze, lin.sitze, npd.proz, lin.proz, alquote, wahlbeteiligung, kanzlerpolls))

# 2013 an Datensatz anf?gen
a <- c(2013,NA,NA,NA,NA,"cdu")
a <- as.numeric(a)
ger.df <- rbind(ger.df, a)
ger.df[18,6] <- "cdu"

# KAN, kurzfristige Variable, Kanzlerunterst?tzung

# Bis 2005: Abgelesen aus Gschwend/Norpoth 2010
# 2009: Zeit-Bericht ?ber das Modell von G/N in dem 71% als Kanzlerunterst?tzung f?r Juli/August angegeben ist.
# 2013: Wieder Zeit-Bericht ?ber Modell von G/N, 67% Kanzlerunterst?tzung f?r Juli/August
# 2013: KAN f?r 2005: 52.4 satt 43

ger.df$KAN <- c(NA, 73, 67, 58, 63, 65, 69, 57, 68, 44, 49, 63, 54, 41, 57, 52.4, 71, 62)

# AMT, kurzfristige Variable, Verschlei? der Regierung im Amt (gemessen an der Zahl der Regierungsperioden einer Regierung)

ger.df$AMT <- c(NA, 1, 2, 3, 4, 5, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 1, 2)

# nach G/N mit CDU als "starke" Partei 2005 f?r 2009 (das ist das Gro?e Koalition-Problem)
# 2013: Ebenfalls aus Zeit-Artikel ?ber der Model ?bernommen

ger.df$PAR <- c(NA, 46.9, 48.5, 45.7, 56.9, 47.7, 49.4, 50.5, 51.1, 43.8, 55.8, 54.8, 54.7, 52.2, 43.3, 46.1, 36.3, 46.4)

# STIM, Stimmenanteil der Regierungsparteien nach G/N
ger.df$STIM <- c(NA, 58.0, 53.6, 45.3, 57.1, 46.1, 54.2, 50.5, 53.5, 38.2, 53.4, 54.9, 49.8, 44.0, 47.1, 42.3, 33.8, NA)

# logAMT erstellen
ger.df$logAMT <- log(ger.df$AMT)

# KAN_L erstellen
ger.df$KAN_L <- ger.df$KAN
ger.df$KAN_L[ger.df$jahr==2005] <- 43 #(52.4-9.4, s. NG2005)

# STIM_SP
ger.df$STIM_SP <- ger.df$STIM

# PAR_GK erstellen
ger.df$PAR_GK <- ger.df$PAR
ger.df$PAR_GK[ger.df$jahr==1969] <- 83.4
ger.df$PAR_GK[ger.df$jahr==2009] <- 74.2

# STIM_GK
ger.df$STIM_GK <- ger.df$STIM
ger.df$STIM_GK[ger.df$jahr==1969] <- 88.8
ger.df$STIM_GK[ger.df$jahr==2009] <- 64.9

# #AMT_SPD erstellen
# ger.df$AMT_SPD <- ger.df$AMT
# ger.df$AMT_SPD[ger.df$jahr==2009] <- 3
# 
# #logAMT_SPD erstellen
# ger.df$logAMT_SPD <- ger.df$AMT_SPD
# 
# #PAR_SPD
# ger.df$PAR_SPD <- ger.df$PAR
# ger.df$PAR_SPD[ger.df$jahr==2009] <- 34.2
# 
# #KAN_SPD
# ger.df$KAN_SPD <- ger.df$KAN
# ger.df$KAN_SPD[ger.df$jahr==2009] <- 30
# 
# # KAN_L_SPD erstellen
# ger.df$KAN_L_SPD <- ger.df$KAN_L
# ger.df$KAN_L_SPD[ger.df$jahr==2009] <- 30 #(52.4-9.4, s. NG2005)
# 
# # STIM_SPD
# ger.df$STIM_SPD <- ger.df$STIM
# ger.df$STIM_SPD[ger.df$jahr==2009] <- 23

# STIM von 1969 und 2009 auf NA setzen
ger.df$STIM[ger.df$jahr==1969] <- NA
ger.df$STIM[ger.df$jahr==2009] <- NA

# Jahr fÃ¼r Prognose angeben
j.ng <- 2013

# Vorhersagejahr und spÃ¤tere Jahre auf NA setzen
ger.df$STIM[ger.df$jahr>=j.ng] <- NA

# falls 2009 vorhergesagt werden soll: PAR Ã¤ndern
if (j.ng==2009) ger.df$PAR[ger.df$election==2009] <- 44.1

#Modell von Norpoth/Gschwend aufstellen:
ng.lm <- lm(STIM ~ PAR + KAN + AMT, data=ger.df, na.action=na.exclude)
summary(ng.lm)
fitted(ng.lm)

# Vorhersage treffen
stim.pred <- predict(ng.lm, ger.df[ger.df$jahr==j.ng,], se.fit= TRUE)
stim.pred

# Varianz basierend auf historie
var.hist.ng <- (summary(ng.lm)$sigma)^2
var.hist.ng

#-------------------------------------------------------------

# Daten vorbereiten
dataset <- ger.df[-length(ger.df$jahr),]
new <- ger.df[length(ger.df$jahr),]

# alle Modelle aufstellen
list.of.models <- c("STIM ~ PAR + KAN + AMT",
                    "STIM ~ PAR + KAN + logAMT",
                    "STIM ~ PAR + KAN_L + AMT",
                    "STIM ~ PAR + KAN_L + logAMT",              
                    
                    "STIM_SP ~ PAR + KAN + AMT",
                    "STIM_SP ~ PAR + KAN + logAMT",
                    "STIM_SP ~ PAR + KAN_L + AMT",                    
                    "STIM_SP ~ PAR + KAN_L + logAMT"
                    
#                     ,"STIM_GK ~ PAR_GK + KAN + AMT",
#                     "STIM_GK ~ PAR_GK + KAN + logAMT",
#                     "STIM_GK ~ PAR_GK + KAN_L + AMT",
#                     "STIM_GK ~ PAR_GK + KAN_L + logAMT"
#                     
#                     ,"STIM_SPD ~ PAR_SPD + KAN_SPD + AMT_SPD",
#                     "STIM_SPD ~ PAR_SPD + KAN_SPD + logAMT_SPD",
#                     "STIM_SPD ~ PAR_SPD + KAN_L_SPD + AMT_SPD",
#                     "STIM_SPD ~ PAR_SPD + KAN_L_SPD + logAMT_SPD"
                    )
                    
# Liste in Vektor umwandeln
vector.of.models <- unlist(list.of.models)

# Lineares Modell schätzen für alle Modelle; interessierende Größen extrahieren
list.of.fits <- lapply(vector.of.models, function(x) {
  
  formula    <- as.formula(x)
  fit        <- lm(formula, data = dataset)
  adj.r2 <- summary(fit)$adj.r.squared
  result.AIC <- extractAIC(fit)
  prediction.01 <- predict(fit, new)[1]
  res.var <- summary(fit)$sigma
  
  data.frame(num.predictors = result.AIC[1],
             adjr2          = summary(fit)$adj.r.squared[1],
             model          = x,
             ypred.01        = prediction.01,
             res.var        = res.var)
})

# Ergebnisse in ein data.frame schreiben
result <- do.call(rbind, list.of.fits)

# Überblick über Vorhersagen
# Anmerkung 23.08.2021; Hieraus ist Tabelle 2 entstanden, wenn ich das richtig zuordne sind
# STIM_SP die Modelle MIT den großen Koalitionen. PAR ist im Paper B, KAN ist im Paper C und AMT ist T
result[order(result$adjr2),]
# hist(result$ypred.01)
# hist(result$ypred.02)

# Spezifikationsunsicherheit
summary(result$ypred.01)
var.spez.ng <- (sd(result$ypred.01))^2

##----------------------------------------------------------

#Datensatz laden
#poll2.df <- read.dta("wahlrecht_prep.dta", convert.underscore = TRUE)
poll.df <- read.dta("data_wahlrecht.dta", convert.underscore = TRUE)

#Variable Ns rauswerfen
poll.df <- subset(poll.df,select=-c(N,day, year, month)) 

# Nur Parteien beibehalten, die jemals an Reg beteiligt
poll.df <- poll.df[ which(poll.df$party=="CDU/CSU"
                          |poll.df$party=="FDP"
                          |poll.df$party=="GRUENE"
                          |poll.df$party=="SPD"), ]

# Umfragen ?lter als 300 Tage rauswerfen
poll.df <- subset(poll.df, daystoelec<=30 & daystoelec>=0)

#Datensatz sortieren
poll.df <- poll.df[order(poll.df$election, -poll.df$daystoelec),]

# Einzelne Umfragen zusammenfassen in mehreren Schritten (Ziel: Eine Zeile pro Umfrage)
#Schritt 1: Subset bilden und poll-Ergebnisse zusammenfassen
subset1.df <- subset(poll.df, select=-c(vote))
subset2.df <- reshape::cast(subset1.df, election+institute+date+edate+daystoelec~party, value="poll", fun.aggregate=mean)
#subset1.df[subset1.df$election==2009 & subset1.df$institute=="Forsa",] doppelte addiert?
# data.fc[data.fc$election==2002,] f'gruppe wahlen noch falscher name bei gruenen
#Schritt 2: Variablen neu benennen
subset2.df <- reshape::rename(subset2.df, c("CDU/CSU" ="cdsu.poll"))
subset2.df <- reshape::rename(subset2.df, c("SPD"="spd.poll"))
subset2.df <- reshape::rename(subset2.df, c("FDP"="fdp.poll"))
subset2.df <- reshape::rename(subset2.df, c("GRUENE"="grue.poll")) 
#Schritt 3: Wieder subset bilden und Wahlergebnisse (vote)  zusammenfassen
subset3.df <- subset(poll.df, select=-c(poll))
subset4.df <- cast(subset3.df, election+institute+date+edate+daystoelec~party, value="vote", fun.aggregate=mean)
#Schritt 4: Wieder Variablen neu benennen
subset4.df <- reshape::rename(subset4.df, c("CDU/CSU" ="cdsu.vote"))
subset4.df <- reshape::rename(subset4.df, c("SPD"="spd.vote"))
subset4.df <- reshape::rename(subset4.df, c("FDP"="fdp.vote"))
subset4.df <- reshape::rename(subset4.df, c("GRUENE"="grue.vote")) 
# Schritt 5: Subsets 2 und 4 zusammenfassen
poll.df <- merge(subset2.df,subset4.df,by=c("election","institute", "date", "edate", "daystoelec")) 

remove(subset1.df)
remove(subset2.df)
remove(subset3.df)
remove(subset4.df)

# Dezimalzahlen in Prozentangaben umrechnen(damit einheitlich mit anderem Datensatz)
poll.df$cdsu.poll <- c(poll.df$cdsu.poll*100)
poll.df$spd.poll <- c(poll.df$spd.poll*100)
poll.df$fdp.poll <- c(poll.df$fdp.poll*100)
poll.df$grue.poll <- c(poll.df$grue.poll*100)

poll.df$cdsu.vote <- c(poll.df$cdsu.vote*100)
poll.df$spd.vote <- c(poll.df$spd.vote*100)
poll.df$fdp.vote <- c(poll.df$fdp.vote*100)
poll.df$grue.vote <- c(poll.df$grue.vote*100)

#Dummy-Variablen generieren: Wer war wann an der Regierung beteiligt?
poll.df$cdsu.reg <-ifelse(poll.df$election==c(1998)|poll.df$election==c(2009)|poll.df$election==c(2013),1,0)
poll.df$spd.reg <-ifelse(poll.df$election==c(2002)|poll.df$election==c(2005)|poll.df$election==c(2009),1,0)
poll.df$fdp.reg <-ifelse(poll.df$election==c(1998)|poll.df$election==c(2013),1,0)
poll.df$grue.reg <-ifelse(poll.df$election==c(2002)|poll.df$election==c(2005),1,0)

#Stimmenanteil der Regierungsparteien in polls ausrechnen
poll.df <- within(poll.df, { reg.poll <- cdsu.poll*cdsu.reg + spd.poll*spd.reg + fdp.poll*fdp.reg + grue.poll*grue.reg })

#Stimmenanteil der Regierunsparteien in vote (nach der Wahl) ausrechnen
poll.df <- within(poll.df, { reg.vote <- cdsu.vote*cdsu.reg + spd.vote*spd.reg + fdp.vote*fdp.reg + grue.vote*grue.reg })

poll.df <- subset(poll.df,select=-c(date, edate, cdsu.poll, spd.poll,  grue.poll,  fdp.poll,	cdsu.vote,	spd.vote,	grue.vote,	fdp.vote))

poll.df$reg.poll.lag <- c(NA, poll.df$reg.poll[1:length(poll.df$reg.poll)-1])

# Vorhersagejahr j angeben
j.p <- 2013

# poll.df so sortieren, dass innerhalb der Jahre nach alter der Umfrage sortiert ist
poll.df <- poll.df[order(poll.df$election, -poll.df$daystoelec),]

# reg.vote der Vorhersagejahres auf NA setzen
poll.df$reg.vote[poll.df$election>=j.p] <- NA

# Regression: von den Umfrageergebnissen auf das Wahlergebnis
reg.vote.lm <- lm(reg.vote ~ reg.poll, data=poll.df, na.action=na.exclude)
summary(reg.vote.lm)

# Residuen von Regression abgreifen, von Liste in Vektor umwandeln, quadrieren und
# an Datensatz anhÃ¤ngen
names(reg.vote.lm)
res <- reg.vote.lm[2]
res <- unlist(res)
res2 <- (res^2)
poll.df$res2 <- c(res2, rep(NA, length(poll.df$election[poll.df$election>=j.p])))

# Dummy 1 für Allensbach
poll.df$institute.all <- ifelse(poll.df$institute %in% "IfD Allensbach", 1,0)

# Regression: Residuen auf Tage vor der Wahl
res2.lm <- lm(res2~daystoelec + institute, data= poll.df, contrasts=list(institute=contr.treatment(6,base=4)), na.action=na.exclude)
summary(res2.lm) 

# fitted quardierte Residuen an poll.df anhÃ¤ngen
poll.df$fit.res2 <- c(fitted(res2.lm))

# Funktion zur Vorhersage
poll.predict <- function(data.frame) {
  predictions <- NA
  for (i in 1:length(data.frame[,1])){
  p <- predict(reg.vote.lm, data.frame[i,], se.fit = TRUE, interval="confidence", level=.95)
  predictions <- c(predictions, p$fit[1])
  }
  return(predictions[-1])
}

#Vorhersage
poll.predictions <- poll.predict(poll.df[poll.df$election>=j.p,])

# Vorhersage an poll.df anhaengen
poll.df$poll.pred <- c(rep(NA, length(poll.df$election[poll.df$election<j.p])), poll.predictions)

##--------------------------------------------------------------------

# Gesamten Fehler von N/G-Modell berechnen
ng.error <- var.hist.ng + var.spez.ng

# Funktion Vorhersage mit Shrinkage estimator
predict.total <- function(poll.data, ng.error, stim.pred) {
  pred.total <- c(NA)
  for (i in 1:length(poll.data$election)) {
  weight.ng <- predict(res2.lm, poll.data[i,])/(predict(res2.lm, poll.data[i,])+ng.error)
  weight.poll <- ng.error/(predict(res2.lm, poll.data[i,])+ng.error)
  shr.est <- (weight.ng*stim.pred$fit[[1]]
                 + weight.poll*poll.data$poll.pred[i])
  pred.total <- c(pred.total, shr.est)  
  }
  return(pred.total[-1])  
}

pred.total <- predict.total(poll.df[poll.df$election>=j.p,],
              ng.error,
              stim.pred)
pred.total
poll.df$pred.total <- c(rep(NA, length(poll.df$election[poll.df$election<j.p])), pred.total)
#tatsächliches Ergebnis: 46,3%

# estimate posterior variance
post.var.func <- function(error1, data.frame){
  post.var <- c(NA)
  for (i in 1:length(data.frame[,1])){
    error2 <- predict(res2.lm, data.frame[i,])    
    help <- 1/(error1 + error2)
    post.var <- c(post.var, help)
  }
  return(post.var[-1])  
}

post.var <- post.var.func(ng.error, poll.df[poll.df$election>=j.p,])
poll.df$post.var <- c(rep(NA, length(poll.df$election[poll.df$election<j.p])), post.var)

# Berechne Konfidenzintervall
##Obergrenze
KI.O.func <- function(MW, var) {
 KI.O <- MW + 1.96*sqrt(var)
 return(KI.O)
}

poll.df$KI.O <- c(rep(NA, length(poll.df$election[poll.df$election<j.p])),
                  mapply(KI.O.func, 
                       poll.df$pred.total[poll.df$election>=j.p], 
                       poll.df$post.var[poll.df$election>=j.p]))

## Untergrenze
KI.U.func <- function(MW, var) {
  KI.U <- MW - 1.96*sqrt(var)
  return(KI.U)
}

poll.df$KI.U <- c(rep(NA, length(poll.df$election[poll.df$election<j.p])),
                  mapply(KI.U.func, 
                         poll.df$pred.total[poll.df$election>=j.p], 
                         poll.df$post.var[poll.df$election>=j.p]))

# write.csv(poll.df,"poll.csv")
# write.csv(ger.df,"ger.csv")

# differenz konfidenzintervalle berechnen
poll.df$konf.dif <- poll.df$KI.O - poll.df$KI.U

#RMSE berechnen
rmse <- function(x) sqrt(mean((x-46.3)^2))
a <- rmse(poll.df$poll.pred[poll.df$election==j.p])
a
b <- rmse(poll.df$pred.total[poll.df$election==j.p])
b
c <- rmse(poll.df$KI.U[poll.df$election==j.p])
c
#rmse of 23 to 30 days before the election
d <- rmse(poll.df$pred.total[poll.df$election==j.p & poll.df$daystoelec>23])
d
#rmse of lower bound 30 to 23 days before the election
e <- rmse(poll.df$KI.U[poll.df$election==j.p & poll.df$daystoelec>23])
e
#rmse 15 tage und weniger 
f <- rmse(poll.df$pred.total[poll.df$election==j.p & poll.df$daystoelec<15])
f
#rmse of lower bound 15 days before the election
g <- rmse(poll.df$KI.U[poll.df$election==j.p & poll.df$daystoelec<15])
g

save(poll.df, file="poll_for_figures.Rda")
