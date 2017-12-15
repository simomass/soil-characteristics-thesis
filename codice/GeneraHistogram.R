### R code from vignette source 'Master_Sweave.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: PacchettiRichiesti
###################################################
library(nlme)
library(plyr)
library(lattice)
library(xtable)
library(foreign)
library(agricolae)
                                        # library(gnumeric) # non piu' sviluppato per windows
library(multcomp)



###################################################
### code chunk number 2: setupDir
###################################################

DirMain <-"~/Simo_GIT/"
DirData <-
    file.path(DirMain,"dati_grezzi")
DirElab <-
    file.path(DirMain,"dati_elaborati")
DirGraf <-
    file.path(DirMain, "grafici")
DirFunz <-
    file.path(DirMain, "funzioni")
DirCod <-
    file.path(DirMain, "codice")
DirTab <-
    file.path(DirMain, "tabelle")


##################################################
##Analisi prendendo come dato il d(50)
##################################################

df.decili <- read.table(file.path(DirData, "simone_stabilita1005_corr.csv"),
               sep = "\t", dec = ".", header=TRUE
               ##,colClasses = c("factor", "numeric")
               )[, c(1:4,107, 110)]
df.decili$Sample.Name <- df.decili$Sample.Name[drop=TRUE]
df.decili$Ultrasonic.level <-
    ifelse(df.decili$Ultrasonic.level == 0, FALSE, TRUE)
attach(df.decili)
butta <-as.character(Sample.Name)
df.decili$Sample.Name <-
    factor(
        unlist(
            strsplit(butta, "[ ]")
        )[c(TRUE, FALSE)]
    )
df.decili$HUMIDITY <-
    factor(
        unlist(
            strsplit(butta, "[ ]")
        )[!c(TRUE, FALSE)]
    )
rm(butta)
detach(df.decili)
df.decili$SUBJECT <- as.numeric(row.names(df.decili))

ordinebutta <- matrix(c(1,37,73,109), ncol = 4, nrow = 36, byrow = TRUE)
for(i in 1:36){
    ordinebutta[i+1,] <- 1+ordinebutta[i,]
}
ordinebutta <- matrix(t(ordinebutta), nrow = 1)
fattore.butta <-
    with(df.decili,
         interaction(Sample.Name, Ultrasonic.level, HUMIDITY )
         )
fattore.butta <-
    factor(fattore.butta,
           levels  = levels(fattore.butta)[ordinebutta])


## for(i in (2:nrow(df.decili))){
##     if(with(df.decili, paste(Sample.Name, Ultrasonic.level, HUMIDITY))[i]!=with(df.decili, paste(Sample.Name, Ultrasonic.level, HUMIDITY))[i-1]&with(df.decili, paste(Sample.Name, Ultrasonic.level, HUMIDITY))[i]!=with(df.decili, paste(Sample.Name, Ultrasonic.level, HUMIDITY))[i+1]){
        
##     }
## }
##volevo farlo automatizzato compreso nel for, ma mi da problemi
df.decili<- df.decili[-682, ]
##praticamente in questo punto il macchinario ha bloccato gli ultrasuoni
##non so perché, quindi lo rimuovo(da problemi)

for(i in (2:nrow(df.decili))){
    if(with(df.decili, paste(Sample.Name, Ultrasonic.level, HUMIDITY))[i]==with(df.decili, paste(Sample.Name, Ultrasonic.level, HUMIDITY))[i-1]){
        df.decili$SUBJECT[i] <- df.decili$SUBJECT[i-1]+1
    }else {
        df.decili$SUBJECT[i] <- 1}
}
##############################Cambio il nome, avevo sbagliato
df.decili$TEMPO <- df.decili$SUBJECT
df.decili$SUBJECT <- as.numeric(row.names(df.decili))
k <- 1
for(i in (2:nrow(df.decili))){
    if(with(df.decili, paste(Sample.Name, Ultrasonic.level, HUMIDITY))[i]!=with(df.decili, paste(Sample.Name, Ultrasonic.level, HUMIDITY))[i-1]){
        k <- k+1
        df.decili$SUBJECT[i] <- k
    }else{
        df.decili$SUBJECT[i] <- k
    }
}

###################################################
##Metto a posto il d.5
df.decili$d.1 <- df.decili$d.1/1000
df.decili$d.5 <- df.decili$d.5/1000
df.decili$d.9 <- df.decili$d.9/1000
df.decili$D..4..3....Volume.weighted.mean <- df.decili$D..4..3....Volume.weighted.mean/1000

##################################################
######################esporta per multivariata MOLTE
numerazione <- read.table(file.path(DirData, "numerazione_campioni.csv"),
               sep = "\t", dec = ".", header=TRUE)
df.decili <- merge(numerazione, df.decili)
df.decili <- df.decili[df.decili$TEMPO=="1"&df.decili$Ultrasonic.level==FALSE,]
head(df.decili)
df.decili <- df.decili[,c(2:6, 12, 7:10)]
df.decilianno <- cbind(YEAR = rep("y16", nrow(df.decili)), df.decili)
df.decilianno2 <- df.decili
df.decilianno2[,7:10] <- NA
df.decilianno2 <- cbind(YEAR = rep("y15", nrow(df.decili)), df.decilianno2)
df.decilianno2$FIELD <- mapvalues(df.decilianno2$FIELD, from = c("2", "4", "9", "10"), to = c("01", "03", "09", "10"))
df.decilianno$FIELD <- mapvalues(df.decilianno$FIELD, from = c("2", "4", "9", "10"), to = c("02", "04", "09", "10"))

df.nocrop <- rbind(df.decilianno2, df.decilianno)
names(df.nocrop)[11] <- "DIAM.MEAN.WEIGH"

df.nocrop <- cbind(df.nocrop, x = with(df.nocrop, paste(YEAR, MAN, FIELD, sep = "")))
levels(df.nocrop$x)
df.finale <- cbind(df.nocrop, CROP = mapvalues(df.nocrop$x,
          from = c("y15Co09", "y15Co10", "y15Or01", "y15Or03", "y16Co09", "y16Co10", "y16Or02", "y16Or04"),
          to = c("Sun", "Bar", "Bar", "Sun", "Bar", "Sun", "Sun", "Bar")
          ))[,-12]
df.finale <- df.finale[, c(1:7, 12, 8:11)]

names(df.finale)[c(9, 10, 11, 12)] <- c("d.1.micron", "d.5.micron", "d.9.micron", "DIAM.MEAN.WEIGH.micron")

write.table( df.finale, "/home/simone/Dropbox/MOLTE/SoloSuolo/dati_grezzi/Stabil.Terr.csv", sep = ";")




pdf(file.path(DirGraf,"DECILI.pdf"))
xyplot(d.5 ~ TEMPO|fattore.butta,
       data= df.decili,
       pch=".",
       #scales= list(y = "free"),
       ##equispaced.log=FALSE,
       ##col=1+df.long$Ultrasonic.level,
       layout=c(4,4), #scales = list(y = "free"),#ylim=c(-10,110),
       ##prepanel = function(x, y) prepanel.default.xyplot(log(x), y, ylim=c(0,100)),
       ## panel = function(x, y, ...){
       ##     ##     y[which(is.na(y))] <- 0
       ##     ##     yps <- cumsum(y)
       ##     ##     yps <- 100*(yps/max(yps))
       ##     ##     ## lm.1 <- lm(yps ~ x)
       ##     panel.xyplot(y, x)
       ##     panel.abline(v = 0, col = 2)
       ##     ##     testo <- as.character(length(yps))
       ##     ##     panel.text(0,80, testo)
       ## }
       )
dev.off()
graphics.off()

##NON MI RIESCE USARE LATTICE!!!!!!!!!!!!!!!!!!!
x11()
plot(1:12, (df.decili$d.5[with(df.decili, paste(Sample.Name, Ultrasonic.level, HUMIDITY))== "10 FALSE dry"]/1000))




##Provo a fare il modello
###################################################
df.decili$PIPPO <- with(df.decili, interaction(APPEZZAMENTO, PARCELLA, CODICE, SUBJECT))

####################################################
##Questo valore ha 9 misure e l'intercetta mi da un NA
##provo a toglierlo
df.decili <- df.decili[-which(df.decili$SUBJECT==120), ]
####################################################

df.grouped <-
    groupedData(d.5 ~ TEMPO|PIPPO,
                data = df.decili)
df.grouped$PIPPO <- factor(df.grouped$PIPPO, levels = levels(df.grouped$PIPPO)[order(levels(df.grouped$PIPPO))])


##LM con d(50)
lis.linear.d5.1 <-
    lmList(d.5 ~ TEMPO, data=df.grouped)
plot(intervals(lis.linear))


lis.linear.d5.2 <-
    lmList(d.5 ~ TEMPO + I(TEMPO^2), data=df.grouped)

plot(intervals(lis.linear.d5.2))


lme.1 <- lme(d.5 ~ TEMPO , data = df.grouped, method = "ML")
plot(augPred(lme.1))
plot(lme.1, resid(., type="p") ~ fitted(.)|PIPPO)

lme.2 <- lme(d.5 ~ TEMPO + I(TEMPO^2), data =  df.grouped,
             method = "ML")
plot(lme.2, resid(., type="p") ~ fitted(.)|PIPPO)

anova(lme.1, lme.2)




##d.1 e d.9
lis.linear.d1.1 <-
    lmList(d.1 ~ TEMPO, data=df.grouped)
plot(intervals(lis.linear))
lis.linear.d9.1 <-
    lmList(d.9 ~ TEMPO, data=df.grouped)
plot(intervals(lis.linear))





#########################################################
#########################################################
############Parte vecchia
#########################################################
#########################################################
df.stab <-
    read.table(file.path(DirData, "simone_stabilita1005_corr.csv"),
               sep = "\t", dec = ".", header=TRUE
               ##,colClasses = c("factor", "numeric")
               )[, c(6:107,110)]
df.stab$Sample.Name <- df.stab$Sample.Name[drop=TRUE]
df.stab$Ultrasonic.level <-
    ifelse(df.stab$Ultrasonic.level == 0, FALSE, TRUE)


attach(df.stab)
butta <-as.character(Sample.Name)
df.stab$Sample.Name <-
    factor(
        unlist(
            strsplit(butta, "[ ]")
        )[c(TRUE, FALSE)]
    )
df.stab$HUMIDITY <-
    factor(
        unlist(
            strsplit(butta, "[ ]")
        )[!c(TRUE, FALSE)]
    )
rm(butta)
detach(df.stab)

df.stab$SUBJECT <- row.names(df.stab)
diametri <-
    as.numeric(
        substr(names(df.stab)[1:101], 2,
               nchar(names(df.stab)[1:101]))
    )

df.long <-
    reshape(df.stab,
            varying = names(df.stab)[1:101],
            timevar = "DIAM",
            idvar = "SUBJECT",
            direction="long",
            v.names = "PERCENTUALE",
            sep="X")

erre <- with(data = df.long, order(Sample.Name, Ultrasonic.level,HUMIDITY, SUBJECT))
require(plyr)
df.long$DIAM <-
    mapvalues(df.long$DIAM, unique(df.long$DIAM), diametri)
                                        #funzione che sostituisce i valori progressivi con i 

df.long$Sample.Name <-
    factor(df.long$Sample.Name, levels = as.character(1:36))

ordinebutta <- matrix(c(1,37,73,109), ncol = 4, nrow = 36, byrow = TRUE)
for(i in 1:36){
    ordinebutta[i+1,] <- 1+ordinebutta[i,]
}
ordinebutta <- matrix(t(ordinebutta), nrow = 1)
fattore.butta <-
    with(df.long,
         interaction(Sample.Name, HUMIDITY )
         )
fattore.butta <-
    factor(fattore.butta,
           levels  = levels(fattore.butta)[ordinebutta])


pdf(file.path(DirGraf,"PISA.pdf"))
xyplot(PERCENTUALE ~ log10(DIAM)|fattore.butta,
       data= df.long,
       pch=".",
       ##scales=list(x = list(log = 10)),
       ##equispaced.log=FALSE,
       col=1+df.long$Ultrasonic.level,
       layout=c(1,1), #scales = list(y = "free"),#ylim=c(-10,110),
       ##prepanel = function(x, y) prepanel.default.xyplot(log(x), y, ylim=c(0,100)),
       ## panel = function(x, y, ...){
       ##     ##     y[which(is.na(y))] <- 0
       ##     ##     yps <- cumsum(y)
       ##     ##     yps <- 100*(yps/max(yps))
       ##     ##     ## lm.1 <- lm(yps ~ x)
       ##     panel.xyplot(y, x)
       ##     panel.abline(v = 0, col = 2)
       ##     ##     testo <- as.character(length(yps))
       ##     ##     panel.text(0,80, testo)
       ## }
       )
dev.off()
graphics.off()

questi <- df.long$Sample.Name=="1" & df.long$HUMIDITY=="wet"
questaltri <- interaction(df.long[questi,]$Sample.Name, df.long[questi,]$Ultrasonic.level)#, df.long[questi,]$DIAM)

require(ggplot2)

jpeg(file.path("~/Simo_GIT/grafici","SplitStab.jpeg"))
vet.clas <- as.numeric(substr(
         colnames(df.stab)
         [1:101], 2, nchar(colnames(df.stab)[1:101])
))
boh <- as.vector(df.stab[1,1:101], mode = "numeric")
boh[1] <- 0
freq <- c()
for(i in 1:length(boh)){
    freq <-
        c(freq, rep(vet.clas[i], (boh[i]*10000)))
}
 ## h <- hist(log10(freq),  plot = FALSE)
 ## h$counts <- h$counts / sum(h$counts)
## plot(h, main = "Particle size distribution",
##      xlab = expression(paste("Particle diameter (Log of ", mu, "m)")), ylab = "Volume (%)",
##      axes = TRUE, labels = FALSE, nclass = NULL)
## qplot(log10(freq),
##       geom="histogram")#, 
## binwidth = 0.5)

 boh <-    data.frame(
     mah  =  1:length(freq),
     freq = freq,
     colora = ifelse(freq<=20, "green", ifelse(freq>=252, "red", "blue"))
     )

p1 <- ggplot(data = boh, aes(x = freq, fill = colora)) +
    geom_histogram(aes(y=(..count..)/sum(..count..)),
                   col="black",
                   alpha = .7) +
    labs(title="Particle size distribution") +
    labs(x=expression(paste("Particle diameter (", mu, "m)")), y= "Volume (%)")

bs <-  unique(freq)[c(T,F,F,F)]
bs <- round(bs, 1)
bs[4:length(bs)] <- round(bs[4:length(bs)], 0)

p1 + scale_x_log10(breaks=c(20,250), labels=round(c(20,250), 1))+theme(legend.position="none")+
       theme(axis.text.x=element_text(size=20))#,
dev.off()


pdf(file.path(DirGraf,"PISACumulata.pdf"))
xyplot(PERCENTUALE ~ log10(DIAM)|fattore.butta,#HUMIDITY+Sample.Name,
       data= df.long,
       pch=".",
       ##scales=list(x = list(log = 10)),
       ##equispaced.log=FALSE,
       col=as.numeric(df.long$HUMIDITY),
       layout=c(4,4), ylim=c(-10,110),
                                        #prepanel = function(x, y) prepanel.default.xyplot(log(x), y, ylim=c(0,100)),
       panel = function(x, y, ...){
           y[which(is.na(y))] <- 0
           yps <- cumsum(y)
           yps <- 100*(yps/max(yps))
           ## lm.1 <- lm(yps ~ x)
           panel.xyplot(y=yps, x)
           panel.spline(y=yps, x=x, col = 2)
           testo <- as.character(length(yps))
           panel.text(0,80, testo)
       }
       )
dev.off()


fun.SbrigliaPicchi <-
    function(data = df.stab, colonne = 1:101){
        df <- data[colonne]
        indice.colonna.max <- ## indice della colonna con DIAM pari al picco
            apply(df, 1, function(x){
                indiceMassimo <- which(x==max(x,na.rm=TRUE))}
                )
        diametri <-
            as.numeric(
                substr(names(df), 2,
                       nchar(names(df))
                       )
            )[indice.colonna.max]
        perc.max <- ## percentuale in cui trovo il picco, ovvero il
            ## valore massimo della distribuzione granulometrica
            apply(df, 1, function(x) max(x,na.rm=TRUE))
        df.risultati <-
            cbind.data.frame(
                data[,102:105],
                DIAM.PERC.MAX = diametri,
                PERC.MAX = perc.max)
        df.risultati
    }


df.picchi <- fun.SbrigliaPicchi()

pdf(file.path(DirGraf,"PisaPicchi.pdf"))
butta <- with(df.picchi,
              interaction(Sample.Name, Ultrasonic.level, HUMIDITY)
              )
butta <- factor(butta, levels = unique(butta))
##
tabella <- table(butta)
secchio <- list()
for(i in 1:length(tabella)){
    secchio[[i]] <- 1:tabella[i]
}
df.picchi$TEMPO <- unlist(secchio)
df.picchi$SUBJECT <- butta
##

plot(PERC.MAX ~ TEMPO, , data = df.picchi[1:12,])



xyplot(DIAM.PERC.MAX ~ TEMPO|Sample.Name + Ultrasonic.level + HUMIDITY,
       data= df.picchi,
       layout = c(5,3),
       scales = list("free"),
       ## xlim = c(0,30),
       ## ylim= c(0,350),
       ## pch= ".",
       ##panel = function(x, y, ...){
           ##     ics <- 1:length(y)
           ##panel.xyplot(y, x)
           ##panel.text(10, 900, as.character(length(y)))
           ##     parabola <- lm(y ~ ics + ics^2)
       ##}
       )
##


df.picchi <- groupedData(DIAM.PERC.MAX ~ TEMPO|SUBJECT,
                         data = df.picchi)
plot(df.picchi)
write.table(df.picchi, file= "picchi.csv")
##
lis.parab <-
    lmList(DIAM.PERC.MAX ~ TEMPO + I(TEMPO^2), data=df.picchi)
plot(intervals(lis.parab))
ctrl <- lmeControl(opt='optim');
lme.1 <- lme(lis.parab, control = ctrl)
plot(augPred(lme.1))
##
plot(lme.1, resid(., type="p") ~ fitted(.)|SUBJECT)
dev.off()
