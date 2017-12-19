### R code from vignette source 'Tesi_GIT.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: PacchettiRichiesti
###################################################
library(nlme)
library(plyr)
library(lattice)
library(xtable)
library(foreign)
library(multcomp)


###################################################
### code chunk number 2: setupDir
###################################################
info.sistema <-
Sys.info()[c(1,7)]
if(info.sistema[1]=="Windows"){
  DirMain <- paste("C:/Users/", info.sistema[2], "/Dropbox/MOLTE_R", sep="")
} else {
  if(info.sistema[2]=="simone" ){
    DirMain <-"~/Simo_GIT"
  }else{
    DirMain <-"~/Documenti/BitBucket/Simo_GIT"
  }
}
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


###################################################
### code chunk number 18: Import_aggregati
###################################################
vec.paletti <- c(250, 20)
df.data <-
read.table(file.path(DirElab, "Stabilita_wide.csv"),
sep = ";", dec = ".", header=TRUE
)

df.data$TIME <- df.data$TIME-1 ## corregge i tempi che partono con 1
df.data$TIME2 <- df.data$TIME
UltraVeri <- which(df.data$Ultrasonic.level)
df.data$TIME2[UltraVeri] <- df.data$TIME2[UltraVeri]+12
##df.data[1:30, c("TIME", "TIME2", "Ultrasonic.level","Sample.Name")]
anomali32 <-
with(df.data,
which(Sample.Name == "32" & Ultrasonic.level & HUMIDITY == "dry"))
##df.data$TIME2[anomali32] <- 12:20

df.boyocous <-
read.table(file.path(DirData, "outputUSDA3.csv"),
sep = ";", dec = ",", header=TRUE
)

diametri <-
as.numeric(
substr(names(df.data)[19:119], 2,
nchar(names(df.data)[19:119]))
)


lis.paletti <-
list(uno=which(diametri > vec.paletti[1])+18,
due=which(diametri < vec.paletti[1] & diametri > vec.paletti[2] )+18,
tre=which(diametri < vec.paletti[2])+18
)

df.tessitura <-
data.frame(df.data[,c(1:13,18,120)],
MACRO = apply(df.data[,lis.paletti$uno], 1,
function(x) sum(x, na.rm=TRUE)),
MESO = apply(df.data[,lis.paletti$due], 1,
             function(x) sum(x, na.rm=TRUE)),
MICRO = apply(df.data[,lis.paletti$tre], 1,
              function(x) sum(x, na.rm=TRUE))
)
require(compositions)

###############################
## due umidità con ultrasuoni
###############################

getModelMatrix <- function(object,
                           newdata=NULL,
                           na.action=na.pass) {
    if( is.null(newdata) )
        return(model.matrix(object))
    Terms <- delete.response(terms(object))
    mf <- model.frame(Terms,newdata,na.action=na.action,
                      xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
        .checkMFClasses(cl, mf)
    model.matrix(Terms, mf, contrasts.arg = object$contrasts)
}
lis.paletti <-
    list(uno=which(diametri > vec.paletti[1])+18,
         due=which(diametri < vec.paletti[1] & diametri > vec.paletti[2] )+18,
         tre=which(diametri < vec.paletti[2])+18
         )
df.tessitura <-
    data.frame(df.data[,c(1:13,18,120)],
               MACRO = apply(df.data[,lis.paletti$uno], 1,
                             function(x) sum(x, na.rm=TRUE)),
               MESO = apply(df.data[,lis.paletti$due], 1,
                            function(x) sum(x, na.rm=TRUE)),
               MICRO = apply(df.data[,lis.paletti$tre], 1,
                             function(x) sum(x, na.rm=TRUE))
               )
questi <-
    with(df.tessitura, which(HUMIDITY=="wet"))
df.plot <- df.tessitura[questi,]
Y.Msizer1 <-
    acomp(df.plot[, 16:18])
lm.2 <-
    with(df.plot, lm(ilr(Y.Msizer1) ~ COND+TIME2+I(TIME2^2)))



###################################################
### code chunk number 19: plotacompWETDRY
###################################################
tessitura8rino <-
    read.table(file.path(DirData, "tessitureXstabilità.csv"), sep = ";", dec = ",", header = TRUE)[,-1]
tessitura8rino2 <-
    cbind(tessitura8rino[,1:3][c(TRUE,FALSE,FALSE),],
          matrix(tessitura8rino[,4], ncol = 3, byrow = TRUE))[,c(1:3, 6,5,4)]
colnames(tessitura8rino2)[4:6] <- c("MACRO", "MESO", "MICRO")


modello <- lm.2
coefs <-  ilrInv(coef(modello), orig = Y.Msizer1)
alpha <- 0.05

## si riparte coi campioni dry
questi <-
    with(df.tessitura, which(HUMIDITY=="dry"))
df.plotDRY <- df.tessitura[questi,]
Y.Msizer <- acomp(df.plotDRY[, 16:18])
lm.2.2 <-   with(df.plotDRY,lm(ilr(Y.Msizer) ~ COND+TIME2+I(TIME2^2)))
modelloMsizer <- lm.2.2
coefsDRY <-  ilrInv(coef(modelloMsizer), orig=Y.Msizer)

fun.triangolo.stabilita <-
    function(dimen=0.5, legenda=TRUE,
             df=Y.Msizer1,
             tinta=df.plot$COND,tipo=1 ){
        plot(df, pch=tipo,
             cex=dimen,
             col=tinta,
             axes = TRUE, plotMissings = FALSE,
             add = FALSE)
        if(legenda){
        legend("topleft",
               c("CONV Umidi", "BIO Umidi"),
               fill=3:4, bty = "n", cex=1.1)
        legend("topright",
               c( "CONV Secchi", "BIO Secchi"),
               fill=1:2, bty = "n", cex=1.1)
        }
        text(0.1,0.025, expression(paste(">250 ", mu, "m")))
        text(0.85,0.025, expression(paste("tra 20 e 250 ", mu, "m")))
        text(0.5,0.72, expression(paste("<20 ", mu, "m")))
}
pdf(file.path(DirGraf,"stabilita.pdf"))
par(mfrow = c(1,1))
## solo secchi e no ultrasuoni
for(i in 1:24){
    questo <- rep(1,24)[1:i]
    trasparenti <- rep("transparent", 840-i)
    questo <- c(questo, trasparenti)
fun.triangolo.stabilita(dimen=2,
                        tipo =20,
                        tinta= questo,
                        legenda= FALSE)
}
coloriWET_NOsonic <-
    ifelse(df.plot$Ultrasonic.level,
           "transparent", as.numeric(df.plot$COND)+2)
coloriWET_all <- as.numeric(df.plot$COND)+2
coloriDRY_NOsonic<-
    ifelse(df.plotDRY$Ultrasonic.level,
           "transparent", df.plotDRY$COND)
## WET no ultrasuoni
fun.triangolo.stabilita(dimen=0.5,
                        df=Y.Msizer1,
                        tinta=coloriWET_NOsonic)
## WET ultrasuoni
fun.triangolo.stabilita(df=Y.Msizer1,
                        tinta= coloriWET_all)
# punto accensione ultrasuoni
on.ultra.off <- c(0, 11, 23)
## equazioni WET
fun.triangolo.stabilita(df=Y.Msizer1,
                        tinta=coloriWET_all)
lineaCOwet <-
    acomp(coefs[1,]) +
    acomp(coefs[3,])*0:23 +
    acomp(coefs[4,])*(0:23)^2
startStopCOwet <-
    acomp(coefs[1,]) +
    acomp(coefs[3,])*on.ultra.off +
    acomp(coefs[4,])*on.ultra.off^2
rigaModificata <- startStopCOwet[1, ]+c(0,0,0.05)
startStopCOwet <- acomp(rbind(rigaModificata, startStopCOwet[-1,]))
lineaOOwet <-
    acomp(coefs[1,]) + acomp(coefs[2,])+
     acomp(coefs[3,])*0:23+
    acomp(coefs[4,])*(0:23)^2
startStopOOwet <-
    acomp(coefs[1,]) + acomp(coefs[2,]) +
    acomp(coefs[3,])*on.ultra.off +
    acomp(coefs[4,])*on.ultra.off^2
rigaModificata <- startStopOOwet[1, ]+c(0,0,-0.04)
startStopOOwet <-
    acomp(rbind(rigaModificata, startStopOOwet[-1,]))
lineaCOdry <-
    acomp(coefsDRY[1,]) +
     acomp(coefsDRY[3,])*0:23 +
    acomp(coefsDRY[4,])*(0:23)^2
startStopCOdry <-
    acomp(coefsDRY[1,]) +
    acomp(coefsDRY[3,])*on.ultra.off +
    acomp(coefsDRY[4,])*on.ultra.off^2
startstopOOdry <-
    acomp(coefsDRY[1,]) +
    acomp(coefsDRY[3,])*on.ultra.off +
    acomp(coefsDRY[4,])*on.ultra.off^2
rigaModificata <- startStopCOdry[1, ]+c(0,0,+0.04)
startStopCOdry <-
    acomp(rbind(rigaModificata, startStopOOwet[-1,]))
lineaOOdry <-
    acomp(coefsDRY[1,]) + acomp(coefsDRY[2,]) +
    acomp(coefsDRY[3,])*0:23+
    acomp(coefsDRY[4,])*(0:23)^2
startStopOOdry <-
    acomp(coefsDRY[1,]) + acomp(coefsDRY[2,]) +
    acomp(coefsDRY[3,])*on.ultra.off +
    acomp(coefsDRY[4,])*on.ultra.off^2
rigaModificata <- startStopOOdry[1, ]
plot(lineaCOwet,
     col=3,  lwd=3, type="l", add=TRUE)## sequenza temporale CO wet
## bollini bianchi per lettere
plot(startStopCOwet,
     col="white",
     add=TRUE, pch=16, cex = 3)## start stop CO
## lettere
plot(startStopCOwet,
     col=3, add=TRUE, pch=c("c", "U","C")[3:1],
     cex=1.5)## start stop CO
##
fun.triangolo.stabilita(df=Y.Msizer1,
                        tinta=coloriWET_all)
plot(lineaCOwet,
     col=3,  lwd=3,type="l", add=TRUE)## sequenza temporale CO wet
## bollini bianchi per lettere
plot(startStopCOwet,
     col="white",
     add=TRUE, pch=16, cex = 3)## start stop CO
plot(startStopCOwet,
     col=3, add=TRUE,
     pch=c("c", "U","C")[3:1], cex=1.5)## start stop CO
plot(lineaOOwet,
     col=4, lwd=3, add=TRUE, type="l")## sequenza temporale OO wet
## punti OO wet
## bollini bianchi per lettere
plot(startStopOOwet,
     col="white", add=TRUE,
     pch= 16, cex = 3)## start stop OO
plot(startStopOOwet,
     col=4, add=TRUE,
     pch= c("b","U","B")[3:1],
     cex=1.5)## start stop OO
##############################################################
## Campioni DRY
fun.triangolo.stabilita(df=Y.Msizer,
                        tinta=coloriDRY_NOsonic)
fun.triangolo.stabilita(df=Y.Msizer,
                        tinta=df.plotDRY$COND)
##
fun.triangolo.stabilita(df=Y.Msizer,
                        tinta=df.plotDRY$COND)
plot(lineaCOdry,
col=1, lwd= 3, type="l", add=TRUE)## sequenza temporale CO dry
plot(startStopCOdry,
     col="white", add=TRUE, pch=16, cex = 3)## start stop CO dry
plot(startStopCOdry,
     col=1, add=TRUE,
     pch=c("c", "S","C")[3:1],
     cex=1.5)## start stop COdry
##
fun.triangolo.stabilita(df=Y.Msizer,
                        tinta=df.plotDRY$COND)
plot(lineaCOdry,
     col=1, lwd= 3,
     type="l", add=TRUE)## sequenza temporale CO dry
plot(startStopCOdry,
     col="white", add=TRUE,
     pch=16, cex = 3)## start stop CO dry
plot(startstopOOdry,
     col=1, add=TRUE,
     pch=c("c", "S","C")[3:1], cex=1.5)## start stop COdry
plot(lineaOOdry,
    col=2,  lwd= 3, add=TRUE, type="l")## sequenza temporale OO dry
plot(startStopOOdry,
     col="white", add=TRUE, pch= 16, cex = 3)## start stop OO dry
plot(startStopOOdry,
     col=2, add=TRUE,
     pch= c("b","S","B")[3:1], cex=1.5)## start stop OO dry
### DIAPO FINALE
fun.triangolo.stabilita(df=Y.Msizer1, tinta="transparent")
## relazione conv DRY
plot(lineaCOdry,
     col=1, lwd= 3, type="l", add=TRUE)## sequenza temporale CO dry
## plot(acomp(coefsDRY[1,]) +
##      acomp(coefsDRY[3,])*on.ultra.off +
##      acomp(coefsDRY[4,])*on.ultra.off^2,
##      col="white", add=TRUE, pch=16, cex = 2)## start stop CO dry
plot(startStopCOdry,
     col=1, add=TRUE,
     pch=c("c", "S","C")[3:1], cex=1.5)## start stop COdry
## relazione bio DRY
plot(lineaOOdry,
     col=2,  lwd= 3, add=TRUE, type="l")## sequenza temporale OO dry
## plot(acomp(coefsDRY[1,]) + acomp(coefsDRY[2,]) +
##      acomp(coefsDRY[3,])*on.ultra.off +
##      acomp(coefsDRY[4,])*on.ultra.off^2,
##      col="white", add=TRUE, pch= 16, cex = 2)## start stop OO dry
plot(startStopOOdry,
     col=2, add=TRUE, pch= c("b","S","B")[3:1], cex=1.5)## start stop OO dry
## relazione conv WET
plot(lineaCOwet,
     col=3,  lwd=3,type="l", add=TRUE)## sequenza temporale CO wet
## bollini bianchi per lettere
## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col="white",
##      add=TRUE, pch=16, cex = 3)## start stop CO
plot(startStopCOwet,
     col=3, add=TRUE,
     pch=c("c", "U","C")[3:1], cex=1.5)## start stop CO
## relazione bio WET
plot(lineaOOwet,
     col=4, lwd=3, add=TRUE, type="l")## sequenza temporale OO wet
## ## bollini bianchi per lettere
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col="white", add=TRUE, pch= 16, cex = 3)## start stop OO
##
plot(startStopOOwet,
     col=4, add=TRUE,
     pch= c("b","U","B")[3:1], cex=1.5)## start stop
plot(acomp(c(0.42582946, 0.4786861, 0.09548441)),
##    c( 0.47624943, 0.4423284, 0.08142218)),
     col=4, add=TRUE,
     pch= 1, cex=10)# start stop
plot(acomp(startStopOOdry[1,]),
##    c( 0.47624943, 0.4423284, 0.08142218)),
     col=1, add=TRUE,
     pch= 1, cex=10)# start stop
## OO
## inserisce le tessiture apparenti ricalcolate
## plot(acomp(tessitura8rino2[,4:6]), cex=1.5,
##      pch = 18,
##      col=as.numeric(rev(tessitura8rino2$CONDUZIONE)),
##      add = TRUE)
dev.off()

iiii
## ###################################################
## ### code chunk number 20: IUFW
## ###################################################
## nomi <-
##     colnames(df.iufw)
## df.iufw <-
##     data.frame(
##         df.iufw[,1:2],
##         apply(df.iufw[,3:5], 2, as.numeric)
##     )
## names(df.iufw) <- nomi
## rm(nomi)

## xt.IUFW <- xtable(df.iufw,

##                   label = 'tab:iufw',
##                   caption = 'Distribuzione dei frammenti di particelle degli aggregati inumiditi in seguito a:
## i) inizio misura (immersione):
## ii) inizio sonicatura
## iii) fine ciclo ',  digits = 1
## )
## print(xt.IUFW,include.rownames=FALSE,
##       caption.placement = "top")


## ###################################################
## ### code chunk number 21: IUFD
## ###################################################

## nomi <-
##     colnames(df.iufd)
## df.iufd <-
##     data.frame(
##         df.iufd[,1:2],
##         apply(df.iufd[,3:5], 2, as.numeric)
##     )
## names(df.iufd) <- nomi
## rm(nomi)
## df.iufd[c(1,5),2] <- "inizio misura"
## xt.IUFD <- xtable(df.iufd,
##                   label = 'tab:iufd',
##                   caption = 'Distribuzione dei frammenti di particelle degli aggregati essiccati in seguito a:
## i) inizio misura (immersione):
## ii) inizio sonicatura
## iii) fine ciclo ', digits = 1
## )
## print(xt.IUFD, include.rownames=FALSE,caption.placement = "top")


## ###################################################
## ### code chunk number 22: anova_acompWET
## ###################################################
## anova.comp.wet <- anova(lm.2)
## anova.comp.wet[1:4,6] <- "<10^-3"

## rownames(anova.comp.wet) <- c("Intercetta","Conduzione", "Tempo", "Tempo^2","Totale")
## anova_tabella <- xtable(anova.comp.wet,
##                         label = 'tab:anova_compWET', align = 'lrrrrrr',
##                         caption = 'Analisi della varianza relativa ai dati composizionali per la stabilit\\`a degli aggregati inumiditi'
##                         )
## print(anova_tabella, caption.placement = "top")#, math.style.exponents = TRUE)


## ###################################################
## ### code chunk number 23: anova_acompDRY
## ###################################################
## anova.comp.dry <- anova(lm.2.2)
## rownames(anova.comp.dry) <- c("Intercetta","Conduzione","Tempo", "Tempo^2","Totale")
## anova.comp.dry[1:4,6] <- "<10^-3"
## anova_tabella_dry<- xtable(anova.comp.dry,
##                            label = 'tab:anova_compDRY', align = 'lrrrrrr',
##                            caption = 'Analisi della varianza relativa ai dati composizionali per la stabilit\\`a degli aggregati essiccati'
##                            )
## print(anova_tabella_dry, caption.placement = "top")#, math.style.exponents = TRUE)


## ###################################################
## ### code chunk number 24: figboh3
## ###################################################
## tessitura8rino <-
##     read.table(file.path(DirData, "tessitureXstabilità.csv"), sep = ";", dec = ",", header = TRUE)[,-1]
## tessitura8rino2 <-
##     cbind(tessitura8rino[,1:3][c(TRUE,FALSE,FALSE),],
##           matrix(tessitura8rino[,4], ncol = 3, byrow = TRUE))[,c(1:3, 6,5,4)]
## colnames(tessitura8rino2)[4:6] <- c("MACRO", "MESO", "MICRO")


## modello <- lm.2
## coefsDRY <-  ilrInv(coef(modello), orig = Y.Msizer1)
## alpha <- 0.05

## plot(Y.Msizer1, cex=0.15, col=df.plot$COND, axes = TRUE, plotMissings = FALSE)
## plot(acomp(tessitura8rino2[,4:6]), cex=1.5,
##      pch = 18,
##      col=as.numeric(rev(tessitura8rino2$CONDUZIONE)),
##      add = TRUE)

## ## macro meso micro
## ## plot(acomp(c(2,2,2)), pch = 20, cex = 2, add = TRUE, col = 1)
## ## plot(acomp(c(80,5,15)), pch = 20, cex = 2, add = TRUE, col = 2)
## ## plot(acomp(c(75, 5,20)), pch = 20, cex = 2, add = TRUE, col = 3)

## ## plot(acomp(c(5, 75,20)), pch = 20, cex = 2, add = TRUE, col = 3)

## ## plot(acomp(c(5, 55 ,40)), pch = 20, cex = 2, add = TRUE, col = 3)
## on.ultra.off <- c(0, 11, 23)
## CO <- round(matrix(coefs[1,] +
##                    acomp(coefs[3,])*on.ultra.off +
##                    acomp(coefs[4,])*on.ultra.off^2,
##                    ncol = 3, byrow = FALSE), 3)
## OO <- round(matrix(acomp(coefs[1,]) +
##                    acomp(coefs[2,]) +
##                    acomp(coefs[3,])*on.ultra.off +
##                    acomp(coefs[4,])*on.ultra.off^2,
##                    ncol = 3, byrow = FALSE), 3)
## dati.iufd <- rbind(CO*100, OO*100)
##                                         #i = inizio, u = accension ultrasuoni, f = fine d = dry/w = wet
## Conduzione <-
##     c("Convenzionale"," ", " ",
##       "Biologico "," "," ")
## Fase <- rep(c("inizio misura","inizio sonicatura", "fine misura"),2)
## colnames(dati.iufd) <- c("Macro (%)", "Meso (%)", "Micro (%)")
## df.iufd <- cbind(Conduzione, Fase, dati.iufd)
## df.iufd <- rbind(df.iufd[1:3,], "", df.iufd[4:6,])



## ## banda.predizione <- FALSE
## ## for(i in 1:2){
## ##     for(k in 0:23){
## ##         newdata <-
## ##             list(COND = levels(df.plot$COND)[i],
## ##                  TIME2 = k)
## ##         X <- getModelMatrix(modello,newdata)
## ##         prediction <- ilrInv(predict(modello,newdata=newdata),orig=Y)
## ##         varEpsilon = var(modello)
## ##         XX <- kronecker(diag(ncol(predict(modello))),X)
## ##         estVar <- XX %*% vcov(modello) %*% t(XX)
## ##         predVar <- estVar+varEpsilon
## ##         plot(prediction,  add=FALSE, col= i, type= "b")
## ##         ## ellipses(prediction,ilrvar2clr(estVar),
## ##         ##          r=ConfRadius(modello,1-alpha), col=i)
## ##         if(banda.predizione)
## ##             ellipses(prediction,ilrvar2clr(predVar),
## ##                      r=ConfRadius(modello,1-alpha), col=i)
## ##     }
## ## }
## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*0:23 +
##      acomp(coefs[4,])*(0:23)^2,
##      col=1,  type="l", add=FALSE)## sequenza temporale CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*0:23+
##      acomp(coefs[4,])*(0:23)^2,
##      col=2, add=TRUE, type="l")## sequenza temporale OO

## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col="white",
##      add=TRUE, pch=16, cex = 2)## start stop CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col="white", add=TRUE, pch= 16, cex = 2)## start stop OO



## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=1, add=TRUE, pch=c("c", "S","C"))## start stop CO

## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=2, add=TRUE, pch= c("o","S","O"))## start stop OO

## questi <-
##     with(df.tessitura, which(HUMIDITY=="dry"))
## df.plot <- df.tessitura[questi,]
## Y.Msizer <- acomp(df.plot[, 16:18])
## lm.2.2 <-   with(df.plot,lm(ilr(Y.Msizer) ~ COND+TIME2+I(TIME2^2)))
## modelloMsizer <- lm.2.2
## coefs <-  ilrInv(coef(modelloMsizer), orig=Y.Msizer)
## ##############################################################
## CO <- round(matrix(coefs[1,] +
##                    acomp(coefs[3,])*on.ultra.off +
##                    acomp(coefs[4,])*on.ultra.off^2,
##                    ncol = 3, byrow = FALSE), 3)
## OO <- round(matrix(acomp(coefs[1,]) +
##                    acomp(coefs[2,]) +
##                    acomp(coefs[3,])*on.ultra.off +
##                    acomp(coefs[4,])*on.ultra.off^2,
##                    ncol = 3, byrow = FALSE), 3)
## dati.iufw <- rbind(CO*100, OO*100)
##                                         #i = inizio, u = accension ultrasuoni, f = fine d = dry/w = wet
## rm(CO);rm(OO)
## Conduzione <-
##     c("Convenzionale"," ", " ",
##       "Biologico "," "," ")
## Fase <- rep(c("inizio misura","inizio sonicatura", "fine misura"),2)
## colnames(dati.iufw) <- c("Macro (%)", "Meso (%)", "Micro (%)")
## df.iufw <- cbind(Conduzione, Fase, dati.iufw)
## df.iufw <- rbind(df.iufw[1:3,], "", df.iufw[4:6,])

## plot(Y.Msizer, cex=0.25, col=coloriWET_all, add=TRUE)
## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*0:23 +
##      acomp(coefs[4,])*(0:23)^2,
##      col=3,  type="l", add=TRUE)## sequenza temporale CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*0:23+
##      acomp(coefs[4,])*(0:23)^2,
##      col=4, add=TRUE, type="l")## sequenza temporale OO
## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col="white", add=TRUE, pch=16, cex = 2)## start stop CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col="white", add=TRUE, pch= 16, cex = 2)## start stop OO
## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=3, add=TRUE, pch=c("c", "U","C"))## start stop CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=4, add=TRUE, pch= c("o","U","O"))## start stop OO
## ## for(i in 1:2){
## ##     for(k in 0:23){
## ##         newdata <-
## ##             list(COND = levels(df.plot$COND)[i],
## ##                  TIME2 = k)
## ##         X <- getModelMatrix(modello,newdata)
## ##         prediction <- ilrInv(predict(modello,newdata=newdata),orig=Y)
## ##         varEpsilon = var(modello)
## ##         XX <- kronecker(diag(ncol(predict(modello))),X)
## ##         estVar <- XX %*% vcov(modello) %*% t(XX)
## ##         predVar <- estVar+varEpsilon
## ##         plot(prediction,  add=TRUE, col= i, type= "b")
## ##         ## ellipses(prediction,ilrvar2clr(estVar),
## ##         ##          r=ConfRadius(modello,1-alpha), col=i)
## ##         if(banda.predizione)
## ##             ellipses(prediction,ilrvar2clr(predVar),
## ##                      r=ConfRadius(modello,1-alpha), col=i)
## ##     }
## ## }
## legend("topleft",
##        c("Dry.CO", "Dry.OO", "CO.Wet", "OO.Wet"),
##        fill=1:4, bty = "n")


## ###################################################
## ### code chunk number 25: qqplotAcomp
## ###################################################
## qqnorm(ilrInv(resid(modelloMsizer), orig=Y.Msizer))
## ## il codice rimane qui, ma la figura è migrata alle appendici


## ###################################################
## ### code chunk number 26: Import_porosimetria
## ###################################################
                                        #PoroElabora <- FALSE
                                        # ## Si importano i soli file "CSV" MAIUSCOLI, che derivano dalla manipolazione dei dati grezzi della porosimetria
vec.paletti.poro <- c(0.5, 50)
## if(PoroElabora){
##    source(file.path(DirCod, "ElaboraPorosimetriaConPaletti.R"))
##     }else{
##         NULL}

Export.Dir <-
    file.path(DirMain, "dati_elaborati/porosimetria")
nomi.file.elab <-
    list.files(Export.Dir, pattern = ".CSV")

df.data <- data.frame()

for(i in 1:length(nomi.file.elab)){
    file.in.elaborazione <-
        nomi.file.elab[i]
    path.completo <-
        file.path(Export.Dir,file.in.elaborazione)
    df.this <-
        read.table(file=path.completo,
                   sep=";", dec=",", header=TRUE,
                   fileEncoding =  "UTF-16LE")[-4,]
    df.this$MAN <- substr(file.in.elaborazione, 3,3)
    df.this$FIELD <-  substr(file.in.elaborazione, 4,5 )
    df.this$TIL <-  substr(file.in.elaborazione, 6,6)
    df.this$ROW <-  substr(file.in.elaborazione, 7,7)
    df.this$REP <-  substr(file.in.elaborazione, 8,8)
    df.this$REPPORO <-  substr(file.in.elaborazione, 10,10)
    df.data <- rbind.data.frame(df.data, df.this)
}

df.data <-
    df.data[c(2,3,13:18)]

df.data <-
    cbind.data.frame(df.data[c(T,F,F),-(1:2)],
                     matrix(df.data[,1], ncol=3, byrow=TRUE)
                     )

names(df.data)[7:9] <-
    c("Residuals", "Storage", "Transmission")
require(compositions)
row.names(df.data) <- 1:dim(df.data)[1]
PESI <- read.table(file.path(DirElab, "pesoaggre.csv"), sep = ";")$x

df.data <- data.frame(
    df.data[,1:6],
    RESIDUALS = (df.data[,7]-df.data[,8])/PESI,
    STORAGE = (df.data[,8]-df.data[,9])/PESI,
    TRANSMISSION = df.data[,9]/PESI
                                        #    TOT = df.data[,7]/PESI
)

## lm.micropori <-
##     with(df.data, lm(MICRO ~ MAN)) #+ TIL))
## anova(lm.micropori)
## summary(lm.micropori)
## lm.macropori <-
##     with(df.data, lm(MACRO ~ MAN)) #+ TIL))
## anova(lm.macropori)
## summary(lm.macropori)
## lm.totale <-
##     with(df.data, lm(TOT ~ MAN))#+TIL))
## anova(lm.totale)
## summary(lm.totale)


## par(mfrow = c(2,2))
## qqnorm(resid(lm.macropori))

## lm.ratio <-
##     lm(MACRO/MICRO ~ MAN*TIL, data = df.data)
## anova(lm.ratio)


df.porosimetria <-
    df.data[-c(3,6,11,10),]

Y <- acomp(df.porosimetria[, 7:9])
modelloPoro <-
    with(df.porosimetria, lm(ilr(Y) ~ MAN+TIL))
coefs.Poro <-  ilrInv(coef(modelloPoro), orig=Y)
alpha <- 0.05
##df.data$TOT <- rowSums(df.data[,7:8])
##df.data$P.INDEX <- df.data$TOT/df.data$PESI
## lm.totale <-
##     lm(df.data$P.INDEX ~ df.data$MAN+df.data$TIL)
## lm.totale2 <-
##     lm(df.data$P.INDEX ~ df.data$MAN*df.data$TIL)
## anova(lm.totale, lm.totale2)
##anova.PorTot <-
##    anova(lm.totale)


## ###################################################
## ### code chunk number 27: summary_pori
## ###################################################
## sommario <-
##     aggregate(Y, by = list(Conduzione = df.porosimetria$MAN,
##                            Lavorazione = df.porosimetria$TIL),
##               function(x) round(mean(x),2)
##               )
## attach(sommario)
## sommario <- sommario[order(Conduzione, Lavorazione),]
## detach(sommario)
## sommario[,3:5] <- sommario[,3:5]*100
## sommario$Conduzione <- c("Convenzionale","","", "Biologico", "", "")
## sommario$Lavorazione <- c("Arato", "Rippato", "Frangizollato", "Arato", "Rippato", "Frangizollato")
## colnames(sommario)[3:5] <- c("Residui (%)", "Immagazzinamento (%)", "Trasmissione (%)")
## sommario <-
##     rbind(sommario[1:3,], rep(NA, 5), sommario[4:6,])
## tabella_sommario <-
##     xtable(sommario,
##            caption = "Distribuzione dei pori nelle tre classi dimensionali dei dati ottenuti dalla analisi
## con porosimetria a mercurio", label = 'tab:Poro_medie', align = "lllp{1.25cm}p{3.25cm}p{2cm}", digits = 0)
## print(tabella_sommario, include.rownames=FALSE,
##       caption.placement = "top",
##       table.placement= "hb")
## anova.poros <- round(anova(modelloPoro), 3)
## rownames(anova.poros) <- c("Intercetta","Conduzione", "Lavorazione","Totale")
## anova.poros[1,6] <- "<10^-3"
## anova_tabella <- xtable(anova.poros,
##                         label = 'tab:poros_anova', align = 'lrrrrrr',
##                         caption = 'Analisi della varianza relativa al modello composizionale dei dati ottenuti dalle analisi con porosimetria a mercurio '
##                         )
## print(anova_tabella, caption.placement = "top")#, math.style.exponents = TRUE)


## ###################################################
## ### code chunk number 28: plotacompPore
## ###################################################
##jpeg(filename = "plotAcompPore", width = 550, height = 450)

##commento perché sennò mi da NULL DEVICE
pdf(file.path(DirGraf,"AcompPORO.pdf"))
par(mar = c(3,5,3,5))
plot(acomp(df.porosimetria[, 7:9]), cex = 2,
     col = as.numeric(as.factor(df.porosimetria$MAN)),
     pch = as.numeric(as.factor(df.porosimetria$TIL)))
        text(0.5,0.72, expression(paste(">50 ", mu, "m")))
        text(0.85,0.025, expression(paste("tra 0.5 e 50 ", mu, "m")))
        text(0.1,0.025, expression(paste("<0.5 ", mu, "m")))
##plot(Y)#, cex=2, col=as.numeric(df.porosimetria$MAN), axes = TRUE)
## intercetta <-
## ilrInv(coef(modelloPoro)[1,], orig=Y)
## bi <-
## ilrInv(rbind(0, coef(modelloPoro)[-1,]), orig=Y)
## medie <- intercetta+acomp(bi)
## plot(medie, col= 3:6, pch = 20, add = TRUE)
legend("topleft",
       c("CONV Arato", "CONV Rippato", "CONV Frang.to"),
       cex = 1.1, pt.cex = 2,
       col = rep(1,3),
       pch = 1:3, border = "white", fill=NULL)
legend("topright",
       c("BIO Arato", "BIO Rippato", "BIO Frang.to"),
       cex = 1.1, pt.cex = 2,
       col = rep(2, 3),
       pch = 1:3, border = "white", fill=NULL)
dev.off()

######Faccio ggplot
##require(ggplot2)
##require(ggtern)

## prova <- acomp(df.porosimetria[, 7:9])
## prova <- data.frame(prova)

## plot <- ggtern() +
##          theme_bw() +
##          theme_hidetitles() +
##          geom_point(data = prova,
##              aes(x = RESIDUALS, y = STORAGE, z = TRANSMISSION), alpha = 0.8, size = 1)
##dev.off()


## ###################################################
## ### code chunk number 29: pori_totali
## ###################################################

## lm.totale <-
##     lm(RESIDUALS+STORAGE+TRANSMISSION ~ MAN+TIL, data = df.data)
## anova.totale <-
##     anova(lm.totale)
## rownames(anova.totale) <- c("Conduzione", "Lavorazione", "Totale")
## anova_tabellaTot <- xtable(anova.totale,
##                            label = 'tab:tot_anova', align = 'lrrrrr',
##                            caption = 'Analisi della varianza del modello adattato alla porosit\\`a totale', digits = c(0,0, rep(3,4))
##                            )
## print(anova_tabellaTot, caption.placement = "top")#, math.style.exponents = TRUE)




## ###################################################
## ### code chunk number 30: sommario_totali
## ###################################################
## ## sommario.totale <-
## ##     round(coef(summary(lm.totale)),3)
## fattore <- interaction(df.data$MAN, df.data$TIL)
## df.data$TOTALE <- df.data$RESIDUALS+df.data$STORAGE+df.data$TRANSMISSION
## amod <- aov(TOTALE ~ fattore, data=df.data)
## HSD.test(amod, "fattore", group=TRUE, alpha = 0.05)
## tuk <- glht(amod, linfct = mcp(fattore = "Tukey"))
## tuk.cld<- cld(tuk)
## summary.totale <-
##     data.frame(
##         aggregate(TOTALE ~ MAN+TIL,
##                   data = df.data,
##                   function(x){round(mean(x, na.rm=TRUE),2)}),
##         ST.DEV=
##             aggregate(TOTALE ~ MAN+TIL,
##                       data = df.data,
##                       function(x){round(sd(x, na.rm=TRUE),2)})[,3],
##         n= aggregate(TOTALE ~ MAN+TIL,
##                      data = df.data,
##                      function(x){sum(!is.na(x))})[,3],
##         Tukey  = as.vector(tuk.cld$mcletters$Letters)
##     )
## ##names(summary.campo)[4] <- "Dens.app.g.cmc"
## attach(summary.totale)
## summary.totale <-
##     summary.totale[order(MAN,TIL),]
## detach(summary.totale)
## names(summary.totale)[1:3] <-c("Conduzione", "Lavorazione", "Porosità totale")

## summary.totale$Conduzione <- c("Convenzionale", "", "", "Biologico", "", "")
## summary.totale$Lavorazione <- mapvalues(summary.totale$Lavorazione, from = c("a", "b", "c"), to = c("Arato", "Rippato", "Frangizollato"))

## summary.totale <- rbind(summary.totale[1:3,], rep(NA, 6), summary.totale[4:6,])


## sommario_tabellaTot <- xtable(summary.totale,
##                               label = 'tab:tot_sommario', align = 'lllcccc',
##                               caption = 'Sintesi dei dati della porosit\\`a totale ricavati dalle analisi della porosimetria a mercurio'
##                               )
## print(sommario_tabellaTot, caption.placement = "top", include.rownames = FALSE)#, math.style.exponents = TRUE)


## ###################################################
## ### code chunk number 31: figkek
## ###################################################
## par(mfrow = c(2,2))
## plot(lm.Densita)


## ###################################################
## ### code chunk number 32: figlel
## ###################################################
## par(mfrow = c(2,2))
## plot(lm.spinta)


## ###################################################
## ### code chunk number 33: figboh4
## ###################################################
## qqnorm(ilrInv(resid(modelloMsizer), orig=Y.Msizer))
## ## il codice rimane qui, ma la figura è migrata alle appendici


## ###################################################
## ### code chunk number 34: qqplotpori
## ###################################################
##  qqnorm(ilrInv(resid(modelloPoro), orig=Y))


## ###################################################
## ### code chunk number 35: figurina2
## ###################################################
##  qqnorm(ilrInv(resid(modelloPoro), orig=Y))
