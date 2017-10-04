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
library(agricolae)
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
### code chunk number 3: ImportDensCampo
###################################################
#oldpar <- par()

df.dueAnni <-
    read.table(file.path(DirElab, "df_dueanni.csv"),
               header = T, sep = "")
df.dueAnni <-  df.dueAnni[df.dueAnni$REPLICA == "m", ]

##with(df.dueAnni, table(YEAR, TRT,LAVORAZIONE, APPEZZAMENTO))
## df.dueAnni <-
## read.table(file.path(DirElab, "dens_app.csv"),
## header = T, sep = ";")
df.dueAnni <- df.dueAnni[df.dueAnni$REPLICA== "m",]
###################################################
###DescrittivaDensitaCampo1
###################################################
##with(df.dueAnni, table(TRT, LAVORAZIONE, APPEZZAMENTO, YEAR))
options(contrasts=c("contr.treatment","contr.poly"))

fattore <- interaction(df.dueAnni$YEAR, df.dueAnni$TRT, df.dueAnni$LAVORAZIONE)
amod <- aov(densita.apparente ~ fattore, data=df.dueAnni)
HSD.test(amod, "fattore", group=TRUE, alpha = 0.05)
tuk <- glht(amod, linfct = mcp(fattore = "Tukey"))
tuk.cld<- cld(tuk)
summary.campo <-
    data.frame(
        aggregate(densita.apparente ~ YEAR+TRT+LAVORAZIONE,
                  data = df.dueAnni,
                  function(x){round(mean(x, na.rm=TRUE),2)}),
        ST.DEV=
            aggregate(densita.apparente ~ YEAR+TRT+LAVORAZIONE,
                      data = df.dueAnni,
                      function(x){round(sd(x, na.rm=TRUE),2)})[,4],
        n= aggregate(densita.apparente ~ YEAR+TRT+LAVORAZIONE,
                     data = df.dueAnni,
                     function(x){sum(!is.na(x))})[,4],
        Tukey  = as.vector(tuk.cld$mcletters$Letters)
    )
##names(summary.campo)[4] <- "Dens.app.g.cmc"
attach(summary.campo)
summary.campo <-
    summary.campo[order(YEAR, TRT, LAVORAZIONE),]
detach(summary.campo)



###################################################
### code chunk number 4: Summary_Campo
###################################################
names(summary.campo) <-  
    c("Anno", "Conduzione", "Lavorazione", "Media", "Dev. std", "n", "Tukey")
summary.campo$Anno <- 
    c("2015", rep(" ", 5), "2016", rep(" ", 5))
summary.campo$Conduzione <-
    c("Co", rep(" ", 2), "Or", rep(" ", 2),
      "Co", rep(" ", 2), "Or", rep(" ", 2))




###################################################
### code chunk number 6: ModelloDensitaCampo
###################################################
lm.Densita <-
    lm(densita.apparente ~ YEAR+TRT+LAVORAZIONE, data = df.dueAnni)
lm.DensSign <-
    lm(densita.apparente ~ YEAR+TRT, data = df.dueAnni)


###################################################
### code chunk number 7: anova_modello
###################################################
anova.dens <- anova(lm.Densita)
rownames(anova.dens) <-     c("Anno", "Conduzione", "Lavorazione", "residui")
##%   c("Anno", "Management", "Tillage", "residui")
## tabella.anova.dens <-
##     xtable(anova.dens,
##            label = 'tab:anova del modello', align  = "rrrrrr",
##            caption = 'Tabella ANOVA per i valori di densità rilevati col metodo \\emph{Core}')
## print(tabella.anova.dens,
##       include.rownames=TRUE,
##       caption.placement = "top")


##par(mfrow = c(2,2))
##plot(lm.Densita)


###################################################
### code chunk number 11: Sommario_petrolio
###################################################
df.petrolio <-
    read.table(file.path(DirElab, "df_spinta.csv"),
               header = T, sep = "")
fattore <- with(df.petrolio, interaction(TRT, LAVORAZIONE))
amod <- aov(df.petrolio$densita.apparente ~ fattore)
HSD.test(amod, "fattore", group=TRUE, alpha = 0.05)
tuk <- glht(amod, linfct = mcp(fattore = "Tukey"))
tuk.cld <- cld(tuk) #letter-based display)
summary.petrolio <-
    data.frame(
        aggregate(densita.apparente ~ TRT+LAVORAZIONE,
                  data = df.petrolio,
                  function(x){round(mean(x, na.rm=TRUE),2)}),
        ST.DEV=
            aggregate(densita.apparente ~ TRT+LAVORAZIONE,
                      data = df.petrolio,
                      function(x){round(sd(x, na.rm=TRUE),2)})[,3],
        n= aggregate(densita.apparente ~ TRT+LAVORAZIONE,
                     data = df.petrolio,
                     function(x){sum(!is.na(x))})[,3],
        Tukey =     as.vector(tuk.cld$mcletters$Letters)
    )

##names(summary.campo)[4] <- "Dens.app.g.cmc"
attach(summary.petrolio)
summary.petrolio <- summary.petrolio[order(TRT, LAVORAZIONE),]
detach(summary.petrolio)
names(summary.petrolio) <- c("Conduzione","Lavorazione","Media","Dev. std", "n", "Tukey")
summary.petrolio$Conduzione <- c("Co", rep("", 2), "Or", rep("", 2))


###################################################
### code chunk number 14: Analisi_Petrolio
###################################################
lm.spinta <-
    lm(densita.apparente ~ TRT + LAVORAZIONE, data = df.petrolio)

anova_spinta <-
    anova(lm.spinta)

summary_spinta <-
    summary(lm.spinta)


###################################################
### code chunk number 15: Analisi_Petrolio
###################################################
rownames(anova_spinta) <- c("Conduzione", "Lavorazione", "Residui")

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
## due umidit\`a con ultrasuoni
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
## par(mfrow = c(1,1))
## modello <- lm.2
## coefs <-  ilrInv(coef(modello), orig = Y.Msizer1)
## alpha <- 0.05
## plot(Y.Msizer1, cex=0.15, col=df.plot$COND, axes = TRUE, plotMissings = FALSE)
## macro meso micro
## plot(acomp(c(2,2,2)), pch = 20, cex = 2, add = TRUE, col = 1)
## plot(acomp(c(80,5,15)), pch = 20, cex = 2, add = TRUE, col = 2)
## plot(acomp(c(75, 5,20)), pch = 20, cex = 2, add = TRUE, col = 3)

## plot(acomp(c(5, 75,20)), pch = 20, cex = 2, add = TRUE, col = 3)

## plot(acomp(c(5, 55 ,40)), pch = 20, cex = 2, add = TRUE, col = 3)
on.ultra.off <- c(0, 11, 23)
CO <- round(matrix(coefs[1,] +
                   acomp(coefs[3,])*on.ultra.off +
                   acomp(coefs[4,])*on.ultra.off^2,
                   ncol = 3, byrow = FALSE), 3)
OO <- round(matrix(acomp(coefs[1,]) +
                   acomp(coefs[2,]) +
                   acomp(coefs[3,])*on.ultra.off +
                   acomp(coefs[4,])*on.ultra.off^2,
                   ncol = 3, byrow = FALSE), 3)
dati.iufd <- rbind(CO*100, OO*100)
                                        #i = inizio, u = accension ultrasuoni, f = fine d = dry/w = wet
Conduzione <-
    c("Convenzionale"," ", " ",
      "Biologico "," "," ")
Fase <- rep(c("misura","inizio sonicatura", "fine misura"),2)
colnames(dati.iufd) <- c("Macro (%)", "Meso (%)", "Micro (%)")
df.iufd <- cbind(Conduzione, Fase, dati.iufd)
df.iufd <- rbind(df.iufd[1:3,], "", df.iufd[4:6,])




## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*0:23 +
##      acomp(coefs[4,])*(0:23)^2,
##      col=1,  type="l", add=TRUE)## sequenza temporale CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*0:23+
##      acomp(coefs[4,])*(0:23)^2,
##      col=2, add=TRUE, type="l")## sequenza temporale OO
## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=1, add=TRUE, pch=c("c", "D","C"))## start stop CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=2, add=TRUE, pch= c("o","D","O"))## start stop OO



questi <-
    with(df.tessitura, which(HUMIDITY=="dry"))
df.plot <- df.tessitura[questi,]
Y.Msizer <- acomp(df.plot[, 16:18])
lm.2.2 <-   with(df.plot,lm(ilr(Y.Msizer) ~ COND+TIME2+I(TIME2^2)))
modelloMsizer <- lm.2.2
coefs <-  ilrInv(coef(modelloMsizer), orig=Y.Msizer)
##############################################################
CO <- round(matrix(coefs[1,] +
                   acomp(coefs[3,])*on.ultra.off +
                   acomp(coefs[4,])*on.ultra.off^2,
                   ncol = 3, byrow = FALSE), 3)
OO <- round(matrix(acomp(coefs[1,]) +
                   acomp(coefs[2,]) +
                   acomp(coefs[3,])*on.ultra.off +
                   acomp(coefs[4,])*on.ultra.off^2,
                   ncol = 3, byrow = FALSE), 3)
dati.iufw <- rbind(CO*100, OO*100)
                                        #i = inizio, u = accension ultrasuoni, f = fine d = dry/w = wet
rm(CO);rm(OO)
Conduzione <-
    c("Convenzionale"," ", " ",
      "Biologico "," "," ")
Fase <- rep(c("inizio misura","inizio sonicatura", "fine misura"),2)
colnames(dati.iufw) <- c("Macro (%)", "Meso (%)", "Micro (%)")
df.iufw <- cbind(Conduzione, Fase, dati.iufw)
df.iufw <- rbind(df.iufw[1:3,], "", df.iufw[4:6,])

## plot(Y.Msizer, cex=0.25, col= as.numeric(df.plot$COND)+2, add=TRUE)
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
##      col=3, add=TRUE, pch=c("c", "W","C"))## start stop CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=4, add=TRUE, pch= c("o","W","O"))## start stop OO
## legend("topleft",
##        c("Dry.CO", "Dry.OO", "CO.Wet", "OO.Wet"),
##        fill=1:4)





###################################################
### code chunk number 22: anova_acompWET
###################################################
anova.comp.wet <- anova(lm.2)
rownames(anova.comp.wet) <- c("Intercetta","Conduzione", "Tempo", "Tempo^2","Residui")

###################################################
### code chunk number 23: anova_acompDRY
###################################################
anova.comp.dry <- anova(lm.2.2)
rownames(anova.comp.dry) <- c("Intercetta","Conduzione","Tempo", "Tempo^2","Residui")


###################################################
### code chunk number 24: figboh3
###################################################

modello <- lm.2
coefs <-  ilrInv(coef(modello), orig = Y.Msizer1)
alpha <- 0.05
##plot(Y.Msizer1, cex=0.15, col=df.plot$COND, axes = TRUE, plotMissings = FALSE, add = TRUE)
on.ultra.off <- c(0, 11, 23)
CO <- round(matrix(coefs[1,] +
                   acomp(coefs[3,])*on.ultra.off +
                   acomp(coefs[4,])*on.ultra.off^2,
                   ncol = 3, byrow = FALSE), 3)
OO <- round(matrix(acomp(coefs[1,]) +
                   acomp(coefs[2,]) +
                   acomp(coefs[3,])*on.ultra.off +
                   acomp(coefs[4,])*on.ultra.off^2,
                   ncol = 3, byrow = FALSE), 3)
dati.iufd <- rbind(CO*100, OO*100)
##i = inizio, u = accension ultrasuoni, f = fine d = dry/w = wet
Conduzione <-
    c("Convenzionale"," ", " ",
      "Biologico "," "," ")
Fase <- rep(c("misura","inizio sonicatura", "fine misura"),2)
colnames(dati.iufd) <- c("Macro (%)", "Meso (%)", "Micro (%)")
df.iufd <- cbind(Conduzione, Fase, dati.iufd)
df.iufd <- rbind(df.iufd[1:3,], "", df.iufd[4:6,])

## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*0:23 +
##      acomp(coefs[4,])*(0:23)^2,
##      col=1,  type="l", add=TRUE)## sequenza temporale CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*0:23+
##      acomp(coefs[4,])*(0:23)^2,
##      col=2, add=TRUE, type="l")## sequenza temporale OO
## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=1, add=TRUE, pch=c("c", "D","C"))## start stop CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=2, add=TRUE, pch= c("o","D","O"))## start stop OO

questi <-
    with(df.tessitura, which(HUMIDITY=="dry"))
df.plot <- df.tessitura[questi,]
Y.Msizer <- acomp(df.plot[, 16:18])
lm.2.2 <-   with(df.plot,lm(ilr(Y.Msizer) ~ COND+TIME2+I(TIME2^2)))
modelloMsizer <- lm.2.2
coefs <-  ilrInv(coef(modelloMsizer), orig=Y.Msizer)
##############################################################
CO <- round(matrix(coefs[1,] +
                   acomp(coefs[3,])*on.ultra.off +
                   acomp(coefs[4,])*on.ultra.off^2,
                   ncol = 3, byrow = FALSE), 3)
OO <- round(matrix(acomp(coefs[1,]) +
                   acomp(coefs[2,]) +
                   acomp(coefs[3,])*on.ultra.off +
                   acomp(coefs[4,])*on.ultra.off^2,
                   ncol = 3, byrow = FALSE), 3)
dati.iufw <- rbind(CO*100, OO*100)
##i = inizio, u = accension ultrasuoni, f = fine d = dry/w = wet
rm(CO);rm(OO)
Conduzione <-
    c("Convenzionale"," ", " ",
      "Biologico "," "," ")
Fase <- rep(c("inizio misura","inizio sonicatura", "fine misura"),2)
colnames(dati.iufw) <- c("Macro (%)", "Meso (%)", "Micro (%)")
df.iufw <- cbind(Conduzione, Fase, dati.iufw)
df.iufw <- rbind(df.iufw[1:3,], "", df.iufw[4:6,])

## plot(Y.Msizer, cex=0.25, col= as.numeric(df.plot$COND)+2, add=TRUE)
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
##      col=3, add=TRUE, pch=c("c", "W","C"))## start stop CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=4, add=TRUE, pch= c("o","W","O"))## start stop OO

## legend("topleft",
##        c("Dry.CO", "Dry.OO", "CO.Wet", "OO.Wet"),
##        fill=1:4)


###################################################
### code chunk number 25: qqplotAcomp
###################################################
##qqnorm(ilrInv(resid(modelloMsizer), orig=Y.Msizer))
## il codice rimane qui, ma la figura è migrata alle appendici


###################################################
### code chunk number 26: Import_porosimetria
###################################################
##PoroElabora <- FALSE
## ## Si importano i soli file "CSV" MAIUSCOLI, che derivano dalla manipolazione dei dati grezzi della porosimetria

vec.paletti.poro <- c(0.05, 0.1) 
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
    c("micropori", "mesopori", "macropori")
require(compositions)
row.names(df.data) <- 1:dim(df.data)[1]
PESI <- read.table(file.path(DirElab, "pesoaggre.csv"), sep = ";")$x

df.data <- data.frame(
    df.data[,1:6],
    MICRO = (df.data[,7]-df.data[,8])/PESI, 
    MESO = (df.data[,8]-df.data[,9])/PESI, 
    MACRO = df.data[,9]/PESI
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
df.data$TOT <- rowSums(df.data[,7:9])
##df.data$P.INDEX <- df.data$TOT/df.data$PESI
## lm.totale <- 
##     lm(df.data$P.INDEX ~ df.data$MAN+df.data$TIL)
## lm.totale2 <- 
##     lm(df.data$P.INDEX ~ df.data$MAN*df.data$TIL)
## anova(lm.totale, lm.totale2)
##anova.PorTot <- 
##    anova(lm.totale)


###################################################
### code chunk number 27: plotacompPore
###################################################
##plot(Y, cex=0.15, col=as.numeric(df.porosimetria$MAN), axes = TRUE)
intercetta <-
    ilrInv(coef(modelloPoro)[1,], orig=Y)
bi <-
    ilrInv(rbind(0, coef(modelloPoro)[-1,]), orig=Y)
medie <- intercetta+acomp(bi)
##plot(medie, col=as.factor(df.porosimetria$TIL), add = TRUE)



###################################################
### code chunk number 28: figurina
#####################################################
##plot(Y, cex=0.15, col=as.numeric(df.porosimetria$MAN), axes = TRUE)
intercetta <-
    ilrInv(coef(modelloPoro)[1,], orig=Y)
bi <-
    ilrInv(rbind(0, coef(modelloPoro)[-1,]), orig=Y)
medie <- intercetta+acomp(bi)
##plot(medie, col=as.factor(df.porosimetria$TIL), add = TRUE)



###################################################
### code chunk number 29: summary_pori
###################################################
sommario <- 
    aggregate(Y, by = list(Conduzione = df.porosimetria$MAN, 
                           Lavorazione = df.porosimetria$TIL), 
              function(x) round(mean(x),2)
              )
attach(sommario)
sommario <- sommario[order(Conduzione, Lavorazione),]
detach(sommario)
sommario$Conduzione <- c("Co","","", "Or", "", "")
sommario$Lavorazione <- c("Ara", "Rip", "Fzo", "Ara", "Rip", "Fzo")


###################################################
### code chunk number 30: pori_totali
###################################################
lm.totale <- 
    lm(MICRO+MACRO+MESO ~ MAN, data = df.data)
anova.totale <- 
    anova(lm.totale)


###################################################
### PCA prova Simone
###################################################
df.petrolio2 <- cbind(rep("y16", nrow(df.petrolio)),
                      df.petrolio)[, -c(4, 8, 9)]
names(df.petrolio2)[1] <- "YEAR"
names(df.petrolio2)[7] <- "densita.clod"
str(df.petrolio2)
df.petrolio2 <-
    with(df.petrolio2,
         aggregate(densita.clod, by = list(YEAR, TRT, APPEZZAMENTO, LAVORAZIONE, PARCELLA), FUN = function(x) mean(x, na.rm = TRUE)))
df.petrolio2$REPLICA <- rep("m", nrow(df.petrolio2))#Lo faccio perché poi dopo mi serve che sia indicato come m

names(df.petrolio2) <- c("YEAR", "TRT", "APPEZZAMENTO", "LAVORAZIONE", "PARCELLA", "densita.clod", "REPLICA")
df.petrolio2 <- df.petrolio2[, c(1,2,3,5,4,7,6)]
fittizi2015 <- data.frame(
    YEAR = rep("y15", nrow(df.dueAnni[df.dueAnni$YEAR=="y15",])),
    TRT  = df.dueAnni[df.dueAnni$YEAR=="y15",]$TRT,
    APPEZZAMENTO = df.dueAnni[df.dueAnni$YEAR=="y15",]$APPEZZAMENTO,
    PARCELLA = df.dueAnni[df.dueAnni$YEAR=="y15",]$PARCELLA,
    LAVORAZIONE = df.dueAnni[df.dueAnni$YEAR=="y15",]$LAVORAZIONE,
    REPLICA = df.dueAnni[df.dueAnni$YEAR=="y15",]$REPLICA,
    densita.clod = rep(NA, nrow(df.dueAnni[df.dueAnni$YEAR=="y15",]))
)
df.petrolio3 <- rbind(df.petrolio2, fittizi2015)

attach(df.petrolio3)
df.petrolio3 <- df.petrolio3[order(YEAR, TRT, APPEZZAMENTO, PARCELLA, LAVORAZIONE, REPLICA),]
detach(df.petrolio3)
attach(df.dueAnni)
df.dueAnni <- df.dueAnni[order(YEAR, TRT, APPEZZAMENTO, PARCELLA, LAVORAZIONE, REPLICA),]
detach(df.dueAnni)

df.densitaCompleto <-
    merge(df.dueAnni, df.petrolio3)



df.datiporimedi <- read.table(file.path(Export.Dir, "mediaDiametriPond.csv"),
             sep = ";")
df.porimedi2 <- data.frame(
     YEAR = rep("y16", nrow(df.datiporimedi)),
     df.datiporimedi
)
df.porimedi2 <-
    df.porimedi2[-c(3, 6, 10, 11), c(1:3, 5, 4, 6:8)]
names(df.porimedi2)[1:6] <-
    names(df.densitaCompleto)[1:6]
df.porimedi2[7] <- NULL
names(df.porimedi2)[7] <- "Diam.Medio.Pori"


df.porimedi2$LAVORAZIONE <-
    mapvalues(df.porimedi2$LAVORAZIONE, from = c("a", "b", "c"), to = c("Ara", "Rip", "Fzo"))
df.porimedi2$APPEZZAMENTO <-
    as.numeric(df.porimedi2$APPEZZAMENTO)
df.porimedi2$TRT <-
    mapvalues(df.porimedi2$TRT, from = c("C", "O"), to = c("CO", "OO"))

df.fittiziopori <-
    cbind(df.densitaCompleto[,1:6], rep(NA, nrow(df.densitaCompleto)))
names(df.fittiziopori)[7] <- "Diam.Medio.Pori"

for(i in 1:nrow(df.fittiziopori)){
    for(k in 1:nrow(df.porimedi2)){
        if(with(df.fittiziopori, paste(YEAR, TRT, APPEZZAMENTO, LAVORAZIONE, PARCELLA, REPLICA))[i]
           ==with(df.porimedi2, paste(YEAR, TRT, APPEZZAMENTO, LAVORAZIONE, PARCELLA, REPLICA))[k]){
            df.fittiziopori$Diam.Medio.Pori[i] <- df.porimedi2$Diam.Medio.Pori[k]
        }
    }
}
#########Se ci vogliamo mettere i pori totali, scommentare questo
#################################################################
df.datiporitot <- data.frame(
    YEAR = rep("y16", nrow(df.data)),
    PARCELLA = rep(2, nrow(df.data)),
    df.data
)

df.datiporitot <-
    df.datiporitot[,c(1, 3, 4, 2, 5, 7, 12)]

names(df.datiporitot)[1:6] <-
    names(df.densitaCompleto)[1:6]
names(df.datiporitot)[7] <- "PoriTotali"

df.datiporitot$LAVORAZIONE <-
    mapvalues(df.datiporitot$LAVORAZIONE, from = c("a", "b", "c"), to = c("Ara", "Rip", "Fzo"))
df.datiporitot$APPEZZAMENTO <-
    as.numeric(df.datiporitot$APPEZZAMENTO)
df.datiporitot$TRT <-
    mapvalues(df.datiporitot$TRT, from = c("C", "O"), to = c("CO", "OO"))

df.fittiziopori <-
    cbind(df.densitaCompleto[,1:6], rep(NA, nrow(df.densitaCompleto)))
names(df.fittiziopori)[7] <- "Por.Tot"

for(i in 1:nrow(df.fittiziopori)){
    for(k in 1:nrow(df.datiporitot)){
        if(with(df.fittiziopori, paste(YEAR, TRT, APPEZZAMENTO, LAVORAZIONE, PARCELLA, REPLICA))[i]
           ==with(df.datiporitot, paste(YEAR, TRT, APPEZZAMENTO, LAVORAZIONE, PARCELLA, REPLICA))[k]){
            df.fittiziopori$Por.Tot[i] <- df.datiporitot$PoriTotali[k]
        }
    }
}

PoriTotPCA <-
    with(df.fittiziopori, aggregate(Por.Tot, by = list(TRT, LAVORAZIONE, APPEZZAMENTO), FUN = function(x) mean(x, na.rm = TRUE)))


df.stabilita <-
    read.table("/home/simone/Dropbox/MOLTE/SoloSuolo/dati_grezzi/Stabil.Terr.csv", sep = ";")

################per i dry
df.stabilita2 <- df.stabilita[df.stabilita$HUMIDITY=="dry",c(1:6, 12)]

names(df.stabilita2)[1:6] <- names(df.densitapori)[1:6]
df.stabilita2$LAVORAZIONE <-
    mapvalues(df.stabilita2$LAVORAZIONE, from = c("Plw", "Chp", "Dsh"), to = c("Ara", "Rip", "Fzo"))
df.stabilita2$TRT <-
    mapvalues(df.stabilita2$TRT, from = c("Co", "Or"), to = c("CO", "OO"))

df.finale <-
    merge(df.densitapori, df.stabilita2)
names(df.finale)[10] <- "Diam.Medio.Aggr.Dry"

########Per i Wet
df.stabilita3 <- df.stabilita[df.stabilita$HUMIDITY=="wet",c(1:6, 12)]

names(df.stabilita3)[1:6] <- names(df.densitapori)[1:6]
df.stabilita3$LAVORAZIONE <-
    mapvalues(df.stabilita3$LAVORAZIONE, from = c("Plw", "Chp", "Dsh"), to = c("Ara", "Rip", "Fzo"))
df.stabilita3$TRT <-
    mapvalues(df.stabilita3$TRT, from = c("Co", "Or"), to = c("CO", "OO"))

df.finale <-
    merge(df.finale, df.stabilita3)


names(df.finale)[11] <- "Diam.Medio.Aggr.Wet"




#######CN

df.CNHCL <-
    read.table(file.path(DirData, "CNHCL_serie_m.csv"), sep = "\t", header = TRUE, dec = ",")
names(df.CNHCL)[c(3,4)] <- c("Nitrogen_HCl", "Carbon_HCl")
df.CN <-
    read.table(file.path(DirData, "CN_serie_m.csv"), sep = ";", header = TRUE, dec = ",")

df.CNTOT <-
    merge(df.CN, df.CNHCL)
df.CNTOT[,1] <- NULL

df.CNTOT$TRT <- substr(as.character(df.CNTOT$PARCELLA), 1, 2)
df.CNTOT$APPEZZAMENTO <- as.numeric(substr(as.character(df.CNTOT$PARCELLA), 3, nchar(as.character(df.CNTOT$PARCELLA))-2))
df.CNTOT$PARCELLA2 <- substr(as.character(df.CNTOT$PARCELLA), nchar(as.character(df.CNTOT$PARCELLA)), nchar(as.character(df.CNTOT$PARCELLA)))
df.CNTOT$LAVORAZIONE <- substr(as.character(df.CNTOT$PARCELLA), nchar(as.character(df.CNTOT$PARCELLA))-1, nchar(as.character(df.CNTOT$PARCELLA))-1)
df.CNTOT$REPLICA <- rep("m", nrow(df.CNTOT))
df.CNTOT$PARCELLA <- "y16"
names(df.CNTOT)[1] <- "YEAR"
names(df.CNTOT)[8] <- "PARCELLA"
df.CNTOT.ordinato <-
    df.CNTOT[,c(1, 6:10, 2:5)]
df.CNTOT$LAVORAZIONE <- mapvalues(df.CNTOT$LAVORAZIONE, from = c("A","B","C"), to = c("Ara","Rip","Fzo"))

fittiziCN2015 <- data.frame(
    YEAR = rep("y15", nrow(df.dueAnni[df.dueAnni$YEAR=="y15",])),
    TRT  = df.dueAnni[df.dueAnni$YEAR=="y15",]$TRT,
    APPEZZAMENTO = df.dueAnni[df.dueAnni$YEAR=="y15",]$APPEZZAMENTO,
    PARCELLA = df.dueAnni[df.dueAnni$YEAR=="y15",]$PARCELLA,
    LAVORAZIONE = df.dueAnni[df.dueAnni$YEAR=="y15",]$LAVORAZIONE,
    REPLICA = df.dueAnni[df.dueAnni$YEAR=="y15",]$REPLICA,
    Nitrogen = rep(NA, nrow(df.dueAnni[df.dueAnni$YEAR=="y15",])),
    Carbon = rep(NA, nrow(df.dueAnni[df.dueAnni$YEAR=="y15",])),
    Nitrogen_HCl= rep(NA, nrow(df.dueAnni[df.dueAnni$YEAR=="y15",])),
    Carbon_HCl= rep(NA, nrow(df.dueAnni[df.dueAnni$YEAR=="y15",]))
)

df.CNFIN <- rbind(fittiziCN2015, df.CNTOT)

df.CNFIN$PARCELLA <- as.numeric(df.CNFIN$PARCELLA)

df.finale <-
    merge(df.finale, df.CNFIN)

write.table(df.finale, file.path(DirElab, "PerPCA.csv"), sep = ";", row.names = T)


with(df.finale, cor(densita.apparente, densita.clod, use = "complete.obs"))





###########Piglio le cose di Lorenzo
df.prod <-
    read.table("~/Dropbox/MOLTE/SoloSuolo/dati_grezzi/Produttivita2016.csv", sep = ";", header = TRUE, dec = ",")
df.chem <-
    read.table("~/Dropbox/MOLTE/SoloSuolo/dati_grezzi/Analisi_chimiche_2016.csv", sep = ";", header = TRUE, dec = ",")

df.prod$Crop <-
    with(df.prod, mapvalues(paste(Treatment, Crop), from = c("CO BAR", "CO SUN", "OO SUN", "OO BAR"), to = c("09","10", "02", "04"))) 

df.prod2 <-
    with(df.prod, aggregate(Weight, by = list(Treatment, Tillage, Crop), FUN = function(x) mean(x, na.rm = TRUE)))
names(df.prod2) <- c("TRT", "LAVORAZIONE", "APPEZZAMENTO", "PRODUZIONE")
df.prod2$LAVORAZIONE <- mapvalues(df.prod2$LAVORAZIONE, from = c("A", "B", "C"), to = c("Ara", "Rip", "Fzo"))


df.chem$CROP <-
    with(df.chem, mapvalues(paste(MAN, CROP), from = c("Co Bar", "Co Sun", "Or Sun", "Or Bar"), to = c("09","10", "02", "04"))) 
df.chem2 <-
    with(df.chem, aggregate(list(P2O5.mg.kg, S.O.perc, N.TOT.g.kg), by = list(MAN, TIL, CROP), FUN = function(x) mean(x, na.rm = TRUE)))
names(df.chem2) <- c("TRT", "LAVORAZIONE", "APPEZZAMENTO", "P2O5", "SO.perc", "NTOT")
df.chem2$TRT <-
    mapvalues(df.chem2$TRT, from = c("Co", "Or"), to = c("CO", "OO"))
df.chem2$LAVORAZIONE <-
    mapvalues(df.chem2$LAVORAZIONE, from = c("Plw", "Chp", "Dsh"), to = c("Ara", "Rip", "Fzo"))

df.chem2[c(1:3, 10:12), 4:6] <-
    jitter(as.matrix(df.chem2[4:9, 4:6]), 150)        

df.Lorenzo <-
    merge(df.chem2, df.prod2)
names(df.Lorenzo)[2] <- "LAV"

##non sappiamo qual è il metodo di misura che risponde alla nostra esigenza, questo significa che i domain a livello
##di aggregato non sono estrapolabili a livello di cilindro

require(FactoMineR)
df.PCA <-
    df.finale[df.finale$YEAR=="y16",c(2,3, 5,7:15)]

names(PoriTotPCA) <- c("TRT", "LAV", "APPEZZAMENTO", "PoriTot")
PoriTotPCA <- PoriTotPCA[!PoriTotPCA$APPEZZAMENTO%in%c(1,3),]


df.PCA1 <-
    aggregate(df.PCA[, c(4:12)], by = list(TRT = df.PCA$TRT, APPEZZAMENTO = df.PCA$APPEZZAMENTO, LAV = df.PCA$LAVORAZIONE), FUN  =  function(x)
        mean(x, na.rm = TRUE))

df.Lorenzo$APPEZZAMENTO <- as.numeric(df.Lorenzo$APPEZZAMENTO)
df.PCA1 <-
    merge(merge(df.PCA1, df.Lorenzo), PoriTotPCA)
param.chim <- 8:14
param.fis <- c(3:7, 16)


res <- PCA(df.PCA1, quali.sup = c(1,2), quanti.sup = c(param.fis, 15), graph = FALSE)
par(mfrow = c(1,2))
plot.PCA(res, choix = "var", habillage = "TRT", axes = c(1,2))
plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(1,2))
dimdesc(res)

df.PCA2 <-
    df.PCA[,-6]
df.PCA2 <-
    df.PCA2[complete.cases(df.PCA2),-2]
res <- PCA(df.PCA2, quali.sup = c(1,2), graph = FALSE)
par(mfrow = c(2,2))
plot.PCA(res, choix = "var", habillage = "TRT", axes = c(1,2))
plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(1,2))
plot.PCA(res, choix = "var", habillage = "TRT", axes = c(1,3))
plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(1,3))
dimdesc(res)

##Domani: L'unità statistica è la parcella: medie petrolio, CN,
##aggiungere diametro medio ponderato secco, diametro medio ponderato dei pori
