###################################################
### code chunk number 1: PacchettiRichiesti
###################################################
library(nlme)
library(plyr)
library(xtable)
library(foreign)
# library(gnumeric) # non piu' sviluppato per windows
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
          DirMain <-"~/Dropbox/densita/"
      }else{
DirMain <-"~/Dropbox/MOLTE_R"
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
### code chunk number 3: setupSweave
###################################################
##options(width=60, digits=4, continue=" ", useFancyQuotes=FALSE)

#################################################
### code chunk number 4: ImportDensCampo
###################################################

## Importazione dati e tare
df.data <-read.table(file.path(DirData, "DensAppCampo_2016_Campione_m.csv"),
               sep = ";", dec = ",", header=TRUE,
               colClasses =
                   c(rep("factor", 6),
                     "integer", "numeric", "factor", "factor",
                     "integer", "integer", rep("numeric",3),
                     "integer", "character"))
df.tare <-
    read.table(file.path(DirData, "TaraVasche.csv"),
               sep = ";", dec = ",", header=TRUE,
               colClasses = c("factor", "numeric"))
#Prendo la replica m
df.data <- df.data[df.data$REPLICA=="m",]
#drop=TRUE consente di non perdere il tipo dell'oggetto (?data.frame?)
df.data$REPLICA <- df.data$REPLICA[drop=TRUE]
##df.data[-which(df.data$Vasca.N  %in% df.tare$scatola),]

## Mescolamento ordinato di dati e tare (merge unisce i data frame per colonne simili)
df.data <-
    merge(df.data, df.tare,
          by.x = "Vasca.N", by.y = "scatola")
#prende i dati del 2015-16
df.2015 <-
    read.table(file.path(DirElab, "/2016_ZAN/dati_completi.csv"),
               sep = ";", dec = ",", header=TRUE)

df.2015$TRT <- mapvalues(df.2015$TRT, c("BV", "CO"), c("OO", "CO"))
df.2015$PARCELLA <- factor(df.2015$PARCELLA)
df.2015$APPEZZAMENTO <- factor(df.2015$APPEZZAMENTO)

punti.abc <-
    unique(df.2015$coord.Y[which(df.2015$REPLICA %in% letters[1:3])])

df.2015$variogramma <- as.logical(df.2015$variogramma)

df.2015 <- df.2015[which(df.2015$coord.Y %in% punti.abc),]

## ricodifica repliche da abc del 2015 a bma 2016 (basso_medio_alto, rispetto alla Pesa)
df.2015$REPLICA <- df.2015$REPLICA[drop=TRUE]
df.2015$REPLICA<-
    mapvalues(df.2015$REPLICA, letters[1:3], letters[c(2,13,1)])
##df.2015 <- df.2015[df.2015$REPLICA=="b",]

## cbind(names(df.2015)[
##     c(1,2,
##             4,5,
##             3,7,8)], names(df.data)[1:7])



###################################################
### code chunk number 5: CalcoliDensitaCampo
###################################################

## si assegna un valore densità al suolo e alla SO paglia
densita.suolo <- 2.65 ## gr/cmc
densita.paglia <-
    mean(c(0.177, 0.097)) ## http://thescipub.com/PDF/ajeassp.2012.98.106.pdf

## Calcoli per volumi
df.data$cmc.SCHELETRO <-
    df.data$ml.cilindro -
    df.data$ml.acqua +
    df.data$ml.Sassi.grossi

df.data$cmc.PAGLIA <-
    (df.data$Lordo.SO.105.g- df.data$Tara.SO.gr)/densita.paglia

## Dimensioni del cilindro di ottone
cm.diam.cilindro <- 9.6
cm.alt.cilindro <- 11.8
cmc.cilindro.volume <-
    round((cm.diam.cilindro / 2) ^ 2 * pi * cm.alt.cilindro , 1)

## si trova il peso e il volume della terra fine al netto della tara

df.data$gr.TERRA.FINE <-
    df.data$Lordo.Suolo.105.gr-df.data$gr.TARA

df.data$cmc.TERRA.FINE.REALE <-
    df.data$gr.TERRA.FINE/densita.suolo

df.data$cmc.TERRA.FINE.APPARENTE<-
    cmc.cilindro.volume-df.data$cmc.SCHELETRO#-df.data$cmc.PAGLIA

## si definisce l'operazione per ottenere la porosità
df.data$cmc.porosita <-
    df.data$cmc.TERRA.FINE.APPARENTE -
    df.data$cmc.TERRA.FINE.REALE

df.data$poros.perc <-
    df.data$cmc.porosita /
    df.data$cmc.TERRA.FINE.APPARENTE

## si definisce la bulk density (densità apparente)
df.data$densita.apparente <-
    df.data$gr.TERRA.FINE /
    df.data$cmc.TERRA.FINE.APPARENTE

df.data$YEAR <- "y16"
df.2015$YEAR <- "y15"
df.2015$TRT[df.2015$TRT=="BV"]<-"OO"
df.2015$TRT
df.dueAnni <-
    rbind.data.frame(df.data[, c(28, 2, 3, 5, 7, 6, 27)],
                     df.2015[, c(28, 1, 2, 4, 3, 5, 25)])
## prima c'erano questi numeri di colonne invece di 29 e 30: 27,28

write.table(df.dueAnni, file.path(DirElab, "df_dueanni.csv"))


###########################Preparo dataframe per multivariata
#############################################################
df.dueAnni$TRT <- mapvalues(df.dueAnni$TRT, from = c("CO", "OO"), to = c("Co", "Or"))
df.dueAnni$LAVORAZIONE <- mapvalues(df.dueAnni$LAVORAZIONE, from = c("Ara", "Rip", "Fzo"), to = c("Plw", "Chp", "Dsh"))
df.dueAnni$APPEZZAMENTO <- mapvalues(df.dueAnni$APPEZZAMENTO, from = c("1", "2", "3", "4", "9", "10"), to = c("01", "02", "03", "04", "09", "10"))
names(df.dueAnni) <- c("YEAR", "MAN", "FIELD", "PLOT", "TIL", "REP", "BD.g.cmc")
df.ordina <- within(df.dueAnni, x <- paste(YEAR, MAN, FIELD, PLOT, TIL, REP, sep = ""))
df.dueAnni <- df.ordina[with(df.ordina, order(x)),]
df.dueAnni.m <- df.dueAnni[df.dueAnni$REP=="m",-8]




###################################################
### code chunk number 5: ContiPetrolio
###################################################
df.petrolspinta <-
    read.table(path.expand(file.path(DirData, "SpintaPetrolio.csv")),
               sep = ";", dec = ",", header=TRUE, skip=2,
               colClasses =
                   c("integer",
                     rep("factor",6),
                     rep("numeric", 4)
                 )
               )
df.spinta <- df.petrolspinta[,-1]
df.spinta$P.105.gr[38] <- 9.105 #correzione

df.infoparcelle.m <-df.petrolspinta[1:36, 1:7]
densita.petrolio <- 0.761 ## gr/cmc

####################prendo anche le repliche mi da degli errori!!!!!!
df.spintarep <- read.table(path.expand(file.path(DirData, "SpintaRep.csv")),
                           sep = "\t", dec = ",", header=TRUE)
df.spintarep <- within(df.spintarep, x <- paste(PARCELLA, REPLICA, sep = ""))
df.ordina <- df.spinta[,1:6]
df.ordina <- within(df.ordina, x <- paste(TRT, APPEZZAMENTO, COD, PARCELLA, REPLICA, sep = ""))
df.spintarep <- df.spintarep[with(data = df.spintarep, order(x)),]
df.ordina <- df.ordina[with(df.ordina, order(x)),]

df.merge <- merge(df.ordina, df.spintarep, by = c("x"))
df.merge <- df.merge[,c(-1, -8, -9)]
names(df.merge)[4] <- "PARCELLA"
names(df.merge)[5] <- "REPLICA"
df.spinta <- df.spinta[,c(1:6, 9, 10)]
df.spinta <- rbind(df.spinta, df.merge)
rm(df.spintarep)
rm(df.ordina)
rm(df.merge)

##faccio media delle repliche

df.spinta$densita.apparente <-
    df.spinta$P.105.gr /
    (df.spinta$P.Spinta.gr/densita.petrolio)

library(plyr)
df.spinta <- ddply(df.spinta, .(TRT, APPEZZAMENTO, COD, LAVORAZIONE, PARCELLA, REPLICA), summarize, P.Spinta.gr = mean(P.Spinta.gr),  P.105.gr = mean(P.105.gr), densita.apparente = mean(densita.apparente))
write.table(df.spinta, file.path(DirElab, "df_spinta.csv"))
#####################################costruisco data frame per multivariata
###########################################################################
df.spinta$TRT <- mapvalues(df.spinta$TRT, from = c("CO", "OO"), to = c("Co", "Or"))
df.spinta$LAVORAZIONE <- mapvalues(df.spinta$LAVORAZIONE, from = c("Ara", "Rip", "Fzo"), to = c("Plw", "Chp", "Dsh"))
df.spinta$APPEZZAMENTO <- mapvalues(df.spinta$APPEZZAMENTO, from = c("2", "4", "9", "10"), to = c("02", "04", "09", "10"))
df.spinta.m <- df.spinta[df.spinta$REP=="m",c(1, 2, 5, 4, 6, 9)]
anno <- rep("y16", nrow(df.spinta.m))
df.spinta.m <- cbind(anno, df.spinta.m)
names(df.spinta.m) <- c("YEAR", "MAN", "FIELD", "PLOT", "TIL", "REP", "CLOD.g.cmc")
anno.2 <- cbind(df.dueAnni.m[df.dueAnni.m$YEAR=="y15", 1:6], "CLOD.g.cmc" = rep(NA, sum(df.dueAnni.m$YEAR=="y15")))
df.spinta.m <- rbind(df.spinta.m, anno.2)
df.ordina <- within(df.spinta.m, x <- paste(YEAR, MAN, FIELD, PLOT, TIL, REP, sep = ""))
df.spinta.m <- df.ordina[with(df.ordina, order(x)),-8]
df.nocrop <- merge(df.dueAnni.m, df.spinta.m)
df.nocrop <- cbind(df.nocrop, x = with(df.nocrop, paste(YEAR, MAN, FIELD, sep = "")))
df.crop <- mapvalues
levels(df.nocrop$x)
df.finale <- cbind(df.nocrop, CROP = mapvalues(df.nocrop$x,
                                               from = c("y15Co09", "y15Co10", "y15Or01", "y15Or03",
                                                        "y16Co09", "y16Co10", "y16Or02", "y16Or04"),
                                               to = c("Sun", "Bar", "Bar", "Sun",
                                                      "Bar", "Sun", "Sun", "Bar")
          ))[,c(1:6, 10, 7, 8)]

write.table( df.finale, "/home/simone/Dropbox/MOLTE/SoloSuolo/dati_grezzi/dens_app.csv", sep = ";")
