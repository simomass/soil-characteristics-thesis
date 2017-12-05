library(nlme)
library(plyr)
library(ggplot2)
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
### code chunk number 3: ImportDensità
###################################################
df.dueAnni <-
    read.table(file.path(DirElab, "df_dueanni.csv"),
               header = T, sep = "")

df.vecchio <- df.dueAnni

levels(df.dueAnni$YEAR) <- c("2015", "2016")
levels(df.dueAnni$TRT) <-c("Convenzionale", "Biologico")
levels(df.dueAnni$LAVORAZIONE) <- c("Arato", "Frangizollato", "Rippato")

p10 <- ggplot(aes(y = densita.apparente, x = 1, fill = LAVORAZIONE), data = df.dueAnni) +
    geom_boxplot(position=position_dodge(1))+
    facet_grid(.~YEAR+TRT)+
    geom_hline(yintercept = 1.35)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+    
    theme(text = element_text(size = 20),
          axis.text.x = element_text(angle=0, hjust=0))+
    theme(legend.position = "bottom",
          legend.background = element_rect(color = "white", 
                                           fill = "white", size = 1, linetype = "solid"),
          legend.direction = "horizontal") + scale_fill_discrete(name= NULL)+ylab(expression(paste("Densità apparente g ", cm^-3)))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

pdf("../tesi/boxCore.pdf")
p10
dev.off()

df.dueAnni <- df.vecchio
df.dueAnni <-  df.dueAnni[df.dueAnni$REPLICA == "m", ]
df.petrolio <-
    read.table(file.path(DirElab, "df_spinta.csv"),
               header = T, sep = "")


df.vecchio <- df.petrolio
levels(df.petrolio$TRT) <-c("Convenzionale", "Biologico")
levels(df.petrolio$LAVORAZIONE) <- c("Arato", "Frangizollato", "Rippato")


p10 <- ggplot(aes(y = densita.apparente, x = 1, fill = LAVORAZIONE), data = df.petrolio) +
    geom_boxplot(position=position_dodge(1))+
    facet_grid(.~TRT)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+    
    theme(text = element_text(size = 20),
          axis.text.x = element_text(angle=0, hjust=0))+
    theme(legend.position = "bottom",
          legend.background = element_rect(color = "white", 
                                           fill = "white", size = 1, linetype = "solid"),
          legend.direction = "horizontal") + scale_fill_discrete(name= NULL)+ylab(expression(paste("Densità apparente g ", cm^-3)))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

pdf("../tesi/boxClod.pdf")
p10
dev.off()

df.vecchio <- df.petrolio

##vec.paletti <- c(250, 20)
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
##require(compositions)

###############################
## due umidit\`a con ultrasuoni
###############################

## getModelMatrix <- function(object,
##                            newdata=NULL,
##                            na.action=na.pass) {
##     if( is.null(newdata) )
##         return(model.matrix(object))
##     Terms <- delete.response(terms(object))
##     mf <- model.frame(Terms,newdata,na.action=na.action,
##                       xlev = object$xlevels)
##     if (!is.null(cl <- attr(Terms, "dataClasses")))
##         .checkMFClasses(cl, mf)
##     model.matrix(Terms, mf, contrasts.arg = object$contrasts)
## }
## lis.paletti <-
##     list(uno=which(diametri > vec.paletti[1])+18,
##          due=which(diametri < vec.paletti[1] & diametri > vec.paletti[2] )+18,
##          tre=which(diametri < vec.paletti[2])+18
##          )
## df.tessitura <-
##     data.frame(df.data[,c(1:13,18,120)],
##                MACRO = apply(df.data[,lis.paletti$uno], 1,
##                              function(x) sum(x, na.rm=TRUE)),
##                MESO = apply(df.data[,lis.paletti$due], 1,
##                             function(x) sum(x, na.rm=TRUE)),
##                MICRO = apply(df.data[,lis.paletti$tre], 1,
##                              function(x) sum(x, na.rm=TRUE))
##                )

## questi <-
##     with(df.tessitura, which(HUMIDITY=="wet"))
## df.plot <- df.tessitura[questi,]
## Y.Msizer1 <-
##     acomp(df.plot[, 16:18])
## lm.2 <-
##     with(df.plot, lm(ilr(Y.Msizer1) ~ COND+TIME2+I(TIME2^2)))



###################################################
### code chunk number 19: plotacompWETDRY
###################################################

vec.paletti.poro <- c(0.05, 0.1) 

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
row.names(df.data) <- 1:dim(df.data)[1]
PESI <- read.table(file.path(DirElab, "pesoaggre.csv"), sep = ";")$x

df.data <- data.frame(
    df.data[,1:6],
    MICRO = (df.data[,7]-df.data[,8])/PESI, 
    MESO = (df.data[,8]-df.data[,9])/PESI, 
    MACRO = df.data[,9]/PESI
                                        #    TOT = df.data[,7]/PESI
)


df.porosimetria <-
    df.data[-c(3,6,11,10),]

df.data$TOT <- rowSums(df.data[,7:9])

###################################################
### PCA
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

df.porimedi3 <- df.fittiziopori

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

df.densitapori <- merge(merge(df.densitaCompleto, df.porimedi3), df.fittiziopori)

df.stabilita <-
    read.table("/home/simone/Dropbox/MOLTE/SoloSuolo/dati_grezzi/Stabil.Terr.csv", sep = ";")



################per i dry
df.stabilita2 <- df.stabilita[df.stabilita$HUMIDITY=="dry",c(1:6, 12)]
df.stabilita3 <- df.stabilita[df.stabilita$HUMIDITY=="wet",c(1:6, 12)]
names(df.stabilita2)[7] <- "Diam.aggr.dry"
df.stabilita2$Diam.aggr.wet <- NA
for(i in 1:nrow(df.stabilita2)){
    for(k in 1:nrow(df.stabilita3)){
        if(with(df.stabilita2[i, ], paste(YEAR, MAN, FIELD, PLOT, TIL, REP))==
           with(df.stabilita3[k,], paste(YEAR, MAN, FIELD, PLOT, TIL, REP))){
            df.stabilita2[i,8] <- df.stabilita3[k,7]
        }else{
            NULL
        }
    }
}

##Tolgo il 2015 va
df.stabilita4 <- df.stabilita2[df.stabilita2$YEAR=="y16",]
df.densitapori <- df.densitapori[df.densitapori$YEAR=="y16",]

names(df.stabilita4)[1:6] <- names(df.densitapori)[1:6]
df.stabilita4$LAVORAZIONE <-
    mapvalues(df.stabilita4$LAVORAZIONE, from = c("Plw", "Chp", "Dsh"), to = c("Ara", "Rip", "Fzo"))
df.stabilita4$TRT <-
    mapvalues(df.stabilita4$TRT, from = c("Co", "Or"), to = c("CO", "OO"))


df.finale <-
    merge(df.densitapori, df.stabilita4)

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
C.inorg <- df.finale$Carbon-df.finale$Carbon_HCl
df.finale$Carbon <- C.inorg 
rm(C.org)
names(df.finale)[c(14,16)] <- c("C.inorganico","C.organico")
df.finale$YEAR <- NULL
write.table(df.finale, file.path(DirElab, "PerPCA.csv"), sep = ";", row.names = T)


###########Piglio le cose di Lorenzo
df.prod <-
    read.table("~/Dropbox/MOLTE/SoloSuolo/dati_grezzi/Produttivita2016.csv", sep = ";", header = TRUE, dec = ",")
df.prod$Crop <-
    with(df.prod, mapvalues(paste(Treatment, Crop), from = c("CO BAR", "CO SUN", "OO SUN", "OO BAR"), to = c("09","10", "02", "04"))) 
names(df.prod) <- c("TRT", "LAVORAZIONE", "PARCELLA", "APPEZZAMENTO", "PRODUZIONE")
df.prod$LAVORAZIONE <- mapvalues(df.prod$LAVORAZIONE, from = c("A", "B", "C"), to = c("Ara", "Rip", "Fzo"))
df.prod$APPEZZAMENTO <- as.numeric(df.prod$APPEZZAMENTO)
df.prod$REPLICA <- "m"
df.prod <- df.prod[,c(1,4,3,2,6,5)]
## df.chem <-
##     read.table("~/Dropbox/MOLTE/SoloSuolo/dati_grezzi/Analisi_chimiche_2016.csv", sep = ";", header = TRUE, dec = ",")
## df.chem$CROP <-
##     with(df.chem, mapvalues(paste(MAN, CROP), from = c("Co Bar", "Co Sun", "Or Sun", "Or Bar"), to = c("09","10", "02", "04"))) 
## df.chem2 <-
##     with(df.chem, aggregate(list(P2O5.mg.kg, S.O.perc, N.TOT.g.kg), by = list(MAN, TIL, CROP), FUN = function(x) mean(x, na.rm = TRUE)))
## names(df.chem2) <- c("TRT", "LAVORAZIONE", "APPEZZAMENTO", "P2O5", "SO.perc", "NTOT")
## df.chem2$TRT <-
##     mapvalues(df.chem2$TRT, from = c("Co", "Or"), to = c("CO", "OO"))
## df.chem2$LAVORAZIONE <-
##     mapvalues(df.chem2$LAVORAZIONE, from = c("Plw", "Chp", "Dsh"), to = c("Ara", "Rip", "Fzo"))
## df.chem2[c(1:3, 10:12), 4:6] <-
##     jitter(as.matrix(df.chem2[4:9, 4:6]), 150)    
##df.Lorenzo <-
##    merge(df.chem2, df.prod2)
##names(df.Lorenzo)[2] <- "LAV"
##non sappiamo qual è il metodo di misura che risponde alla nostra esigenza, questo significa che i domain a livello
##di aggregato non sono estrapolabili a livello di cilindro
df.PCA <-
    merge(df.finale,df.prod)[,c(1:4, 6:16)]
df.PCAFisica <-
    df.finale[, c(1:4, 6:11)]
df.PCAChimica <-
    df.finale[, c(1:4, 12:15)]


write.table(df.PCA, file.path(DirElab, "df.PCATotale.csv"), sep = ";", row.names = T)
write.table(df.PCAFisica, file.path(DirElab, "df.PCAFisica.csv"), sep = ";", row.names = T)
write.table(df.PCAChimica, file.path(DirElab, "df.PCAChimica.csv"), sep = ";", row.names = T)

##names(PoriTotPCA) <- c("TRT", "LAV", "APPEZZAMENTO", "PoriTot")
##PoriTotPCA <- PoriTotPCA[!PoriTotPCA$APPEZZAMENTO%in%c(1,3),]
##df.PCA1 <-
##    aggregate(df.PCA[, c(4:12)], by = list(TRT = df.PCA$TRT, APPEZZAMENTO = df.PCA$APPEZZAMENTO, LAV = df.PCA$LAVORAZIONE), FUN  =  function(x)
##        mean(x, na.rm = TRUE))
##df.Lorenzo$APPEZZAMENTO <- as.numeric(df.Lorenzo$APPEZZAMENTO)
##df.PCA1 <-
##    merge(merge(df.PCA1, df.Lorenzo), PoriTotPCA)
##df.PCA1 <-
##    df.PCA1[,c(1,3,2,16,4,5,7,8,6,17,11:15,9,10)]
## df.PCA1$NTOT <- NULL
## df.PCA1$SO.perc <- NULL
## df.PCA1$Nitrogen <- NULL
## df.PCA1$Carbon <- NULL
##df.PCAChimica <- df.PCA[,c(1:3, 10:14)]
##df.PCAFisica <- df.PCA[,c(1:3, 4:9, 14)]
## df.PCAChimica$P2O5 <- NA
## df.PCAChimica$SO.perc <- NA
## df.PCAChimica$NTOT <- NA
## df.chem2$APPEZZAMENTO <- as.numeric(df.chem2$APPEZZAMENTO)
## for(i in 1:nrow(df.PCAChimica)){
##     for(k in 1:nrow(df.chem2)){
##         if(with(df.PCAChimica[i, ], paste(TRT, LAVORAZIONE, APPEZZAMENTO))==
##            with(df.chem2[k,], paste(TRT, LAVORAZIONE, APPEZZAMENTO))){
##             df.PCAChimica$P2O5[i] <- df.chem2$P2O5[k]
##             df.PCAChimica$SO.perc[i] <- df.chem2$SO.perc[k]
##             df.PCAChimica$NTOT[i] <- df.chem2$NTOT[k]            
##         }else{
##             NULL
##             }
##     }
## }
