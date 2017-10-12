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
## library(gnumeric) # non piu' sviluppato per windows
library(multcomp)

###################################################
### code chunk number 2: setupDir
###################################################

DirMain <-"/home/ottorino/Documenti/BitBucket/GIT_MOLTE/SIMONE"
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

df.wide <-
    read.table(file.path(DirData, "simone_stabilita1005_corrETTO8rino.csv"),
               sep = ";", dec = ".", header=TRUE
               ##,colClasses = c("factor", "numeric")
               )[, c(6:106, 1:4, 107:110)]
df.wide$Sample.Name <- df.wide$Sample.Name[drop=TRUE]
df.wide$Ultrasonic.level <-
    ifelse(df.wide$Ultrasonic.level == 0, FALSE, TRUE)

attach(df.wide)
butta <-as.character(Sample.Name)
df.wide$Sample.Name <-
    factor(
        unlist(
            strsplit(butta, "[ ]")
        )[c(TRUE, FALSE)]
    )
df.wide$HUMIDITY <-
    factor(
        unlist(
            strsplit(butta, "[ ]")
        )[!c(TRUE, FALSE)]
    )
rm(butta)
detach(df.wide)

df.wide$NOMI.RIGA <- row.names(df.wide)

df.numerazione <-
    read.table(file.path(DirData, "numerazione_campioni.csv"),
               sep = ";", dec = ".",
               header=TRUE, colClasses = "factor")
df.numerazione$COND <-
    factor(
        substr(
            as.character(df.numerazione$APPEZZAMENTO),
            1,2),
        levels=c("CO", "OO"))
df.numerazione$PARCELLA <-
    mapvalues(df.numerazione$PARCELLA, LETTERS[1:3], c("Ara","Rip", "Fzo"))

df.wide <-
    merge(df.wide, df.numerazione,
          by.x = "Sample.Name",
          by.y = "Sample.Name")
names(df.wide)[113] <- "LAV"
df.wide$SUBJECT <-
    with(df.wide,
         interaction(COND, LAV, RIGA.CAMPO, Ultrasonic.level, HUMIDITY )
         )
indici.di.salto <-
    c(1,
      1 +
      which(diff(as.integer(df.wide$SUBJECT))!=0)
      )
indici.di.salto <-
    c(indici.di.salto,
      dim(df.wide)[1])


df.wide$TIME <- NA
for (i in 2:length(indici.di.salto)){
    df.wide$TIME[indici.di.salto[i-1]:indici.di.salto[i]] <-
        1:(indici.di.salto[i]-indici.di.salto[i-1])
}
df.wide$TIME[ dim(df.wide)[1]] <- 12

df.long <-
    reshape(df.wide,
            varying = names(df.wide)[2:102],
            timevar = "DIAM",
            idvar = "NOMI.RIGA",
            direction="long",
            v.names = "PERCENTUALE",
            sep="X")

diametri <-
    as.numeric(
        substr(names(df.wide)[2:102], 2,
               nchar(names(df.wide)[2:102]))
    )
df.long$DIAM <- ## comando  che sostituisce i valori progressivi con i veri diametri
    mapvalues(df.long$DIAM, unique(df.long$DIAM), diametri)
df.long <-
    df.long[, c(11:16, 1, 6:10, 2:5, 17:19)]
df.long <-
    df.long[order( as.numeric(df.long$NOMI.RIGA), df.long$TIME, df.long$DIAM),]

write.table(df.long, file.path(DirElab, "Stabilita_long.csv"),
            sep = ";", dec = ".", col.names = NA)
df.wide <- df.wide[,c(112:117, 1, 107:111, 103:106, 118, 2:102 )]
write.table(df.wide, file.path(DirElab, "Stabilita_wide.csv"),
            sep = ";", dec = ".", col.names = NA)


## xyplot(PERCENTUALE~DIAM|SUBJECT,
##        groups=TIME, pch=". ",
##        data=df.long
##        )
