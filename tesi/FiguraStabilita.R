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
    c("Convenzionale", rep(" ", 2), "Organico", rep(" ", 2),
      "Convenzionale", rep(" ", 2), "Organico", rep(" ", 2))
summary.campo$Lavorazione <-
    mapvalues(summary.campo$Lavorazione, from = c("Ara", "Rip", "Fzo"), to = c("Arato", "Rippato", "Frangizollato"))
summary.campo <- rbind(summary.campo[1:3,], rep(NA, 7),
                       summary.campo[4:6,], rep(NA, 7), summary.campo[7:9,], rep(NA, 7),
                       summary.campo[10:nrow(summary.campo),])
tabella.campo <-
    xtable(summary.campo[,-7],
           label = 'tab:summaryCore', align  = "llllccc",
           caption =
               "Valori medi della densit\\`a apparente (in \\SI{}{\\gram\\per\\cubic\\centi\\metre}),
suddivisi per \\emph{Anno}, \\emph{Conduzione} e \\emph{Lavorazione} " )
print(tabella.campo, include.rownames=FALSE,
      caption.placement = "top",
      table.placement= "hb")



###################################################
### code chunk number 5: ResiduiCampo
###################################################
limiti.x <- c(0,13)
limiti.y <- c(1, 1.8)
plot(1, xlim = limiti.x, ylim = limiti.y, type = "n",
     yaxt = "n", xaxt = "n", xlab = "", ylab = "", main = "Metodo Core")
mtext(expression(paste("Densit\`a apparente g ", cm^-3)),
      side = 2, line = 2)
rect(0.25,1, 3.45,1.8, col = "lightgray", border = NA)
rect(3.55,1, 6.5,1.8, col = "gray", border = NA)
rect(6.5,1, 9.45,1.8,col = "lightgray", border = NA)
rect(9.55,1, 12.75,1.8,col = "gray", border = NA)
rect(0.25,1, 6.5,1.8,lwd = 2)
rect(6.5,1, 12.75,1.8,lwd = 2)
abline(h = 2.65/2, col = 2)
with(df.dueAnni,
     boxplot(densita.apparente ~ LAVORAZIONE+TRT+YEAR,
             las = 2, xaxt = "n",
             col = c(2,3,4), add = TRUE)
     )
text(3.5, 1.8, "Anno 2015", pos = 1, cex = 2)
text(9.5, 1.8, "Anno 2016", pos = 1, cex = 2)
legend(9.3, 1.2,
       c("Arato","F.zollato","Rippato"),
       box.col = "transparent",
       fill=c(2,3,4), cex=1,  ncol = 1)
## points(y = jitter(rep(1:12, each = 6), 0.5),
##        x = unlist(split(densita.apparente, interaction(LAVORAZIONE, TRT, YEAR))),
##        cex = 2, pch = 4)
text( 2, 1.7, "CO", pos = 1, cex = 1.5)
text( 5, 1.7, "OO", pos = 1, cex = 1.5)
text( 8, 1.7, "CO", pos = 1, cex = 1.5)
text(11, 1.7, "OO", pos = 1, cex = 1.5)


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
anova.dens <- round(anova(lm.Densita), 3)
rownames(anova.dens) <-     c("Anno", "Conduzione", "Lavorazione", "Totale")
##%   c("Anno", "Management", "Tillage", "Totale")
tabella.anova.dens <-
    xtable(anova.dens,
           label = 'tab:anova del modello', align  = "lrrrrr",
           caption = 'Analisi della varianza relativa ai valori di densità rilevati col metodo \\emph{Core}',
           digits = c(0,0,rep(3,4)),
           display = NULL, auto = FALSE)
print(tabella.anova.dens,
      include.rownames=TRUE,
      caption.placement = "top")


###################################################
### code chunk number 8: Summary_modello2 (eval = FALSE)
###################################################
##
## sommario.dens <-
##     round(summary(lm.DensSign)$coefficients, 3)
##
## rownames(sommario.dens) <-
## c("CO 2015", "Scostamento 2016", "Scostamento OO")
## sommario.dens[,4] <- round(sommario.dens[,4],2)
## sommario.dens[1,4] <- "<10^-3"
## tabella.summary.dens <-
## xtable(sommario.dens,
## label = 'tab:t-table del modello2', align  = "lrrrr",
## caption = '')
##
## print(tabella.summary.dens,
## include.rownames=TRUE,
## caption.placement = "top")


###################################################
### code chunk number 9: boxplot2 (eval = FALSE)
###################################################
## fattore <- interaction(df.dueAnni$YEAR, df.dueAnni$TRT)
## amod <- aov(densita.apparente ~ fattore, data=df.dueAnni)
## HSD.test(amod, "fattore", group=TRUE, alpha = 0.05)
## tuk <- glht(amod, linfct = mcp(fattore = "Tukey"))
## tuk.cld<- cld(tuk)   #letter-based display
## posiz.y <- 1.7
## ## tapply(df.dueAnni$BD.g.cmc,
## ##             fattore,
## ##           function(x) mean(x, na.rm = TRUE)
## ##         )
## limiti.x <- c(0,5)
## limiti.y <- c(1, 1.8)
## plot(1, xlim = limiti.x, ylim = limiti.y, type = "n",
##      yaxt = "n", xaxt = "n", xlab = "",ylab = "", main = "Metodo Core")
## ##mtext(expression(paste("Densit\`a apparente g ", cm^-3)), side = 2, line = 2)
## rect(0.25,1, 2.45,1.8, col = "lightgray", border = NA)
## rect(2.48,1, 4.75,1.8, col = "gray", border = NA)
## rect(0.25,1, 2.45,1.8,lwd = 2)
## rect(2.48,1, 4.75,1.8,lwd = 2)
## with(df.dueAnni,
##      boxplot(densita.apparente ~ YEAR + TRT, ### ATTENZIONE A NON INVERTIRE TRT con YEAR !!
##              las = 2, xaxt = "n",
##              col = c(5, 6), add = TRUE))
## mtext(expression(paste("Densit\`a apparente g ", cm^-3)), side = 2, line = 2)
## text(1.5, 1.8, "Anno 2015", pos = 1, cex = 1.5)
## text(3.5, 1.8, "Anno 2016", pos = 1, cex = 1.5)
## text(x = 1:4, y = posiz.y, tuk.cld$mcletters$Letters, cex = 1.5, col = "red")
## legend(3, 1.2,
##        c("Convenzionale", "Biologico"),
##        box.col = "transparent",
##        fill=c(5,6), cex=0.8,  ncol = 1)
## ##points(y = jitter(rep(1:12, each = 6), 0.5),
## ##x = unlist(split(densita.apparente, interaction(LAVORAZIONE, TRT, YEAR))),
## ##cex = 2, pch = 4)


###################################################
### code chunk number 10: qqplotCampo
###################################################
par(mfrow = c(2,2))
plot(lm.Densita)


###################################################
### code chunk number 11: importa_petrolio
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
summary.petrolio$Conduzione <- c("Convenzionale", rep("", 2), "Organico", rep("", 2))
summary.petrolio$Lavorazione <- mapvalues(summary.petrolio$Lavorazione, from = c("Ara", "Rip", "Fzo"),
                                          to = c("Arato", "Rippato", "Frangizollato"))
summary.petrolio <- rbind(summary.petrolio[1:3,], rep(NA, 6),
                          summary.petrolio[3:6,])


###################################################
### code chunk number 12: Analisi_Petrolio
###################################################
lm.spinta <-
lm(densita.apparente ~ TRT + LAVORAZIONE, data = df.petrolio)

anova_spinta <-
anova(lm.spinta)

summary_spinta <-
summary(lm.spinta)


###################################################
### code chunk number 13: Analisi_Petrolio
###################################################
rownames(anova_spinta) <- c("Conduzione", "Lavorazione", "Totale")
tabella.spinta.anova <-
xtable(anova_spinta,
label = 'tab:anova piccoli aggregati', align = 'lrrrrr',
caption = ' Analisi della varianza relativa ai valori di densit\\`a misurati con il metodo \\emph{Clod}',
digits = c(0,0,rep(3,4))
)
print(tabella.spinta.anova, caption.placement = "top")


###################################################
### code chunk number 14: boxplotpetr
###################################################

with(data = df.petrolio,
{
  plot(1, xlim = c(0.5, 6.5), ylim = c(1.4, 2.5), type = "n",
  yaxt = "n", xaxt = "n", xlab = "",ylab = "")
  rect(0.5, 1.4, 3.45,2.5, col = "lightgray", border = NA)
  rect(3.48,1.4, 6.5,2.5, col = "gray", border = NA)
  boxplot(jitter(densita.apparente) ~ LAVORAZIONE+TRT, col = c(2,3,4),
  main = "Metodo Clod",
  ##xlab = "Densità apparente",
  las = 1, xaxt = 'n',
  ylim = c(1.5, 2.3),
  yaxs = "i",
  add=TRUE
  )
  ##axis(1, labels = FALSE)
  ##etichette <- levels(interaction(LAVORAZIONE,TRT))
  ## Plot x labs at default x position
  ##text(x =  seq_along(etichette), y = 1.3,
  ##srt = 45, adj = 1,
  ##labels = etichette, xpd = TRUE)
  mtext(expression(paste("Densità apparente g ", cm^-3)), side = 2, line = 2)
  text(2, 2.3, "CO", pos = 3, cex = 2)
  text(5, 2.3, "OO", pos = 3, cex = 2)
  ## points(y = jitter(rep(1:6, each = 22), 0.5),
  ##   x = unlist(split(densita.apparente, interaction(LAVORAZIONE, TRT))),
  ##   cex = 2, pch = 4)

  legend(4.5, 1.7,
  c("Arato","F.zollato","Rippato"),
  box.col = "transparent",
  fill=c(2,3,4), cex=1,  ncol = 1)
}
)



###################################################
### code chunk number 15: Sommario_petrolio
###################################################
tabella.petrolio <-
xtable(summary.petrolio,
label = 'tab:summaryClod', align  = "lllcccc",
caption =
"Valori medi della densit\\`a apparente (in
\\SI{}{\\gram\\per\\cubic\\centi\\metre}),
relativi al tipo di conduzione e lavorazione, metodo \\emph{Clod} ",
digits =  c(rep(0,3),2,2,0,0))
print(tabella.petrolio, include.rownames=FALSE, caption.placement = "top")


###################################################
### code chunk number 16: Summary_Petrolio (eval = FALSE)
###################################################
## summary_spinta <- coef(summary_spinta)
## rownames(summary_spinta) <-
## c("CO Arato","Scostamento OO",
## "Scostamento Frangizollato","Scostamento Rippato")
##
## tabella.summary.spinta <- xtable(summary_spinta,
## label = 'tab:sommario piccoli aggregati', align = 'lrrrr',
## caption = 'sommario del modello adattato per i valori misurati con il metodo \\emph{Clod}',
##
## )
## #print(tabella.summary.spinta, caption.placement = "top")
##
## #vec.paletti <- c(180, 50)
## #https://books.google.it/books?hl=it&lr=&id=gDnLAwAAQBAJ&oi=fnd&pg=PA15&dq=the+soil+habitat+voroney+heck&ots=rfoIOSpZhk&sig=4KK3HFUIYP2oAsd8FZIhEpv_iNg#v=onepage&q=the%20soil%20habitat%20voroney%20heck&f=false


###################################################
### code chunk number 17: qqplotPetrolio
###################################################
par(mfrow = c(2,2))
plot(lm.spinta)


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

fun.triangolo.stabilita <-
    function(dimen=0.5,
             df=Y.Msizer1,
             tinta=df.plot$COND,tipo=1 ){
        plot(df, pch=tipo,
             cex=dimen,
             col=tinta,
             axes = TRUE, plotMissings = FALSE,
             add = FALSE)
        legend("topleft",
               c( "CO.Dry", "OO.Dry","CO.Wet", "OO.Wet"),
               fill=1:4, bty = "n")
        text(0.1,0.025, "> 250 um")
        text(0.85,0.025, "tra 20 e 250 um")
        text(0.5,0.72, "< 20 um")
    }

pdf("stabilita.pdf")
par(mfrow = c(1,1))
## solo secchi e no ultrasuoni
fun.triangolo.stabilita(dimen=2,tipo =20, tinta= c(6,rep("transparent",839)))
colori <-
    ifelse(df.plot$Ultrasonic.level,
           "transparent", as.numeric(df.plot$COND)+2)
fun.triangolo.stabilita(dimen=0.5,
                        df=Y.Msizer1, tinta=colori)
fun.triangolo.stabilita(df=Y.Msizer1,
                        tinta= as.numeric(df.plot$COND)+2)
# punto accensione ultrasuoni
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
Fase <- rep(c("inizio misura","inizio sonicatura", "fine misura"),2)
colnames(dati.iufd) <- c("Macro (%)", "Meso (%)", "Micro (%)")
df.iufd <- cbind(Conduzione, Fase, dati.iufd)
df.iufd <- rbind(df.iufd[1:3,], "", df.iufd[4:6,])
## equazioni WET
fun.triangolo.stabilita(df=Y.Msizer1,
                        tinta= as.numeric(df.plot$COND)+2)
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*0:23 +
     acomp(coefs[4,])*(0:23)^2,
     col=3,  lwd=3, type="l", add=TRUE)## sequenza temporale CO wet
## bollini bianchi per lettere
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col="white",
     add=TRUE, pch=16, cex = 3)## start stop CO
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=3, add=TRUE, pch=c("c", "W","C"))## start stop CO
##
fun.triangolo.stabilita(df=Y.Msizer1,
                        tinta= as.numeric(df.plot$COND)+2)
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*0:23 +
     acomp(coefs[4,])*(0:23)^2,
     col=3,  lwd=3,type="l", add=TRUE)## sequenza temporale CO wet
## bollini bianchi per lettere
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col="white",
     add=TRUE, pch=16, cex = 3)## start stop CO
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=3, add=TRUE, pch=c("c", "W","C"))## start stop CO
plot(acomp(coefs[1,]) + acomp(coefs[2,])+
     acomp(coefs[3,])*0:23+
     acomp(coefs[4,])*(0:23)^2,
     col=4, lwd=3, add=TRUE, type="l")## sequenza temporale OO wet
## bollini bianchi per lettere
plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col="white", add=TRUE, pch= 16, cex = 3)## start stop OO
##
plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=4, add=TRUE, pch= c("o","W","O"))## start stop OO
## si riparte coi campioni dry
questi <-
    with(df.tessitura, which(HUMIDITY=="dry"))
df.plotDRY <- df.tessitura[questi,]
Y.Msizer <- acomp(df.plotDRY[, 16:18])
lm.2.2 <-   with(df.plotDRY,lm(ilr(Y.Msizer) ~ COND+TIME2+I(TIME2^2)))
modelloMsizer <- lm.2.2
coefsDRY <-  ilrInv(coef(modelloMsizer), orig=Y.Msizer)
##############################################################
CO <- round(matrix(coefsDRY[1,] +
                   acomp(coefsDRY[3,])*on.ultra.off +
                   acomp(coefsDRY[4,])*on.ultra.off^2,
                   ncol = 3, byrow = FALSE), 3)
OO <- round(matrix(acomp(coefsDRY[1,]) +
                   acomp(coefsDRY[2,]) +
                   acomp(coefsDRY[3,])*on.ultra.off +
                   acomp(coefsDRY[4,])*on.ultra.off^2,
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
##
colori <-
    ifelse(df.plotDRY$Ultrasonic.level,
           "transparent", df.plotDRY$COND)
fun.triangolo.stabilita(df=Y.Msizer, tinta=colori)
fun.triangolo.stabilita(df=Y.Msizer,
                        tinta=df.plotDRY$COND)
##
fun.triangolo.stabilita(df=Y.Msizer,
                        tinta= tinta=df.plotDRY$COND)
plot(acomp(coefsDRY[1,]) +
     acomp(coefsDRY[3,])*0:23 +
     acomp(coefsDRY[4,])*(0:23)^2,
     col=1, lwd= 3, type="l", add=TRUE)## sequenza temporale CO dry
plot(acomp(coefsDRY[1,]) +
     acomp(coefsDRY[3,])*on.ultra.off +
     acomp(coefsDRY[4,])*on.ultra.off^2,
     col="white", add=TRUE, pch=16, cex = 2)## start stop CO dry
plot(acomp(coefsDRY[1,]) +
     acomp(coefsDRY[3,])*on.ultra.off +
     acomp(coefsDRY[4,])*on.ultra.off^2,
     col=1, add=TRUE, pch=c("c", "W","C"))## start stop COdry
##
fun.triangolo.stabilita(df=Y.Msizer,
                        tinta=df.plotDRY$COND)
plot(acomp(coefsDRY[1,]) +
     acomp(coefsDRY[3,])*0:23 +
     acomp(coefsDRY[4,])*(0:23)^2,
     col=1, lwd= 3, type="l", add=TRUE)## sequenza temporale CO dry
plot(acomp(coefsDRY[1,]) +
     acomp(coefsDRY[3,])*on.ultra.off +
     acomp(coefsDRY[4,])*on.ultra.off^2,
     col="white", add=TRUE, pch=16, cex = 2)## start stop CO dry
plot(acomp(coefsDRY[1,]) +
     acomp(coefsDRY[3,])*on.ultra.off +
     acomp(coefsDRY[4,])*on.ultra.off^2,
     col=1, add=TRUE, pch=c("c", "W","C"))## start stop COdry
plot(acomp(coefsDRY[1,]) + acomp(coefsDRY[2,]) +
     acomp(coefsDRY[3,])*0:23+
     acomp(coefsDRY[4,])*(0:23)^2,
     col=2,  lwd= 3, add=TRUE, type="l")## sequenza temporale OO dry
plot(acomp(coefsDRY[1,]) + acomp(coefsDRY[2,]) +
     acomp(coefsDRY[3,])*on.ultra.off +
     acomp(coefsDRY[4,])*on.ultra.off^2,
     col="white", add=TRUE, pch= 16, cex = 2)## start stop OO dry
plot(acomp(coefsDRY[1,]) + acomp(coefsDRY[2,]) +
     acomp(coefsDRY[3,])*on.ultra.off +
     acomp(coefsDRY[4,])*on.ultra.off^2,
     col=2, add=TRUE, pch= c("o","W","O"))## start stop OO dry
### DIAPO FINALE
fun.triangolo.stabilita(df=Y.Msizer1, tinta="transparent")
plot(acomp(coefsDRY[1,]) +
     acomp(coefsDRY[3,])*0:23 +
     acomp(coefsDRY[4,])*(0:23)^2,
     col=1, lwd= 3, type="l", add=TRUE)## sequenza temporale CO dry
## plot(acomp(coefsDRY[1,]) +
##      acomp(coefsDRY[3,])*on.ultra.off +
##      acomp(coefsDRY[4,])*on.ultra.off^2,
##      col="white", add=TRUE, pch=16, cex = 2)## start stop CO dry
plot(acomp(coefsDRY[1,]) +
     acomp(coefsDRY[3,])*on.ultra.off +
     acomp(coefsDRY[4,])*on.ultra.off^2,
     col=1, add=TRUE, pch=c("c", "W","C"))## start stop COdry
plot(acomp(coefsDRY[1,]) + acomp(coefsDRY[2,]) +
     acomp(coefsDRY[3,])*0:23+
     acomp(coefsDRY[4,])*(0:23)^2,
     col=2,  lwd= 3, add=TRUE, type="l")## sequenza temporale OO dry
## plot(acomp(coefsDRY[1,]) + acomp(coefsDRY[2,]) +
##      acomp(coefsDRY[3,])*on.ultra.off +
##      acomp(coefsDRY[4,])*on.ultra.off^2,
##      col="white", add=TRUE, pch= 16, cex = 2)## start stop OO dry
plot(acomp(coefsDRY[1,]) + acomp(coefsDRY[2,]) +
     acomp(coefsDRY[3,])*on.ultra.off +
     acomp(coefsDRY[4,])*on.ultra.off^2,
     col=2, add=TRUE, pch= c("o","W","O"))## start stop OO dry
## ROBA WET
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*0:23 +
     acomp(coefs[4,])*(0:23)^2,
     col=3,  lwd=3,type="l", add=TRUE)## sequenza temporale CO wet
## bollini bianchi per lettere
## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col="white",
##      add=TRUE, pch=16, cex = 3)## start stop CO
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=3, add=TRUE, pch=c("c", "W","C"))## start stop CO
plot(acomp(coefs[1,]) + acomp(coefs[2,])+
     acomp(coefs[3,])*0:23+
     acomp(coefs[4,])*(0:23)^2,
     col=4, lwd=3, add=TRUE, type="l")## sequenza temporale OO wet
## ## bollini bianchi per lettere
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col="white", add=TRUE, pch= 16, cex = 3)## start stop OO
##
plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=4, add=TRUE, pch= c("o","W","O"))## start stop OO
## inserisce le tessiture apparenti ricalcolate
plot(acomp(tessitura8rino2[,4:6]), cex=1.5,
     pch = 18,
     col=as.numeric(rev(tessitura8rino2$CONDUZIONE)),
     add = TRUE)
dev.off()

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
##      col=1, add=TRUE, pch=c("c", "D","C"))## start stop CO

## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=2, add=TRUE, pch= c("o","D","O"))## start stop OO

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
##      col="white", add=TRUE, pch=16, cex = 2)## start stop CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col="white", add=TRUE, pch= 16, cex = 2)## start stop OO
## plot(acomp(coefs[1,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=3, add=TRUE, pch=c("c", "W","C"))## start stop CO
## plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
##      acomp(coefs[3,])*on.ultra.off +
##      acomp(coefs[4,])*on.ultra.off^2,
##      col=4, add=TRUE, pch= c("o","W","O"))## start stop OO
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
##                                         #PoroElabora <- FALSE
##                                         # ## Si importano i soli file "CSV" MAIUSCOLI, che derivano dalla manipolazione dei dati grezzi della porosimetria
## vec.paletti.poro <- c(0.5, 50)
## ## if(PoroElabora){
## ##    source(file.path(DirCod, "ElaboraPorosimetriaConPaletti.R"))
## ##     }else{
## ##         NULL}

## Export.Dir <-
##     file.path(DirMain, "dati_elaborati/porosimetria")
## nomi.file.elab <-
##     list.files(Export.Dir, pattern = ".CSV")

## df.data <- data.frame()

## for(i in 1:length(nomi.file.elab)){
##     file.in.elaborazione <-
##         nomi.file.elab[i]
##     path.completo <-
##         file.path(Export.Dir,file.in.elaborazione)
##     df.this <-
##         read.table(file=path.completo,
##                    sep=";", dec=",", header=TRUE,
##                    fileEncoding =  "UTF-16LE")[-4,]
##     df.this$MAN <- substr(file.in.elaborazione, 3,3)
##     df.this$FIELD <-  substr(file.in.elaborazione, 4,5 )
##     df.this$TIL <-  substr(file.in.elaborazione, 6,6)
##     df.this$ROW <-  substr(file.in.elaborazione, 7,7)
##     df.this$REP <-  substr(file.in.elaborazione, 8,8)
##     df.this$REPPORO <-  substr(file.in.elaborazione, 10,10)
##     df.data <- rbind.data.frame(df.data, df.this)
## }

## df.data <-
##     df.data[c(2,3,13:18)]

## df.data <-
##     cbind.data.frame(df.data[c(T,F,F),-(1:2)],
##                      matrix(df.data[,1], ncol=3, byrow=TRUE)
##                      )

## names(df.data)[7:9] <-
##     c("Residuals", "Storage", "Transmission")
## require(compositions)
## row.names(df.data) <- 1:dim(df.data)[1]
## PESI <- read.table(file.path(DirElab, "pesoaggre.csv"), sep = ";")$x

## df.data <- data.frame(
##     df.data[,1:6],
##     RESIDUALS = (df.data[,7]-df.data[,8])/PESI,
##     STORAGE = (df.data[,8]-df.data[,9])/PESI,
##     TRANSMISSION = df.data[,9]/PESI
##                                         #    TOT = df.data[,7]/PESI
## )

## ## lm.micropori <-
## ##     with(df.data, lm(MICRO ~ MAN)) #+ TIL))
## ## anova(lm.micropori)
## ## summary(lm.micropori)
## ## lm.macropori <-
## ##     with(df.data, lm(MACRO ~ MAN)) #+ TIL))
## ## anova(lm.macropori)
## ## summary(lm.macropori)
## ## lm.totale <-
## ##     with(df.data, lm(TOT ~ MAN))#+TIL))
## ## anova(lm.totale)
## ## summary(lm.totale)


## ## par(mfrow = c(2,2))
## ## qqnorm(resid(lm.macropori))

## ## lm.ratio <-
## ##     lm(MACRO/MICRO ~ MAN*TIL, data = df.data)
## ## anova(lm.ratio)


## df.porosimetria <-
##     df.data[-c(3,6,11,10),]

## Y <- acomp(df.porosimetria[, 7:9])
## modelloPoro <-
##     with(df.porosimetria, lm(ilr(Y) ~ MAN+TIL))
## coefs.Poro <-  ilrInv(coef(modelloPoro), orig=Y)
## alpha <- 0.05
## ##df.data$TOT <- rowSums(df.data[,7:8])
## ##df.data$P.INDEX <- df.data$TOT/df.data$PESI
## ## lm.totale <-
## ##     lm(df.data$P.INDEX ~ df.data$MAN+df.data$TIL)
## ## lm.totale2 <-
## ##     lm(df.data$P.INDEX ~ df.data$MAN*df.data$TIL)
## ## anova(lm.totale, lm.totale2)
## ##anova.PorTot <-
## ##    anova(lm.totale)


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
## ##jpeg(filename = "plotAcompPore", width = 550, height = 450)

## ##commento perché sennò mi da NULL DEVICE

## plot(acomp(df.porosimetria[, 7:9]), cex = 2, col = as.numeric(as.factor(df.porosimetria$MAN)),
##      pch = as.numeric(as.factor(df.porosimetria$TIL)))
## ##plot(Y)#, cex=2, col=as.numeric(df.porosimetria$MAN), axes = TRUE)
## ## intercetta <-
## ## ilrInv(coef(modelloPoro)[1,], orig=Y)
## ## bi <-
## ## ilrInv(rbind(0, coef(modelloPoro)[-1,]), orig=Y)
## ## medie <- intercetta+acomp(bi)
## ## plot(medie, col= 3:6, pch = 20, add = TRUE)

## legend("topleft", c("CO Ara", "CO Rip", "CO Fzo", "OO Ara", "OO Rip", "OO Fzo"), cex = 0.7, pt.cex = 1,
##        col = rep(1:2, each = 3), pch = 1:3, border = "white", fill=NULL)
## ######Faccio ggplot
## ##require(ggplot2)
## ##require(ggtern)

## ## prova <- acomp(df.porosimetria[, 7:9])
## ## prova <- data.frame(prova)

## ## plot <- ggtern() +
## ##          theme_bw() +
## ##          theme_hidetitles() +
## ##          geom_point(data = prova,
## ##              aes(x = RESIDUALS, y = STORAGE, z = TRANSMISSION), alpha = 0.8, size = 1)
## ##dev.off()


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
