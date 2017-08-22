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
#with(df.dueAnni, table(YEAR, TRT,LAVORAZIONE, APPEZZAMENTO))
## df.dueAnni <- 
## read.table(file.path(DirElab, "dens_app.csv"), 
## header = T, sep = ";")

###################################################
###DescrittivaDensitaCampo1
###################################################
#with(df.dueAnni, table(TRT, LAVORAZIONE, APPEZZAMENTO, YEAR))
options(contrasts=c("contr.treatment","contr.poly"))
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
function(x){sum(!is.na(x))})[,4]
)
##names(summary.campo)[4] <- "Dens.app.g.cmc"
attach(summary.campo)
summary.campo <- summary.campo[order(YEAR, TRT, LAVORAZIONE),]
detach(summary.campo)


###################################################
### code chunk number 4: Summary_Campo
###################################################
names(summary.campo) <-  
    c("Anno", "Management", "Tillage", "Media", "Dev. std", "n")
summary.campo$Anno <- 
    c("2015", rep(" ", 5), "2016", rep(" ", 5))
summary.campo$Management <- 
    c("Co", rep(" ", 2), "Or", rep(" ", 2), 
      "Co", rep(" ", 2), "Or", rep(" ", 2))
tabella.campo <-
    xtable(summary.campo,
           label = 'tab:riassunto_1', align  = "llllccc",
           caption = 
               "Valori medi della densit\\`a apparente (in \\SI{}{\\gram\\per\\cubic\\centi\\metre}), condizionati per anno, conduzione e lavorazione " )
print(tabella.campo, include.rownames=FALSE,
      caption.placement = "top", table.placement= "hb")



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
text( 2, 1.7, "Co", pos = 1, cex = 1.5)
text( 5, 1.7, "Or", pos = 1, cex = 1.5)
text( 8, 1.7, "Co", pos = 1, cex = 1.5)
text(11, 1.7, "Or", pos = 1, cex = 1.5)



###################################################
### code chunk number 6: figboh2
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
text( 2, 1.7, "Co", pos = 1, cex = 1.5)
text( 5, 1.7, "Or", pos = 1, cex = 1.5)
text( 8, 1.7, "Co", pos = 1, cex = 1.5)
text(11, 1.7, "Or", pos = 1, cex = 1.5)



###################################################
### code chunk number 7: ModelloDensitaCampo
###################################################
lm.Densita <-
    lm(densita.apparente ~ YEAR+TRT+LAVORAZIONE, data = df.dueAnni)
lm.DensSign <-
    lm(densita.apparente ~ YEAR+TRT, data = df.dueAnni)


###################################################
### code chunk number 8: anova_modello
###################################################
anova.dens <- anova(lm.Densita)
rownames(anova.dens) <-     c("Anno", "Conduzione", "Lavorazione", "residui")
##%   c("Anno", "Management", "Tillage", "residui")
tabella.anova.dens <- 
    xtable(anova.dens,
           label = 'tab:anova del modello', align  = "rrrrrr",
           caption = 'Tabella ANOVA per i valori di densità rilevati col metodo \\emph{Core}')
print(tabella.anova.dens,
      include.rownames=TRUE,
      caption.placement = "top")


###################################################
### code chunk number 9: Summary_modello2
###################################################

sommario.dens <- summary(lm.DensSign)$coefficients

rownames(sommario.dens) <-
    c("Co 2015", "Scostamento 2016", "Scostamento Or")

tabella.summary.dens <- 
    xtable(sommario.dens,
           label = 'tab:t-table del modello2', align  = "rrrrr",
           caption = '')

print(tabella.summary.dens,
      include.rownames=TRUE,
      caption.placement = "top")


###################################################
### code chunk number 10: boxplot2
###################################################
fattore <- interaction(df.dueAnni$YEAR, df.dueAnni$TRT)
amod <- aov(densita.apparente ~ fattore, data=df.dueAnni)
HSD.test(amod, "fattore", group=TRUE, alpha = 0.05)
tuk <- glht(amod, linfct = mcp(fattore = "Tukey"))
tuk.cld<- cld(tuk)   #letter-based display
posiz.y <- 1.7
## tapply(df.dueAnni$BD.g.cmc,
##             fattore, 
##           function(x) mean(x, na.rm = TRUE)
##         )
limiti.x <- c(0,5)
limiti.y <- c(1, 1.8)
plot(1, xlim = limiti.x, ylim = limiti.y, type = "n",
     yaxt = "n", xaxt = "n", xlab = "",ylab = "", main = "Metodo Core")
##mtext(expression(paste("Densit\`a apparente g ", cm^-3)), side = 2, line = 2)   
rect(0.25,1, 2.45,1.8, col = "lightgray", border = NA)
rect(2.48,1, 4.75,1.8, col = "gray", border = NA)
rect(0.25,1, 2.45,1.8,lwd = 2)
rect(2.48,1, 4.75,1.8,lwd = 2)
with(df.dueAnni,
     boxplot(densita.apparente ~ YEAR + TRT, ### ATTENZIONE A NON INVERTIRE TRT con YEAR !!
             las = 2, xaxt = "n",
             col = c(5, 6), add = TRUE))
mtext(expression(paste("Densit\`a apparente g ", cm^-3)), side = 2, line = 2)
text(1.5, 1.8, "Anno 2015", pos = 1, cex = 1.5)
text(3.5, 1.8, "Anno 2016", pos = 1, cex = 1.5)
text(x = 1:4, y = posiz.y, tuk.cld$mcletters$Letters, cex = 1.5, col = "red")
legend(3, 1.2, 
       c("Convenzionale", "Biologico"),
       box.col = "transparent", 
       fill=c(5,6), cex=0.8,  ncol = 1)
##points(y = jitter(rep(1:12, each = 6), 0.5),
##x = unlist(split(densita.apparente, interaction(LAVORAZIONE, TRT, YEAR))),
##cex = 2, pch = 4)


###################################################
### code chunk number 11: figboh
###################################################
fattore <- interaction(df.dueAnni$YEAR, df.dueAnni$TRT)
amod <- aov(densita.apparente ~ fattore, data=df.dueAnni)
HSD.test(amod, "fattore", group=TRUE, alpha = 0.05)
tuk <- glht(amod, linfct = mcp(fattore = "Tukey"))
tuk.cld<- cld(tuk)   #letter-based display
posiz.y <- 1.7
## tapply(df.dueAnni$BD.g.cmc,
##             fattore, 
##           function(x) mean(x, na.rm = TRUE)
##         )
limiti.x <- c(0,5)
limiti.y <- c(1, 1.8)
plot(1, xlim = limiti.x, ylim = limiti.y, type = "n",
     yaxt = "n", xaxt = "n", xlab = "",ylab = "", main = "Metodo Core")
##mtext(expression(paste("Densit\`a apparente g ", cm^-3)), side = 2, line = 2)   
rect(0.25,1, 2.45,1.8, col = "lightgray", border = NA)
rect(2.48,1, 4.75,1.8, col = "gray", border = NA)
rect(0.25,1, 2.45,1.8,lwd = 2)
rect(2.48,1, 4.75,1.8,lwd = 2)
with(df.dueAnni,
     boxplot(densita.apparente ~ YEAR + TRT, ### ATTENZIONE A NON INVERTIRE TRT con YEAR !!
             las = 2, xaxt = "n",
             col = c(5, 6), add = TRUE))
mtext(expression(paste("Densit\`a apparente g ", cm^-3)), side = 2, line = 2)
text(1.5, 1.8, "Anno 2015", pos = 1, cex = 1.5)
text(3.5, 1.8, "Anno 2016", pos = 1, cex = 1.5)
text(x = 1:4, y = posiz.y, tuk.cld$mcletters$Letters, cex = 1.5, col = "red")
legend(3, 1.2, 
       c("Convenzionale", "Biologico"),
       box.col = "transparent", 
       fill=c(5,6), cex=0.8,  ncol = 1)
##points(y = jitter(rep(1:12, each = 6), 0.5),
##x = unlist(split(densita.apparente, interaction(LAVORAZIONE, TRT, YEAR))),
##cex = 2, pch = 4)


###################################################
### code chunk number 12: Sommario_petrolio
###################################################
df.petrolio <- 
    read.table(file.path(DirElab, "df_spinta.csv"), 
               header = T, sep = "")
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
                     function(x){sum(!is.na(x))})[,3]
    )
##names(summary.campo)[4] <- "Dens.app.g.cmc"
attach(summary.petrolio)
summary.petrolio <- summary.petrolio[order(TRT, LAVORAZIONE),]
detach(summary.petrolio)
names(summary.petrolio) <- c("Management","Tillage","Media","Dev. std", "n")
summary.petrolio$Management <- c("Co", rep("", 2), "Or", rep("", 2))

tabella.petrolio <-
    xtable(summary.petrolio,
           label = 'tab:summary petrolio', align  = "lllccc",
           caption = 
               "Valori medi della densit\\`a apparente (in 
                \\SI{}{\\gram\\per\\cubic\\centi\\metre}), 
                condizionati per conduzione e lavorazione " )
print(tabella.petrolio, include.rownames=FALSE, caption.placement = "top")



###################################################
### code chunk number 13: boxplotpetr
###################################################

with(data = df.petrolio,
{
    fattore <- interaction(TRT, LAVORAZIONE)
    amod <- aov(densita.apparente ~ fattore)
    HSD.test(amod, "fattore", group=TRUE, alpha = 0.05)
    tuk <- glht(amod, linfct = mcp(fattore = "Tukey"))
    tuk.cld <- cld(tuk) #letter-based display
    plot(1, xlim = c(0.5, 6.5), ylim = c(1.4, 2.5), type = "n", 
         yaxt = "n", xaxt = "n", xlab = "",ylab = "")
    rect(0.5, 1.4, 3.45,2.5, col = "lightgray", border = NA)
    rect(3.48,1.4, 6.5,2.5, col = "gray", border = NA)
    boxplot(jitter(densita.apparente) ~ LAVORAZIONE+TRT, col = c(2,3,4),
            main = "Metodo Clod",
            ##xlab = "Densità apparente",
            las = 1, xaxt = "n",
            ylim = c(1.5, 2.3),
            yaxs = "i",
            add=TRUE
            )
    axis(1, labels = FALSE)
    etichette <- levels(interaction(LAVORAZIONE,TRT))
## Plot x labs at default x position
    text(x =  seq_along(etichette), y = 1.3,
         srt = 45, adj = 1,
         labels = etichette, xpd = TRUE)
    mtext(expression(paste("Densità apparente g ", cm^-3)), side = 2, line = 2)
    text(2, 2.3, "Co", pos = 3, cex = 2)
    text(5, 2.3, "Or", pos = 3, cex = 2)
    ## points(y = jitter(rep(1:6, each = 22), 0.5),
    ##   x = unlist(split(densita.apparente, interaction(LAVORAZIONE, TRT))),
    ##   cex = 2, pch = 4)
    text(x = 1:6, y = 2.2, tuk.cld$mcletters$Letters, cex = 2, col = "red")
    legend(4.5, 1.7, 
           c("Arato","F.zollato","Rippato"),
           box.col = "transparent", 
           fill=c(2,3,4), cex=1,  ncol = 1)
}
)



###################################################
### code chunk number 14: figmah
###################################################

with(data = df.petrolio,
{
    fattore <- interaction(TRT, LAVORAZIONE)
    amod <- aov(densita.apparente ~ fattore)
    HSD.test(amod, "fattore", group=TRUE, alpha = 0.05)
    tuk <- glht(amod, linfct = mcp(fattore = "Tukey"))
    tuk.cld <- cld(tuk) #letter-based display
    plot(1, xlim = c(0.5, 6.5), ylim = c(1.4, 2.5), type = "n", 
         yaxt = "n", xaxt = "n", xlab = "",ylab = "")
    rect(0.5, 1.4, 3.45,2.5, col = "lightgray", border = NA)
    rect(3.48,1.4, 6.5,2.5, col = "gray", border = NA)
    boxplot(jitter(densita.apparente) ~ LAVORAZIONE+TRT, col = c(2,3,4),
            main = "Metodo Clod",
            ##xlab = "Densità apparente",
            las = 1, xaxt = "n",
            ylim = c(1.5, 2.3),
            yaxs = "i",
            add=TRUE
            )
    axis(1, labels = FALSE)
    etichette <- levels(interaction(LAVORAZIONE,TRT))
## Plot x labs at default x position
    text(x =  seq_along(etichette), y = 1.3,
         srt = 45, adj = 1,
         labels = etichette, xpd = TRUE)
    mtext(expression(paste("Densità apparente g ", cm^-3)), side = 2, line = 2)
    text(2, 2.3, "Co", pos = 3, cex = 2)
    text(5, 2.3, "Or", pos = 3, cex = 2)
    ## points(y = jitter(rep(1:6, each = 22), 0.5),
    ##   x = unlist(split(densita.apparente, interaction(LAVORAZIONE, TRT))),
    ##   cex = 2, pch = 4)
    text(x = 1:6, y = 2.2, tuk.cld$mcletters$Letters, cex = 2, col = "red")
    legend(4.5, 1.7, 
           c("Arato","F.zollato","Rippato"),
           box.col = "transparent", 
           fill=c(2,3,4), cex=1,  ncol = 1)
}
)



###################################################
### code chunk number 15: Analisi_Petrolio
###################################################
lm.spinta <- 
lm(densita.apparente ~ TRT + LAVORAZIONE, data = df.petrolio)

anova_spinta <- 
anova(lm.spinta)

summary_spinta <- 
summary(lm.spinta)


###################################################
### code chunk number 16: Analisi_Petrolio
###################################################
rownames(anova_spinta) <- c("Management", "Tillage", "Residui")
tabella.spinta.anova <-
    xtable(anova_spinta,
           label = 'tab:anova piccoli aggregati', align = 'rrrrrr',
           caption = ''
           )
print(tabella.spinta.anova, caption.placement = "top")



###################################################
### code chunk number 17: Summary_Petrolio
###################################################

summary_spinta <- coef(summary_spinta)
rownames(summary_spinta) <- 
    c("Co Arato","Scostamento Or",
      "Scostamento Frangizollato","Scostamento Rippato")

tabella.summary.spinta <- xtable(summary_spinta,
label = 'tab:sommario piccoli aggregati', align = 'rrrrr',
caption = ''
)
print(tabella.summary.spinta, caption.placement = "top")
vec.paletti <- c(150, 50)
#vec.paletti <- c(180, 50)



###################################################
### code chunk number 18: Import_aggregati
###################################################
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
Y <-
    acomp(df.plot[, 16:18])
lm.2 <-
    with(df.plot, lm(ilr(Y) ~ COND+TIME2+I(TIME2^2)))



###################################################
### code chunk number 19: plotacompWETDRY
###################################################
modello <- lm.2
coefs <-  ilrInv(coef(modello), orig = Y)
alpha <- 0.05
plot(Y, cex=0.15, col=df.plot$COND, axes = TRUE )
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
Fase <- rep(c("inizio misura","inizio sonicatura", "fine misura"),2)
colnames(dati.iufd) <- c("Macro (%)", "Meso (%)", "Micro (%)")
df.iufd <- cbind(Conduzione, Fase, dati.iufd)
df.iufd <- rbind(df.iufd[1:3,], "", df.iufd[4:6,])



## banda.predizione <- FALSE
## for(i in 1:2){
##     for(k in 0:23){
##         newdata <-
##             list(COND = levels(df.plot$COND)[i],
##                  TIME2 = k)
##         X <- getModelMatrix(modello,newdata)
##         prediction <- ilrInv(predict(modello,newdata=newdata),orig=Y)
##         varEpsilon = var(modello)
##         XX <- kronecker(diag(ncol(predict(modello))),X)
##         estVar <- XX %*% vcov(modello) %*% t(XX)
##         predVar <- estVar+varEpsilon
##         plot(prediction,  add=TRUE, col= i, type= "b")
##         ## ellipses(prediction,ilrvar2clr(estVar),
##         ##          r=ConfRadius(modello,1-alpha), col=i)
##         if(banda.predizione)
##             ellipses(prediction,ilrvar2clr(predVar),
##                      r=ConfRadius(modello,1-alpha), col=i)
##     }
## }

plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*0:23 +
     acomp(coefs[4,])*(0:23)^2,
     col=1,  type="l", add=TRUE)## sequenza temporale CO
plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
     acomp(coefs[3,])*0:23+
     acomp(coefs[4,])*(0:23)^2,
     col=2, add=TRUE, type="l")## sequenza temporale OO
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=1, add=TRUE, pch=c("c", "D","C"))## start stop CO
plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=2, add=TRUE, pch= c("o","D","O"))## start stop OO

questi <-
    with(df.tessitura, which(HUMIDITY=="dry"))
df.plot <- df.tessitura[questi,]
Y.2 <- acomp(df.plot[, 16:18])
lm.2.2 <-   with(df.plot,lm(ilr(Y.2) ~ COND+TIME2+I(TIME2^2)))
modello <- lm.2.2
coefs <-  ilrInv(coef(modello),orig=Y.2)
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

plot(Y.2, cex=0.25, col= as.numeric(df.plot$COND)+2, add=TRUE)
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*0:23 +
     acomp(coefs[4,])*(0:23)^2,
     col=3,  type="l", add=TRUE)## sequenza temporale CO
plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
     acomp(coefs[3,])*0:23+
     acomp(coefs[4,])*(0:23)^2,
     col=4, add=TRUE, type="l")## sequenza temporale OO
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=3, add=TRUE, pch=c("c", "W","C"))## start stop CO
plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=4, add=TRUE, pch= c("o","W","O"))## start stop OO
## for(i in 1:2){
##     for(k in 0:23){
##         newdata <-
##             list(COND = levels(df.plot$COND)[i],
##                  TIME2 = k)
##         X <- getModelMatrix(modello,newdata)
##         prediction <- ilrInv(predict(modello,newdata=newdata),orig=Y)
##         varEpsilon = var(modello)
##         XX <- kronecker(diag(ncol(predict(modello))),X)
##         estVar <- XX %*% vcov(modello) %*% t(XX)
##         predVar <- estVar+varEpsilon
##         plot(prediction,  add=TRUE, col= i, type= "b")
##         ## ellipses(prediction,ilrvar2clr(estVar),
##         ##          r=ConfRadius(modello,1-alpha), col=i)
##         if(banda.predizione)
##             ellipses(prediction,ilrvar2clr(predVar),
##                      r=ConfRadius(modello,1-alpha), col=i)
##     }
## }
legend("topleft",
       c("Dry.CO", "Dry.OO", "CO.Wet", "OO.Wet"),
       fill=1:4)


###################################################
### code chunk number 20: IUFW
###################################################
xt.IUFW <- xtable(df.iufw,
       label = 'tab:iufw', digits = 1,
       caption = 'Progressione della distribuzione delle particelle 
                  liberatisi dagli aggregati precedentemente inumiditi in seguito a: 
                  i) immersione:
                  ii) inizio sonicazione 
                  iii) fine  '
       )
print(xt.IUFW,include.rownames=FALSE)


###################################################
### code chunk number 21: IUFD
###################################################
xt.IUFD <- xtable(df.iufd,
       label = 'tab:iufd', digits = 1,
       caption = 'Progressione della distribuzione delle particelle 
                  liberatisi dagli aggregati secchi in seguito a: 
                  i) immersione:
                  ii) inizio sonicazione 
                  iii) fine  '
       )
print(xt.IUFD, include.rownames=FALSE,caption.placement = "top")


###################################################
### code chunk number 22: anova_acompWET
###################################################
anova.comp.wet <- anova(lm.2)
rownames(anova.comp.wet) <- c("Intercetta","Management", "Tempo", "Tempo^2","Residui")
anova_tabella <- xtable(anova.comp.wet,
       label = 'tab:anova_compWET', align = 'rrrrrrr',
       caption = 'anova stabilit\\`a aggregati per i dati WET '
       )
print(anova_tabella, caption.placement = "top")#, math.style.exponents = TRUE)


###################################################
### code chunk number 23: anova_acompDRY
###################################################
anova.comp.dry <- anova(lm.2.2)
rownames(anova.comp.dry) <- c("Intercetta","Management","Tempo", "Tempo^2","Residui")
anova_tabella_dry<- xtable(anova.comp.dry,
       label = 'tab:anova_compWET', align = 'rrrrrrr',
       caption = 'Aggregati per i dati WET '
       )
print(anova_tabella_dry, caption.placement = "top")#, math.style.exponents = TRUE)


###################################################
### code chunk number 24: figboh3
###################################################
modello <- lm.2
coefs <-  ilrInv(coef(modello), orig = Y)
alpha <- 0.05
plot(Y, cex=0.15, col=df.plot$COND, axes = TRUE )
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
Fase <- rep(c("inizio misura","inizio sonicatura", "fine misura"),2)
colnames(dati.iufd) <- c("Macro (%)", "Meso (%)", "Micro (%)")
df.iufd <- cbind(Conduzione, Fase, dati.iufd)
df.iufd <- rbind(df.iufd[1:3,], "", df.iufd[4:6,])



## banda.predizione <- FALSE
## for(i in 1:2){
##     for(k in 0:23){
##         newdata <-
##             list(COND = levels(df.plot$COND)[i],
##                  TIME2 = k)
##         X <- getModelMatrix(modello,newdata)
##         prediction <- ilrInv(predict(modello,newdata=newdata),orig=Y)
##         varEpsilon = var(modello)
##         XX <- kronecker(diag(ncol(predict(modello))),X)
##         estVar <- XX %*% vcov(modello) %*% t(XX)
##         predVar <- estVar+varEpsilon
##         plot(prediction,  add=TRUE, col= i, type= "b")
##         ## ellipses(prediction,ilrvar2clr(estVar),
##         ##          r=ConfRadius(modello,1-alpha), col=i)
##         if(banda.predizione)
##             ellipses(prediction,ilrvar2clr(predVar),
##                      r=ConfRadius(modello,1-alpha), col=i)
##     }
## }

plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*0:23 +
     acomp(coefs[4,])*(0:23)^2,
     col=1,  type="l", add=TRUE)## sequenza temporale CO
plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
     acomp(coefs[3,])*0:23+
     acomp(coefs[4,])*(0:23)^2,
     col=2, add=TRUE, type="l")## sequenza temporale OO
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=1, add=TRUE, pch=c("c", "D","C"))## start stop CO
plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=2, add=TRUE, pch= c("o","D","O"))## start stop OO

questi <-
    with(df.tessitura, which(HUMIDITY=="dry"))
df.plot <- df.tessitura[questi,]
Y.2 <- acomp(df.plot[, 16:18])
lm.2.2 <-   with(df.plot,lm(ilr(Y.2) ~ COND+TIME2+I(TIME2^2)))
modello <- lm.2.2
coefs <-  ilrInv(coef(modello),orig=Y.2)
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

plot(Y.2, cex=0.25, col= as.numeric(df.plot$COND)+2, add=TRUE)
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*0:23 +
     acomp(coefs[4,])*(0:23)^2,
     col=3,  type="l", add=TRUE)## sequenza temporale CO
plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
     acomp(coefs[3,])*0:23+
     acomp(coefs[4,])*(0:23)^2,
     col=4, add=TRUE, type="l")## sequenza temporale OO
plot(acomp(coefs[1,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=3, add=TRUE, pch=c("c", "W","C"))## start stop CO
plot(acomp(coefs[1,]) + acomp(coefs[2,]) +
     acomp(coefs[3,])*on.ultra.off +
     acomp(coefs[4,])*on.ultra.off^2,
     col=4, add=TRUE, pch= c("o","W","O"))## start stop OO
## for(i in 1:2){
##     for(k in 0:23){
##         newdata <-
##             list(COND = levels(df.plot$COND)[i],
##                  TIME2 = k)
##         X <- getModelMatrix(modello,newdata)
##         prediction <- ilrInv(predict(modello,newdata=newdata),orig=Y)
##         varEpsilon = var(modello)
##         XX <- kronecker(diag(ncol(predict(modello))),X)
##         estVar <- XX %*% vcov(modello) %*% t(XX)
##         predVar <- estVar+varEpsilon
##         plot(prediction,  add=TRUE, col= i, type= "b")
##         ## ellipses(prediction,ilrvar2clr(estVar),
##         ##          r=ConfRadius(modello,1-alpha), col=i)
##         if(banda.predizione)
##             ellipses(prediction,ilrvar2clr(predVar),
##                      r=ConfRadius(modello,1-alpha), col=i)
##     }
## }
legend("topleft",
       c("Dry.CO", "Dry.OO", "CO.Wet", "OO.Wet"),
       fill=1:4)


###################################################
### code chunk number 25: qqplotAcomp
###################################################
qqnorm(ilrInv(resid(modello),orig=Y))


###################################################
### code chunk number 26: figboh4
###################################################
qqnorm(ilrInv(resid(modello),orig=Y))


###################################################
### code chunk number 27: Import_porosimetria
###################################################
Export.Dir <-
    file.path(DirMain, "dati_elaborati/porosimetria")
nomi.file.elab <- list.files(Export.Dir, pattern = ".CSV")

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

df.data <- df.data[c(2,3,13:18)]

df.data <-
    cbind.data.frame(df.data[c(T,F,F),-(1:2)],
                 matrix(df.data[,1], ncol=3, byrow=TRUE)
                 )
names(df.data)[7:9] <-
    c("uno","due", "tre")
require(compositions)
row.names(df.data) <- 1:dim(df.data)[1]
df.analisi <-
    df.data[-c(3,6,11,10),]

Y <- acomp(df.analisi[, 7:9])
modello <-
    with(df.analisi, lm(ilr(Y) ~ MAN*TIL))
coefs <-  ilrInv(coef(modello), orig=Y)
alpha <- 0.05


