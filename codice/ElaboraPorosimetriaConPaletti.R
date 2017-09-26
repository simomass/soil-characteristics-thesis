## FILE proveniente da
## /home/ottorino/Documenti/CONCLUSI/densita (1) Dropbox Simone Massenzio/Porosimetria/Analisi5_Amstr.R

## elaborazione dato porosimetria per Gigi. 6 Apr 2012
######################################################
## Per far correre questo file qui sotto, spostarsi
## nella directory di lavoro che DEVE contenere le due
## dir "dati_grezzi" e "dati_elaborati".
## Aprire questo file con R
##
## Allo stato attuale il programma elabora dati in forma
## CSV, (comma separated value) con "tab" come separatore
## di campo e "." come separatore decimale.
## ovvero il formato con cui escono dal porosimetro.
##
## Assicurarsi che nella dir "dati_grezzi" ci siano SOLO
## i files da elaborare denominati "nomefile.txt"
## (importanti le minuscole del ".txt")
## che corrispondono al formato sopra selezionato.
##
## La presenza di qualsiasi file pippo.txt che non
## corrisponda al formato, blocca tutta l'elaborazione
##
## Il risultato e gli eventuali errori compaiono nel
## file "result.out".
######################################################

DirMain<-"~/Simo_GIT"
##Si sta lavorando offline, in caso scommentare
vec.paletti.poro <- c(0.05, 0.1) 
MOLTE <- vec.paletti.poro
Main.Dir <- DirMain
##getwd()
Data.Dir <-file.path(Main.Dir,"dati_grezzi/porosimetria")
Export.Dir <-  file.path(Main.Dir,"dati_elaborati/porosimetria")
nomi.file <- list.files(Data.Dir,pattern = ".txt")
######################################################
## qui sotto specificare 1 o 2 tra parentesi quadre
## 1 significa che il programma prende dei punti fissi
## 2 che prende 1 punto ogni X
## 3 che prende i valori del vettore MOLTE
## specificare anche il valore di "ogni X"
######################################################
punti <-c("fissi", "ogni X", "MOLTE")[3]
ogni.X <- 30
##MOLTE <- c(5, 20)## sotto 50, tra 50 e 200, sopra 200
######################################################
######################################################
## se si vuole inserire i valori dei volumi, mettere
## TRUE, altrimenti FALSE
segna.volumi.sul.grafico <- FALSE
######################################################
######################################################
## se si vuole inserire il valore del secondo punto a
## grande raggio, mettere TRUE, altrimenti FALSE
secondo.punto <- FALSE
###################################################
## qui sotto puoi specificare "type" come
## "p", "l", "b", "o", "h", "s" che significano
## point, lines, both, o?, height, step
tipo.punti <- "l"
xlim.grafico <-c(0.5, 6) #minimo e massimo
ylim.grafico <- c(0,400) #minimo e massimo
## inizio dell'elaborazione
aggregati.weight <- c()
for(i in 1:length(nomi.file)){
    file.in.elaborazione <-
        nomi.file[i]
    path.completo <-
        file.path(Data.Dir,file.in.elaborazione)
    radice.export <-
        strsplit(file.in.elaborazione,split=".",
                 fixed =TRUE)[[1]][1]
    blocco.iniziale <-
        readLines(path.completo,150)
    blocco.iniziale <-
        gsub("\\\xb2", "2", blocco.iniziale)
    blocco.iniziale <-
        gsub("\\\xb0C", "Celsius", blocco.iniziale)
    riga.iniziale.results <-
        1 +which(regexpr('R E S U L T S',
                         blocco.iniziale) > -1)
    riga.iniziale.dati <-
        3+ which(regexpr('D A T A   R E P O R T',
                         blocco.iniziale) > -1)
    righe.nomi.dati <-
        ((riga.iniziale.dati-2):(riga.iniziale.dati-1))
    nomi.colonne <-
        NA
    lis.colonne <-
        strsplit(blocco.iniziale[righe.nomi.dati],"\t")
    for(k in 1:length(lis.colonne[[1]])){
        nomi.colonne[k] <-
            paste(lis.colonne[[1]][k],
                  lis.colonne[[2]][k],collapse="")
    }
    nome.export <-
        paste(radice.export,".csv")
    df.data <-
        read.table(path.completo,
                   sep="\t",header=FALSE,
                   dec=".",skip= riga.iniziale.dati-1)
    names(df.data) <-
        nomi.colonne
    results <-
        blocco.iniziale[riga.iniziale.results:(riga.iniziale.results+5)]
    results <-
        gsub("\t", "", results)
    aggregati.weight[i] <-
        as.numeric(unlist(strsplit(blocco.iniziale[12], "\t"))[2])

######################################################
    ## esportazione di tutti i dati grezzi
    write.table(df.data,
                file.path(Export.Dir,nome.export),
                sep= ";", row.names=FALSE, dec =",",
                fileEncoding = "UTF-16LE")
    ## nome.results <-
    ##      paste(radice.export,"_results.CSV")
    ## write.table(results,
    ##              file.path(Export.Dir,nome.results),
    ##              sep= ";",row.names=FALSE,dec =",")
    closeAllConnections()
    if(punti=="fissi"){
        punti.desiderati <-
            c(0,10,  25,50, 75,100,
              250, 500,750, 1000,
              2500, 5000,7500, 10000,
              25000, 50000,75000, 100000,250000,500000,750000,1000000)/2
    }else if(punti=="ogni.X"){
        punti.desiderati <-
            seq(1,dim(df.data)[1], by=ogni.X)
    }else{
        punti.desiderati <- MOLTE
    }
    indice <-
        NA
######################################################
    ## questi passi sotto cercano i dati piu' vicini
    ## a quelli desiderati oppure quelli "diradati"
    if(punti=="fissi"|punti=="MOLTE"){
        for(k in 1:length(punti.desiderati)){
            indice[k] <-
                which.min(abs(df.data[,3]-punti.desiderati[k]))
        }
    }else{
        indice<-
            punti.desiderati
    }
    ## Gigi dice che gli serve il primo,
    ## il secondo (a scelta) e l'ultimo punto
    ## oltre a quelli definiti sopra
    indice <-
        if(secondo.punto){
            c(1,2, indice, dim(df.data)[1])
        }else{
            c(1, indice, dim(df.data)[1])
        }
    indice <-
        sort(unique(indice))
    ## Gigi vuole i dati ordinati per volume
    ## crescente e non decrescente
    ## per questo il "rev"
    df.elabora <-
        df.data[rev(indice),]
    nome.elaborazione <-
        paste(radice.export,"_elaborato.CSV", sep="")
######################################################
    ## parte centrale del calcolo
    diff.volumi <-
        abs(diff(df.elabora[,2],1))
    log10.diam <-
        log10(df.elabora[,3]*2)
    diff.log10.diam <-
        abs(diff(log10.diam,1))
    dV.dlogDiam.ratio <-
        c(diff.volumi/diff.log10.diam)
    log10.diam <-
        log10.diam[-length(log10.diam)]
    mtx.results <-
        cbind(diff.volumi,diff.log10.diam,
              log10.diam, dV.dlogDiam.ratio)
    mtx.results <-
        rbind(mtx.results,NA)
    df.elabora <-
        cbind.data.frame(df.elabora,
                         round(mtx.results,3))
    ## fine parte centrale calcolo
######################################################
    ## esportazione dati elaborati
    write.table(df.elabora,
                file.path(Export.Dir,nome.elaborazione),
                sep= ";", row.names=FALSE, dec =",",
                fileEncoding = "UTF-16LE")
######################################################
    ## esportazione grafico pdf
    nome.grafico <-
        paste(radice.export,".pdf")
    pdf(file= file.path(Export.Dir,nome.grafico),
        width=8, height=9)
    layout(matrix(c(1,2)), heights=c(3,1))
    plot(df.elabora[ ,c(11,12)], type= tipo.punti,
         ## qui sopra puoi specificare "type" come
         ## "p", "l", "b", "o", "h", "s" che significano
         ## point, lines, both, o?, height, step
         xlim=xlim.grafico,
         ylim=ylim.grafico)
    if(segna.volumi.sul.grafico){
        text(df.elabora[,c(11,12)],
             as.character(df.elabora[,2]),
             pos= 3)
    }
    axis(3, at=1:5, labels = 10^(1:5))
    mtext(radice.export, side=3, line=3)
    mtext("diam nm", side=3, line=1, at= 5.5)
    plot(1:10,rep(1:5,2), axes=FALSE, ann= FALSE, type="n")
    text(rep(c(1,5),6),4:2, results, pos=4, cex=0.6)
    dev.off()
    closeAllConnections()
}
write.table(as.numeric(aggregati.weight),
            file.path(Export.Dir, "pesoaggre.csv"),
            sep = ";")
