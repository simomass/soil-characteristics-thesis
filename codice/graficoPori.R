df.data <-
    read.table(
        "../dati_elaborati/DiametroPori.csv",
        sep = ";", dec=",", header=TRUE, )
sapply(df.data, class)

maxmin <- c(0.0001, 10000)#range(df.data[,-1], na.rm=TRUE)
jpeg("../grafici/DiametroPoriLetteratura.jpg", width = 1024, height = 768)
plot(x=maxmin, y=1:2,
     ##xlim=c(0.00001, 10000, na.rm=TRUE)),
     ylim=c(0, nrow(df.data)), type="n",
     log="x",
     axes=FALSE, ann=FALSE)
for(i in 1:nrow(df.data)){
    vettore <- df.data[i, -1]
    questi <- which(!is.na(df.data[i,-1]))
    points(y=rep(i,length(questi)),
           x=rev(as.vector(unlist(matrix(vettore)[,1]))[questi]),
           type="b", pch=4, cex=4)
}
nostri.paletti <- c(0.5, 50)
abline(v= nostri.paletti, col=2, lwd=2)
axis(side= 2, at=1:11, labels=as.character(df.data$Autore), las=1, line=-10)
axis(side= 1, las=1, line=1)
mtext("micron", side=1)
mtext(text= as.character(nostri.paletti), side=1, at=nostri.paletti)
##
segments(30,2, 0.002,2, col=1, lwd=10)
segments(75,3, 0.002,3, col=1, lwd=10)
segments(30,4, 5,4, col=1, lwd=10)
segments(0.005, 5,0.002,5, col=1, lwd=10)
segments(0.1,6, 0.002,6, col=1, lwd=10)
segments(30,7, 0.002,7, col=1, lwd=10)
segments(0.2,8, 0.002,8, col=1, lwd=10)
segments(0.5,10, 0.002,10, col=1, lwd=10)
segments(10,11, 0.002,11, col=1, lwd=10)
dev.off()
