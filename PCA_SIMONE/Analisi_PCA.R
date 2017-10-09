### R code from vignette source 'Tesi_GIT.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: PacchettiRichiesti
###################################################
##df.PCA1$APPEZZAMENTO <- NULL
##param.chim <- 10:ncol(df.PCA1)
##param.fis <- 5:10-1
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
df.PCATotale<-
    read.table(file.path(DirElab, "df.PCATotale.csv"), sep = ";")
df.PCAFisica<-
    read.table(file.path(DirElab, "df.PCAFisica.csv"), sep = ";")
require(FactoMineR)
##res <- PCA(df.PCA1, quali.sup = c(1, 2, 3), quanti.sup = c(param.chim, 16), graph = FALSE)
require(Factoshiny)
##

require(factoextra)

require(car)
require(corrplot)
corrplot(res$var$cos2)
fviz_pca_var(res, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_pca_ind(res,
             geom.ind = "point", col.ind = df.PCA1$TRT, addEllipses = TRUE)
fviz_pca_biplot(res, col.ind = df.PCA1$TRT, palette = "jco", addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE)



pdf(file.path(DirGraf, "RisultatiChimica-Fisica.pdf"))
res <- PCA(df.PCAFisica, quali.sup = c(1, 2, 3, 4), quanti.sup = c(8,9), graph = FALSE)
fviz_screeplot(res, addlabels = TRUE)
corrplot(res$var$cos2)
fviz_pca_var(res, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
##fviz_pca_ind(res, geom.ind = "point", col.ind = df.PCAFisica$TRT, addEllipses = TRUE)
fviz_pca_biplot(res, col.ind = df.PCAFisica$TRT, palette = "jco", addEllipses = FALSE, label = "var", col.var = "black", repel = TRUE)
fviz_ellipses(res, habillage = df.PCAFisica$TRT, axes = c(1, 2), addEllipses = TRUE,
       ellipse.type = "confidence", , palette = NULL, pointsize = 1,
       geom = c("point", "text"))

res <- PCA(df.PCAChimica, quali.sup = c(1, 2, 3), quanti.sup = 8:11, graph = FALSE)
fviz_screeplot(res, addlabels = TRUE)
corrplot(res$var$cos2)
fviz_pca_var(res, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
##fviz_pca_ind(res,geom.ind = "point", col.ind = df.PCAChimica$TRT, addEllipses = TRUE)
fviz_pca_biplot(res, col.ind = df.PCAChimica$TRT, palette = "jco", addEllipses = FALSE, label = "var", col.var = "black", repel = TRUE)
fviz_ellipses(res, habillage = df.PCAChimica$TRT, axes = c(1, 2), addEllipses = TRUE,
       ellipse.type = "confidence", , palette = NULL, pointsize = 1,
       geom = c("point", "text"))
dev.off()


library(chemometrics)
library(robustbase)

pdf(file.path(DirGraf, "RisultatiPCA.pdf"))
res <- PCA(df.PCAChimica, quali.sup = c(1, 2, 3),
           quanti.sup =  c(6, 8, 10, 11), graph = FALSE)
plot.PCA(res, choix = "var", habillage = "TRT",
         axes = c(1,2), title = "Chimica")
plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(1,2),
         title = "Chimica")
plotellipses(res, keepvar = c("TRT", "LAVORAZIONE"),
             level = c(0.90, 0.99))
res <- PCA(df.PCAFisica, quali.sup = c(1, 2, 3),
           quanti.sup = 10, graph = FALSE)
plot.PCA(res, choix = "var", habillage = "TRT", axes = c(1,2),
         title = "Fisica")
plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(1,2),
         title = "Chimica")
plotellipses(res, keepvar = c("TRT", "LAVORAZIONE"))
## df.PCA2 <- df.PCA1[,c(1:4, 7, 8, 10, 12, 13)]
## res <- PCA(df.PCA2, quali.sup = c(1, 2), quanti.sup = 9, graph = FALSE)
## plot.PCA(res, choix = "var", habillage = "TRT", axes = c(1,2), title = "Tolta la porosità anche dal dataframe e gli N")
## plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(1,2), title = "Tolta la porosità anche dal dataframe e gli N")
## plot.PCA(res, choix = "var", habillage = "TRT", axes = c(1,3), title = "Tolta la porosità anche dal dataframe e gli N")
## plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(1,3), title = "Tolta la porosità anche dal dataframe e gli N")
## plot.PCA(res, choix = "var", habillage = "TRT", axes = c(2,3), title = "Tolta la porosità anche dal dataframe e gli N")
## plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(2,3), title = "Tolta la porosità anche dal dataframe e gli N")
## ##
 res <- PCA(df.PCA, quali.sup = c(1, 3, 2), quanti.sup = c(14), graph = FALSE)
## plot.PCA(res, choix = "var", habillage = "TRT", axes = c(1,2), title = "tutti dentro")
## plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(1,2), title = "tutti dentro")
## ##
## res1 <- PCA(df.PCA1, quali.sup = c(1, 2), quanti.sup = c(param.chim, 3, 7, 8), graph = FALSE)
## plot.PCA(res1, choix = "var", habillage = "TRT", axes = c(2,3), title = "fisica dentro chimica fuori, anche distroporo")
## plot.PCA(res1, choix = "ind", habillage = "TRT", axes = c(2,3), title = "fisica dentro chimica fuori, anche distroporo")
## ##PRova mia
## res2 <- PCA(df.PCA1, quali.sup = c(1, 2), quanti.sup = c(3, 4, 7, 8, 9, 10, 12), graph = FALSE)
## plot.PCA(res2, choix = "var", habillage = "TRT", axes = c(1,2), title = "fisica dentro chimica fuori, anche distroporo")
## plot.PCA(res2, choix = "ind", habillage = "TRT", axes = c(1,2), title = "fisica dentro chimica fuori, anche distroporo")
## ##Senza Core
## res3 <- PCA(df.PCA1, quali.sup = c(1, 2), quanti.sup = c(3,4,param.chim), graph = FALSE)
## plot.PCA(res3, choix = "var", habillage = "TRT", axes = c(1,2), title = "fisica dentro chimica fuori, anche core")x11()
## plot.PCA(res3, choix = "ind", habillage = "TRT", axes = c(1,2), title = "fisica dentro chimica fuori, anche core")
dev.off()


modello <- lm(C.Organico ~ TRT/C.Inorganico-1, data = df.PCA1)
summary(modello)
plot(df.PCA1[,c(11,13)], col = df.PCA1$TRT, pch  = 20,
     cex = 1.5, xlim = c(-1,5), ylim = c(-0.5,1.5))
abline(v = 0, lty = 2);abline(h = 0, lty = 2);
abline(coef(modello)[c(1,3)], col = 1)
abline(coef(modello)[c(2,4)], col = 2)
points(x = c(0,0), y = coef(modello)[1:2], col = 1:2, cex = 3)
modello <- lm(C.Organico ~ TRT*C.Inorganico, data = df.PCA1)
summary(modello)

modello <- lm(C.Inorganico ~ TRT, data = df.PCA1)

##dimdesc(res2)

## Lm.prova <-
##     lm(densita.clod/PoriTot ~ TRT, data = df.PCA1)
## anova(lm.prova)
## summary(lm.prova)
## plot(lm.prova)


## plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(1,2))
## dimdesc(res)

## df.PCA2 <-
##     df.PCA[,-6]
## df.PCA2 <-
##     df.PCA2[complete.cases(df.PCA2),-2]
## res <- PCA(df.PCA2, quali.sup = c(1,2), graph = FALSE)
## par(mfrow = c(2,2))
## plot.PCA(res, choix = "var", habillage = "TRT", axes = c(1,2))
## plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(1,2))
## plot.PCA(res, choix = "var", habillage = "TRT", axes = c(1,3))
## plot.PCA(res, choix = "ind", habillage = "TRT", axes = c(1,3))
## dimdesc(res)


library("PerformanceAnalytics")
chart.Correlation(df.PCA[, 4:7, 9:11], histogram=TRUE, pch=19)
require(rgl)
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) {
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}
get_colors <- function(groups, group.col = palette()){
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col))
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}
rgl_add_axes <- function(x, y, z, axis.col = "darkgrey",
                xlab = "Dim 1", ylab="Dim 2", zlab="Dim 3", show.plane = TRUE,
                show.bbox = FALSE, bbox.col = c("#333377","black"))
  {

  lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
  # Add axes
  xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
  rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)

   # Add a point at the end of each axes to specify the direction
   axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0),
                 c(0, 0, zlim[2]))
   rgl.points(axes, color = axis.col, size = 3)

  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
             adj = c(0.5, -0.8), size = 2)

  # Add plane
  if(show.plane)
    xlim <- xlim/1.1; zlim <- zlim /1.1
    rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
             z = c(zlim[1], zlim[2], zlim[2], zlim[1]))

  # Add bounding box decoration
  if(show.bbox){
    rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5,
          emission=bbox.col[1], specular=bbox.col[1], shininess=5,
          xlen = 3, ylen = 3, zlen = 3)
  }
  }

##############################################################################
##############################################################################
##############################################################################
df.PCAcompleto <- df.PCAFisica[-3, -c(7,8)]
res <- PCA(df.PCAcompleto[questi,], quali.sup = c(1, 2, 3, 4),
           graph = FALSE)

x <- res$ind$coord[,1]
y <- res$ind$coord[,2]
z <- res$ind$coord[,3]
groups <- df.PCAcompleto$TRT[questi]
levs <- levels(groups)
rgl_init()
rgl.spheres(x, y, z, r = 0.07,
            color = get_colors(groups))
rgl_add_axes(x, y, z, show.bbox = TRUE)
group.col <- 1:length(levs)
for (i in 1:length(levs)) {
    group <- levs[i]
    selected <- groups == group
    xx <- x[selected]; yy <- y[selected]; zz <- z[selected]
    ellips <- ellipse3d(cov(cbind(xx,yy,zz)),
              centre=c(mean(xx), mean(yy), mean(zz)), level = 0.95)
    shade3d(ellips, col = group.col[i], alpha = 0.1, lit = FALSE)
    wire3d(ellips, col =  group.col[i],  lit = FALSE)
    # show group labels
    texts3d(mean(xx),mean(yy), mean(zz), text = group,
            col= group.col[i], cex = 2)
  }
# Compute and draw the ellipse of concentration
aspect3d(1,1,1)
xyz.coo <- res$var$coord
for (i in 1:4){
rgl.lines(x=c(0,xyz.coo[i, 1]),
          y=c(0,xyz.coo[i, 2]),
          z=c(0,xyz.coo[i, 3]),
          color = "green", lwd=2, magnify=2)
texts3d(x=c(0,xyz.coo[i, 1]),
          y=c(0,xyz.coo[i, 2]),
          z=c(0,xyz.coo[i, 3]), text = rownames(xyz.coo)[i],
            col= "green", cex =1)
}











## rgl.snapshot("3D_1_2_HUMAN.png",fmt="png")
## rgl.snapshot("3D_2_3_HUMAN.png",fmt="png")
## rgl.snapshot("3D_1_3_HUMAN.png",fmt="png")
## rgl.snapshot("3D_1_2_CLUST.png",fmt="png")
## rgl.snapshot("3D_2_3_CLUST.png",fmt="png")
## rgl.snapshot("3D_1_3_CLUST.png",fmt="png")

## Create a movie
## movie3d(spin3d(axis = c(1, 0, 0)), duration = 12,
##         dir = getwd(), convert=TRUE, movie = "movieCLUSTX")
## movie3d(spin3d(axis = c(0, 1, 0)), duration = 12,
##         dir = getwd(), convert=TRUE, movie = "movieCLUSTY")

## movie3d(spin3d(axis = c(1, 0, 0)), duration = 12,
##         dir = getwd(), convert=TRUE, movie = "movieHUMANX")

## movie3d(spin3d(axis = c(0, 1, 0)), duration = 12,
##         dir = getwd(), convert=TRUE, movie = "movieHUMANY")


## scatter3d(x = res$ind$coord[,1],
##           y = res$ind$coord[,2],
##           z = res$ind$coord[,3],
##           xlab = "Dim 1", ylab = "Dim 2", zlab = "Dim 3",
##           groups = df.MCA$CLUST,
##           surface.col=1:5,
##           #point.col=as.numeric(df.MCA$HUMAN),
##           surface = FALSE,
##           ellipsoid = TRUE,
##           grid = TRUE)



## ## Fr ha pochi elementi e non riesce ovviamente a disegnare l'ellisse
## questi <-
##     df.MCA$HUMAN %in% c("Ln", "Ln.Lv", "Lv", "Lv.Lnf",
##                         "Lnf", "Lt", "Ltr", "Frm",
##                         "S")
## livelli <- df.MCA$HUMAN[questi]
## livelli <- livelli[drop = TRUE]
## scatter3d(x = res$ind$coord[questi,1],
##           y = res$ind$coord[questi,2],
##           z = res$ind$coord[questi,3],
##           xlab = "Dim 1", ylab = "Dim 2", zlab = "Dim 3",
##           #groups = livelli,
##           groups = res.hcpc$data.clust$clust,
##           #surface.col=1:length(livelli),
##           #point.col=as.numeric(df.MCA$HUMAN),
##           surface = FALSE,
##           ellipsoid = TRUE,
##           grid = TRUE)

## scatter3d(x = res$ind$coord[questi,3],
##           y = res$ind$coord[questi,4],
##           z = res$ind$coord[questi,5],
##           xlab = "Dim 3", ylab = "Dim 4", zlab = "Dim 5",
##           groups = livelli,
##           #res.hcpc$data.clust$clust,
##           surface.col=1:length(livelli),
##           #point.col=as.numeric(df.MCA$HUMAN),
##           surface = FALSE,
##           ellipsoid = TRUE,
##           grid = TRUE)



## scatter3D(x = res$ind$coord[questi,1],
##           y = res$ind$coord[questi,2],
##           z = res$ind$coord[questi,3],
##           xlab = "Dim 1", ylab = "Dim 2", zlab = "Dim 3",
##           col.var = livelli, pch=20, cex=2)

## scatter3d(x = res$quanti.sup$coord[,1],
##           y = res$quanti.sup$coord[,2],
##           z = res$quanti.sup$coord[,3],
##           xlab = "Dim 1", ylab = "Dim 2", zlab = "Dim 3",surface = FALSE
##  )

## arrows3D(x0=rep(0,16),
##          y0=rep(0,16),
##          z0=rep(0,16),
##          x1 = res$quanti.sup$coord[,1],
##          y1 = res$quanti.sup$coord[,2],
##          z1 = res$quanti.sup$coord[,3],
##          xlab = "Dim 1",
##          ylab = "Dim 2",
##          zlab = "Dim 3",
##          colvar=1:16,
##          theta = 0, phi = 0)


X <-
    df.PCAChimica[,4:7]
X_scala <-
    scale(X, center = TRUE, scale = TRUE)
X_mcd <-
    covMcd(X)
d_Mahalanobis_robust <- sqrt(mahalanobis(X, center = X_mcd$center, cov = X_mcd$cov))
res <- Moutlier(X, quantile = 0.95, plot = FALSE)
questi <- which(res$rd<3)
df.PCAChimica[-questi,1:3]
plot(1:length(res$rd), res$rd, pch = ".")
text(1:length(res$rd), res$rd, paste(df.PCAChimica$TRT, df.PCAChimica$APPEZZAMENTO, df.PCAChimica$LAVORAZIONE, sep = ""))
abline(h = res$cutoff, lty = 2)


x11()
X <-
    df.PCAFisica[-3,c(5, 6, 9,10)]
X_scala <-
    scale(X, center = TRUE, scale = TRUE)
X_mcd <-
    covMcd(X)
d_Mahalanobis_robust <- sqrt(mahalanobis(X, center = X_mcd$center, cov = X_mcd$cov))
res <- Moutlier(X, quantile = 0.95, plot = FALSE)
questi <- which(res$rd<res$cutoff)
df.PCAFisica[-questi,1:3]
plot(1:length(res$rd), res$rd, pch = ".")
text(1:length(res$rd), res$rd, paste(df.PCAFisica$TRT, df.PCAFisica$APPEZZAMENTO, df.PCAFisica$LAVORAZIONE, df.PCAFisica$PARCELLA, sep = ""))
abline(h = res$cutoff, lty = 2)
