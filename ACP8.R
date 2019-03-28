ACP8 <- function(df.pca)
{
colorPanel <- function (y,p) #p entier >0 : niveau de dégradés couleurs topographiques
{
vcol <- rainbow(p,start=2/39,end=9/39)
#exmple avec la courbe cosinus carré: 
x <- seq(1:length(y))
rang <- ceiling(y/max(y)*p)
rang <- replace(rang,(rang==0),1)
#plot(y~x,cex=2,pch=16,col=vcol[rang])
#return(vcol[p+1-rang])
return(vcol[rang])
}
"ATTENTION ! 
Le séparateur décimal de la plage EXCEL importée doit être le point ."
#X <- read.table(file("clipboard"),sep="\t",header=TRUE,dec=".")
print("library(stringr) requise pour la fonction str_c()")
library(stringr)

#X <- read.table("D:/Bureau/0Analyses Factorielles/jeuxD/automobiles.csv",header=T,sep=" ",dec=",",row.names=1)
#X <- dfconf7.pca
# Pour remplacer toutes les valeurs manquantes par Zéro (0), 
X <- df.pca ; X <- replace(X,is.na(X),0);fr <- X
datas <- X 
i <- dim(X)[1];j <- dim(X)[2];W <- colnames(fr);V <- rownames(fr) 
#*********************** ---------------------- *******************************
M <- cor(X,method="kendall")
M <- cor(X,method="pearson")

MM <- data.frame(M,row.names=W)
colnames(MM)=W

A <- scale(X,scale=TRUE)
#A2 <- scale(X2,scale=TRUE) Matrice A2 = A avec l individu illustratif retiré de l'ACP 
P <- eigen(t(A)%*%A)$vec
P1 <- eigen(M)$vec

h <- round(eigen(M)$value/j,digits=5)*100

D <- A%*%P

#Matrice des individus projetes sur les axes ppx
DD <- as.data.frame(D)
rownames(DD)=V
colnames(DD)=str_c("axeppal",seq(1:j))
#cercle de corrélation 

C <- t(cor(D,A))
CC <- data.frame(C,row.names=W)
colnames(CC)=str_c("axeppal",seq(1:j))



if (j > 3) {fr1 <- data.frame(D[,1],D[,2],D[,3],D[,4],row.names=V); colnames(fr1)=c("x","y","z","k")
} else {fr1 <- data.frame(D[,1],D[,2],D[,3],row.names=V); colnames(fr1)=c("x","y","z")}

Q <- D^2/apply(D^2,1,sum)
#Matrice Qualité en degrés

fr2 <- data.frame(QC1=Q[,1],QC2=Q[,2],QC3=Q[,3],QC4=Q[,4],row.names=V)

#Représentation individus sur 2 premiers plans principaux.
#Les individus mal représentés sur l'axe 1 sont caractérisés par 1 triangle rouge.
# rappel : h est le vecteur renfermant les valeurs propres de la matrice d'inertie
#la commande par(mfrow=c(2,1)) 
#ouvre le fenetre en deux sous-fenêtres sur 2 lignes et 1 colonne.
#Ainsi si par(mfrow=c(3,3)) 
#ouvrirait la fenêtre en 9 sous-fenêtres sur 3 lignes et 3 colonnes. 
xminmax <- c(min(D[,1]),max(D[,1]))
yminmax <- c(min(D[,2]),max(D[,2]))

tit1 <- paste("inertie:",as.character(round(h[1])),'%')
tit2 <- paste("inertie:",as.character(round(h[2])),'%')
par(mar=c(3.9,3.9,1,1))
plot(0,0,
	type="n",
	axes=TRUE,panel.first=grid(5,5),
	xlim=xminmax,
	ylim=yminmax,
	xlab=tit1,
	ylab=tit2,
	cex.axis=0.5)
#plot(0,0,type="n",axes=TRUE,panel.first=grid(5,5),
#xlim=xminmax,ylim=yminmax,xlab=tit1,ylab=tit2,cex.lab=0.5)
#axis(1,tick = TRUE, col = "red", lwd = 1, lty = "dotted", las = 1, cex.axis = 0.6)
#title(xlab="1er Plan Principal",font.lab=1,col.lab="blue",cex.lab=1)

#Le langage R est conçu pour un usage vectoriel qui permet de limiter l'usage des boucles. 
#En Analyse Factorielle, on introduit la notion de  représentation sur le plan principal. 
#Ainsi, Quand l'angle du vecteur individu fait un angle inférieur à 50°, 
#On considère (arbitrairement) que l'individu est bien représenté et on lui
#applique la couleur 3 pour la projection. Sinon on applique la couleur 2. 
#Nous allons voir un exemple où on applique cette règle pour une collection d'individus
#Soit, le vecteur c(25,45,55,75) correspondant aux angles que font les vecteurs individus. 
#En appliquant à ce vecteur la fonction (ligne 156) f(x)=0.5*sign(50-x)+2.5 on obtient
#le vecteur couleur aycol = c(2,2,3,3). 

aycol=colorPanel(Q[,1],5) 
points(D[,1],D[,2],pch=16,col=aycol,cex=0.8)
text(D[,1],D[,2]-max(D)/100,V,cex=0.5)
abline(h=0,v=0,col="blue")
#Plan principal sur 2nd et 3th valeurs propres
dev.new()
plot.new()
xminmax <- c(min(D[,3]),max(D[,3]))
yminmax <- c(min(D[,4]),max(D[,4]))
tit3 <- paste("inertie:",as.character(round(h[3])),'%')
tit4 <- paste("inertie:",as.character(round(h[4])),'%')
par(mar=c(3.9,3.9,1,1))
plot(0,0,type="n",panel.first=grid(10,10),xlim=xminmax,ylim=yminmax,xlab=tit3,ylab=tit4)
#title(xlab="1er Plan Principal",font.lab=1,col.lab="blue",cex.lab=1)
aycol=colorPanel(Q[,3]+Q[,4],5) 
points(D[,3],D[,4],pch=16,col=aycol,cex=0.8)
text(D[,3],D[,4]-max(D)/100,V,cex=0.5)
abline(h=0,v=0,col="black")
#Pour représenter les vecteurs variables, on utilisera la commande segments avec la matrice C cercle corr
#On visualisera le cercle de corrélation sur un deuxième graphique avec dev.new()
dev.new()
plot.new()
#par(pty="s")
par(mfrow=c(1,2))
par(usr=c(-1,1,-1,1))
abline(h=0,v=0,col="green",lty="dotted")
#VISUALISATION AVEC GGPLOT2
#Il s'agit d'introduire un diametre de type bubble_chart dans l'affichage de chaque individu. 
#Chaque point / individu se voient donc attribuer un diamètre d'affichage 
#qui va dépendre par ex. d'une valeur intrinsèque à une variable donnée de l'ACP.
#Pour des données sur l'automobile, on pourra choisir le prix de vente
#Chaque point / individ se voient également attribuer une couleur dans leur affichage. 
#Cette couleur dépend directement de la qualité de la représentation sur le plan principal choisi
#Q contient les cos carrés pour chaque individus. 
#Q[,i]+Q[,j] (compris entre 0 et 1) matérialise donc la qualité de la représentation sur le plan c(i,j)
acp.synthese <- cbind(data.frame(DD),cos2_1=Q[,1],cos2_2=Q[,2],cos2_3=Q[,3],var.size=df.pca[,1])
p1 <- 1 ; p2 <- 2 
p1 <- ggplot(acp.synthese,aes(x=axeppal1,y=axeppal2))
p2 <- p1 + geom_point(aes(size=(var.size),color=cos2_1))
p3 <- p2 #+ geom_text(aes(label=V,color=cos2_1))
p4 <- p3 + scale_color_gradient(low="red",high="blue")

#FIN EXPERIMENTATION GGPLOT2
plot(C[,1],C[,2],panel.first=grid(10,10),pch=5,xlim=c(-1,1),ylim=c(-1,1),cex=0.2,xlab=tit1,ylab=tit2)
omega <- seq(0,6.3,length=50)
points(cos(omega),sin(omega),type="l")
symbols(0,0,circles=0.8,inches=F,add=T)
for (k in 1:j) {
segments(0,0,C[k,1],C[k,2],col="#0066FFFF",lwd=1)
text(C[k,1],C[k,2],W[k],cex=0.7,col="#000F00FF")
}
plot(C[,3],C[,4],panel.first=grid(10,10),pch=5,xlim=c(-1,1),ylim=c(-1,1),cex=0.2,xlab=tit3,ylab=tit4)
points(cos(omega),sin(omega),type="l")
for (k in 1:j) {
segments(0,0,C[k,3],C[k,4],col="red",lwd=1)
text(C[k,3],C[k,4],W[k],cex=0.6)
}
dev.new()
plot.new()
par(usr=c(-1,1,-1,1))
abline(h=0,v=0,col="green",lty="dotted")


plot(C[,3],C[,4],panel.first=grid(10,10),pch=5,xlim=c(-1,1),ylim=c(-1,1),cex=0.2,xlab=tit3,ylab=tit4)
points(cos(omega),sin(omega),type="l")
for (k in 1:j) {
segments(0,0,C[k,3],C[k,4],col="red",lwd=1)
text(C[k,3],C[k,4],W[k],cex=0.6)
}
dev.new()
print(p4)
return(fr2)
}