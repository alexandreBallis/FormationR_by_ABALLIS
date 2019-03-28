pch <- function(n,lwd){#A matrix class 
# soit le tableau suivant 
# |-----------------|
# | n=3 | 1 | 2 | 3 | 
# |-----------------|
# | 1   | 1 | 2 | 3 |
# | 2   | 4 | 5 | 6 |
# | 3   | 7 | 8 | 9 |
# |-----------------|
# le but est de construire une fonction bijective qui du couple (2,2) donne 5 ainsi que sa fonction réciproque. 
# soit n un entier, i et j compris entre 1 et n.Soit la fonction phi(i,j) = (i-1)*n+j = k bijective
# la fonction réciproque de phi est obtenue en considérant que 
# si k est non divisible par n , j est le reste non nul de la division de k par (n-1)
# si k est divisible par n, j est le reste de la division de k-1 par n +1 soit (k-1)%%n + 1 
# i-1 est le quotient de k-j  par n-1 soit (k-j) %/% (n-1) 
# Afin de ne plus distinguer la divisibilité de k par n, il nous faut constater que 
# pour tout k compris entre 1 et n^2, k-1 mod n + 1 est bien un reste non nul de k par n quand n>1. 
phi	<- function(i,j,n) {
	if (i<=0 || j<=0 || i>n || j>n) {return("les indices doivent être compris entre 1 et n")}	
	else 
	{return ((i-1)*n+j)}
}
inv_phi <- function(k,n) {
	return(str_c((k-(k-1)%%n+1)%/%n+1,"&",(k-1)%%n+1))
}
A <- matrix(1:n^2,ncol=n,byrow = TRUE)
image(A,col = rainbow(25))
for (i in 1:n) { 
	for (j in 1:n) {
    #points((i-1)/(n-1),(j-1)/(n-1),lwd=lwd,cex=5,pch=phi(i,j,n),col=1)
	points((j-1)/(n-1),1-(i-1)/(n-1),lwd=lwd,cex=5,pch=phi(i,j,n),col=1)
	text((j-1)/(n-1),1-(i-1)/(n-1),phi(i,j,n),cex=lwd,col=7)
	}
}
}
#----------------------------------------------------------------------------------------------#
diffx <- function(fichier,baseLog) {
library(dplyr)
#fichier <- "COMMANDES_SI2B_20180328160846.csv"
#baseLog = 10
#gestion erreur: le fichier doit provenir d'une exctraction IBIS sur les commandes 
if (is.na(stringr::str_match(fichier,"COMMANDE"))) {print("fichier IBIS requis !");stop;return()}

HMX<- read.csv(fichier,sep=";",header=TRUE,dec=",",stringsAsFactors=FALSE)
x <- HMX$Montant.TTC.commande #colonnes de montants TTC
x <- sort(x[x>=1]) #élimination des montants inférieurs strictement à 1. puis tri croissant
#diffx <- data.frame(montants = xsort,ecartAuPrec <- c(0,diff(xsort)))
#selectionner dans diffx, les ecartauprec < strict. à 1 pour chaque tranches [10^i,10^(i+1)] , i > 0
diffx <- data.frame(montants=x,ecart=c(0,diff(x)),
					classe=cut(x,breaks=baseLog^(0:(log(10^10,baseLog))))) %>%
					cbind(.,supA1=.$ecart >=1) 
T <- table(diffx$classe,diffx$supA1) %>% 
	 cbind(.,total=apply(.,1,sum)) %>%
	 cbind(.,ratio=.[,1]/.[,2])
			
#length(x[x>=2^i & x<=2^(i+1)]) 
					
return(T)	
}
#BILAN / INTERPRETATION
#(1,5]                   0    0     0        NaN
#(5,25]                 11    9    20 1.22222222
#(25,125]               59   29    88 2.03448276
#(125,625]             195  155   350 1.25806452
#(625,3.12e+03]        555  603  1158 0.92039801
#(3.12e+03,1.56e+04]   470 1054  1524 0.44592030
#(1.56e+04,7.81e+04]   384 1218  1602 0.31527094
#(7.81e+04,3.91e+05]   140  819   959 0.17094017
#(3.91e+05,1.95e+06]    12  122   134 0.09836066
#(1.95e+06,9.77e+06]     0    7     7 0.00000000
#(9.77e+06,4.88e+07]     0    2     2 0.00000000
#(4.88e+07,2.44e+08]     0    0     0        NaN
#(2.44e+08,1.22e+09]     0    0     0        NaN
#(1.22e+09,6.1e+09]      0    0     0        NaN
# Dans l'application IBIS, rechercher une commande par montant TTC sur une fourchette de 1 € est sujet 
# à confusion pour les montants compris entre 25€ et 125 €.En effet, sur cet intervalle, 
# plus de 67% du montants des commandes ont une commande qui précède d'un montant inférieur à 1 €.  
"--------------------------------------------------------------------------------------------------------------------------------------"
coutLoc <- function(geode)
{
geode <- geode240117 
#Nettoyage et mise en forme du fichier 
geode$Loyer.HT.Moteur <- str_replace(geode$Loyer.HT.Moteur,",", ".")
geode <- subset(geode,!is.na(geode$Loyer.HT.Moteur))
# 		Regroupements des configurations 
geode$Config <- str_replace(geode$Config,"Config3B", "Config3A")
geode$Config <- str_replace(geode$Config,"Config6", "Config7")
#		Mise en Dates exploitables
geode$Date.install. <- floor(as.numeric((as.Date("2016-12-31") - as.Date(geode$Date.install.,format="%d/%m/%Y")))/30)
geode <- geode[!is.na(geode$Date.install.),]
geode <- geode[geode$Date.install. > 0,]
print(nrow(geode[geode$Date.install. > 0 & geode$Date.install. <= 12,]))
geode1 <- geode
#grille tarifaire 
grilleTarif <- tariff()[[4]][1:6,2:6]
#Choix des colonnes retenues pour les bilans
df.geode <- geode[,c(19,16,45)]
df.geode[,1] <- factor(df.geode[,1]) #Config.
df.geode[,2] <- factor(df.geode[,2]) #Type de Loyer
iConf <- as.numeric(df.geode[,1])
jLoyer<- as.numeric(df.geode[,2])
df.geode[,4] <- sapply(1:nrow(df.geode),function(k) {return(grilleTarif[iConf[k],jLoyer[k]])})
df.geode[,5] <- df.geode[,4]*df.geode[,3]
colnames(df.geode) <- c("Config","Loyer","NbMois","Loyer.HT.Moteur","total.loyer")
#Erreur sur fichier Geode : les resultats loyer sont différents avec les données prises sur geode. 
#Confection du même data.frame en prenant les données du fichier Géode. Ces différences s'expliquent peut-être par des loyers différents de l'annexe financière: 
#1) Soit le matériel est d'occasion et le loyer diffère du prix à neuf.
#2) Soit la souscription à option de location a changé en cours de marché. 
geodeReel <- geode1[,c(19,16,45,60)]
geodeReel[,5] <- as.numeric(geodeReel[,4])*geodeReel[,3]
colnames(geodeReel) <- c("Config","Loyer","NbMois","Loyer.HT.Moteur","total.loyer")
#Calcul des tables mais la table de pilote comme fonctionnalité CALC est plus adapté pour la diffusion des résultats. 
Xmois <- xtabs(NbMois~Config+Loyer,data=df.geode)
Xmachine <- table(df.geode$Config,df.geode$Loyer)
write.table(df.geode,file="S_dfCoutLocations.csv",sep=";",row.names=F,dec=",")
return(list("cout_annexe_financiere"=df.geode,"cout_scc"=geodeReel))
}
"--------------------------------------------------------------------------------------------------------------------------------------"
"--------------------------------------------------------------------------------------------------------------------------------------"
"--------------------------------------------------------------------------------------------------------------------------------------"
print("Une analyse factorielle maison (en composantes principales)")
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
colorPanelRGB <- function(y)
{
y <- floor(runif(100,1,2000))
#data.frame des codes couleurs 
df.color <- data.frame("R"=c(181,252,254,255),"G"=c(0,141,232,255),"B"=c(0,89,200,255))
df.color <- data.frame("R"=c(255,254,252,181),"G"=c(255,232,141,0),"B"=c(255,200,89,0))
rangCol <- cut(y,breaks=c(-1,1,100,1000,Inf),labels=1:4)
return(df.color[rangCol,])
}
"ATTENTION ! 
Le séparateur décimal de la plage EXCEL importée doit être le point ."
#X <- read.table(file("clipboard"),sep="\t",header=TRUE,dec=".",row.names=1)
print("library(stringr) requise pour la fonction str_c()")
library(stringr)

#X <- read.table("D:/Bureau/0Analyses Factorielles/jeuxD/automobiles.csv",header=T,sep=" ",dec=",",row.names=1)
#X <- dfconf7.pca
# Pour remplacer toutes les valeurs manquantes par Zéro (0), 
X <- df.pca ; X <- replace(X,is.na(X),0);fr <- X
datas <- X 
i <- dim(X)[1];j <- dim(X)[2];W <- colnames(fr);V <- rownames(fr) 
#*********************** ---------------------- *******************************
Mk <- cor(X,method="kendall")
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

Q <- D^2/apply(D^2,1,sum)

if (j > 3) {
	fr1 <- data.frame(D[,1],D[,2],D[,3],D[,4],row.names=V); colnames(fr1)=c("x","y","z","k")
	fr2 <- data.frame(QC1=Q[,1],QC2=Q[,2],QC3=Q[,3],QC4=Q[,4],row.names=V)
	}
		else {
		fr1 <- data.frame(D[,1],D[,2],D[,3],row.names=V); colnames(fr1)=c("x","y","z")
		fr2 <- data.frame(QC1=Q[,1],QC2=Q[,2],QC3=Q[,3],row.names=V)
		}

#Matrice Qualité en degrés



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
titre <- str_c(c("axe",1,":",round(h[1],2),"% |_| axe",2,":",round(h[2],2),"%"),collapse="")
par(mar=c(3,3,1,1))
plot(0,0,
	type="n",
	axes=TRUE,panel.first=grid(5,5),
	main=titre,
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
text(D[,1],D[,2]-log(max(D))/100,V,cex=0.35)
abline(h=0,v=0,col="blue")
#Plan principal sur 2nd et 3th valeurs propres si j > 3 
if (j>3) 
{
dev.new()
plot.new()
xminmax <- c(min(D[,3]),max(D[,3]))
yminmax <- c(min(D[,4]),max(D[,4]))
tit3 <- paste("inertie:",as.character(round(h[3])),'%')
tit4 <- paste("inertie:",as.character(round(h[4])),'%')
par(mar=c(3,3,1,1))
titre <- str_c(c("axe",3,":",round(h[3],2),"% |_| axe",4,":",round(h[4],2),"%"),collapse="")
plot(0,0,type="n",main=titre,panel.first=grid(10,10),xlim=xminmax,ylim=yminmax,xlab=tit3,ylab=tit4)
#title(xlab="1er Plan Principal",font.lab=1,col.lab="blue",cex.lab=1)
"--------------------------------------------------------------------------------------------------------------------------------------"
aycol=colorPanel(Q[,3]+Q[,4],5) 
points(D[,3],D[,4],pch=16,col=aycol,cex=0.8)
text(D[,3],D[,4]-log(max(D))/100,V,cex=0.5)
abline(h=0,v=0,col="blue")
}
#Pour représenter les vecteurs variables, on utilisera la commande segments avec la matrice C cercle corr
#On visualisera le cercle de corrélation sur un deuxième graphique avec dev.new()
dev.new()
plot.new()
#par(pty="s")
par(mfrow=c(1,2))
par(usr=c(-1,1,-1,1))
abline(h=0,v=0,col="green",lty="dotted")
"--------------------------------------------------------------------------------------------------------------------------------------"
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
p3 <- p2 + geom_text(aes(label=V,color=cos2_1))
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
if(j>3) {
	plot(C[,3],C[,4],panel.first=grid(10,10),pch=5,xlim=c(-1,1),ylim=c(-1,1),cex=0.2,xlab=tit3,ylab=tit4)
	points(cos(omega),sin(omega),type="l")
	for (k in 1:j) {
		segments(0,0,C[k,3],C[k,4],col="red",lwd=1)
		text(C[k,3],C[k,4],W[k],cex=0.6)
	}
}
dev.new()
plot.new()
par(usr=c(-1,1,-1,1))
abline(h=0,v=0,col="green",lty="dotted")


if (j>3) {
	plot(C[,3],C[,4],panel.first=grid(10,10),pch=5,xlim=c(-1,1),ylim=c(-1,1),cex=0.2,xlab=tit3,ylab=tit4)
	points(cos(omega),sin(omega),type="l")
	for (k in 1:j) {
		segments(0,0,C[k,3],C[k,4],col="red",lwd=1)
		text(C[k,3],C[k,4],W[k],cex=0.6)
	}
}
#dev.new()
print(p4)
return(list(fr1,fr2))
}
#--------------------------------------------------------------------------------------------------------------------------------------
versAFD <- function() #requiert la librairie MASS 
{
library(MASS)
s <- matrix(c(1,0.8,0.8,1),2) #matrice de covariance de deux lois N. 
x1 <- mvrnorm(50,c(1/2,-1/2),s) #loi de X1
x2 <- mvrnorm(50,c(-1/2,1/2),s) #loi de X2
x <- rbind.data.frame(x1,x2)
x <- scale(x,scale=F)
fac <- factor(rep(1:2,rep(50,2)))
plot(x,col=as.integer(fac))

library(car)
#ellipse recouvrant tous les points à moins d'un écart type de la moyenne X1
#rappel : pnorm(1)-pnorm(-1) ~ 68.3% des effectifs. 
ellipse(c(1/2,-1/2),s,1) 
#ellipse recouvrant tous les points à moins d'un écart type de la moyenne X2
ellipse(c(-1/2,1/2),s,1)
abline(h=0);abline(v=0)
arrows(0,0,2,0,lwd=2)
text(2,0,"A",pos=1,cex=2)
arrows(0,0,sqrt(2),sqrt(2),lwd=2)
text(sqrt(2),sqrt(2),"B",pos=4,cex=2)
arrows(0,0,0,2,lwd=2)
text(0,2,"C",pos=1,cex=2)
arrows(0,0,-sqrt(2),sqrt(2),lwd=2)
text(-sqrt(2),sqrt(2),"D",pos=2,cex=2)
m <- scan(n=1)
par(mfrow = c(2,2))
f1 <- function(a,b,cha) {
z <- a+x[,1]+b*x[,2]
z0 <- seq(-4,4,le=50)
z1 <- z[fac == 1]
z2 <- z[fac == 2]
hist(z,proba=TRUE,col=grey(0.9),xlim = c(-4,4),main=cha)
lines(z0,dnorm(z0,mean(z),sd(z)),lwd=2)
lines(z0,dnorm(z0,mean(z),sd(z)),lwd=2)
lines(z0,dnorm(z0,mean(z),sd(z)),lwd=2)
}
f1(1,0,"A")
f1(1/sqrt(2),1/sqrt(2),"B")
f1(0,1,"C")
f1(-1/sqrt(2),1/sqrt(2),"D")
return()
}
#--------------------------------------------------------------------------------------------------------------------------------------
print("text_explode(texte)")
text_explode <- function(texte) 
{
library(stringi)
library(stringr)
#texte = read.table(file("clipboard"),sep="\t",stringsAsFactors =FALSE) 
#texte est donc un data.frame. 
s0 <- toString(tolower(texte)) #extraction des phrases
s1 <- unlist(str_match_all(s0,"[a-zA-Zàéèâôêç]{4,}")) #extraction des mots 3 ou plus
s3 <- s1[!duplicated(s1)] #mise au format minuscule 
dico <- str_c(s3,"/")
text_pasting <-  str_c(s1,collapse="/")
denombrement <- str_count(text_pasting,dico)

synthese <- list(s3,denombrement)
analyse <- data.frame(mot=synthese[[1]],occurence=synthese[[2]])
analyse <- unique(analyse[order(analyse[,2],decreasing=T),])
#Classification des mots par l' occurences constatees des lettres alphabetiques"
v <- NULL
for(k in 1:length(s3))
{
v <- append(v, str_count(s3[k],letters))

}  
M <- data.frame(row.names=s3,matrix(v,ncol=26,byrow=T))

colnames(M) <- letters
return(list(analyse,M))
}
#-------------------------------------------------------------------------------------------------#
list_explode <- function(list) 
# en entrée, list est une liste de vecteurs de réels attachés chacuns à un label 
#renvoie une liste , pour chaque vecteur attaché à son label, le nombre d'éléments non nuls 
#exemple summaries[[2]] du script grid4_projet.r 
{ 
 return (sapply(list,function(x) {length(x[x>0])}))
}
binome <- function(n)
{
s=seq(0:n) 
h=1/2 * choose(n,s)*(1/2^n+3^(n-s)/4^n)
barplot(h)
}
f <- function(x) #loi de densité N(0,1)
{ 
y <- 1/sqrt(2*pi)*exp(-1/2*x^2)
return(y)
}
B <- function(n,p)
{
i <- 0:n
y <- choose(n,i)*(p^i*(1-p)^(n-i))
return(y)
}
"-------------------------------------------------------------------------------------------------------------------------------------"
MatIndep <- function(X) {
#Fonction intégrée dans R Base sous le nom : prop.table
#Calcule les effectifs marge ligne et marge colonne. 
i=dim(X)[1]
j=dim(X)[2]
ML <- matrix(rep(apply(X,1,sum),j),nrow=i,ncol=j)
print(ML)
n=sum(diag(ML))
print(n)
MC <- t(matrix(rep(apply(X,2,sum),i),nrow=j,ncol=i))
print(MC)
return (ML*MC/n)
}

MatInertie <- function(A) {
n <- dim(A)[1]
Af <- A/sum(A)
Fj <-apply(Af,2,sum)
Fi <-apply(Af,1,sum)
K <- matrix(data=rep(0,n*n),nr=n,nc=n)
for (i in 1:n) {
	for(j in 1:n) {
		K[i,j] <- Af[i,j]/sqrt(Fi[i]*Fj[j])
}
}
return (K)
}
"-------------------------------------------------------------------------------------------------------------------------------------"
print("p_class: fonction utilisée pour classifier un individu en fonction d'un score compris entre 0 et 1")
#Le score d'un individu par le biais d'une régression logistique permet de le classer en fonction d'un découpage pertinent du domaine de score    
p_class <- function(p)
{
if (p < 0 | p>1) {stop("probabilité attendue")}
p_voyelle <- c(0.3514,0.5337,0.7135,0.8642,0.9926,1)
voyelles <- c("a","e","i","o","u","y")
return(voyelles[p<=p_voyelle][1])
}
"-------------------------------------------------------------------------------------------------------------------------------------"
print("generer(n): générer n mots de passe aléatoires")
generer <- function(n)
{
library(stringr) 
alphabet <- stri_join(letters,LETTERS,collapse="")
valphabet <- c(str_c(letters),str_c(LETTERS),str_c(0:9),str_c(0:9),str_c(0:9))
vprob <- runif(length(valphabet))
vprob <- vprob/sum(vprob)
password <- str_c(sample(valphabet,9,prob=vprob,replace=T),collapse="")
passwords <- sapply(1:n,function(i) {str_c(sample(valphabet,9,prob=vprob,replace=T),collapse="")})
return(passwords) 
}
print("voyellesplus(n,p) : générer n mots de passe aléatoires en fixant une probabilité p de séléctionner les voyelles")
voyellesplus <- function(n,p,nb) #comme la fonction generer mais avec plus de voyelles dans les mots. plus p est élevé, plus les voyelles aparraissent 
#si p=1, pas de modification du vecteur de probabilité dans sample(). plus p augmente, plus les probabilités associées aux voyelles augmentent. 
{
library(stringr) 
#p est une probabilité comprise entre 0 et 1
if (p<0 | p>1) {print("error") ; stop()}
#fonction préliminaire 
fbin <- function(n,p) 
{ k <- 0:n
u <- choose(n,k)
vp <- rep(p,n+1)
v1mp <- rep(1-p,n+1)
return(round(u*vp^k*v1mp^(n-k),5))
}
#p est une probabilité comprise entre 0 et 1 
# le but est de trouver une fonction réciproque de f(x) sur R >=0 ayant les propriétes suivantes 
#f(0)=0
#lim f(x) quand x tend à l'infini, = 1 
# la fonction f est croissante mais dont la dérivée a pour limite nulle en l'infini. 
# f est la fonction sigmoide 
f <- function(x) {2/(1+exp(-x))-1}
#fonction réciproque 
fr <- function(x) {-log((1-x)/(1+x))}
#--------------------------------------------------------------------------------------------------#
alphabet <- stri_join(letters,LETTERS,collapse="")
valphabet <- c(str_c(letters),str_c(LETTERS),str_c(0:9))
vprob <- c(rep(1,length(valphabet)))
vprob[c(1,5,9,15,21,25,27,31,35,41,47,51)] <- fr(p) * vprob[c(1,5,9,15,21,25,27,31,35,41,47,51)]
#vprob <- vprob/sum(vprob)
password <- str_c(sample(valphabet,n,prob=vprob,replace=T),collapse="")
passwords <- sapply(1:nb,function(i) {str_c(sample(valphabet,n,prob=vprob,replace=FALSE),collapse="")})
return(passwords) 
}

"-------------------------------------------------------------------------------------------------------------------------------------"
toBinVect <- function(x,n)
{ 
	v <- c(rep(0,n+1))
	lg = log(x,2)
	if ( lg == floor(lg)) {v[floor(lg)+1] <- 1} else {
	
	for (k in n:0) 
	{ 
	if (x-2^k >= 0) {x <- x-2^k;v[k+1] <- 1} 
	}
	}
	return(v)
}	
"**********************"
combin <- function(facture,sousTot) #facture est un vecteur de réel ; par exemple , facture = c(13,11,7,5,3,2,1). sousTot est un réel

{	print("combin(facture,sousTot) : cherche une sousTotal dans une facture")
	n <- length(facture) 
	for (i in 1:2^n -1 ) 
	{
		if ( sum(facture*toBinVect(i,n-1)) == sousTot ) 
		{
		res <- facture*toBinVect(i,n-1)
		print(sort(res[res>0]))
		}
	}
}
"-------------------------------------------------------------------------------------------------------------------------------------"
f <- function(lis)
{len=NULL 
x = NULL
A=NULL
for (e in lis) {
  x = c(x,length(e))
}
p=max(x)
n = length(lis)
A=matrix(rep(0,n*p),nrow=n,ncol=p)
i=1
for (e in lis) {
	j=1
	x <- c(rep(0,p))
		for (e2 in e) {
			A[i,j]=e2
			x[j] <- e2 
			j <- j+1
		}	
	A[i,] <- sort(x,decreasing=TRUE) 
	i <- i+1 
}
return(A)
}
#----------------------------------------------------------------------------------------------
print("g(y): fonction proposant des graduations adaptées à un scalaire y")
g <- function(y) {
	M <- ceiling(y/10^floor(log(y,10)-1)) #Majorant 
	M <- M + M%%2 #de manière à toujours avoir des valeurs paires dans les axes et graduations#
	p <- floor(log(y,10)-1)
	return(c(M,p))
}
#----------------------------------------------------------------------------------------------
print("draw_barplot(X,total,titre,couleur,exercice_comptable,noms_groupes,block1_txt,...,limy,e)")
print("fonction requise : g(y)")
print("simplification d'un tracé de diagramme barres en regroupant les paramètres dans une fonction") 
draw_barplot <- function(X,total,titre,couleur,exercice_comptable,noms_groupes,block1_txt,block2_txt,limy,e) 
{
dev.new()
#voir sprintf() pour le format d'affichage
vlog <- g(max(limy))
limsup <- (vlog[1])*10^vlog[2]
par(mar=c(9,3.1,2.1,1),las=3,bg="whitesmoke",font.axis=7,col.axis=4,col.main="#3B5A70",cex.axis=10/11,cex.main=6/7)
titre = c(paste("HORS-MARCHES ",exercice_comptable),paste(titre,formatC(total,digits=2,format="f",big.mark=" ")," € TTC"))
res.traces <- barplot(X,col=couleur,ylim=c(0,limsup*1.2),main=titre,names=noms_groupes,yaxp=c(0,limsup,vlog[1]/2))
text(res.traces,block1_txt+e,block2_txt
,cex=6/7,srt=67.5)
gridframe(limsup)
#grid(ny=NULL,nx=NA,lwd=1,col="grey")
}
#----------------------------------------------------------------------------------------------
bar_margeligne <- function(X,total,titre,couleur,exercice_comptable,noms_groupes,block1_txt,block2_txt,limy,e) 
{
dev.new()
#voir sprintf() pour le format d'affichage
vlog <- g(max(limy))
limsup <- (vlog[1])*10^vlog[2]
par(mar=c(5,3.1,1.5,1),las=3,bg="whitesmoke",font.axis=7,col.axis=4,col.main="#3B5A70",cex.axis=10/11,cex.main=6/7)
titre = c(paste("HORS-MARCHES ",exercice_comptable),paste(titre,formatC(total,digits=2,format="f",big.mark=" ")," € TTC"))
res.traces <- barplot( t(X/apply(X,1,sum))
,col=couleur,ylim=c(0,limsup*1.2),main=titre,names=noms_groupes,yaxp=c(0,limsup,vlog[1]/2))

i <- length(res.traces)
text(res.traces,1+e,block2_txt
,cex=6/7,srt=37.5)
gridframe(limsup)
#grid(ny=NULL,nx=NA,lwd=1,col="grey")
}
#----------------------------------------------------------------------------------------------
print("gridframe(y) : dessine une graduation adaptée aux variables")
gridframe <- function(y) {
#y est un nombre (un maximum d'une var. quantitative. 
	M <- ceiling(y/10^floor(log(y,10)-1))
	p <- floor(log(y,10)-1)
	for (k in seq(0,M,10)*10^p) {
			abline(a=k,b=0,col="black",h=0.5,lty=3)
	}
}
#----------------------------------------------------------------------------------------------
dialogue_grid6 <- function(fichier) {
HMX<- read.csv(fichier,sep=";",header=T,dec=",")
#TRANSFORMATION DE HMX3 POUR DIALOGUE AVEC GRID6
HMX <- data.frame(HMX[,1:3],PROJET=HMX[,4],
					HMX[,5:16],
					TITULAIRE=HMX[,17],
					MARCHE=HMX[,18],
					HMX[,19:38],
					Montant.paye=HMX[,39],
					HMX[,40:41])
return(HMX)
}
weibull <- function(x)
{(a/b^a)*x^(a-1)*exp(-(x/b)^a)}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
"Corrige une adresse IP avec un encodage uTF8 malencontreux induisant un code  qu'un remplacement de chaine ne suffit pas à éradiquer.
En effet, Ce code est visualisé en blanc en format ANSI mais n'en est pas un. la fonction replace est donc inopérante.
Malheureusement cette fonction en apparence simple fait appel à un arsenal de guerre pour venir à bout de ces adresses erronnées saisies par les disis."
print("corrigeIP(ip) : corrige syntaxiquement une adresse IP saisie à la main par des opérateurs") 
corrigeIP <- function(ip)
{
ip1 <- str_replace_all(ip,"[,]",".")
ip1 <- unlist(str_split(ip1,"[.]",n=4))
ip2 <- as.double(str_match(ip1,"[0-9]+")[,1])
ip3 <- ip2[1]
for (i in 2:4) { ip3 <- str_c(ip3,ip2[i],sep=".") }
return(ip3)
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("adresseIp(vecteur de char) : création adresse ip à partir d'un vecteur de ses 4 termes. Cela fonctionne également sur 2^n termes.")
adresseIp <- function(ve) #vecteur de charactères en entrée. dim(ve) = 2^n non nulle. 
{
#exemple: adresseIp(c("10","253","195","1")) renvoie "10.253.195.1"
	ctrl <- log(length(ve),2)
	if (floor(ctrl)==ctrl & ctrl != 0) #la dim(ve) est - elle une puissance de 2 non nulle? 
	{#oui
	vs <- sapply(seq(1,length(ve),2),function(i){paste(paste(ve[i],".",sep=''),ve[i+1],sep='')})
		if (length(vs) > 1)
			{vs <- adresseIp(vs)}	
	}
	else
	{#non 
	return("erreur! la dimension du vecteur doit être une puissance de 2 non nulle")
	}
return(vs)
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
concatener <- function(ve) #vecteur de charactères en entrées. dim(ve) doit être >1. 
{
	ctrl <- length(ve)
	if(ctrl > 1) {
	vs <- sapply(seq(1,ctrl-(ctrl%%2),2),function(i) {paste(ve[i],ve[i+1],sep='')})
	if (ctrl%%2 == 1)
			{return(paste(concatener(vs),ve[ctrl],sep=""))}
			else {return(concatener(vs))}
	}
	return(ve)
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("digitParser() : la chaîne de char IP_kg16/253f/155.2leg est corrigée par  16.253.155.2)")
normaliser <- function(chaine) #chaine doit être de type char
{ 
explod <- sapply(1:nchar(chaine),function(i) {substr(chaine,i,i)})
matching <- sapply(1:nchar(chaine),function(i) {grepl("[0-9]|[.]" , substr(chaine,i,i))}) 
filtre <- explod[matching]
return (concatener(filtre))
#exemple d'effet de la fonction normaliser 
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("fonction cleanNS() avec data.frame en argument pour nettoyer les n° de Séries des fichiers disis")
#Nettoyage "industriel" du fichier disis par numéro de série. 
#Nettoyage des n° séries saisis par les Disis avec uniquement une approche par regular expression.  
cleanNS <- function(dataframe,kiemeCol)
{
adressesIP <- sapply(dataframe[,kiemeCol],function(i) {corrigeIP(i)})
#print(dataframe$nserie)
dataframeRectifie <- data.frame(structure=dataframe[,1],nserie=toupper(str_match(str_replace(dataframe[,kiemeCol],"O","0"),expreg)[,1]),adresse=adressesIP)
dataframe[,kiemeCol] <- str_replace_na(dataframe[,kiemeCol],"   ")
return(dataframeRectifie)
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("anti_doublon(dataframe,k)")
anti_doublon <- function(dataframe,k) #En arguments , un data.frame (ex: disi78); un entier (ex: k=2) précisant le n° de colonne à dé-doublonner.  
{
doubles <- duplicated(dataframe[,k])
output <- dataframe[!is.element(dataframe[,k],dataframe[,k][doubles]),]
return(output)
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("subnet(@IP) : décomposition d'une adresse @IP en ses 4 composantes ")
subnet <- function(x) #x est une collection d'adresses @IP 
{
df <- data.frame(ip=x,first_sub = sapply(x,function(i) {str_c("10",str_split(i,"[.]")[[1]][c(2)],sep=".")}),
						  sd_sub = sapply(x,function(i) {str_c("10",str_split(i,"[.]")[[1]][2],str_split(i,"[.]")[[1]][3],sep=".")}),
						  last_term = sapply(x,function(i) {str_split(i,"[.]")[[1]][4]})
					)
df <- df[with(df,order(sd_sub,first_sub)),]
return (df)
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("filtre_doublons(dataframe)")
filtre_doublons <- function(dataframe) #paramètres en entrées: data.frames 
#Dataframe est la collecte des adresses IP des directions DGFIP. 
#Filtre les doublons sur la partie Adresse IP du data-frame
{

doublon <- duplicated(dataframe[,3])
dataframe <- subset(cbind(dataframe,doublon),!doublon)
dataframe <- data.frame(structure = dataframe$structure,nserie=dataframe$nserie,adresse = gsub(" ","",(dataframe$adresse)),stringsAsFactors=FALSE)
dataframe <- data.frame(structure = dataframe$structure,nserie=dataframe$nserie,adresse = str_replace_all(dataframe$adresse,",","."),stringsAsFactors=FALSE)
#dataframe <- data.frame(structure = dataframe$structure,nserie=dataframe$nserie,adresse = str_c(unlist(str_extract_all(dataframe$adresse,"[0-9.]"))))
dataframe$nserie <- str_replace_na(dataframe$nserie,"   ")
return(dataframe)
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("regular(listTexte) : décompose des numéros de séries (texte) pour y découvrir des expressions régulières")  
regular <- function(listNSerie)
{
#Rejeter les listes vides
listNSerie <- na.omit(listNSerie)
if (length(na.omit(listNSerie)) > 0)
{
j <- str_length(listNSerie[1])
l <- length(listNSerie)
A <- matrix(rep(0,j*l),nrow=l) #initialisation matrice A 
s <- listNSerie
pattern <- NULL 
for (i in 1:l) {
for (k in 1:j) {A[i,k] <- str_sub(s[i],k,k)}
} 
for (k in 1:j) {
#print(str_c("index",k))
	occurence <- levels(as.factor(A[,k]))
	motif <- str_c(occurence,collapse="")
	if (length(occurence) == 1) {pattern <- str_c(pattern,occurence)} else {pattern <- str_c(pattern,"[",motif,"]")}
}
}
else
{pattern <- ""}
return(pattern) 
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
recolte <- function(dfr,ks,kc) #ks =15 et kc =12   
#Ventilation des n° de séries selon la configuration Solimp II. dfr est le data.frame contenant les données,de preference "gere" , 
#ks = 15 étant la colonne des n° de séries et kc=12 étant la colonne des modeles : MP 2554, MP C3003 etc...
{
rec <- NULL 
config <- levels(as.factor(dfr[,kc]))

G14 <- subset(dfr,str_sub(dfr[,ks],1,1) == "G")[,ks] #MP 2554
S701 <- subset(dfr,str_sub(dfr[,ks],1,2) == "70")[,ks] #MX511
S74 <- subset(dfr,str_sub(dfr[,ks],1,2) == "74")[,ks] #MX711
E155 <- subset(dfr,str_sub(dfr[,ks],1,2) == "E1")[,ks] #C3003
S610 <- subset(dfr,str_sub(dfr[,ks],1,2) == "45")[,ks] #MP 610 
G17  <- subset(dfr,str_sub(dfr[,ks],1,3) == "G17")[,ks]
return(list(config,G14,E155,S74,S701,S610,G17))
}
print("dispatch : pour une requête de selection sur les modeles de matériels impression. ")
dispatchH <- function(histogramme1) 
{
df.ms610 <- subset(histogramme1,histogramme1$model == "MS610dn")
df.mx511 <- subset(histogramme1,histogramme1$model == "MX511dhe")
df.mx711 <- subset(histogramme1,histogramme1$model == "MX711")
df.mp2554 <- subset(histogramme1,histogramme1$model == "MP 2554")
df.mpc3003 <- subset(histogramme1,histogramme1$model == "MP C3003")
df.mp4054 <- subset(histogramme1,histogramme1$model == "MP 4054")
return(list(df.mp4054,df.mpc3003,df.mp2554,df.mx711,df.mx511,df.ms610))
}
dispatchG <- function(gere,dfscore) 
{
rownames(gere) <- gere[,2]
v4054 <- c(16,20,26,29,33,37)
v2554 <- c(15,29,37)
v610 <- c(16)
df.ms610 <- subset(gere,gere$Modèle == "MS610dn")    [,v610] #les compteurs pertinents se situent en colonnes 16 à 50 
df.ms610 <- mergeRownames(df.ms610,dfscore,1)
df.mx511 <- subset(gere,gere$Modèle == "MX511dhe")   [,c(16:50)] #les compteurs pertinents se situent en colonnes 16 à 50
df.mx511 <- mergeRownames(df.mx511,dfscore,1)
df.mx711 <- subset(gere,gere$Modèle == "MX711")      [,c(16:50)] #les compteurs pertinents se situent en colonnes 16 à 50 
df.mx711 <- mergeRownames(df.mx711,dfscore,1)
df.mp2554 <- subset(gere,gere$Modèle == "MP 2554")   [,v2554] #les compteurs pertinents se situent en colonnes 16 à 50 
df.mp2554 <- mergeRownames(df.mp2554,dfscore,1)
df.mpc3003 <- subset(gere,gere$Modèle == "MP C3003") [,c(16:50)] #les compteurs pertinents se situent en colonnes 16 à 50 
df.mpc3003 <- mergeRownames(df.mpc3003,dfscore,1)
df.mp4054 <- subset(gere,gere$Modèle == "MP 4054")   [,v4054] #les compteurs pertinents se situent en colonnes 16 à 50 
df.mp4054 <- mergeRownames(df.mp4054,dfscore,1) 
rownames(df.mp4054) <- df.mp4054 [,1] #identification des individus dans rownames 
rownames(df.mpc3003) <- df.mpc3003 [,1] #identification des individus dans rownames
rownames(df.mp2554) <- df.mp2554 [,1] #identification des individus dans rownames
rownames(df.mx711) <- df.mx711 [,1] #identification des individus dans rownames
rownames(df.mx511) <- df.mx511 [,1] #identification des individus dans rownames
rownames(df.ms610) <- df.ms610 [,1] #identification des individus dans rownames
 
return(list(df.mp4054,df.mpc3003,df.mp2554,df.mx711,df.mx511,df.ms610))
}
mergeRownames <- function(df1,df2,id2) #les identifiants de df1 sont contenus dans rownames(). la fonction merge ne peut pas joindre ce data.frame. 
{
df.id1 <- cbind(rownames(df1),df1)
df.joins <- merge(df.id1,df2,by.x=1,by.y=id2)
return(df.joins)
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("regR(dataframe,kiemeCol,expreg) : corrige une colonne k de data.frame selon expression régulière expreg")
regR <- function(dataframe,kiemeCol)
{
#Fonction utilisée sur le fichier scc5 issu de GEODEHISTO pour normaliser les n° de série . -#
mp2554 <- "G14[4-6][JP][1-9A-C]0[0128][0-9]{3}"	
mp3003 <- "E15[4-6]M[1-9A-C][23][0-5][0-9]{3}"			
ms610 <- "4514[0-6][0-7P]HH[0-3][0-6D-Z][0-9B-Z][0-9A-Z]{2}"			
mx511 <- "701[56][3-6][25-7P]HH[01][0-9M-Z][0-7C-Y][0-9B-Z]{2}"
mx711 <- "7463[456]C660[0-3][0-9B-Z]{3}"
mp4054 <- "G17[45][5JR][1-4C][05]0[0-9]{3}"
mp2554ex <- "NO70FC"
mp9999 <- "[0-9A-Z]+" #Autres numéros de série non encore répertoriés ou en cours de qualifications
vexpreg <- c(mp2554ex,mp2554,mp3003,ms610,mx511,mx711,mp4054)
expreg <- str_c(vexpreg,collapse="|")
cx <- dataframe[,kiemeCol] 
dataframe[,kiemeCol] <- str_match(cx,expreg)[,1] 
print(str_c("dimension css5:",dim(dataframe))) 
dataframe1 <- subset(dataframe,!is.na(dataframe[,kiemeCol]))
print(str_c("dimension après rectif css5:",dim(dataframe1)))
return(dataframe)
}
"--------Découpage selon les modèles pour ensuite générer dessus l'expression régulière afférente $$ fonction regular(listeNS)-------------------------"
#le fichier GEODE de l'extraction de l'outil de suivi solimp2 de SCC contient une colonne "Moteur" synonyme de "modèle" de copieurs mutliFonctions
#on peut donc ventiler ce fichier geode en fonction des "moteurs" par l'expression ci-dessous
#utilisation du data.frame GEODE 
factorisation <- function()
{
#utilisation du data.frame GERE de KAPAX 
#Extraction selon fichier GEODE (attention, supprimer les champs vides sur SN) regR(geode,23)
nbModele <- length(levels(factor(geode$Moteur)))
serieXmoteur <- sapply(1:nbModele,function(i) {subset(geode,geode$Moteur == levels(factor(geode$Moteur))[i])$SN})

dfMoteurNoserie1 <- data.frame("moteur"=levels(factor(geode$Moteur)),"regularExp"=
		sapply(1:nbModele,function(i) {regular(serieXmoteur[[i]])}))
#Extraction selon fichier KAPAX
nbModele <- length(levels(factor(gere$Modèle)))
serieXmoteur <- sapply(1:nbModele,function(i) {subset(gere,gere$Modèle == levels(factor(gere$Modèle))[i])$"Numéro.de.série"})
dfMoteurNoserie2 <- data.frame("moteur"=levels(factor(gere$Modèle)),"regularExp"=
		sapply(1:nbModele,function(i) {regular(serieXmoteur[[i]])}))
write.table(dfMoteurNoserie1,file="S_expreg1.csv",sep=";",row.names=F)	
write.table(dfMoteurNoserie2,file="S_expreg2.csv",sep=";",row.names=F)		
#geodeExplodeMoteur est donc de classe "Liste" et on applique la fonction regular() à chacune de ses composantes. 
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("wordExpect(string) : avec l aide de nsPredict, corriger automatiquement les erreurs de saisies des n° de séries.")
wordExpect<- function(string,stream) #Corrige pour l'instant seulement la config 7. 
#L'utilisation du package stringDist est requise pour la fonction amatch() [distance de levenshtein]
# amatch("E155230260",nosseries,maxDist=1)
{
library(stringdist)
return(stream[amatch(string,stream,maxDist=5)])
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("nsPPredict(gere): une analyse de fréquences des caractères composant les N° de séries")
print("nsPredict(gere) : une introduction à l'analyse prédictive type correction orthographique immédiate")
nsPPredict <- function(gere) #Vers une prédicition d'une n° de série à partir d'une faute d'orthographe ou de saisie. 
{
noSeries <- gere[,2] #en colonne 2, les n° de séries des matériels. 
s3003 <- subset(gere,gere[,5]=="MP C3003")[,2] #récupère les n° de séries de la configuration 7a
s2554 <- subset(gere,gere[,5]=="MP 2554")[,2] #récupère les n° de séries de la configuration 7a
s711  <- subset(gere,gere[,5]=="MX711")[,2] #récupère les n° de séries de la configuration 7a
s511  <- subset(gere,gere[,5]=="MX511dhe")[,2] #récupère les n° de séries de la configuration 7a
s610  <- subset(gere,gere[,5]=="MS610dn")[,2] #récupère les n° de séries de la configuration 7a
L <- list(s3003,s2554,s711,s511,s610)
#s <- subset(gere,gere[,5]=="")[,2] #récupère les n° de séries de la configuration .... 
model <- c("s3003","s2554","s711","s511","s610")
is_model <- 1:5
noms_colonnes <- c(str_c(0:9),LETTERS[1:26]) ; noms_colonnes <- c("is_modele",noms_colonnes)
charMet <- c(str_c(0:9),LETTERS[1:26])
vecteur <- NULL 
alphabet <- LETTERS[1:26]
matrice <- sapply(1:length(L),function(i) {
Li <- L[[i]]
freqN <- sapply(0:9,function(j) {sum(str_count(L[[i]],str_c(j)))/length(L[[i]])})
freqL <- sapply(1:26,function(j) {sum(str_count(Li,alphabet[j]))/length(Li)})
freq <- c(freqN,freqL)
vecteur <- append(vecteur,freq)
})
df <- as.data.frame(matrice)
colnames(df) <- model
#Le dataframe df moyennes des charactères rencontrés 
list.matOcc <- sapply(1:length(L),function(i) {
list(sapply(1:mean(str_length(L[[i]])),function(k) {str_count(str_c(str_sub(L[[i]],k,k),collapse=""),charMet)/length(L[[i]])*100}))
})
dframe.matOcc <- sapply(1:length(list.matOcc),function(i) 
{data.frame(list.matOcc[[i]])})
print("LECTURE DU TABLEAU: Le nombre d occurences en moyenne pour chaque caracteres de chaques modeles") 

return(list("general"=df,"frequencesParTypes"=dframe.matOcc))
}
"-------------------------------------------------------------------------------------------------------------------"
library(kernlab) #ksvm(x,y,type="C-svc",kernel="rbfdot",kpar=list(sigma=2),prob.model=TRUE) 
library(scatterplot3d) # scatterplot3d(x,pch=y+14,color=y,cex.symbols=1.25,type="h")
library(car) # scatter3d(x[,1],x[,2],x[,3],point.col=y)

x <- rbind(matrix(rnorm(20),,2),matrix(rnorm(20,mean=3),,2))

x <- rbind(matrix(rnorm(20),,2),matrix(rnorm(20,mean=5),,2),matrix(rnorm(20,mean=10),,2))
x3 <- c(rnorm(10,mean=1),rnorm(10,mean=5),rnorm(10,mean=10))
x <- cbind(x,x3)
y <- matrix(c(rep(1,10),rep(-1,10)))
y <- matrix(c(rep(1,10),rep(2,10),rep(3,10)))
rR <- scatterplot3d(x,pch=y+14,color=y,cex.symbols=1.25,type="h") #package scatterplot3d
rR$points3d(x[,1],x[,2],x[,3],cex=rep(seq(1,10),3))
svp <- ksvm(x,y,type="C-svc",kernel="rbfdot",kpar=list(sigma=2),prob.model=TRUE)
svp2 <- ksvm(x,y,type="C-svc",kernel="splinedot",prob.model=TRUE)
svp3 <- ksvm(x,y,type="C-svc",kernel="laplacedot",prob.model=TRUE)
#deux dimensions
#plot(x,pch=y+1,cex=1/5)
#text(x[,1],x[,2],1:20,cex=8/7,col=2)
#dev.new()
#plot(svp)
print(SVindex(svp2)) #doit indiquer les lignes des vecteurs supports. 
"-------------------------------------------------------------------------------------------------------------------"
"----------------------------------------------------------------------------------------------------------"
nsPredict <- function(gere,monnoserie) #Vers une prédicition d'une n° de série à partir d'une faute d'orthographe ou de saisie. 
#gere est le data.frame des mopieurs de la DGFIP. Les seules colonnes qui intéresse la procédure nsPredict sont :
# 1) La colonne format character contenant les numéros de séries. 
# 2) La colonne format factor contenant la typologie (la colonne qui supervise l'appartenance à tel ou tel groupes. 
# La recherche de ces colonnes peut faire l'objet d'une recherche automatique. 
{
library(dplyr)
library(magrittr)
library(FactoMineR)
library(kernlab)
library(MASS)
library(mda)
library(stringr)
library(e1071)
# fonction de création du data.frame d'entrainement du modèle SVM 
# On choisit aléatoirement , pour chaque modèle, p mopieurs (p supérieur à 50 de préférence)
# plus p est élevé, plus le risque de sur-apprentissage est élèvé. Cela ne pose pas nécessairement de difficulté sur un parc 
# de maéteriel relativement immobile. 
# Le but étant de prédire l'appartenance à un type de fabrication en partant du numéro de série. 

buildDT <- function(gere,p)
{
idNoSerie <- gere$Numéro.de.série
#idNoSerie <- str_sub(gere$Numéro.de.série,1,1) #on élimine les derniers termes des numéros de série car ils introduisent 
#un biais important sur la détection des modèles MS610dh , MX711) 

models <- levels(as.factor(gere$Modèle)) 
modelsEtudies <- c("MP 2554","MP 2555","MP C3003","MS610dn","MX511dhe","MX711")
modelsEtudies <- c("MP 2554","MP C3003","MS610dn","MX511dhe","MX711")
models <- intersect(models,modelsEtudies)
x <- sapply(models,function(fact) {
	sample(subset(gere$Numéro.de.série,gere$Modèle==fact),p,replace=TRUE)
	})
dfx <- as.data.frame(x) 
return(data.frame(noserie=as.character((x)),is_modèle = gl(length(models),p,labels=models)))
}
# fonction auxilliaire pour l'élaboration de matrice de fréquence. 
mindHertz <- function(noserie,pattern)
{
df.mineHz <- sapply(pattern,function(p) {stringr::str_count(noserie,p)}) 
colnames(df.mineHz) <- pattern		
return(df.mineHz)		
}
"-----------------------------------------------------------------------------------------"
mindHertzX <- function(noserie,pattern)
{
df.mineHz <- t(sapply(noserie,function(serialNumber) {
				sapply(pattern,function(p) {
					sum(1/str_locate_all(serialNumber,p)[[1]][,1])})}
						)		
)
return(df.mineHz)
}
"-----------------------------------------------------------------------------------------"
patternRecurrence <- function(noserie,pattern)
#arguments: monnoserie=une chaîne de caractère ; pattern : un vecteur de motifs alphanumériques et/ou d'expressions régulières:
#ex: monnoserie : "70155PHH0WP3H", pattern : ["H+","0","W","P"]
#En sortie : un data_frame recensant le nombre de motifs apparus pour chaque arguments 
#ex: colnames =>	H+ 	0	W	P
#70155PHH0WP3H		2	2	1	2 
{
mineHzX <- sapply(pattern,function(p) {sum(1/str_locate_all(noserie,p)[[1]][,1])})
df <- data.frame(t(mineHzX))
colnames(df) <- pattern	
return(df)
}

#mineHz <- sapply(pattern,function(p) {stringr::str_count(noserie,p)})
# try function : stri_count_fixed(c("70155PHH0WP3H"), c("H+","0","W","P"))
#df <- data.frame(t(mineHz))
#colnames(df) <- pattern		
#return(df)		

### CHOIX DES pattern POUR L ELABORATION DE LA MATRICE DES FREQUENCES.
pattern <- setdiff(stringr::str_c(0:9),c("0","1","2","3","8","9")) #Les motifs recherchés sont les chiffres en simple
pattern <- append(pattern,
	setdiff(LETTERS[c(1:26)],
		c("A","B","D","E","F","H","I","J","K","L","O","P","Q","R","S","T","U","V","W","X"))) 
pattern <- append(pattern,c("[JPN]"))
#Attention, il ne peut être choisi un caractère dont la fréqence serait la même dans un groupe donné. 
# Par exemple, le choix de "E" est prohibé puisque la config 7a a tous ses n° de série contenant E.
#De même, l'expression régulière 7463 ne conviendrait pas à cause de la config 2b.
#L'erreur provoquée est la suivante : Error in lda.default(x, grouping, ...) : variable n°9 appears to be constant within groups signifiant 
#une variable constante sur l'ensemble d'un groupe provoque des matrices singulières non diagonalisable. 
#pattern <- append(pattern,c("M","P","X")) #Quels pattern pour discriminer au plus les modeles entre eux ? 
#noSeries <- subset(gere[,2],gere[,2] != "NO70FC") #exception prejudiciable au modele à exclure 
# pp=4 machines pour constitution de l'échantillon de test. 
# p=200 machines de chaques classes pour constitution de l'échantillon d'apprentissage. 

#gere <- gere[-which(gere[,2]=="NO70FC"),]

p <- 500
pp <- 3
data.training 	<- buildDT(gere,p)
data.test 		<- buildDT(gere,pp)	
"--------------------------------"
data.dico <- data.frame("noserie"=gere[,2],"is_modele"=gere[,5])
#t(sapply(pattern,function(mot){sum(stringr::str_count(extractNoserie[k],mot))*100/str_length(extractNoserie[k])}))									
df.training <- mindHertzX(data.training[,1],pattern)
df.test 	<- mindHertzX(data.test[,1],pattern)
"---------A ACTIVER POUR VISUALISER LES CORRELATIONS DE VARIABLES ------------------------------------------"
#df.dico		<- mindHertzX(data.dico,pattern)			
#df.training.PCA <- df.dico							
"-----------------------------------------------------------------------------------------------------------"
df.training	<- data.frame(df.training,"is_modele"=factor(data.training[,2]))
df.test 	<- data.frame(df.test,"is_modele"=factor(data.test[,2]))
#---Analyse Correspondances occurences X models---#
df.CorrAnalyse <- aggregate(df.training,list(df.training$is_modele),mean)
rownames(df.CorrAnalyse) <- df.CorrAnalyse[,1]
df.CorrAnalyse <- df.CorrAnalyse %>% .[,-1]
df.CorrAnalyse <- df.CorrAnalyse[,-dim(df.CorrAnalyse)[2]]
#---Regression multiples pour élimination des variables peu discriminantes
y <- as.numeric(df.training$is_modele)
X <- df.training[,-dim(df.training)[2]]
reg <- lm(y~.,X)
#modeles  #	0		1		2		3		4		5		6		7		8		9
#---------------------------------------------------------------------------------------#
#C3003		0.72	1.56	0.81	1.38	0.41	2.23	0.55	0.33	0.44	0.40
#MP2554		1.94	1.78	0.40	0.40	1.47	1.12	0.58	0.36	0.37	0.38	
#MX711		1.34	0.30	0.35	1.40	1.09	1.05	3.10	1.10	0.16	0.14
#MX511		2.04	1.13	0.06	0.06	0.2		2.14	0.12	1.28	0.06	0.07
#MS610		0.10	1.19	1.03	0.25	2.10	2.14	0.46	0.23	0.14	0.14
#---------------------------------------------------------------------------------------#
#--- Fin analyse de variance ------------------------#
names(df.training) 	<- c(pattern,"is_modele")
names(df.test) 		<- c(pattern,"is_modele")
modele.lda <- lda(factor(is_modele) ~ .,data=df.training) #Package MASS
#modele.mda <- mda(factor(is_modele) ~ .,data=df.training) #Package MDA
modele.svm <- svm(is_modele~.,data=df.training) #Package e1071
#------------KERNLAB MODELE ET PREDICTIVITE--------------------------------------------------------------------------------------------------------#
modele.kernlab <- ksvm(is_modele~.,data=df.training,
		type="C-bsvc",
		kernel="rbfdot",
		kpar=list(sigma=0.1),
		C=10,
		prob.model=TRUE) 	 							 #Package kernlab
#modele.kernlab <- ksvm(is_modele~.,data=df.training,type="C-bsvc",kernel="rbfdot",prob.model=TRUE)		
#------------------------------------------------------------------------------------------------------------------
#models <- c("C3003","MP2554","MX711","MX511","MS610")
models <- levels(as.factor(gere$Modèle)) 
modelsEtudies <- c("MP 2554","MP 2555","MP C3003","MS610dn","MX511dhe","MX711")
modelsEtudies <- c("MP 2554","MP C3003","MS610dn","MX511dhe","MX711") 
#LAISSER LE MODELE MP 2555 DEGRADE ENORMEMENT LA QUALITE DU MODELE POUR LA DETECTION DES MS610DN
#DECISION: ABANDONNER LE MODELE MP 2555 DANS L ENTRAINEMENT DE L ALGORITHME
models <- intersect(models,modelsEtudies)
lprop <- round(predict(modele.kernlab,patternRecurrence(monnoserie,pattern),type="probabilities"),2) #prédictibilité selon le package kernlab
#lprop <- round(predict(modele.svm,patternRecurrence(monnoserie,pattern), probability = FALSE),2) #prédictibilité selon le package e1071
print(stringr::str_c("la predictivité du ",monnoserie," concernant Cle modele ",models," est de ",lprop[end(lprop)[1],]*100,"%")) 
#selon le package e1071 ..... 
a <- modele.svm ; x <- patternRecurrence(monnoserie,pattern)
print(stringr::str_c("selon le package e1071, la prédicitibilité du ",
monnoserie," est le modele ",
predict(a,x,probability = TRUE)))
#------------------AFFICHAGE DE PREDICTIBILITE DU MODELE-------------------------------------------------------------------------------------------#
lprop <- lprop[end(lprop)[1],]
print("force de predicitibilite du modele")
print(lprop)
#------------------Fin module de PREDICTIBILITE DU MODELE------------------------------------------------------------------------------------------#
#------------KERNLAB MODELE -----------------------------------------------------------------------------------------------------------------------#
ldaTab <- table(df.test$is_modele,predict(modele.lda,df.test)$class)
#mdaTab <- table(df.test$is_modele,predict(modele.mda,newdata=df.test,type="class"))
svmTab <- table(df.test$is_modele,predict(modele.svm,newdata=df.test,type="class"))
ksvmTab <- table(df.test$is_modele,predict(modele.kernlab,newdata=df.test,type="response")) #Package kernlab
df.test.lda <- data.frame(df.test,"predict.class"=predict(modele.lda,df.test)$class)
#data.training <- data.frame(data.training,"predict.class"=predict(modele.lda,df.training)$class)
#modele mda
#df.test.mda <- data.frame(df.test,"predict.class"=predict(modele.mda,df.test,type="class"),"modele"=config[predict(modele.mda,df.test,type="class")])
#df.test.svm <- data.frame(df.test,"predict.class"=predict(modele.svm,df.test,type="class"),"modele"=config[predict(modele.svm,df.test,type="class")])
#data.training <- data.frame(data.test,"predict.class"=predict(modele.mda,df.test,type="class"))
#plot.new()
viewPCA <- PCA(df.training,axes=c(1,2),graph=FALSE,quali.sup=length(pattern)+1)
plot.PCA(viewPCA,habillage=length(pattern)+1,label="none")
#Mon numero de série saisie noserie en arguments 
df.test <- cbind(round(predict(modele.kernlab,df.test,type="probabilities"),2),data.test)
return(list(modele.kernlab,df.training,df.test,viewPCA,ldaTab,ksvmTab,df.CorrAnalyse,reg,data.training))
#summary(sapply(1:100,function(i){(1-sum(diag(nsPredict(gere)[[2]])/500))*100})) : pour vérifier le modèle. 
}
"----------------------------------------------------------------------------------------------------------"
"----------------------------------------------------------------------------------------------------------"
"----------------------------------------------------------------------------------------------------------"
"----------------------------------------------------------------------------------------------------------"
print("atelier(): une fonction de travaux dirigé sur diverses notions de statistiques") 
atelier <-  function(n)
{
#génération d'un data.frame de 2000 Hommee et Femmes  avec leurs ages respectifs
genre <- factor(round(runif(n)),labels=c("H","F"))
age <- rnorm(n,50,15)
breaksAge1 <- cut(age,breaks=c(seq(0,90,10),120))
breaksAge2 <- cut(age,breaks=seq(0,120,30))
df.pop <- data.frame("genre"=genre,"age"=round(age),"tranche1"=breaksAge1,"tranche2"=breaksAge2)
table.pop <- table(df.pop$genre,df.pop$tranche1)
print(table.pop)
return(df.pop) 
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
bilanGen <- function(gere)
{
dfGeneral <- data.frame("modele"=gere[,5],"consommation"=gere[,15])
res <- tapply(dfGeneral$consommation,dfGeneral$modele,function(i) {sum(i)})
return(res)
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("dory(gere) : une fonction bien pratique pour répondre aux ventilations des matériels solimp 2 demandées par Daniel Ory")
library("FactoMineR")
library("ggplot2")
# df.ACM <- df[,c(2,7,9,47)] ; df.ACM$LocalisationLivraison <- factor(str_match(df.ACM[,4],"[A-Z]+"))
# df_xx <- data.frame("x"=c(rnorm(1000,50,50),rnorm(1000,50,25),rnorm(1000,50,10)),"tag"=gl(3,1000))
dory <- function(gere)
{
df <- data.frame("no"=gere$Numéro.de.série,
							"modele"=gere$Modèle,
							"compt.gen"=gere$Compteur.machine,
							"couleur"=gere$"Total.couleur",
							"scan"=gere$Total.scan,
							"A3"=gere$Copie....A3)
#Une analyse en composantes principales n'est pas adaptée pour df à cause d'une trop forte corrélations des variables
#L'idée est alors de discrétiser judicieusement les variables catégorielles et d'effecteur alors une analyse de correspondances multiples
#La discrétisation des variables catégorielles doit se faire après leur centrage et réduction. 
#En effet, la discrétisation sans centrage - réduction, sur des variables fortements corrélées conduit à une discrétisation 
#qui reste elle aussi très corrélée. 
#df <- data.frame("no"=gere$Numéro.de.série,
#							"modele"=gere$Modèle,
#							"compt.gen"=scale(gere$Compteur.machine),
#							"couleur"=scale(gere$"Total.couleur"),
#							"scan"=scale(gere$Total.scan),
#							"A3"=scale(gere$Copie....A3))	
						
df <- replace(df,is.na(df),0)
df <- subset(df,df$modele != "MS510dn")

qplot(df$compt.gen,geom="density",fill=df$modele)

df2554	<- subset(df,df$modele == "MP 2554")
df3003	<- subset(df,df$modele == "MP C3003")
df4054	<- subset(df,df$modele == "MP 4054")
df711	<- subset(df,df$modele == "MX711")
df511	<- subset(df,df$modele == "MX511dhe")
df2 	<- rbind(df511,df711)
df2Facturation <- merge(df2,dfscore,by.x=1,by.y=1)
df3		<- rbind(df2554,df3003)
df3 	<- replace(df3,is.na(df3),0)
#----------------------------- DECOUPAGE EN CLASSES ---------------------------------------------------#

#class.compt  <- cut(df3$compt.gen,breaks=c(0,2.6e4,4.7e4,1e5,1.55e5,1.93e5,Inf),labels=1:6)	
class.compt  <- cut(df3$compt.gen,breaks=c(-Inf,55e3,86500,127125,Inf),labels=1:4)	
class.SCAN  <- cut(df3$scan,breaks=c(-Inf,3510,12232,Inf),labels=c("SCAN","SCAN+","SCAN++"))
class.A3  <- cut(df3$A3,breaks=c(0,211,495,813,1257,1872,3073,Inf),labels=c("A3--","A3-","A3n","A3+","A3++","A3!+","A3!!"))

#----------------------------- DECOUPAGE EN CLASSES ---------------------------------------------------#
df3b <- df3
df3 <- cbind(df3,class.compt,class.SCAN,class.A3)

df3Facturation <- merge(df3,dfscore,by.x=1,by.y=1)
#Super data.frame contenant toutes les données de nature géographiques et fonctionnelles. 
#{départememnt, type de structure (centre foncier, recouvrement, contrôle fiscal,domaine etc..)}.
df3GF <- merge(df3Facturation,geode,by.x=1,by.y=23)
xx <- df3GF[order(df3GF$compt.gen,decreasing=T),] 
#xx <- replace(xx,is.na(xx),0)

#Analyse Factorielle multiple 
df3.ACM <- df3b
df3.ACM <- replace(df3.ACM,is.na(df3.ACM),0)
df3.ACM$modele <- droplevels(df3.ACM$modele)
#Le découpage effectué manuellement ci-dessous a été spécialement étudié dans le but d'optimiser l'analyse correspondance multiple
#La visualisation des découpages optimaux s'est faite en utilisant différents scénarios avec l'estimation par noyau : kernelEstimator(s,h)

df3.ACM$compt.gen <- class.compt
df3.ACM$A3			<- class.A3
df3.ACM$scan			<- class.SCAN
df3.ACM$no <- NULL

#Table de contingence générale en sortie n°10
#affichage préconisé : plot(res,axes=c(1,4),cex=1,select="cos2 6000",habillage="quali",invisible=c("ind"))
#table préconisée : 
gere3 <- subset(gere,gere[,5] == "MP 2554" | gere[,5] == "MP C3003")
x <- gere3[,c(5,15,29,37)] #15 pour les compteurs généraux)#29 pour les scan et 37 pour les A3
x <- replace(x,is.na(x),0)
#
classCompteur <- cut(x[,2],breaks=c(0,17000,32000,50000,80000,Inf),labels=c("<17000","<32000","<50000","<80000","<Inf"))
print("nombre de classes (3 à 8)")
p <- scan(n=1)
classCompteur <- cut(x[,2],breaks=c(0,quantile(x[,2],probs=seq(0,1,1/p))[2:p],max(x[,2])),labels=str_c("C.",1:p))
classScan 	<- cut(x[,3],breaks=c(0,quantile(x[,3],probs=seq(0,1,1/p))[2:p],max(x[,3])),labels=str_c("SCAN.",1:p))
classA3 	<- cut(x[,4],breaks=c(0,quantile(x[,4],probs=seq(0,1,1/p))[2:p],max(x[,4])),labels=str_c("A3.",1:p))
# 
x <- cbind(x,classCompteur,classScan,classA3)
xs <- x
xs$Compteur.machine <- NULL
xs$Total.scan 	<- NULL
xs$Copie....A3	<- NULL
# execution de table
xtable <- list(table(x[,1],x[,5]),table(x[,1],x[,6]),table(x[,1],x[,7]))

print(names(df))
print(nrow(df))
print("ventilation des equipements")
print("1:df général,2:df511+711,3:df2554+3003,4:df3003,5:df4054,6:df511,7:df711,9:df3.ACM,10:Xtable,11:df.ACM")
write.table(contingencyTab(df,2,3,5),file="S_contingencyTab.csv",sep=";",row.names=T,dec=",")
print("PHASE Daniel.ORY terminée ")
return(list(xx,df2,df3,df2554,df3003,df4054,df711,df511,df3.ACM,xtable))
#Liste des utilisations de ces différents data.frame. 
#boxplot : boxplot(df$compt.gen~df$modele,subset=df$modele != "MP 4054",col=rainbow(8),las=3)
#qplot :	qplot(df2$compt.gen,geom="density",fill=df2$modele,alpha=0.8)
#			qplot(df3$compt.gen,geom="density",fill=3,alpha=0.8)
#cutOptim: cutOptim(df2$compt.gen)
#MCA : plot(MCA(df3.ACM),axes=c(1,4),cex=1,select="cos2 6000",habillage="quali",invisible=c("ind"))
#MCA : plot(MCA(A[[9]]),axes=c(1,2),cex=1,select="cos2 6000",habillage="quali",invisible=c("ind"))

}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("solimp.nnet():un perceptron pour modéliser les 3 variables que sont compteur, scan et A3")
solimp.nnet <- function(df)
{
x <- cbind(df$compt.gen,df$scan,df$A3)
y <- PCA(x$ind$coord)
df.nnet <- data.frame("x"=y[,1],"y"=y[,2],"z"=y[,3],"modele"=(df$modele-1)/2)
res.nnet <- neuralnet(modele~ x + y + z,df.nnet,hidden=c(10,5))
return(res.nnet)
}
print("conf37.AFD(df):une analyse discriminante sur les 3 variables que sont compteur, scan et A3")
solimp.AFD <- function(df) #df <- dory(gere)[[1]]
{
library(MASS)
df.AFD <- data.frame(cbind((as.numeric(df$modele)-1)/2,scale(df$compt.gen),df$scan/df$compt.gen,df$A3/df$compt.gen))
colnames(df.AFD)=c("is_3003","compt.gen","scan","A3")
df.qda <- qda(formula=is_3003 ~ .,data=df.AFD)
table.AFD <- table(as.factor(df.AFD$is_3003),predict(df.qda,newdata=df.AFD)$class)
return(table.AFD)}
#Cette analyse discriminante aboutit à un modèle prédictif catastrophique [parc config3 et config7]
#Sur 5930 MP2554 et 791 MP C3003, le modèle aboutit au classement ci-dessous
"|---------------------------|"
"| pred=> | 2554 |3003|TOTAL |"
"|---------------------------|"
"|MP 2554 | 5865 | 65 | 5930 |"                                            
"|---------------------------|"
"|MP 3003 | 766  | 25 | 791  |"
"|---------------------------|"
"|TOTAL   | 6631 | 90 | 6721 |"
"|---------------------------|"
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("kernelEstimator(s,h) : pour lisser un échantillon sur sa loi de densité")
#Par défaut, le noyau gaussien est utilisé 
kernelEstimator <- function(s)
{if (!is.numeric(s)) {stop(" le type de données doit être un vecteur !")}
h <- dpik(s)
n <- length(s)
phi <- function(x) {return(1/sqrt(2*pi)*exp(-x^2/2))} #noyau gaussien
kernEst <- function(x,s,n,h) {return(1/(n*h)*sum(phi((x-s)/h)))} 
f <- function(x) {return(sapply(x,function(i){kernEst(i,s,n,h)}))}
curve(f,min(s),max(s),col=4,lwd=3)
return(f)
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("kernSmooth(x) : pour des découpages de variable optimisée")
library("KernSmooth")
cutOptim <- function(x)
#ATTENTION: activer la fonction dpih() du package KernSmooth
{if (!is.numeric(x)) {stop(" le type de données doit être un vecteur !")}
h <- dpih(x) 
bins <- seq(min(x)-h,max(x)+h,by=h) 
hist(x,col="yellow") 
hist(x,breaks=bins,col=rainbow(156),add=F)
return(bins)
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("gini(x,nb):indice de gini d'une distribution: cas des relevés compteurs sur solimp 2")
gini <- function(x,nbQuantiles) #nbQuantile est généralement = à 100 i.e. decoupage de la distribution en 100 quantiles 
#x doit être un vecteur 
{
#Vérification du type de données : 
if (!is.numeric(x)) {stop(" le type de données doit être un vecteur !")}
x <- sort(x)
k <- ceiling(length(x)/nbQuantiles) 
if (k*nbQuantiles > length(x)) {k <- floor(length(x)/nbQuantiles)}
u <- sapply(1:(nbQuantiles-1),function(i) {sum(x[1:(k*i)])})
u <- append(u,sum(x))/sum(x)
plot((1:nbQuantiles)/nbQuantiles,u,type="l")
u <- append(0,u)
v <- sapply(1:(length(u)-1),function(i) {(u[i]+u[i+1])/2})
coefficient.gini <- 1-sum(1/nbQuantiles*v)*2 
return(list(0:nbQuantiles/nbQuantiles,u,coefficient.gini))
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
print("VarAnalysis(): #Analyse de variance et test de Fisher")
VarAnalysis <- function()
{
T <- matrix( c(1248,1392,1057,3159,891,1065,1118,2934,1138,1456,1224,3090),nrow=3,byrow=T)
N <- dim(T)[1] 
p <- dim(T)[2]
Sa <- sum((apply(T,1,mean)-mean(T))^2)*p # DDL : N-1
Sp <- sum((apply(T,2,mean)-mean(T))^2)*N # DDL : p-1
S <- sum((T - mean(T))^2) #DDL : N*p-1
Sr <- sum((T - mean(T))^2) - Sa - Sp #DDL: (p-1)(N-1)
# Test de saisonnalité : Test de Fisher pour les tableaux de petites tailles . Test du Khi2 sinon. 
#commande R : fisher.test(mytable) 
if ((Sp/Sr*1/(N-1)) > qf(0.95,p-1,(N-1)*(p-1))) {print("rejet H0 : La série est saisonnière")}
print(str_c("F empirique = ",Sp*(N-1)/Sr,"F theor:",qf(0.95,p-1,(N-1)*(p-1)),sep=" "))
# Test de tendance :idem 
if ((Sa/Sr*1/(N-1)) > qf(0.95,N-1,(N-1)*(p-1))) {print("rejet H0 : La chronique est affectée d'une tendance. Refaire le test après élimin.tendance")}
}
"------------------------------------------------------------------------------------------------------------------------------------------------------"
#"----------------------------------FONCTION D AUTOCORRELATION SUR LES TERMES DES RESIDUS DE REGRESSION -----------------------------------------------"
print("correlogramme(lm_class)")
correlogramme <- function(lm_class) #un object de class lm (regression linéaire) 
#Fonction d'autocorrélation sur la série et un décalage de rang k: Corrélogramme sur les résidus d'une régression linéaire 
#Bibliographie: Analyse des séries temporelles (page 15)
{
x <- lm_class$residuals ; n=length(x)
#x <- c(1248,1392,1057,3159,891,1065,1118,2934,1138,1456,1224,3090)
xi <- NULL
for (i in seq(1,n,2)){xi <- c(xi,x[i])} #Prélèvement de E(n/p) périodes sur la série chronologique: Ici tous les 7 unités de temps. (J/S/M/A)  
x <- xi;n=length(x)
m <- matrix(rep(0,n^2),nrow=n);m[1,] <- x
v <- NULL;ddl <- NULL;vtc <- NULL;vt_stud <- NULL;decision <- NULL;w <- NULL
"---------------------------------------------------------------------------------------------------------------------"
for (k in 1:(n-2)) 
{
	m[k,] <- c(rep(0,k-1),x[k:n])
	rk <- cor(m[k,k:n],x[1:(n-k+1)]) #coefficient de corrélation rk des séries décalées de k 
	v <- append(v,rk) #compilation des rk dans v
	ddl <- append(ddl,n-k-1) #degrés de liberté pour le calcul de student
		if (abs(rk) > 0.9999) {tc <- Inf}
		else { tc <- abs(rk)/sqrt(1-rk^2)*sqrt(n-k-1)} #le tc du corrélogramme indéfini quand rk = 1
	t_stud <- qt(0.975,(n-k-1)) #la t-limite du test de student au risque 5% 
		if (tc > t_stud ) { if (rk > 0) 
							 {decision <- append(decision,"OK! r significatif <>0")} else {decision <- append(decision,"KO! r significatif <>0")} }
							else {decision <- append(decision,"KO! r no signific <> 0")}
						  
	vtc <- append(vtc,tc) #Compilation des t correlogramme dans vecteur vtc
	vt_stud <- append(vt_stud,t_stud) #Compilation des p-value vecteur vt_stud
} #Fin For 
#Mise en forme du data.frame de synthèse 
p <- n-2
tableau_autocorr <- data.frame(
"retards" = 0:(p-1), 
"r" = round(v[1:p],3),
"ddl"=ddl[1:p],
"tc:" = round(vtc[1:p],3),
"t à 95%:" = round(vt_stud[1:p],3),
"decision:" = decision[1:p])

yk <- barplot(v[1:p])
lim_rk <- sqrt(vt_stud^2/((p:1) + vt_stud^2))
points(yk,lim_rk,col=2,pch=16,cex=3/4,type="b")
points(yk,-lim_rk,col=2,pch=16,cex=3/4,type="b")
return(tableau_autocorr) 
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
#Methode de theil permet de corriger les valeurs aberrantes dans une régression
#Le package R "mblm" met en oeuvre la méthode de theil pour les modèles linéaires. "quantreg" est un package plus performant. 
"Principe: Soit n points (xi,yi). A chaque couples de points Ai1(xi1,yi1) Ai2(xi2,yi2), on définit le coefficient directeurs a  de la droite passant 
par Ai1 et Ai2 : (yi2-yi1)/(xi2-xi1). En passant en revue les n.(n-1)  couples de points on récolte l'ensemble des coeff a possibles. C'est la médiane de 
cette collection qui donne la meilleure pente possible de régression en effacant en quelque sorte le point aberrant." 
print("theil(w): w vecteur de réels. produit la droite d'ajustement par la méthode de theil")
theil <- function(w) #un vecteur de réels 
{
n <- length(w)
fr <- data.frame(i=1:n,v=w)
m1 <- matrix(rep(0,n^2),nrow=n)
for (i in n:1) {
	for (j in 1:(i-1)) {
	m1[i,j] <- j-i
	m1[j,i] <- -m1[i,j]
}
} 
m2 <- matrix(rep(0,n^2),nrow=n)
for (i in n:1) {
	for (j in 1:(i-1)) {
	m2[i,j] <- fr$v[j]-fr$v[i]
	m2[j,i] <- -m2[i,j]
}
} 
a <- median(m2/m1,na.rm=T)
b <- median(fr$v-fr$i*a,na.rm=T)

return(c(a,b))
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("colsort(data.frame,decrement")
colsort <- function(datafr,decrement) 
#le nom de la colonne ne peut pas être spécifié car le datafr indiquent les relevés hebdo ou journaliers
# le datafr contient aussi dans les 3 dernières colonnes a_coeff b_coeff et aberr. 
# pour trier suivant les derniers relevés, on utilise en général un décrément (entier) égal à 3 
{
l <- dim(datafr)[2] 
p <- l - decrement 		# decrement à -3 pour trier suivant la colonne des commpteurs
datafr <- datafr[order(datafr[,p],decreasing=T),] 
return(datafr)											
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("unicite(x,k): # suppression des doublons en précisant le rang (k) de la colonne concernée pour un data.frame (x) à n>1 colonnes"  )
unicite <- function(x,k) # suppression des doublons en précisant le rang (k) de la colonne concernée pour un data.frame (x) à n>1 colonnes 
{ 
return(x[!duplicated(x[,k]),])
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"

"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("resume(facteurs,variables quantitatives associées aux facteurs): effectue un regroupemment par facteurs de var quantitatives")
print("#pour accéder aux différents résultats de la liste retournée, récupérer la liste dans summaries et saisir summaries[[k]]
#k varie de 1 à 7 
#k=1 : Noms des groupes
#k=2 : Montants numériques par groupe
#k=3 : Resultats en cumul total
#k=4 : Resultats en résumé
#k=5 : Resultats en moyennes 
#k=6 : Resultats en variance
#k=7 : Resultats en maximum de série")
resume <- function(colonne_datafr,colonne_numeric) {
#colonne_datfr est la colonne du dataframe  permettant l'agrégation par Groupes (HMX$PROJET par exemple) et doit être de type as.factor()
#colonne_numeric est la colonne du dataframe exclusive numeric comme HMX$Montant.paye par exemple
groupe <- as.factor(colonne_datafr)
noms_groupe <- levels(colonne_datafr) #a pour effet de synthétiser les noms de chaque groupe #
v_numeric <- as.numeric(as.vector(colonne_numeric)) 
res <- tapply(colonne_numeric,groupe,function(i) {i}) 
res.sum <-sort(tapply(colonne_numeric,groupe,function(i) {sum(i)}),decreasing=TRUE)
res.summary <- tapply(v_numeric,groupe,function(i) {summary(i)})  
res.mean <- sort(tapply(v_numeric,groupe,function(i) {mean(i)}),decreasing=TRUE) 
res.sd <- sort(tapply(v_numeric,groupe,function(i) {sd(i)}),decreasing=TRUE) 
res.max <- sort(tapply(v_numeric,groupe,function(i) {max(i)}),decreasing=TRUE)
return(list(list(groupe),res,res.sum,res.summary,res.mean,res.sd,res.max))
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
#exemple : 
"
summaries <- resume(histo11$modele,histo11$ind_fact.x) 
v <- levels(unlist(summaries[[1]]))
x <- lapply(summaries[[2]],function(list) {log(list)})
boxplot(x,names=v)
wilcox.test(x[[1]],x[[2]])"
contingencyTab <- function(df.data,colonneFacteur,colonneNumerique,nbClasse) 
#nbClasse=0 : découpage manuel sinon découpage automatique
#un dataframe et 2 entiers + 1 entier nb de classes de découpe
#découpe la var.numérique x en 10 effectifs égaux et renvoie le break associé pour usage de la fonction resume#
#par exemple, x pourra être histo11[,3] les relevés compteurs de Kapx [voir script vscc.r]#
#df <- cbind(histo11[c(2,3)],cut(x,breaks=c(0,x[round(seq(0,length(x),length(x)/10))])))
#df <- data.frame("model" = histo11[,2],"releve"=histo11[,3],"classe"=medianeDix(histo11[,3]))
{
df.data[,colonneFacteur] <- droplevels(df.data[,colonneFacteur])
x <- df.data[,colonneNumerique] #selection de la colonne du dataframe
#if (nbClasse > 0) {brks 	<- cut(x,breaks=seq(min(x),max(x),(max(x)-min(x))/nbClasse))} #decoupage inadapté pour les distributions non symétriques
if (nbClasse > 0)  {brks 	<- cut(x,breaks=c(min(x),quantile(x,probs=seq(0,1,1/nbClasse)[2:nbClasse]),max(x)))} 
#cut(x[,3],breaks=c(quantile(x[,3],probs=seq(0,1,1/5))[2:5],Inf))
		else 
		{
			print("saisir le nombres de classes")
			nb <- scan(n=1)
			print("saisir seq. les seuils manuellement")
			v <- scan(n=nb) 
			brks <- cut(x,breaks=v)
		}
#brks <- cut(x,breaks=c(0,75000,175000,240000,Inf))
df <- data.frame("model" = df.data[,colonneFacteur],"releve"=df.data[,colonneNumerique],"classe"=brks)
return(table(df$model,df$classe))
}

"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("weeklySurvey_C3003(histoKapx,surveyType,color): littéralement RelevésHebdomadaires_C3003")
weeklySurvey_C3003 <- function(histoKapx,surveyType,color) #histoKapx un array de data.frames ; 
#surveyType est un entier spécifiant le type de relevé : 0 pour journalier et 1 pour hebdomadaire.   
#color prend soit 0 soit 1. 1 spécifie un relevé comptatible avec les ricoh C3003, mopieurs couleurs 
{
 #seules les 30 variables sont utilisées 
if (color == 1) #pour les photocopieurs couleurs uniquement
{coeff <- 10} #Les impressions couleurs coutent 10 foix plus cher que les N&Blancs
else{coeff <- 0}

k=dim(histoKapx)[2] 
#coeff*unlist(histoKapx[17,k]) : relevés couleurs en colonne 17. A insérer après debbugage. 
serie <- data.frame("n.serie" = unlist(histoKapx[2,k]),
		"modele " = unlist(histoKapx[5,k]),
		"ind_fact" = 1/1000*(unlist(histoKapx[15,k])),
		stringsAsFactors=FALSE,check.names = F)	
# Annonce Debbugage 
"lorsque l erreur type !Error in .subset2(x, i, exact = exact) : subscript out of bounds! se produit,cela signifie qu'un équipement 
non conforme s est inséré dans les historiques hebdomadaires. il faut alors le détecter et le supprimer des tous les historiques le contenant
dernière anomalie le 12/01/2016 avec équipement 701632HH01RGB qui etait dans géré KPAX et qui a basculé dans non géré. 
L'erreur s'est alors produite lors de la jointure des fichiers hebdomadaires"
if (surveyType == 0) {surveyType <- 1} else {surveyType <- 1}
j <- 0
for (i in (k-1):1) 
	{
	#Relevé journalier ou hebdo suivant 
		survey <- data.frame("n.serie" = unlist(histoKapx[2,i]),"ind_fact" = 1/1000*(unlist(histoKapx[15,i])))
	#Vérification que tous les n° série de ce relevé figurent aussi dans le relevé série (sinon !Error in .subset2(x,1,exact = ex ....) 
	"Pour cette vérification, on construit le relevé par intersection des n° de séries en commun" 
		survey <- subset(survey,is.element(survey[,1],serie[,1]))
		serie <- unicite(serie,1)
		survey.from <- str_sub(max(unlist(histoKapx[14,i])),1,10)
		
		serie <- merge(serie,survey,by.x=1,by.y=1,all=T)
		colnames(serie)[4+j] <- survey.from
		j <- j+1
	}
	
	
	return(serie)
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"

"---------------------------------------------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
	print("lissage2(histo,p)")
lissage2 <- function(histo,p) #p est la période p=7 lissage hebdomadaire. fonction plus générale qui lisse par moy mobiles tout l'ensemble
{ 
	dates <- names(histo)
	y <- ts(as.matrix(histo[,3:ncol(histo)])) #ts : série temporelle 
	y1 <- sapply(1:dim(histo)[1],function(i) {filter(y[i,],rep(1/p,p),side=1)}) # filter: lissage moyenne mobile
	histo <- cbind(data.frame(histo[,1:2]),data.frame(t(y1)))
	colnames(histo) <- dates
	return(histo) #retourne un data.frame avec série brute et lissée
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
survey_smooth <- function(histo,p) #p est la période p=7 lissage hebdomadaire. fonction plus générale qui lisse par moy mobiles tout l'ensemble
{ 
	dates <- names(histo)
	y <- ts(as.matrix(histo[,3:ncol(histo)])) #ts : série temporelle 
	y1 <- sapply(1:dim(histo)[1],function(i) {filter(y[i,],rep(1/p,p),side=1)}) # filter: lissage moyenne mobile
	histo <- cbind(data.frame(histo[,1:2]),data.frame(t(y1)))
	colnames(histo) <- dates
	return(histo) #retourne un data.frame avec série brute et lissée
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("lissage(survey,p)")
lissage <- function(survey,p) #p est la période p=7 lissage hebdomadaire
{
	y <- ts(survey) #ts : série temporelle 
	y1 <- filter(survey,rep(1/p,p),side=1) # filter: lissage moyenne mobile 
	return(data.frame("s_brut"=y,"s_lissee"=y1)) #retourne un data.frame avec série brute et lissée
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("kernel_regss: parcours le data.frame en totalité et édition des copieurs qui dépassent la zone de prévision pour Alerte !")
print("Plus efficient que la détection par point aberrants qui ne donnent pas pleine satisfaction concernant la détection des alertes")
print("fonction développée pour le scipt vscc.r et concernant plus particulièrement histo111. ")
print("fonction dérivée de regss_ns pouvant être considérée comme noyau primitif")
kernel_regss <- function(histo) #fonction développée pour le scipt vscc.r et concernant plus particulièrement histogramme1. 
{
	histo <- replace(histo,is.na(histo),0)
n <- nrow(histo) 
p <- ncol(histo)
nserie <- NULL
crossedLimit <- NULL #Limite de démarcation: les relevés qui dépassent les lignes de prédictions (crossedLimit) sont consignés 
for (i in 1:n)
	{
	fr <-as.double(histo[i,3:(p-3)]) #les relevés sont en colonnes 3  à dim(fr)[2]-3 (ncol(fr)-3)  
	fr <- replace(fr,is.na(fr),0)	
	liss <- lissage(fr,7) #moyenne mobile hebdo (p=7) de la série brute du n° série du data.frame histo
	k <- nrow(liss)
	x <- 1:k
	reg <-  lm(liss$s_brut		~poly(x,1,raw=T),data=liss) #regression brute
	#Predictions avec intervalle de prédiction 0.50
	newdata <- data.frame("x"=1:k)
	pred <- predict(reg,newdata,interval="pred",level=0.99) #ne pas confondre intervalle de confiance et intervalle de prédiction !! .
	
	outbound <- sum(fr > pred[,3]) #Compte le nombre de fois où les relevés sont sortis des intervalles de prédictions. 
	if (outbound > 0) {
	nserie <- append(nserie,str_c(histo[i,1]))
	crossedLimit <- append(crossedLimit,outbound)
	}	
	}
	return(data.frame("ns"=nserie,"crossedLimit"=crossedLimit))
} 
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
barplot_ns <- function(histo,nserie)
{
lConso <- subset(histo,histo$noserie == nserie)
x <- data.frame(na.omit(apply(t(lConso),1,as.numeric)))
colnames(x) <- "releve"
rx1 <- str_match(rownames(x),"201[5-9].[0-9]{2}")
x <- cbind(x,factor(rx1))
x <- na.exclude(x)
x <- cbind(x,append(0,diff(x$releve)))
colnames(x) <- c("releve","mois","conso")
#qplot(mois,data=x)
barplot(xtabs(x$conso~x$mois),col=terrain.colors(12),las=3)
return(xtabs(x$conso~x$mois))
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("(histo,nserie,deg)")
print("possibilité de visualiser uniquement la série complète (k=3) ou les derniers relevés (k=10 pour relevé sur 10 jours)")
library("mblm")
library("stringr")
library("quantreg")
regss_ns <- function(histo,nserie,deg,moymobile,nivConf,execMode,sampleSize) #moymobile est un entier spécifiant le nombre de jours 
{
#deg <- 1 : le degré polynomial de l'ajustement linéaire: choisir deg=2 signifie le calcul d'un ajustement parabolique. 
#moymobile <- 2 : paramètre spécifiant le lissage souhaité (3 spécifie une moyenne de 3 termes consécutives, 
#sachant qu'un terme est soit un compteur journalier,soit un compteur hebdomadaire. 
#Si histogramme1 est construit sur un mode hebdomadaire, 2 siginifie que la moyenne mobile est effectué sur 2 semaines. 
#nivConf <- 0.9 : niveau de confiance de l'ajustement linéaire
#execMode <- 1 : Si 1 le mode est destiné à un calcul itératif de dfscore et l'affichage graphique est désactivé.  
#sampleSize <- 1 : une fraction de 1 spécifiant la taille du vecteur à intégrer dans la modélisation lm().

# histo de taille n est un data.frame dont la structure est la suivante (type histo111): 
	#colonne 1 : n° de serie (char)
	#colonne 2 : modele (char)
	#colonne 3,4,5,...,n : Les relevés journaliers (numeric)
	#execMode est un booléen spécifiant la demande de paramètres en cours d'exécution (1) ou le choix par défaut des paramètres (0) 
	#sampleSize est la taille de l'échantillon utilisée pour le modèle linéaire. Elle sera exprimée en % (compris entre 0 et 1) 
	#Par défaut, les paramètres seront i = 1; choix = 0;taille= nombre de relevés disponibles / 2
	#ADJONCTION MICRO FONCTION
	lm.mediane <- function(y)
	{
	yp <- y[!is.na(y)]
	off.set <- length(y)-length(yp) + 1 
	xp <- off.set: length(y)
	#reg <- mblm(yp~xp) fonction non optimisée : trop lent. 
	reg <- rq(yp~xp,tau=1/2) #tau=1/2 signifie médiane; 1/4 signifie premier quartile. (library quantreg)
	return(reg)
	}
	#PRELIMINAIRES : initialisation des variables et paramètres #######################################################
	lim <- 1.2 #rapport d'homothétie sur point du dernier relevé. 
	percentSample <- sampleSize
	p2 <- dim(histo)[2]
	"1.3 signifie que la prévision se fait sur un relevé de valeur futur calculé ainsi: indice dernier relevé multiplié par 1.3"
	listeMopieurs <- str_c(histo[,1])
	i <- match(nserie,listeMopieurs)
	y <- histo[i,3:p2]
	y <- xPatch(y)
	p <- length(y)
	#ELIMINATION DU MOPIEUR si le total abscence de relevé / taille du relevé inférieur à 0.5
	yw0 <- replace(y,is.na(y),0)
	yw1 <- yw0[yw0>0]
	if (floor(length(yw1)+length(yw0)*(percentSample -1)) <= 2)  
	{return(list(NULL,NULL))} 
	else 
	{
	rangeSize <- floor(length(y)*percentSample) #Initialisation de la dimension de l'échantillon d'apprentissage linéaire. 
	#NIVEAU D'EXECUTION (mode automatique ou manuel) ##################################################################
	if (execMode == 1) #mode Manuel 
	{
	choix <- 9
	print("affichage avec résidus (0) ; ou sans résidus (1)")
	while (choix != 0 & choix !=1)
	{choix <- scan(n=1)}
	print(str_c("Selection de la plage de relevés générant l'échantillon d' apprentissage ?",rangeSize,collapse=":")) 
	print(str_c(str_c("l'échantillonage de régression effectué sur le relevé du copieur multifonct. ",nserie),
			str_c(" a une plage égale à :", floor(length(yw1)+length(yw0)*(percentSample-1)))))
	} 
	else #Mode automatique: dans ce mode, la plage d'apprentissage est celle de percentSample (soit 50% du nombre totaux de relevés). 
	{
	choix=0 
	}
	###################################################################################################################
	liss <- lissage(y[1:rangeSize],moymobile)
	y1 <- liss$s_lissee
	y <- xPatch(y)
	x1 <- 1:length(y1)
	x <-  1:length(y)
	nu <- floor(length(y))
	reg <-  lm(y[1:nu]~poly(x[1:nu],deg,raw=T)) #regression brute
	reg1 <- lm(y1~poly(x1,deg,raw=T),data=liss) #regression lissee
	#-------------------------------------------------------------------------------------------------------------------
	#reg2 <- lm.mediane(y) 
	#-------------------------------------------------------------------------------------------------------------------
	#regression theil (elimination de l'influence de valeurs aberrantes dans le modele de reg.)
	#extremement gourmand en temps de calculs ... l'idéal serait de trouver un équivalent plus agile. reg2 est donc mis en commentaire
	#A remettre en clair pour un calcul effectif 
	reg2 <- rq(y~x,tau=1/2)
	
	if (execMode == 1) #Si le mode Automatisé est à 0 (pour la fonction unrollList notamment et son scoring), 
					   #les calculs non nécessaires et les affichages (lignes 700 à 723) sont désactivés. 
	{
	#Predictions avec intervalle de prédiction nivConf=0.90 conseillé
	lim <- 1
	newdata <- data.frame("x"=1:(lim*length(y)))
	pred <- predict(reg,newdata,interval="pred",level=nivConf) #ne pas confondre intervalle de confiance et intervalle de prédiction !! 
	###################################################################################################################
	#REPRESENTATION GRAPHIQUE	
	plot.new()
	layout(1)
	#if (choix == 0) {layout(c(1,2),widths=c(1/2,1))} #Deux fenêtres simultanées. 
	par(mar=c(1.5,3,1,1))
	xp <- 1:nrow(pred)
	plot(x,y,lty=1,lwd=3,	type="l",col=4,main=nserie)
	points(xp,pred[,3],	lty=3,lwd=1.5,	type="l",col=2)	#prev down
	points(xp,pred[,2],	lty=3,lwd=1.5,	type="l",col=2,xlab="Période",ylab="Estimation",main=nserie) #prev up
	
	 #else 
	#plot(x,liss$s_brut,		lty=1,lwd=3,	type="l",col=4,main=nserie)
	points(x1,y1,		lty=2,lwd=3,	type="l",col=3) #smooth
	if (deg == 1) {curve(reg$coefficient[2]*x+reg$coefficient[1],add=T,col=1)
	curve(reg2$coefficient[2]*x+reg2$coefficient[1],add=T,col="violet")
	} else {
	curve(reg$coefficient[3]*x^2+reg$coefficient[2]*x+reg$coefficient[1],add=T,col=1)
	}
	legend("topleft",lty=1:3,col=c(2,4,3,1),c("estimated interval","relevé","smoothed","straight line"))	
	par(mar=c(1.8,2,1,1))
	
	if (choix == 0) 
	#{hist(rstudent(reg1),col=terrain.colors(256),main=str_c(nserie," résidus moymobiles"))}
	#Estimation densité avec ggplot2. 
	{
	dev.new()
	p <- qplot(rstudent(reg1),geom="density",fill="RES",alpha=1)
	print(p)
	}
	}
	if (execMode == 1) {return(list(reg,reg1,reg2,pred))}
	else {return(list(reg,liss,reg2))}
	}
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("regression(histo)")
regression <- function(histo) #data.frame en argument 
{
k <- dim(histo) 
matrice <- histo[,3:k[2]]
k=dim(matrice)
va <- 1:k[2]
list_reg <- NULL
list_intercept <- NULL 
indice_courbure <- NULL 
pts_aberr <- NULL
for (i in 1:k[1])
{
	y <- as.double(matrice[i,])       #régression sur la ligne i 
	#en cas d'historique insuffisant sur un n° de série, on peut obtenir une ligne de Nas 
	if (sum(is.na(y) == FALSE) > 1 ) 
{	
	res <- lm(y~va)
	a <- round(res$coefficients,2)
	list_reg <- c(list_reg,a[2])
	list_intercept <- c(list_intercept,a[1])
	#Calculs du nombre de points aberrants
	rst <- abs(rstudent(res)) >= qt(0.975,res$df.residual) #test bi-latéral. 0.95 pour un test unilatéral. rst est de classe "logical"
	
	#par exemple, environ 95% des observations studentisées se trouvent dans l'intervalle [-2,2] dès lors que ddl >50 
								   # ddl représentant les degrés de liberté soit df.residuals (2 var regression) . 
	nb <- length(subset(rst,rst))
	pts_aberr <- c(pts_aberr,nb)
	} else { 
	list_reg <- c(list_reg,NA)
	list_intercept <- c(list_intercept,NA)
	indice_courbure <- c(indice_courbure,NA)
	pts_aberr <- c(pts_aberr,NA)
	}
}
histo2 <- rev(histo)
return(data.frame("noserie"=histo[,1],"releve"=histo2[,1],"a_coeff " = list_reg,"b_coeff "=list_intercept,"aberr" = pts_aberr))
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("overviewHisto(dataF)")
overviewHisto <- function (dataF)
#affiche un data.frame simplifié et le graphique des évolutions 
{
n <- dim(dataF)[2]
histo <- dataF[,c(1:2,(n-3):n)]
number_serie <- dataF[,1]
matplot((t(dataF[,3:(n-3)])),type="b",pch=1,cex=1/10)
i <- dim(dataF)[1]
j <- dim(dataF)[2]-5
t <- sapply(1:i,function(i) {min(dataF[i,c(3:j)],na.rm=T)}) #récupère les ordonnées. problème de variables manquantes. 
text(2,t,number_serie,cex=1/2)
return(histo)
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
ACP <- function(dataF,config,colvect) 
#config: typologie du photocopieurs : "MP C3003" par ex. 
#colvect : choix des variables d'analyses 
{
X <- subset(dataF,dataF[,5] == config)[,colvect]
row.names(X) <- subset(dataF,dataF[,5] == config)[,2]
resACP <- PCA(X,scale.unit = TRUE,axes=c(1,2),graph=FALSE)
plot(resACP,axe=c(1,2),choix="ind",select="contrib 20",new.plot=T)
plot(resACP,axe=c(3,4),choix="var",select="cos2 0.6",new.plot=T)
	
return(resACP)
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("xPatch: corrige les séries chronologiques croissantes")
xPatch <- function(x) #corrige les termes de x (une série chrono) ayant des valeurs manquantes. x est un vecteur de réels. 
{
if(!is.numeric(x)) {x <- as.numeric(x)}
i <- 1
#détermination du rang i caractérisant le début de la série chronologique x sauf si la série ne démarre pas à 1.
 #On ne corrige pas les premières valeurs NA car ce sont des données absentes. on passe tant que NA
#Données absentes est une notion différente de données manquantes: la série chronologique x ne démarre pas à la première période 
size <- length(x) 
while(is.na(x[i]) & i < size) {i <- i + 1} #Les premières données manquantes ne doivent pas être ajustée: la série chrono. n'existe simplement pas sur 
#la plage de données. Autrement dit, ce ne sont pas des données manquantes au sens statistique du terme; ce sont des données absentes. 
while(i < size)
{	
	while(!is.na(x[i]) & i < size) {i <- i + 1}
	j <- i
	while (is.na(x[j]) & j < size) {j <- j+1} #on se positionne sur le prochain terme non manquant 
		
		 if (j <= size)
		{
			e <- (x[j]-x[i-1])/(j-i+1)
			x[seq(i,j-1)] <- floor(seq(1:(j-i))*e+x[i-1])
		}
	i <- j	
}		
return(x)
}

"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("surveyPatch: détecte les équipements en anomalies (rupture compteur)")
surveyPatch <- function(noserie,survey) #noserie est un facteur ou un string indiquant le n° de série relatif au relevé survey
#cette fonction corrige les chutes de compteurs en partant du principe qu'une telle série est toujours croissante. 
{
	#survey : Typiquement , les relevés d'une machine i 
	    survey <- as.double(survey) #impose que survey soit de type vecteur de réels
		vs <- replace(survey,is.na(survey),0)
		#survey <- xPatch(survey) #xPatch corrige les valeurs manquantes de la série survey. 
		size <- length(vs)
		Ano <- NULL
		minDelta <- min(diff(vs[!is.na(survey)])) #si le vecteur survey contient un seul terme NA, diff(survey) renvoie NA! 
		#Le traitement des valeurs doit donc se faire sur le vecteur normalisé sans valeurs absentes (NA) 
		if (!is.na(minDelta))
		{
			if (minDelta<0 & is.null(Ano)) {Ano <- toString(noserie)} #si minimum de diff(survey) est strict. <0 alors erreur  
			#et stockage du n° de série dans la liste des anomalies fileAno. 
			#si min(diff(survey)) est négatif cela signifie qu'il y a au moins 1 chute de compteur mais pas nécessairement unique. 
			while(minDelta <0) 
			{
				v_i <- unlist(sapply(2:size,function(i) { if(vs[i-1] > vs[i]) {i} } )) #tant qu'il existe des indices de chute constatee, 
				#v_i recense les indices de la série où la chute est constatée
				#cas particulier où la chute de compteur se trouve au dernier indice i. i+1 n'a pas de sens 
				#et donc reprise intégtrale de la valeur précédente
				 vs[v_i] <- vs[v_i -1]
			
			minDelta <- min(diff(vs[!is.na(vs)]))
			}
		}
		return(list(Ano,vs)) #Ano reprend le n° serie en anomalies et survey est la série corrigée. 
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("histoPatch : pour toujours avoir des relevés de fonctions croissantes: la chute d'un compteur n'étant pas normale")
histoPatch <- function(histo) #histo doit avoir le format de histogramme 1 
#corrige les relevés ayant subit une décroissance de compteurs conséquemment à une rupture "réseau" de la remontée des adrr. 
{
dfscore <- unrollListHisto(histo$noserie,histo)
#histo <- replace(histo,is.na(histo),0) #Remplace toutes les valeurs manquantes par 0. La régression linéaire n'aura alors de sens qu'en 
#partant de la série dont les termes sont non nuls. 
fileAno <- NULL 
n <- dim(histo)[1]
p <- dim(histo)[2] #n : nombre de colonnes du data.frame histogramme1
 for (i in 1:n) #On passe en revue les i èmes machines. 
	{
		survey <- histo[i,3:(p-1)] #Extraction des commpteurs de la machine n° i 
		#survey <- xPatch(survey) #Correction des valeurs manquantes. 
		noserie <- histo[i,1]
		scanSurvey <- surveyPatch(noserie,survey) #Cf fonction surveyPatch
		if (!is.null(scanSurvey[[1]])) 
			{fileAno <- append(fileAno,toString(noserie)) #stockage du n° de série dans la liste des anomalies fileAno. 
			histo[i,3:(p-1)] <- scanSurvey[[2]]
			}   
		if (i%%500==0) {print(str_c("histoPatch:normalisation_histogramme_progression..",round(i/n,2)))} #Affichage progression procedure unrollListHisto
	}
 #print(histo111[vrec,1])	#Doit afficher les copieurs qui ont été patchés mais désactivé pour le moment. 
 print(str_c("le taux d'anomalies concernant les ruptures de compteurs sont de ",length(fileAno)/dim(histo)[1]*100,"%"))
 fileAno <- sort(unique(fileAno))
 print(fileAno)
 dfscore.ano <- subset(dfscore,is.element(rownames(dfscore),fileAno)) 
 write.table(dfscore.ano,file="S_fileAno.csv",sep=";",row.names=F,dec=",")
 write.table(dfscore,file="S_fileDfscore.csv",sep=";",row.names=F,dec=",")
 return(list(histo,dfscore,dfscore.ano))	
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("codepostalPatch : recherche du code postal valide (au sens de l'INSEE) le plus proche du code postal usuel (au sens de La POSTE et ses cedex)")
# recherche du code postal valide (au sens de l'INSEE) le plus proche du code postal usuel (au sens de La POSTE et ses cedex) 
codepostalPatch <- function(codPostList,codInseeList) #listes d'entier (obtenus via as.double() sur codes postaux) 
{
if (!is.numeric(codPostList) | !is.numeric(codInseeList)) #contrôle validité des formats. correction en numérique si formats non valides 
{print("WARNING! Format numérique attendu")
codPostList <- as.numeric(codPostList)
codInseeList <- as.numeric(codInseeList)
}
#Liste des Codes Postaux récupérés sur le fichier GEODE de livraison propriété du titulaire du marché SCC.
#Liste des Codes Postaux selon la nomenclature INSEE
yp <- sapply(1:length(codPostList),function(i) {max(codInseeList[codInseeList-codPostList[i]<=0])})
# Principe du calcul des yp 
#Code livré par scc:                62407
#code Insee valide : 62390  62400           62410  62420
# Delta            : -17    -7        0     3      13 
#Choix du plus grand négatif soit 62400  
yp <- sprintf("%05d",yp) #transformation au format Code Postal (LA POSTE) [char sur 5 digits]
return(yp)
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("tauxLinResidus : Parcours les mopieurs de la selection et détecte une situation de linéarité. Calcul le rapport de cas favorable sur nb equip. analysés")
tauxLinResidus <- function(histogramme2) 
{
moyenneMobile <- 1
nb <- 0
compt <- 0
for (k in 1:nrow(histogramme2)) 
{
reg <- regss_ns(histogramme2,histogramme2[k,1],1,moyenneMobile,0.9,0,3/4) #moyenne mobile = 4
if (!is.null(reg[[1]])) 
{
compt <- compt + 1
sh <- shapiro.test(reg[[1]]$residuals) 
if (sh$p.value > 0.05) {nb <- nb + 1}
}
}
print(str_c("le modele lineaire n est observable que dans ",str_c(round(nb/compt*100,2)," % des cas" )))
return(nb/compt)
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("unrollListHisto : Parcours les mopieurs de la selection un a un pour déroulement des mopieurs de la sélection")
sigmoide <- function(x) {return(2/(1+exp(-x))-1)}
unrollListHisto <- function(liste,histo) #un vecteur de chaîne contenant les n° de séries. histo : modèle histogramme1 uniquement. 
#En colonnne 1: le n° de série du copieur multifonction
{
if (is.numeric(liste)) {return("procédure interrompue : la classe de données n'est pas de type character.")}
if (is.factor(liste)) 
{liste <- sapply(liste,function(s) {toString(s)}); print("Warnings: factors class as fitting format isn't appropriate")}
dfscore <- NULL 
listNoserie <- NULL 
listModel <- NULL
#List Scoring: 
print("ListScoring: 
Un indicateur d'écart entre la prévision et la réalisation [date t].L'idée sous jacente est d'estimer la puissance de prévision du modèle linéaire 
en prenant en compte non pas la totalité de la série [la totalité de la série] mais seulement les p% premiers termes 
constituant ainsi un ECHANTILLON D'APPRENTISSAGE linéaire. 
Cette prévision permet ensuite à une date t [la date du jour par ex ou ultérieure à celle de la fin de l'ECHANTILLON D'APPRENTISSAGE linéaire] 
de comparer la réalisation effective à la prévision rendue par le modèle linéaire [sur la base de l' ECHANTILLON D'APPRENTISSAGE].
La comparaison est rendue par le rapport entre le dernier terme de la série par celui de sa prévision issue du modèle linéaire.")
listScoring <- NULL 
print("Listslope:
Le coefficent directeur de la droite de régression issue du modèle linéaire") 
listSlope <- NULL
print("ListCompteur:
La valeur de la dernière réalisation de la série afin de mesurer le niveau d'activité global du matériel d'impression")
listCompteur <- NULL
print("ListOutliers:
Le nombre de résidus aberrants sortant de la plage de l'intervalle de confiance des 5% (résidus considérés suivre une N(mean,sd)).")
listOutliers <- NULL
print("Listtheil: rapport entre la dérivée max en valeur absolue sur la dérivée moyenne pour détecter des ruptures de compteurs") 
listtheil <- NULL
print("ListRate: Le taux d'utilisation de l'équipement")
listRate <- NULL 
listBreak <- NULL
listNBsurvey <- NULL
n <- length(liste)
i <- 1
#Parcours de tous les équipements d'impressions. 
h1 <- Sys.time()  
while (i <= floor(n))
{
nserie <- liste[i]
#print(str_c("deboggage no 1 ",nserie))
reg <- regss_ns(histo,nserie,1,3,0.9,0,1) #La plage de modélisation linéaire est de 1 (100% des données)  . 
#Cela signifie que si l'on a 100 relevés journaliers, le calcul de régression linéaire s'effectuera sur les 75 premiers relevés. 
#-----------------------------------------------------#
if (!is.null(reg[[1]]))
{
slope <- reg[[1]]$coefficients[2]
compteur <- max(reg[[1]]$model[,1])
#passage au logarithme décimal de la variable compteur. 
#if (compteur != 0 & !is.na(compteur)) {compteur <- log(compteur,10)} else {compteur <- 0}
theil <- reg[[3]][1]$coefficients[2]
#THEORIE: Sous hypothèse de normalité des résidus, on montre qu'ils suivent une loi de student à (n-3) degrés de liberté. 
#Cela explique le reg[[1]]$df.residual-3) 
sx <- reg[[2]]$s_brut ; sx <- sx[!is.na(sx)] 
drate  <- diff(range(sx))/length(sx) #daily drate  
NBsurvey <- length(sx)
#-----------------------------------------------------#
listNoserie <- append(listNoserie,nserie)
#listModel <- append(listModel,model)
listCompteur <- append(listCompteur,round(compteur,3))
listtheil <- append(listtheil,round(theil,3))
listRate <- append(listRate,round(drate,3))
listSlope <- append(listSlope,round(slope,3))
listNBsurvey <- append(listNBsurvey,NBsurvey) #quantité entière , taille du relevé. 
}
#temp <- scan(n=1)
i <- i+1
if (i%%500==0) {print(str_c("unrollList_progress..",round(i/n,2)))} #Affichage progression procedure unrollListHisto
}
h2 <- Sys.time()
print(h2-h1)
#-----------------------------------------------------#
dfscore <- data.frame("noserie" = listNoserie,"compteur"=listCompteur,"drate"=listRate,"theil"=listtheil,"slope"=listSlope,
					"taille"=listNBsurvey,row.names=listNoserie)			
return(colsort(dfscore,5)) #Les plus hauts scores signifie une progression réelle très > au prévisions faites sur les débuts du relevé. 
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
tariff <- function()
#Transformation des champs [COÛTS.COPIES.NB.Montant.au.mille] et [COÛTS.COPIES.CL.Montant.au.mille] en type numérique 
#Tableau des Coûts copies
# C3003  2.01€ et 20.15€ couleur (capacité maximale mensuelle 20000) 
# MX711  2.01€
#MP4054  2.04€
#MP2554  2.05€
#MS610dn 2.81€
#MX511   2.99€
{
configCoutsReels <- data.frame(no= 1:6,model=c("MP 2554","MP C3003","MS610dn","MX511dhe","MX711","MP 4054"),
			PuNB = c(2.05,2.01,2.81,2.99,2.01,2.90),
			PuCL = c(0,20.15,0,0,0,21.24),
			LocM = c(29.70,40.75,0,10.82,18.70,47.01))
			#CMaxM = c(20000,30000,3000,3000,7000,30000))
configNeutralisantLesCoutsCopies <- data.frame(no= 1:6,model=c("MP 2554","MP C3003","MS610dn","MX511dhe","MX711","MP 4054"),
			PuNB = c(1e3,1e3,1e3,1e3,1e3,1e3),
			PuCL = c(0,1e3,0,0,0,1e3),
			LocM = c(29.70,40.75,0,10.82,18.70,27.6))	
			#CMaxM = c(20000,30000,3000,3000,7000,30000))
#Pour la présentation, choix de l'utilisateur s'il souhaite une vision coûts réels en devises (€) ou une vision de simple consommation copies. 
# Pour une vision de simple consommation copies, le niveau des relevés est pris en compte sans multiplication des couts-copies. 			
config2 <- data.frame(no= 1:5,model=c("MP 2554","MP C3003","MS610dn","MX511dhe","MX711"),
			PuNB = c(2.05,2.01,2.81,2.99,2.01),
			PuCL = c(0,20.15,0,0,0),
			LocM48 = c(29.70,40.75,0,10.82,18.70),
			LocM36 = c(34.13,48.35,0,12.61,22.14),
			LocM24 = c(42.68,62.69,0,15.44,27.64),
			LocM12 = c(75.63,115.88,0,26.41,49.35))
			#CMaxM = c(20000,30000,3000,3000,7000))
config3Loc <- data.frame(model=c("Config1B","Config2A","Config2B","Config3A","Config4","Config7"),
			LocM00 = rep(0,6),
			LocM12 = c(0,26.41,49.35,75.63,122.64,115.88),
			LocM24 = c(0,15.44,27.64,42.68,68.53,62.69),
			LocM36 = c(0,12.61,22.14,34.13,54.38,48.35),
			LocM48 = c(0,10.82,18.70,29.70,47.01,40.75))
return(list(configCoutsReels,configNeutralisantLesCoutsCopies,config2,config3Loc))	
}

# GENERALISATION A UN DATA.FRAME DE DONNEE QUELCQUONQUE: Construction d'une fonction générique. 
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
Build.df <- function(x,g) #x étant les données quantitatives et g le nombre de groupes d'observations. 
{
df <- data.frame("x"=x,"g"=gl(g,length(x)/g,length(x),1:g))
return(df)
}
ventil <- function()
{

}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
ventile <- function(superMario_c) #un data.frame en entrée. 
#superMario_c <- superMario[,c(14,32,19,23,27,29,31)]
#superMario_c <- superMario_c[order(superMario_c$Dep),]
{
n <- nrow(superMario_c)
superMario_c <- replace(superMario_c,is.na(superMario_c),0)
superMario_c <- superMario_c[order(superMario_c$Dep),]

i <- 1
while (i <= n) 
{Dep <- superMario_c[i,]$Dep
X <- subset(superMario_c,superMario_c$Dep == Dep)
print(X)
#write.table(X,file=str_c(Dep,"solimp2_relanceIP"),sep=";",dec=",",row.names=FALSE)
while (superMario_c[i,]$Dep == Dep & i <= n) 
{i <- i +1 
}
}
return("OK")
}
print("writeout(X)")
#nom est le nom du fichier à créer 
writeout <- function(X,nom,affichage_noms_variables) {
write.table(X,file=str_c(nom,".csv"),sep=";",dec=",",row.names=affichage_noms_variables)
return()
}
print("cleanTab(dtable")
cleanTab <- function(dtable) 
{
vdim <- dim(dtable)
i <- 1 
while (i <= dim(dtable)[1]) {if (sum(dtable[i,]) == 0) {dtable <- dtable[-i,]} else {i <- i+1}}
j <- 1
while (j <= dim(dtable)[2]) {if (sum(dtable[,j]) == 0) {dtable <- dtable[,-j]} else {j <- j+1}}
return(dtable)
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
print("colorPanel <- function(p) #y vecteur de données et p entier strictement positif. Renvoie un vecteur couleurs de dimension dim(y)  
#dont chaque termes de y est remplacé par une valeur correspondant aux codages alphanumérique couleurs de type relief (dégradé variant de orange à vert")
colorPanel <- function (y,p) 
{
vcol <- rainbow(p,start=2/39,end=19/39)
#exmple avec la courbe cosinus carré: 
x <- seq(1:length(y))
rang <- ceiling(y/max(y)*p)
rang <- replace(rang,(rang==0),1)
#plot(y~x,cex=2,pch=16,col=vcol[rang])
return(vcol[rang])
}
"----------------------------------------------------------------------------------------------------------------------------------------------------------"
joinFacturationOrigineCodePostal <- function(dfFacturation,dfFichierLivraison) 
#dfFacturation = histogramme1 et dfFichierLivraison=scc5 par défaut. 
#Les dataframes devront avoir exactement la même structure que respectivement histogramme1 et scc5 !!. (à cause de la ligne merge indiquant by.x=1 et by.y=15. 
{
dataframeJointure <- merge(dfFacturation,dfFichierLivraison,by.x=1,by.y=23)
x <- dataframeJointure #variable tampon pour soulager l'écriture de la fonction 
dep <-  str_replace_all(format(x$CPLivraison,digits=5)," ","0") #Corrige 6 en 06"
dep <-  str_sub(dep,1,2)
x <- cbind(x[,1:dim(dfFacturation)[2]],
		"dep"	  =dep,
		"CP"  	  =sprintf("%05s",x$CPLivraison),
		"ville"	  =x$VilleLivraison,
		"localite"=x$LocalisationLivraison)
dataframeJointure <- x		
return(dataframeJointure)
}
"----------------------------------------------------------------------------------------------------------------------------------------------------------"
eliminate <- function(dfscore)
{
dfscore <- subset(dfscore, row.names(dfscore) != "E155M833373")
dfscore <- subset(dfscore, row.names(dfscore) != "G175J300299")
dfscore <- subset(dfscore, row.names(dfscore) != "74635C66038FH")
#dfscore <- subset(dfscore, dfscore$drate >= 300 & dfscore$drate <= 900)
return(dfscore)
}
"----------------------------------------------------------------------------------------------------------------------------------------------------------"
jenksMethod <- function(x,k) 
{n <- length(x) 
listF <- NULL
p_val <- NULL

for (i in 3:k)
{
c <- seq(1,n,n/i)
c <- append(c,n)
InertieIntra <- mean(sapply(1:(i-1),function(j) {var(x[c[j]:(c[j+1]-1)])}))
InertieInter <- var(sapply(1:(i-1),function(j) {mean(x[c[j]:(c[j+1]-1)])}))
F <- (InertieInter/InertieIntra)*((n-i)/(i-1))
p_value <- qf(0.95,i-1,n-i)
listF <- append(listF,F)
p_val <- append(p_val,p_value)
}
return(data.frame("i"=3:k,"F"=listF,"p.value"=p_val))
}
"----------------------------------------------------------------------------------------------------------------------------------------------------------"
print("jeu_aov(): simuler des échantillons, les visualiser et dire à chaque fois si on accepte ou pas H0 homogénéité des variances")
jeu_aov <- function()
{
p <- 1
if (runif(1) < 0.5) 
{print("H0 favorisée")
foret <- data.frame("taille"=rnorm(3*10^p,1,1/10),"groupe"=gl(3,10^p)) #H0 favorisée 
} 
else 
{print("H0 est rejetée")
foret <- data.frame("taille"=c(rnorm(10^p,1,1/10),rnorm(10^p,2,1/10),rnorm(10^p,1.5,1/10)),"groupe"=gl(3,10^p)) #H1 favorisée 
}
p <- qplot(taille,data=foret,geom="density",fill=groupe,alpha=0.8)
plot(p)
res.aov <- aov(taille~groupe,data=foret)
return(res.aov)
}
"----------------------------------------------------------------------------------------------------------------------------------------------------------"
print("anova : analyse de variance pour comparaison de moyennes de divers echantillons non appariés, indépendants , et suivant une LG")
anova <- function(datas,groupe) #data est un vecteur de données quantitatives: à saisir colonne par colonne puis spécifier le vecteur LABEL des groupes 
{
df.tableau <- Build.df(datas,groupe)
foretss <- data.frame("var"=x,"n.foret"=gl(3,6,18,1:3)) #var: var.quantitative ; n°foret : facteur, modalités d'une var.q. qualitative.
print(forets)
tapply(df.tableau$donnees,df.tableau$groupe,mean) #Moyenne de chaque échantillons
tapply(df.tableau$donnees,df.tableau$groupe,var)	#Variance de chaque échantillon

#Boxplot : visualiser les quantiles de chaque échantillon
qplot(length(groupe),donnees,geom="boxplot",fill=length(groupe),data=df.tableau)
#densités superposées
qplot(donnees,geom="density",fill=groupe,alpha=0.8,data=df.tableau) #alpha=0.8 : niveau de transparence (si non spécifié, opacité du rendu)

#LES TROIS CONDITIONS FONDAMENTALES POUR EFFECUER UNE ANOVA
# PREMIERE CONDITION POUR L'ANOVA: Les p échantillons comparées sont indépendants. Il n'y a pas de 
# tests simples pour déterminer si nous sommes en situation d'indépendance. Seule les modus operandus de 
# l'expérience permettent de préjuger d'une indépendance indéniable. Le cas de non indépendance est typique 
# d'une expérience consistant à mesurer l'effet d'un médicament sur un même échantillons de personnes. 
# L'effet de la modalité "après cachet" sur la situation "avant cachet" produit deux échantillons qui ne 
#sont absolument pas indépenddants puisque ce sont les mêmes personnes... 
shapiro.test(df.tableau$donnees)
# SECONDE CONDITION POUR L'ANOVA : La variable étudiée suit une loi normale dans son ensemble
#Test de shapiro Wilk : Test positif pour l'expérience
bartlett.test(donnees ~ groupe, data = df.tableau)
# TROISIEME CONDITION POUR L'ANOVA : Les résidus des échantillons ont même variance... 
#Test de Bartlett : Test positif pour l'expérience
# Les TROIS conditions sont donc vérifiées: L'anova peut être mise en place ..... 
# VERIFICATION DU THEOREME DE LA DECOMPOSITION DE LA VARIANCE TOTALE (en utilisant la variance naturelle mais biaisée par un facteur [n-1]/n) 
print("THEOREME DE LA DECOMPOSITION DE LA VARIANCE TOTALE")
I <- length(levels(df.tableau$groupe))
J <- length(df.tableau$donnees)/I
n <- I*J
SCTOT <- n*var(df.tableau$donnees)*(n-1)/(n)
SCF <- n*var(tapply(df.tableau$donnees,df.tableau$groupe,mean))*(I-1)/I
SCE <- n*mean(tapply(df.tableau$donnees,df.tableau$groupe,var))*(J-1)/J
ddlF <- I-1 ; ddlE <- n-I ; ddlTot <- n-1 #E comme epsilon résiduel et F comme facteurs
#En cas d'échantillons appariés, les degrés de libertés changent
#ddlF <- I-1 ; ddlE <- J-I ; ddlTot <- J-1

Fobs <- SCF/SCE * (ddlE/ddlF)
tableAnova <- 	data.frame(	"SCTOT" = SCTOT,"SCE" = SCE,"SCF" = SCF,
							"ddlF"	= ddlF,"ddlR" = ddlE,"ddlTot"= n-1,
							"CMf"	= SCF / ddlF,"CMe"=SCE / ddlE,
							"Fobs"	= Fobs,
							"Fc"= 1-pf(Fobs,ddlF,ddlE))
print(str_c("SCTOT: variance Totale"))
print(str_c("SCE: variance résiduelle (inter inertie)"))
print(str_c("SCF: variance due aux facteurs (intra inertie)"))
print(tableAnova)
res <- aov(donnees ~ groupe,data=df.tableau)

print(summary(res))
}
"----------------------------------------------------------------------------------------------------------------------------------------------------------"
Build.df <- function(datas,groupe) #x étant les données quantitatives et groupe le vecteur des groupes d'observations. 
{
g <- length(groupe)
df <- data.frame("donnees"=datas,"groupe"=gl(g,length(datas)/g,length(datas),g,labels = groupe))
qplot(g,datas,geom="boxplot",fill=g,data=df)
#densités superposées
qplot(datas,geom="density",fill=g,alpha=0.8,data=df)
return(df)
}
"----------------------------------------------------------------------------------------------------------------------------------------------------------"
print("nombrePremiers(2) => calculer les nombres premiers compris entre 10^2 et 10^3")
nombresPremiers <- function(e) #permet de calculer les nombres premiers. pas indispensable pour la suite. 
#Permet simplement de proposer des nombres premiers pour les plaques. La procédure LEGO ne l'utilise pas car la recherche des 
#nombres premiers est orthogonal à celle des solutions du jeu des chiffres.# 
{
listePrem <- NULL
for (k in seq(10^e+1,10^(e+1)+1,2))
	{
	x <- seq(2,floor(sqrt(k)))
	r <- k%%x
	if (length(which(r==0)) == 0) {listePrem <- append(listePrem,k)}
	}
return(listePrem)
}

#---------------------------------------------------------------------------------------------------------------------------	
extraire <- function(liste,i,j) #i et j sont les indices des éléments à extraire. 
#exemple : liste = (2,5,7,11,13) i=2,j=3 alors la procedure renvoie 3 variables : liste = (2,11,13) ; e1=5 ; e2=7
{
liste2 <- liste
e1 <- liste[i] ; e2 <- liste[j]
liste2 <- liste[-c(i,j)] #Retire de la liste les éléments situés aux i-ème et j-ème positions.
return(list(liste2,e1,e2))
}
#------------------------------------PROCEDURE RECURSIVE LEGO --------------------------------------------------------------
print("lego(liste,cible,...) : recherche cible en utilisant les nombres de la liste: jeu le compte est BON armand Jannot")
lego <- function(liste,cible,chaine,pileDesSoluces,trouve)
#pileDesSoluces est une pile qui enregistre les solutions trouvées.trouve est un booléen qui signale l'évènement d'une solution en instance de traitement. 
{
exhaustif <- 1 #si exhaustif = 1 alors l'algorithme va rechercher toutes les combinaisons possibles et imaginables. 
#A éviter lorsque le tirage effectué au départ dépasse les 6 plaques. 5explosion combinatoire)
#lorsque exhaustif = 0, on effectue un échantillonnage aléatoire qui diminue de 3/4 (coeff) le nombre d'indices dans la boucle.
# exemple: pour la seconde boucle , au lieu de traiter j=1,2,3,4,5,6 , on traitera j=1,3,5,6.  
p <- length(liste)
if (p>1 & !trouve) 
	{
		i <- 2
		
		while (i <= p & !trouve) {
			j <-1 
			coeff <- 3/4
			if (exhaustif == 0) {
			s_j <- sort(sample(1:i-1,round((i-1)^coeff))) #effectue l'échantillonage [R.language : fonction sample()] lorsque exhaustif = 0. 
			} else 
			{
			s_j <- 1:(i-1)
			}
			while (is.element(j,s_j) & !trouve)  #[R.language : fonction is.element() renvoie un booléen TRUE si l'élément j appartient à la liste s_j]
			{
				listExtr <- extraire(liste,i,j)
				liste2 <- listExtr[[1]]
				liste3 <- liste2
				liste4 <- liste2
				liste5 <- liste2
				liste6 <- liste2
				e1 <- listExtr[[2]]
				e2 <- listExtr[[3]]
		#test addition 
			chaine2 <- str_c(e1,"+",e2," -ooo- ",chaine) #[R.language : fonction str_c() est une fonction "chaîne de caractère" et renvoie la 
			#concaténation des chaînes : e1 ; + ; e2 ; -ooo- et la chaine de suivi des opérations. 
			#En effet, la "chaine" permet de tracer le cheminement combinatoire et d'afficher la solution finale en des termes compréhensibles. 
			#(en chaîne de caractère). 
			if (is.element(cible,e1+e2)) 
				{
				trouve <- TRUE
				pileDesSoluces <- append(pileDesSoluces,list(chaine2))
				}
				else
				{
				liste2 <- append(liste2,e1+e2)
				res.lego <- lego(liste2,cible,chaine2,pileDesSoluces,trouve)
				chaine2 <- res.lego[[1]]
				pileDesSoluces <- res.lego[[2]]
				trouve <- res.lego[[3]]
				}
		#test multiplication 
			chaine3 <- str_c(e1,"x",e2," -ooo- ",chaine)	
			if (is.element(cible,e1*e2)) 
				{
				trouve <- TRUE
				pileDesSoluces <- append(pileDesSoluces,list(chaine3))
				}
				else { 
				if (e1 != 1 & e2!=1) #rien ne sert d'analyser le résultat d'une multiplication par 1. 
					{
					liste3 <- append(liste3,e1*e2)
					res.lego <- lego(liste3,cible,chaine3,pileDesSoluces,trouve)
					chaine3 <- res.lego[[1]]
					pileDesSoluces <- res.lego[[2]]
					trouve <- res.lego[[3]]
					}
				}	
		#test division 
			if (e1%%e2 == 0)
			{	
			chaine4 <- str_c(e1,"/",e2," -ooo- ",chaine)
			if (is.element(cible,e1/e2)) 
				{
				trouve <- TRUE
				pileDesSoluces <- append(pileDesSoluces,list(chaine4))
				}
				else
				{
				liste4 <- append(liste4,e1/e2)
				res.lego <- lego(liste4,cible,chaine4,pileDesSoluces,trouve)
				chaine2 <- res.lego[[1]]
				pileDesSoluces <- res.lego[[2]]
				trouve <- res.lego[[3]]
				}
			}
		#test soustraction 
			if (e1-e2 > 0)
			{
			chaine5 <- str_c(e1,"-",e2," -ooo- ",chaine)	
			if (is.element(cible,e1-e2)) 
				{
				trouve <- TRUE
				pileDesSoluces <- append(pileDesSoluces,list(chaine5))
				}
				else { 
					liste5 <- append(liste5,e1-e2)
					res.lego <- lego(liste5,cible,chaine5,pileDesSoluces,trouve)
					chaine5 <- res.lego[[1]]
					pileDesSoluces <- res.lego[[2]]
					trouve <- res.lego[[3]]
				}
			}
			if (e2-e1 > 0)	
			{
			chaine6 <- str_c(e2,"-",e1," -ooo- ",chaine)	
			if (is.element(cible,e2-e1)) 
				{
				trouve <- TRUE
				pileDesSoluces <- append(pileDesSoluces,list(chaine6))
				}
				else { 
					liste6 <- append(liste6,e2-e1)
					res.lego <- lego(liste6,cible,chaine6,pileDesSoluces,trouve)
					chaine6 <- res.lego[[1]]
					pileDesSoluces <- res.lego[[2]]
					trouve <- res.lego[[3]]
				}
			}
			j <- j+1
			}	
		
		i <- i+1
		}
	}
	return(list(chaine,pileDesSoluces,trouve))
}
#---------------------------------------------------  FIN PROCEDURE RECURSIVE   LEGO ---------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#MAIN PROCEDURE 
print("mainLego(tirage,cible)")
mainLego <- function(tirage,cible) 
{
# exemple : cible <- 957
#           tirage <- c(6,5,25,50,75,100)
print(str_c("cible attendu : ",cible))
liste <- tirage
if (length(liste)>=12) 
	{liste <- sort(sample(liste,9))
	print("un échantillonnage a eu lieu compte tenu d'une explosion combinatoire")
	}
print("liste chiffres et nombres  : ")
print(liste)
chaine <- NULL
trouve <- FALSE
pileDesSoluces <- NULL
h1 <- Sys.time()
pileDesSoluces <- lego(liste,cible,chaine,pileDesSoluces,trouve)[[2]]
laps <- -(h1-Sys.time())
print(laps)
#recherche d'une solution la plus optimale.  
solutionOptimal <- NULL
NbItems <- unlist(lapply(pileDesSoluces,function(v) {str_count(v)})) #vecteur précisant le nombre d'items de calculs. 
if (!is.null(NbItems)) 
{
i <- which(NbItems ==min(NbItems))[1] #première position d'une solution la plus optimale
solutionOptimal <- pileDesSoluces[[i]]
print(solutionOptimal)
} else {print("pas de solutions possibles")}
return(list(solutionOptimal,pileDesSoluces))
}
#---------------------------------------------------------------------------------------------------------------------------
print("cramer()")
cramer <- function()
{
library(questionr)
#cross_tab est obtenue avec l'aide de la fonction table
#101 est la dimension j de cross_tab)  : dim(cross_tab)[2]
j <- dim(cross_tab)[2]
#pour les groupes et simplifier la lecture , on peut utiliser une extraction numérique des groupes de marchandises#
# pour cela X$groupe <- str_match(X$groupe,"[0-9]{2}.[0-9]{2}.[0-9]{2}")
groupes <- colnames(cross_tab)[order((1-sapply(1:j,function(i) {cramer.v(cross_tab[,-i])/cramer.v(cross_tab)}))*100)]
variation <- round((1-sapply(1:j,function(i) {cramer.v(cross_tab[,-i])/cramer.v(cross_tab)}))*10000,digits=1)
dfres <- data.frame("groupes" =groupes , "variation" = variation)
dfres <- dfres[order(dfres[,1]),] #tri suivant la colonne nom de groupe 
dfres <- cbind(dfres,"nb_occ"=apply(cross_tab,2,sum))
return(dfres[order(dfres[,2],decreasing=TRUE),])
}
#Cramer Exemple sur une table totale d'indépendance 
#Création de 1000 individus de 2 caractéristiques totalement aléatoires
#classA <- sample(gl(3,1000,labels=letters[1:3]),1000)
#classB <- sample(gl(3,1000,labels=letters[1:3]),1000)
#cross_tab_alea <- table(classA,classB)
#Visualisation du cramer : cramer.v(cross_tab_alea)
#---------------------------------------------------------------------------------------------------------------------------
print("oscilloscope(p);p étant le degré échantillonage")
oscilloscope <- function(p) #p est un entier 
{
d <- 1/(2*p) #
n <- 10
Iinf <- -pi/16
Isup <- pi/16
dz <- rep(seq(0,1,d),n)
x <- Isup*(2*dz-1)
x1 <- seq(0,1,1/(length(x)-1))
x1 <- (x1-1/2)*(Isup-Iinf)
a <- rnorm(n,pi,2*pi)
b <- runif(n,-pi,pi)
M <- array(x,c(length(x),n))
M1 <- cos(t(a*t(M)+b))
y <- apply(M1,1,sum)
plot(x1,y,cex=1/5,typ="p",lty="solid",lwd=1)
return(list(x,y))
}
#---------------------------------------------------------------------------------------------------------------------------
print("distTableConv()")
print("package ade4 : conversion table de distances en data.frame de coordonnées")
distTableConv <- function() # d est une table de distance obtenue avec dist()
{
library(ade4) 
data(capitales)
d <- dist(capitales$xy)
dd <- -0.5*bicenter.wt(d*d)
dde <- eigen(dd)
ds <- pcoscaled(d)
plot(ds,asp=1)
text(ds[,1],ds[,2],labels=names(capitales$df))
return(dde)
}
distTableConv2 <- function(d) # d est une table de distance obtenue avec dist()
{
#gdist <- data.frame(x=runif(n,min=-10,max=10),y=runif(n,min=-10,max=10),row.names = LETTERS[1:n])
library(ade4) 
#data(capitales)
#d <- dist(capitales$xy)
dd <- -0.5*bicenter.wt(d*d)
dde <- eigen(dd)
ds <- pcoscaled(d)
plot(ds,asp=1)
text(ds[,1],ds[,2],labels=letters[1:nrow(as.matrix(d))],lwd=5,cex=2)
return(list(ds,dde))
}

#Fonctionnement de l'analyse en coordonnées principales 
#Aims : From a points distances matrix, build an points map overview!  
#Ne pas confondre avec l'analyse en composantes principale
#[library(MASS) requise pour l'approximation fractionnaire fractions()]
#[library(ade4) requise pour pcoscaled()]
#  gdist <- data.frame(x=c(0,1,0),y=c(0,0,1),row.names = LETTERS[1:3])
#  d <- as.matrix(dist(gdist))
#  Calcul du double centrage de d^2/2 
#  A <- d%*%d/2
#  A <- A-apply(A,1,mean)
#  
#  m1 <- t(matrix(rep(apply(A,2,mean),3),3))
#  A1 <- A-m1 
#  effets identiques :
#  scale(A,scale=F) 
#  sweep(A,2,apply(A,2,mean))  
#  effets identiques : 
#  scale(A) signifiant que scale(A) - A1%*%diag(us1) est la matrice nulle. 
#  P <- eigen(A1) 
#  D <- pcoscaled(gdist)
#  plot(D,lwd=5,col=9)

bicenterwt <- function (X, row.wt = rep(1, nrow(X)), col.wt = rep(1, ncol(X))) 
{
    X <- as.matrix(X)
    n <- nrow(X)
    p <- ncol(X)
    if (length(row.wt) != n) 
        stop("length of row.wt must equal the number of rows in x")
    if (any(row.wt < 0) || (sr <- sum(row.wt)) == 0) 
        stop("weights must be non-negative and not all zero")
    row.wt <- row.wt/sr
    if (length(col.wt) != p) 
        stop("length of col.wt must equal the number of columns in x")
    if (any(col.wt < 0) || (st <- sum(col.wt)) == 0) 
        stop("weights must be non-negative and not all zero")
    col.wt <- col.wt/st
    row.mean <- apply(row.wt * X, 2, sum)
    col.mean <- apply(col.wt * t(X), 2, sum)
    col.mean <- col.mean - sum(row.mean * col.wt)
    X <- sweep(X, 2, row.mean)
    X <- t(sweep(t(X), 2, col.mean))
    return(X)
}
print("package ade4")
distTableConv2 <- function(d) # d est une table de distance obtenue avec dist()
{
#gdist <- data.frame(x=runif(n,min=-10,max=10),y=runif(n,min=-10,max=10),row.names = LETTERS[1:n])
#jeu d'essais pour ade4 : dist(matrix(runif(64,min=5,max=1000),nrow=8)) 
library(ade4) 
#data(capitales)
#d <- dist(capitales$xy)
dd <- -0.5*bicenter.wt(d*d)
dde <- eigen(dd)
ds <- pcoscaled(d)
plot(ds,asp=1)
text(ds[,1],ds[,2],labels=letters[1:nrow(as.matrix(d))],lwd=5,cex=2)
return(list(ds,dde))
}
