weibull <- function(x)
{(a/b^a)*x^(a-1)*exp(-(x/b)^a)}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
#Analyse de variance et test de Fisher

VarAnalysis <- function()
{
T <- matrix( c(1248,1392,1057,3159,891,1065,1118,2934,1138,1456,1224,3090),nrow=3,byrow=T)
N <- dim(T)[1] 
p <- dim(T)[2]
Sa <- sum((apply(T,1,mean)-mean(T))^2)*p # DDL : N-1
Sp <- sum((apply(T,2,mean)-mean(T))^2)*N # DDL : p-1
S <- sum((T - mean(T))^2) #DDL : N*p-1
Sr <- sum((T - mean(T))^2) - Sa - Sp #DDL: (p-1)(N-1)
# Test de saisonnalite : Test de Fisher pour les tableaux de petites tailles . Test du Khi2 sinon. 
#commande R : fisher.test(mytable) 
if ((Sp/Sr*1/(N-1)) > qf(0.95,p-1,(N-1)*(p-1))) {print("rejet H0 : La serie est saisonnière")}
print(str_c("F empirique = ",Sp*(N-1)/Sr,"F theor:",qf(0.95,p-1,(N-1)*(p-1)),sep=" "))
# Test de tendance :idem 
if ((Sa/Sr*1/(N-1)) > qf(0.95,N-1,(N-1)*(p-1))) {print("rejet H0 : La chronique est affectee d'une tendance. Refaire le test après elimin.tendance")}
}
#"----------------------------------FONCTION D AUTOCORRELATION SUR LES TERMES DES RESIDUS DE REGRESSION -------------------------------------------------"
correlogramme <- function(lm_class) #un object de class lm (regression lineaire) 
#Fonction d'autocorrelation sur la serie et un decalage de rang k: Correlogramme sur les residus d'une regression lineaire 
#Bibliographie: Analyse des series temporelles (page 15)
{
x <- lm_class$residuals ; n=length(x)
#x <- c(1248,1392,1057,3159,891,1065,1118,2934,1138,1456,1224,3090)
xi <- NULL
for (i in seq(1,n,15)){xi <- c(xi,x[i])} #Prelèvement de E(n/p) periodes sur la serie chronologique: Ici tous les 7 unites de temps. (J/S/M/A)  
x <- xi;n=length(x)
m <- matrix(rep(0,n^2),nrow=n);m[1,] <- x
v <- NULL;ddl <- NULL;vtc <- NULL;vt_stud <- NULL;decision <- NULL;w <- NULL
"---------------------------------------------------------------------------------------------------------------------"
for (k in 1:(n-2)) 
{
	m[k,] <- c(rep(0,k-1),x[k:n])
	rk <- cor(m[k,k:n],x[1:(n-k+1)]) #coefficient de correlation rk des series decalees de k 
	v <- append(v,rk) #compilation des rk dans v
	ddl <- append(ddl,n-k-1) #degres de liberte pour le calcul de student
		if (abs(rk) > 0.9999) {tc <- Inf}
		else { tc <- abs(rk)/sqrt(1-rk^2)*sqrt(n-k-1)} #le tc du correlogramme indefini quand rk = 1
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
#Methode de theil permet de corriger les valeurs aberrantes dans une regression
#Le package R "mblm" met en oeuvre la methode de theil pour les modèles lineaires 
"Principe: Soit n points (xi,yi). A chaque couples de points Ai1(xi1,yi1) Ai2(xi2,yi2), on definit le coefficient directeurs a  de la droite passant 
par Ai1 et Ai2 : (yi2-yi1)/(xi2-xi1). En passant en revue les n.(n-1)  couples de points on recolte l'ensemble des coeff a possibles. C'est la mediane de 
cette collection qui donne la meilleure pente possible de regression en effacant en quelque sorte le point aberrant." 
theil <- function(w) #un vecteur de reels 
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
colsort <- function(datafr,decrement) 
#le nom de la colonne ne peut pas être specifie car le datafr indiquent les releves hebdo ou journaliers
# le datafr contient aussi dans les 3 dernières colonnes a_coeff b_coeff et aberr. 
# pour trier suivant les derniers releves, on utilise en general un decrement (entier) egal à 3 
{
l <- dim(datafr)[2] 
p <- l - decrement 		# decrement à -3 pour trier suivant la colonne des commpteurs
datafr <- datafr[order(datafr[,p],decreasing=T),] 
return(datafr)											
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
unicite <- function(x,k) # suppression des doublons en precisant le rang (k) de la colonne concernee pour un data.frame (x) à n>1 colonnes 
{ 
return(x[!duplicated(x[,k]),])
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
regR <- function(dataframe,kiemeCol,expreg)
{
#Lorsque expreg est omis, on utilisera les expressions ci-dessous"-----------#
mp2544 <- "G1[47][4-6][6JPR][0-9ABC][05][01248][0-9]{3}"	
mp3003 <- "(E15[456]M[1-9A-C][23][0-5][0-9]{3})"			
ms610 <- "(4[05][1O][45][4-6][57P][HP]H[02][0-9A-Z]{4})"			
mx511 <- "(7[014][016A][135][45][5-7CP][67H][6H][0H][013M-Z][0-9A-Z]{3})"
mx711 <- "(7463[45]C660[0-3][0-9A-Z][0-9A-Z]{2})"
vexpreg <- c(mp2544,mp3003,ms610,mx511,mx711)
expreg <- str_c(vexpreg,collapse="|")
#----------------------------------------------------------------------------#
cx <- dataframe[,kiemeCol] 
dataframe[,kiemeCol] <- str_match(cx,expreg)[,1] 
print(str_c("dimension :",dim(dataframe))) 
dataframe <- subset(dataframe,!is.na(dataframe[,kiemeCol]))
print(str_c("dimension après rectif :",dim(dataframe)))
return(dataframe)
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
resume <- function(colonne_datafr,colonne_numeric) {
#colonne_datfr est la colonne du dataframe  permettant l'agregation par Groupes (HMX$PROJET par exemple) et doit être de type as.factor()
#colonne_numeric est la colonne du dataframe exclusive numeric comme HMX$Montant.paye par exemple
groupe <- as.factor(colonne_datafr)
noms_groupe <- levels(colonne_datafr) #a pour effet de synthetiser les noms de chaque groupe #
v_numeric <- as.numeric(as.vector(colonne_numeric)) 
res <- tapply(colonne_numeric,groupe,function(i) {i}) 
res.sum <-sort(tapply(colonne_numeric,groupe,function(i) {sum(i)}),decreasing=TRUE)
res.summary <- tapply(v_numeric,groupe,function(i) {summary(i)})  
res.mean <- sort(tapply(v_numeric,groupe,function(i) {mean(i)}),decreasing=T) 
res.sd <- sort(tapply(v_numeric,groupe,function(i) {sd(i)}),decreasing=T) 
res.max <- sort(tapply(v_numeric,groupe,function(i) {max(i)}),decreasing=T)
return(list(list(groupe),res,res.sum,res.summary,res.mean,res.sd,res.max))
}
#pour acceder aux differents resultats de la liste retournee, recuperer la liste dans summaries et saisir summaries[[k]]
#k varie de 1 à 5 
#k=1 : Noms des groupes
#k=2 : Montants numeriques par groupe
#k=3 : Resultats en cumul total
#k=4 : Resultats en resume
#k=5 : Resultats en moyennes 
#k=6 : Resultats en variance
#k=7 : Resultats en maximum de serie
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
#exemple : 
"
summaries <- resume(histo11$modele,histo11$ind_fact.x) 
v <- levels(unlist(summaries[[1]]))
x <- lapply(summaries[[2]],function(list) {log(list)})
boxplot(x,names=v)
wilcox.test(x[[1]],x[[2]])
"
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
weeklySurvey_C3003 <- function(histoKapx,surveyType,color) #histoKapx un array de data.frames ; 
#surveyType est un entier specifiant le type de releve : 0 pour journalier et 1 pour hebdomadaire.   
#color prend soit 0 soit 1. 1 specifie un releve comptatible avec les ricoh C3003, mopieurs couleurs 
{
 #seules les 30 variables sont utilisees 
if (color == 1) #pour les photocopieurs couleurs uniquement
{coeff <- 10} #Les impressions couleurs coutent 10 foix plus cher que les N&Blancs
else{coeff <- 0}

k=dim(histoKapx)[2] 
#coeff*unlist(histoKapx[17,k]) : releves couleurs en colonne 17. A inserer après debbugage. 
serie <- data.frame("n.serie" = unlist(histoKapx[2,k]),
		"modele " = unlist(histoKapx[5,k]),
		"ind_fact" = 1/1000*(unlist(histoKapx[15,k])),
		stringsAsFactors=FALSE,check.names = F)	
# Annonce Debbugage 
"lorsque l erreur type !Error in .subset2(x, i, exact = exact) : subscript out of bounds! se produit,cela signifie qu'un equipement 
non conforme s est insere dans les historiques hebdomadaires. il faut alors le detecter et le supprimer des tous les historiques le contenant
dernière anomalie le 12/01/2016 avec equipement 701632HH01RGB qui etait dans gere KPAX et qui a bascule dans non gere. 
L'erreur s'est alors produite lors de la jointure des fichiers hebdomadaires"
if (surveyType == 0) {surveyType <- 1} else {surveyType <- 1}
j <- 0
for (i in (k-1):1) 
	{
	#Releve journalier ou hebdo suivant 
		survey <- data.frame("n.serie" = unlist(histoKapx[2,i]),"ind_fact" = 1/1000*(unlist(histoKapx[15,i])))
	#Verification que tous les n° serie de ce releve figurent aussi dans le releve serie (sinon !Error in .subset2(x,1,exact = ex ....) 
	"Pour cette verification, on construit le releve par intersection des n° de series en commun" 
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
lissage <- function(histo,nserie,p) #p est la periode p=7 lissage hebdomadaire
{
	fr <- subset(histo,histo[,1] == nserie)
	y <- ts(as.double(fr[,3:(ncol(fr)-3)])) #ts : serie temporelle 
	y1 <- filter(y,rep(1/p,p),side=1) # filter: lissage moyenne mobile 
	return(data.frame("s_brut"=y,"s_lissee"=y1)) #retourne un data.frame avec serie brute et lissee
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
lissage2 <- function(histo,p) #p est la periode p=7 lissage hebdomadaire. fonction plus generale qui lisse par moy mobiles tout l'ensemble
{ 
	dates <- names(histo)
	y <- ts(as.matrix(histo[,3:ncol(histo)])) #ts : serie temporelle 
	y1 <- sapply(1:dim(histo)[1],function(i) {filter(y[i,],rep(1/p,p),sides=1)}) # filter: lissage moyenne mobile
	histo <- cbind(data.frame(histo[,1:2]),data.frame(t(y1)))
	colnames(histo) <- dates
	return(histo) #retourne un data.frame avec serie brute et lissee
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
regss_ns <- function(histo,nserie,deg) 
{
	print("possibilite de visualiser uniquement la serie complète (k=3) ou les derniers releves (k=10 pour releve sur 10 jours)")
	print("historique long(1) ou personalise (2 à n semaines/jours) ?") 
	i <- scan(n=1) 
	i <- 1
	fr <- subset(histo,histo[,1] == nserie)
	y <-as.double(fr[,(i+2):(ncol(fr)-3)]) #les releves sont en colonnes 3 (i+2) à dim(fr)[2]-3 (ncol(fr)-3)  
	x <- 1:length(y)	
	liss <- lissage(histo,nserie,7) #moyenne mobile hebdo (p=7) de la serie brute du n° serie du data.frame histo
	res <-  lm(liss$s_brut		~poly( 1: length(liss$s_brut)    	,deg,raw=T)) #regression brute
	res1 <- lm(liss$s_lissee	~poly( 1: length(liss$s_lissee) 	,deg,raw=T)) #regression lissee , on perd les 7 premiers releves (on demarre à 7)
    res2 <- theil(liss$s_brut) 													 #regression par theil method
	aire <- abs(mean(y - res$coefficients[1],na.rm=T))- res$coefficients[2]*mean(x)
	print(aire)
	plot(x,liss$s_brut,pch=1/2,cex=1/2,type="b",main=nserie)
	points(x,liss$s_lissee,pch=1/2,cex=1/2,type="b",col=4)
	curve(res2[1]*x+res2[2],add=T,col=2)
	
	return(list(res,res1,res2,"aire"=aire))
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
regression <- function(histo) #data.frame en argument 
{
k <- dim(histo) 
matrice <- as.matrix(histo[,3:k[2]])
k=dim(matrice)
va <- 1:k[2]
list_reg <- NULL
list_intercept <- NULL 
indice_courbure <- NULL 
pts_aberr <- NULL
for (i in 1:k[1])
{

	y <- matrice[i,]       #regression sur la ligne i 
	#en cas d'historique insuffisant sur un n° de serie, on peut obtenir une ligne de Nas 
	if (sum(is.na(y) == FALSE) ) 
{	
	a0 <- lm(y~poly(va,2,raw=T))
	res <- lm(y~va)
	a <- round(res$coefficients,2)
	list_reg <- c(list_reg,a[2])
	list_intercept <- c(list_intercept,a[1])
	indice_courbure <- c(indice_courbure,a0$coefficients[3])
	#Calculs du nombre de points aberrants
	print(res$df.residual)
	rst <- abs(rstudent(res)) >= qt(0.975,res$df.residual) #test bi-lateral. 0.95 pour un test unilateral. rst est de classe "logical"
	
	#par exemple, environ 95% des observations studentisees se trouvent dans l'intervalle [-2,2] dès lors que ddl >50 
								   # ddl representant les degres de liberte soit df.residuals (2 var regression) . 
	nb <- length(subset(rst,rst))
	pts_aberr <- c(pts_aberr,nb)
	} else { 
	list_reg <- c(list_reg,NA)
	list_intercept <- c(list_intercept,NA)
	indice_courbure <- c(indice_courbure,NA)
	pts_aberr <- c(pts_aberr,NA)
	}
}
return(data.frame(histo,"a_coeff " = list_reg,"b_coeff "=list_intercept,"aberr" = pts_aberr))
}
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
visualize_histo <- function (dataF)
#affiche un data.frame simplifie et le graphique des evolutions 
{
n <- dim(dataF)[2]
histo <- dataF[,c(1:2,(n-3):n)]
print(histo)
number_serie <- dataF[,1]
matplot((t(dataF[,3:(n-3)])),type="b",pch=1,cex=1/10)
i <- dim(dataF)[1]
j <- dim(dataF)[2]-5
t <- sapply(1:i,function(i) {min(dataF[i,c(3:j)],na.rm=T)}) #recupère les ordonnees. problème de variables manquantes. 
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
#"---------------------------------------------------------------------------------------------------------------------------------------------------------" 
#"---------------------------------------------------------------------------------------------------------------------------------------------------------"
#"---------------------------------------------------------------------------------------------------------------------------------------------------------" 
#"---------------------------------------------------------------------------------------------------------------------------------------------------------"
# PREREQUIS A L'EXECUTION DE CE SCRIPT : scc_report.r dans le repertoire 2_DISIS_REPORT_MENSU
## Packages lies à la manipulation des expressions regulières et chaînes de car. 
require(stringr)
require(stringi)
#niveau de confection des journaux d'historisation : Hebdomadaire ou journalier ?
print("choisir le niveau de confection des journaux d'historisation : Hebdomadaire (1) ou journalier (0)")
h <- scan(n=1)
if (h != 0 & h !=1) {stop("La saisie exige zero(0) ou un(1) : Accès bloque")
		stop()
} else { 
if (h == 1) 
		{pattern_1 <- "KPAX.+1107.+csv$";surveyType <- 1} #type Hebdomadaire
			else {pattern_1 <- "KPAX.+1322.+csv$";surveyType <- 0} #type Journalier
		}
print("choisir type devices couleurs C3003(1) ou monochrome (0)")	
colorType <- scan(n=1)
if (colorType != 0 & colorType !=1) {stop("La saisie exige zero(0) ou un(1) : Accès bloque")
		stop()
} 
print("Choisissez la borne inf - sup (2 nombres de 1 à 999).")
borne <- scan(n=2)
print("choisissez les mopieurs en fonction des points aberrants de la regression: pas de points aberrants detectes (0), avec pts aberr (1)")
pts_aberr <- scan(n=1) 
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
"Le repertoire KPAX_REPORT contient tous les rapports scc_report.r generes par KPAX (rapports journaliers pus rapports hebdomadaire) 
1) fichiers contient la liste de tous les fichiers de type pattern (regular expr) 
2) listefic va plus loin et recupère les infos relatives à ces fichiers
3) rrf recupère ainsi le fichier le plus recent 
4) fichier est le nom complet du fichier le plus recent compatible pour la fonction read.csv"

fichiers <- list.files(path="../2_DISIS_REPORT/KPAX_REPORT",pattern=("KPAX.+csv$"))
listeFic <- file.info(path="../2_DISIS_REPORT/KPAX_REPORT",fichiers)
rrf <- sort(row.names(listeFic),decreasing=T) #dernier fichier le plus recent du repertoire /.KPAX_REPORT
#Saisie du fichier KPAX gere de travail. 
print("choisir le n° de rapport KPAX : ")
print(rrf[1:15])
#num_fic <- scan(n=1) à activer pour un choix different de la date du jour. 
num_fic <- 1
fichier <- str_c("../2_DISIS_REPORT/KPAX_REPORT",rrf[num_fic],sep="/")
gere <- read.csv(fichier,sep=";",header=T,stringsAsFactors=FALSE)
# <- replace(N,is.na(N),0)
print(str_c("fichier telecharge : ", fichier))
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
#Confection du Cube de donnees hebdomadaire histoKapx 
#Recuperation automatique du fichier KPAX le plus recent au niveau Hebdomadaire ! 
fichiers <- list.files(path="../2_DISIS_REPORT/KPAX_REPORT",pattern = pattern_1)
# Pour confectionner un array niveau hebdomadaire , utiliser pattern = ("KPAX.+1107.+csv$") 
# Pour confectionner un array niveau journalier , utiliser pattern = ("KPAX.+1322.+csv$")
serieFichiers <- str_c("../2_DISIS_REPORT/KPAX_REPORT/",fichiers)
p <- length(serieFichiers)
if (surveyType == 0) {serieFichiers <- serieFichiers[50:p]} #50 correspond à un debut historique au 01/01/2016
#AVANTAGES : Le premier est de proposer des regressions sur des consommations en periode de croisières. 
#Une Instabilite des compteurs au debut de la mise en service a ete constatee.)
histoKapx <- sapply(serieFichiers,function(fic) {read.csv(fic,sep=";",header=T,stringsAsFactors=FALSE)[,1:40]}) #Utilise à partir de la ligne 178

"---------CONDITIONNEMENT DU FICHIER PROVENANT DE SCC CONTENANT LES INFORMATIONS GEOGRAPHIQUES --------------------------------------------------------------------------"
scc <- read.csv("VSCC8.csv",sep=";",header=T,stringsAsFactors=FALSE)
scc7 <- read.csv("VSCC7.csv",sep=";",header=T,stringsAsFactors=FALSE)
scc8 <- read.csv("VSCC8.csv",sep=";",header=T,stringsAsFactors=FALSE)
cp <- read.csv("codesPostaux.csv",sep=",",header=T,stringsAsFactors=FALSE) #Load CodesPostaux
cp$CODE <- as.double(cp$CODE)
coutnb <- as.double(str_replace_all(str_match(scc$CPLivraison,"[0-9]+"),",",".")) #statistiques sur le coût copie NB facture par SCC" 
#signification expression reguliere [0-9]+,[0-9]{2} : colonne CPLivraison
#TRAITEMENT DES DONNEES #
#PHASE 1: jointure KPAX & DISIS sur nserie 
#Adapte pour la vue scc_report.r dans KPAX
matchNS <- merge(gere,scc,by.x=2,by.y=23) #Jointure stricte fichiers SCC & KPAX
matchNS1 <- matchNS[,c(1:17,98:131)] #mise en forme simplifiee pour vues compteurs 
resByVille <- resume(matchNS1$Ville,matchNS1$Compteur.machine)
#Extraction des difference au niveau des volumetries: si pas de differences, nrow(comparer...) doit être à zero. 
Delta2b <- subset(matchNS1,matchNS1$Compteur.machine != matchNS1$X2015.COÛTS.COPIES.NB..Total.releve.compteur..)
jointureL <- merge(gere,scc,by.x=2,by.y=23,all=TRUE) #Jointure large fichiers SCC &KPAX 
match_scc <- merge(scc,gere,by.x=23,by.y=2,all=T) #Jointure fichiers sur n°series corriges
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
#Transformation des champs [COÛTS.COPIES.NB.Montant.au.mille] et [COÛTS.COPIES.CL.Montant.au.mille] en type numerique 
#Tableau des Coûts copies pour 1000 
# C3003  2.01€ et 20.15€ couleur
# MX711  2.01€
#MP4054  2.04€
#MP2554  2.05€
#MS610dn 2.81€
#MX511   2.99€
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
## PHASE CARTOGRAPHIQUE franceO3.fr
# Cartographie cantonale (traitement de codes Postaux) 
# recherche du code postal valide (au sens INSEE) le plus proche du code postal declare (souvent des cedex non valide selon l'INSEE) 
cpScc <- match_scc$CPLivraison
 #chargement des codes postaux declares par le titulaire 
cpInsee <- cp$CODE #chargement des codes postaux valides au sens de l'Institut Nationale de la Statistique et Etudes Economiques 
yp <- sapply(1:length(cpScc),function(i) {max(cpInsee[cpInsee-cpScc[i]<=0])})
# Principe du calcul des yp 
#code Insee valide : 62390  62400           62410  62420
#Code livre        :                62407
# differentiel     : -17    -7        0     3      13 
#Choix du plus petit negatif soit 62400  
yp <- sprintf("%05s",yp)
jgd <- data.frame(indicateur_couts=as.double(match_scc$Total.mono+10*as.numeric(str_replace_na(match_scc$Total.couleur,0))),
		CP=yp,Dep=factor(substr(yp,1,2)))
jgd <- data.frame(indicateur_couts=as.double(match_scc$Total.mono+10*as.numeric(str_replace_na(match_scc$Total.couleur,0))),
		CP=yp,Dep=str_match(yp,"[0-9]{2}"))
#ATTENTON! indicateur_couts simplifiee. Il s'agit plutôt d'un indicateur de coûts
"---------------------------------------------------------------------------------------------------------------------------------------------------------"
#Eliminer les lignes sans montants 
jgd <- subset(jgd,!is.na(jgd$indicateur_couts))
scarto <- resume(as.factor(jgd$CP),jgd$indicateur_couts)
codesPostaux <- names(scarto[[3]][1:dim(scarto[[3]])])
factures <- scarto[[3]][1:dim(scarto[[3]])]
resumecp <- data.frame(CP=codesPostaux,facturation=as.double(factures))
scarto_dep <- resume(str_match(jgd$CP,"[0-9]{2}"),jgd$indicateur_couts)
departements <- names(scarto_dep[[3]][1:dim(scarto_dep[[3]])])
factures <- scarto_dep[[3]][1:dim(scarto_dep[[3]])]
resumedep <- data.frame(DEP=departements,facturation=as.double(factures))

#F) PHASE 6: ECRITURE FINALE DES FICHIERS DE SORTIES 
write.table(gere,file="S_gere.txt",sep=";")
write.table(match_scc,file="S_facturation.txt",sep=";",row.names=FALSE,dec=",")
write.table(resumecp,file="S_resumecp.txt",sep=";")
write.table(resumedep,file="S_resumedep.txt",sep=";")
"-------------------------------------------------------------------------------------------------------------------------"
#F) PHASE 6: STATISTIQUES 
## Affichage du palmarès des plus fortes facturations des mopieurs classes selon le modèle et par Situation geographique. 

## Affichage du palmarès des plus fortes volumetries compteur total avec visualisation de progression hebdomadaire. 
# Recuperation dans un data.frame à 3 dimensions (histoKapx) de tous les historiques hebdomadaires disponibles.  
#Cf ligne 51 
 
if (colorType == 1 ) {
histo11 <- weeklySurvey_C3003(histoKapx,1,colorType) #1 pour tous les releves (hebdomadaires et journaliers)
histo11 <- subset(histo11,histo11[,2] == "MP C3003")
}
if (colorType == 0 ) {
histo11 <- weeklySurvey_C3003(histoKapx,surveyType,colorType) #56 pour releves journaliers mais 1 pour les releves hebdomadaires 
}

histo11 <- subset(histo11,histo11[,2] !="MP 4054")
#histo11 <- subset(histo11,histo11[,1] =="E155M433421")
									# Extraction des historiques des compteurs totaux 
histo11 <- histo11[order(histo11[,3],decreasing=F),]				#tri prealable des compteurs maximums 
histo111 <- data.frame(noSerie = histo11[,1],model = histo11[,2],histo11[dim(histo11)[2]:3]) 
	
				#colorType du modèle à analyser : exemple MP C3003


k1 <- round(dim(histo111)[1]*(borne[1])/1000)					#Selection borne minimum  
k2 <- round(dim(histo111)[1]*(borne)[2]/1000)					#Selection borne maximum 

histo13 <- histo111[k1:k2,]											#Traitement du data.frame et Affichage 
#DEFINITIONS DES MODELES 
modeles <- levels(histo13$model)
config <- data.frame(model=modeles,i=as.numeric(1:length(modeles)))

#print graphe pour affichage) 

histo14 <- unicite(histo13,1)




#write.table(histo17,file="S_histo17.txt",sep=";",dec=",",row.names=F)
"-------------------------------------------------------------------------------------------------------------------------"

histo15 <- (merge(histo14,config,by.x=2,by.y=1))
dev.new()
plot.new()
par(mar=c(3,3.1,6,1),las=3,bg="whitesmoke",font.axis=7,col.axis=4,col.main="#3B5A70",cex.axis=10/11,cex.main=6/7)
plot(histo15[,dim(histo15)[2]-4],histo15$a_coeff,pch=histo15$i-1,main=config[,1])
#Plus puissant avec LATTICE 
print("dolibarr est passe par ici , il reviendra par là")
require(lattice)
xyhisto <- cbind(histo15,discretisation.groups = round(log(histo15[,dim(histo15)[2]-4])))
cuts <- levels(cut(histo15[,dim(histo15)[2]-4],breaks=c(0,20,40,60,80,Inf)))
j <- dim(xyhisto)[2]
vise <- xyplot(xyhisto[,j-7] ~ xyhisto$aberr|xyhisto$discretisation.groups,groups=xyhisto$model,data=xyhisto,auto.key=T)
print(vise)
#En insistant sur les quantiles extremes

#Par groupes de departements
"dev.new()
plot.new()
grp <- levels(factor(substr(histod$structure,9,12)))
xyplot(histod$courbure*100~histod$a_coeff|grp,groups=histod$model,data=histod,auto.key=T)"

#Test egalite des moyennes pour des distributions non normales
#summaries <- resume(histo$model,histo$X55)
#wilcox.test(unlist(summaries[[2]][3]),unlist(summaries[[2]][4])) car les va ne sont pas normales. 
#il ressort une egalite des moyennes (au risque 5%) des mopieurs MS610dn et MX511dhe. De Même, pour les mopieurs MP2544 et C3003.
#Ce resultat est relativement innatendu. 

"
for(i in (1:length(number_serie)))
{
plot(1:(ncol(histo15)-4),as.double(histo15[i,3:(ncol(histo15)-2)]))
text(2,number_serie[i])
scan()
}"
# ANALYSE PCA SUR L ENSEMBLE DES COMPTEURS DISPONIBLES AU DELA DES SEULS COMPTEURS GLOBALISES.
#CONSTRUIRE UN DATA FRAME SELON LE CHOIX DES COLONNES CI-DESSOUS 
#Pour les mopieurs C3003 
ACP(gere,"MP C3003",c(20,21,24,25,26,27,29))
log_gere <- gere
log_gere[,c(20,21,24,25,26,27,29)] <- log(gere[,c(20,21,24,25,26,27,29)],10)
ACP(log_gere,"MP C3003",c(20,21,24,25,26,27,29))
#Pour les mopieurs MP2554
gere9 <- subset(gere,gere[,5] == "MP 2554")
gerec3 <- gere9[,c(16,21,24,26,29,33,37)]
gerec3 <- gere9[,c(16,29,37)]
row.names(gerec3) <- gere9[,2]
x <- PCA(gerec3,axes=c(1,2))

#Pour les mopieurs MX511
gere9 <- subset(gere,gere[,5] == "MX511dhe")
gerec3 <- gere9[,c(40,41,46,47)]
row.names(gerec3) <- gere9[,2]
"Compteur.machine = impression.recto + copie.recto + impression.RV + copie.RV"

#Librairie Analyse Factorielle : FactoMineR 
#Fonction PCA 

