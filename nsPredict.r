nsPredict <- function(gere,monnoserie,listnoserie) #Vers une prédicition d'une n° de série à partir d'une faute d'orthographe ou de saisie. 
#gere est le data.frame des mopieurs de la DGFIP. Les seules colonnes qui intéresse la procédure nsPredict sont :
# 1) La colonne format character contenant les numéros de séries. 
# 2) La colonne format factor contenant la typologie (la colonne qui supervise l'appartenance à tel ou tel groupes. 
# La recherche de ces colonnes peut faire l'objet d'une recherche automatique. 
#listnoserie est un vecteur de char contenant plusieurs numeros de série à classifier. 
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
"-----------------------------------------------------------------------------------------"
# fonction auxilliaire pour l'élaboration de matrice de fréquence. 
parse1 <- function(noserie,pattern)
{
df.mineHz <- sapply(pattern,function(p) {stringr::str_count(noserie,p)}) 
colnames(df.mffineHz) <- pattern		
return(df.mineHz)		
}
"-----------------------------------------------------------------------------------------"
parse2 <- function(noserie,pattern)
{
df.mineHz <- t(sapply(noserie,function(serialNumber) {
				sapply(pattern,function(p) {
					sum(1/str_locate_all(serialNumber,p)[[1]][,1])})}
						)		
)
return(df.mineHz)
}
"-----------------------------------------------------------------------------------------"
parse3 <- function(noserie,pattern)
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
#------------------------------------------------------------------------
pattern <- setdiff(stringr::str_c(0:9),c(0:3,8,9)) #Les motifs recherchés sont les chiffres en simple
pattern <- append(pattern,setdiff(LETTERS,c("A","B","D","E","F","H","I","J","K","L","N","O","P","Q","R","S","T","U","V","W","X","Y")))
		
#pattern <- str_c(c(0:9,LETTERS))
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
#utiliser MASS::stepAIC(reg) de MAAS pour une selection de var. optimale
reg.subsets <- leaps::regsubsets(y~.,X) 
#pour le choix de meilleurs var. discriminantes.=>visu:  plot(reg.subsets)

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
#modele.lda <- lda(factor(is_modele) ~ .,data=df.training) #Package MASS
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
lprop <- round(predict(modele.kernlab,parse3(monnoserie,pattern),type="probabilities"),2) #prédictibilité selon le package kernlab
#lprop <- round(predict(modele.svm,parse3(monnoserie,pattern), probability = FALSE),2) #prédictibilité selon le package e1071
print(stringr::str_c("la predictivité du ",monnoserie," concernant Cle modele ",models," est de ",lprop[end(lprop)[1],]*100,"%")) 

#------------------AFFICHAGE DE PREDICTIBILITE DU MODELE-------------------------------------------------------------------------------------------#
lprop <- lprop[end(lprop)[1],]
print("force de predicitibilite du modele")
print(lprop)
#------------------Fin module de PREDICTIBILITE DU MODELE------------------------------------------------------------------------------------------#
#------------KERNLAB MODELE -----------------------------------------------------------------------------------------------------------------------#
#ldaTab <- table(df.test$is_modele,predict(modele.lda,df.test)$class)
#mdaTab <- table(df.test$is_modele,predict(modele.mda,newdata=df.test,type="class"))
svmTab <- table(df.test$is_modele,predict(modele.svm,newdata=df.test,type="class"))
ksvmTab <- table(df.test$is_modele,predict(modele.kernlab,newdata=df.test,type="response")) #Package kernlab
#df.test.lda <- data.frame(df.test,"predict.class"=predict(modele.lda,df.test)$class)
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
df.test <- tibble::as.tibble(df.test)
return(list(	head(data.training),
				aggregate(df.test[modelsEtudies],df.test["is_modèle"],mean),
				df.CorrAnalyse,
				ksvmTab,reg,viewPCA,pattern,reg.subsets))
#summary(sapply(1:100,function(i){(1-sum(diag(nsPredict(gere)[[2]])/500))*100})) : pour vérifier le modèle. 
}
"----------------------------------------------------------------------------------------------------------"


