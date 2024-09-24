

#Leer archivo

infert <- read.csv("C:/Users/2davi/OneDrive/Desktop/2 Anno/Statistica Per la Azienda(COZZUCOLI)/Proggetto bayesiano 1/infert.txt", stringsAsFactors=TRUE)
View(infert)

summary(infert)

#Convertir en factor

infert$case<-as.factor(infert$case)
#infert$induced<-as.factor(infert$induced)
#infert$spontaneous<-as.factor(infert$spontaneous)

parityT<-ifelse(infert$parity==1,"1",">1")
ageT<-ifelse(infert$age<36,"21-35",">35")
inducedT<-ifelse(infert$induced==0,"No","yes")
spontaT<-ifelse(infert$spontaneous==0,"No","yes")

parityT<-as.factor(parityT)
ageT<-as.factor(ageT)
inducedT<-as.factor(inducedT)
spontaT<-as.factor(spontaT)


summary(infert)

attach(infert)
infertT<-data.frame(education, ageT, parityT, inducedT, case, spontaT)

#Exportado el archivo modificado
write.table(data.frame(infertT), file = "infertP.txt", sep = ",")

##Dataset procesado

infertP <- read.csv("C:/Users/2davi/OneDrive/Desktop/2 Anno/Statistica Per la Azienda(COZZUCOLI)/Proggetto bayesiano 1/Infert/infertP.txt", stringsAsFactors=TRUE)
View(infertP)
#Elimino la columna de que cuenta las cantidad de instancias
#infertP<-infertP[][-1]
summary(infertP)

#Funcion para convertir en factor 

infertP[] <-
  lapply(infertP, function(x) {
    if (is.numeric(x) ||
        is.character(x)) {
      return(as.factor(x))
    } else {
      return(x)
    }
  })
summary(infertP)

#Analisis preliminar

#Education
tab <- table(infertP$case, infertP$education)
barplot(tab, 
        main="Grafico a barre: Case VS Education", 
        xlab="Education", 
        ylab="Frequenze", 
        legend = rownames(tab), 
        ylim = c(0, 150), 
        col=c("blue", "red"), 
        beside=TRUE)
#Education
tab1 <- table(infertP$case, infertP$ageT)
barplot(tab1, 
        main="Grafico a barre: Case VS Age", 
        xlab="Age", 
        ylab="Frequenze", 
        legend = rownames(tab1), 
        ylim = c(0, 150), 
        col=c("blue", "red"), 
        beside=TRUE)
#Induced
tab2 <- table(infertP$case, infertP$inducedT)
barplot(tab2, 
        main="Grafico a barre: Case VS Induced", 
        xlab="Induced", 
        ylab="Frequenze", 
        legend = rownames(tab2), 
        ylim = c(0, 150), 
        col=c("blue", "red"), 
        beside=TRUE)
#Spontaneous
tab3 <- table(infertP$case, infertP$spontaT)
barplot(tab3, 
        main="Grafico a barre: Case VS Spontaneous", 
        xlab="Spontaneous", 
        ylab="Frequenze", 
        legend = rownames(tab3), 
        ylim = c(0, 150), 
        col=c("blue", "red"), 
        beside=TRUE)






#Contruccion del la red bayesiana
#Paquetes
library(lattice)
library(BiocManager)
library(gridExtra)
library(gRain)
library(Rgraphviz)
library(graph)
library(bnlearn)


#DAG vacio

nodi<-c("education", "ageT", "parityT", "inducedT", "case", "spontaT")
nodi
class(nodi)
dag<- empty.graph(nodes=nodi, num=1)
dag

class(dag)
dag
plot(dag)

#Definicion de la blacklist

cond1<-data.frame(from=c("case"), to=c("ageT","parityT", "inducedT", "spontaT", "education"))
cond2<-data.frame(from=c("parityT", "inducedT", "spontaT", "education"), to=c("ageT"))
blacklist=data.frame(rbind(cond1, cond2))



#Red con HC criterio BIC

learned <- hc(infertP, blacklist=blacklist)
plot(learned)
graphviz.plot(learned, fontsize = 25)
modelstring(learned)
learned
score(learned, data=infertP, type="bic")
arc.strength(learned, data = infertP, criterion = "x2")

#Red con HC criterio AIC

learned1 <- hc(infertP, score="aic", blacklist=blacklist)
plot(learned1)
graphviz.plot(learned1, fontsize = 25)
modelstring(learned1)
learned1
score(learned1, data=infertP, type="aic")
arc.strength(learned1, data = infertP, criterion = "x2")

#Red con HC criterio bdeu

learned2 <- hc(infertP, score="bde", blacklist=blacklist)
plot(learned2)
graphviz.plot(learned2, fontsize = 25)
modelstring(learned2)
learned2
score(learned2, data=infertP, type="bde")
arc.strength(learned2, data = infertP, criterion = "x2")

#Primera modifica



learned2<- reverse.arc(learned2, from="parityT", to="education")
learned2<- drop.arc(learned2, from="spontaT", to="inducedT")
learned2<- set.arc(learned2, from="inducedT", to="case")

graphviz.plot(learned2, fontsize = 25)
score(learned2, data=infertP, type="bde")
arc.strength(learned2, data = infertP, criterion = "x2")

learned2


learned2<- drop.arc(learned2, from="ageT", to="education")
learned2<- set.arc(learned2, from="ageT", to="parityT")

graphviz.plot(learned2, fontsize = 25)
score(learned2, data=infertP, type="bde")
arc.strength(learned2, data = infertP, criterion = "x2", debug=TRUE)

learned2


#Verificar todas la dependencias del DAG con criterio X2


A<-ci.test("parityT", "ageT","education", test = "x2", data = infertP)
B<-ci.test("parityT", "education","ageT", test = "x2", data = infertP)
C<-ci.test("inducedT", "parityT", test = "x2", data = infertP)
D<-ci.test("spontaT", "parityT", test = "x2", data = infertP)
E<-ci.test("case", "inducedT", "spontaT", test="x2", data=infertP)
F<-ci.test("case", "spontaT", "inducedT", test="x2", data=infertP)

nameR<-c(A$data.name,B$data.name, C$data.name, D$data.name, E$data.name, F$data.name)
p_value<-c(A$p.value, B$p.value, C$p.value, D$p.value, E$p.value, F$p.value)
colN<-c("name", "p-value")

testChi<-cbind(nameR, p_value)
testChi


#Verificar todas la dependencias del DAG con criterio mi


G<-ci.test("parityT", "ageT","education", test = "mi", data = infertP)
H<-ci.test("parityT", "education","ageT", test = "mi", data = infertP)
I<-ci.test("inducedT", "parityT", test = "mi", data = infertP)
J<-ci.test("spontaT", "parityT", test = "mi", data = infertP)
K<-ci.test("case", "inducedT", "spontaT", test="mi", data=infertP)
L<-ci.test("case", "spontaT", "inducedT", test="mi", data=infertP)


nameT<-c(G$data.name,H$data.name, I$data.name, J$data.name, K$data.name, L$data.name)
p_value1<-c(G$p.value, H$p.value, I$p.value, J$p.value, K$p.value, L$p.value)
testMi<-cbind(nameT, p_value1)
testMi

learned

bn.bayes <- bn.fit(learned2, data = infertP, method = "bayes",iss = 10)
bn.bayes

nparams(bn.bayes)

#D-separazione

dsep(learned2, x = "education", y = "ageT")
#Conessione serial
dsep(learned2, x = "ageT", y = "inducedT") # nessuna evidenza su parityT
dsep(learned2, x = "ageT", y = "inducedT", z="parityT") #  evidenza su parityT
#Conessione divergente
dsep(learned2, x = "inducedT", y = "spontaT") #  nessuna evidenza su parityT
dsep(learned2, x = "inducedT", y = "spontaT", z="parityT") #  evidenza su parityT
#Conessione convergente
dsep(learned2, x = "ageT", y = "education", z="parityT") #  evidenza su parityT
dsep(learned2, x = "inducedT", y = "spontaT", z="case") # evidenza su case


#CPT Grafici

bn.fit.dotplot(bn.bayes$parityT, main = "PROBABILITA CONDIZIONATE: Parity",xlab = "Pr(Parity | Age,Education)", ylab = "")
bn.fit.dotplot(bn.bayes$case, main = "PROBABILITA CONDIZIONATE: Case",xlab = "Pr(Case | Spontaneous,Induced)", ylab = "")


#Preguntas

library(gRain)
junction = compile(as.grain(bn.bayes))
junction

#Qual è la probabilità che la donna abbia problemi di infertilità, quando quando ha avuto 
#aborti indotti?
  #senza envidenze
  querygrain(junction, nodes="case", type="marginal")
## $case
## case
## 0 1 
## 0.6708457 0.3291543
#con envidenze
dom1<-setEvidence(junction, nodes="inducedT", states = "yes")
querygrain(dom1, nodes="case", type="marginal")
## $case
## case
## 0 1 
## 0.61902 0.38098

#Qual è la probabilità che la donna abbia problemi di infertilità, quando quando ha avuto 
#aborti spontanei?
#senza envidenze
  querygrain(junction, nodes="case", type="marginal")
## $case
## case
## 0 1 
## 0.6708457 0.3291543
#con envidenze
dom2<-setEvidence(junction, nodes="spontaT", states = "yes")
querygrain(dom2, nodes="case", type="marginal")
## $case
## case
## 0 1 
## 0.4931268 0.5068732

#Qual è la probabilità che la donna abbia problemi di infertilità, quando quando ha avuto 
#aborti spontanei e aborti indotti?
#senza envidenze
querygrain(junction, nodes="case", type="marginal")
## $case
## case
## 0 1 
## 0.6708457 0.3291543
#con envidenze
dom3<-setEvidence(junction, nodes=c("spontaT","inducedT"), states = c("yes","
yes"))
querygrain(dom3, nodes="case", type="join")
## case
## 0 1 
## 0.5149254 0.4850746

#Qual è la probabilità che la donna maggiore di 35 anni e rimasta incinta più di una volta, 
#abbia problemi di infertilità, quando ha avuto aborti spontanei?
  #senza envidenze
  querygrain(junction, nodes="case", type="marginal")
## $case
## case
## 0 1 
## 0.6708457 0.3291543
#con envidenze
dom4<-setEvidence(junction, nodes=c("ageT","parityT","spontaT"), states = c("
>35",">1","yes"))
querygrain(dom4, nodes="case", type="join")
## case
## 0 1 
## 0.4966915 0.5033085


#Qual è la probabilità che una donna con un'istruzione di 0 a 5 anni abbia avuto più di una 
#gravidanza?

#senza envidenze
querygrain(junction, nodes="parityT", type="marginal")
## $parityT
## parityT
## >1 1 
## 0.6264056 0.3735944
#con envidenze
dom5<-setEvidence(junction, nodes="education", states = "0-5yrs")
querygrain(dom5, nodes="parityT", type="marginal")
## $parityT
## parityT
## >1 1 
## 0.7896866 0.2103134
