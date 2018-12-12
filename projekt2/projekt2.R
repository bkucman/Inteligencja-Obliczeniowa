# Błażej Kucman gr. 1
# Projejkt 2 . Zgłędbianie danych
# Baza Heart Disease
heart.disase <- read.csv("processed.cleveland.data",header=TRUE,sep=",")
 
#heart.disase$target
nrow(heart.disase)

heart.disase[88,]

install.packages("editrules")
install.packages("deducorrect")

library(editrules)
library(deducorrect)

#################   Punkt 2   #############################
# ustawienie choroby na tak/nie(0/1) w kolumnie target
## interesują nas dwie wartośći te kolumny - kolumna klasy

## zmiana danych przy użyciu correctWithRules
## jak 0 zostawiam jak (1,2,3,4) zmieniamy na 1. Gdy nie jest liczbą lub inną wartość ustawiam na NA
setSickCorrection <- correctionRules(expression(
  if(is.finite(target)){
    if(target != 0)
     { 
        if (target == 1 | target == 2 | target == 3 | target == 4) {
          target <- 1
      } else
      {
        target <- NA
      }
    }
  }else
  {
    target <- NA
  }
))

tpr <- function(x) {
  (x[1,1]/(x[1,1]+x[1,2]))
}

fpr <- function(x) {
  (x[2,1]/(x[2,1]+x[2,2]))
}



# Uruchomienie powyższych warunków
heart.disase.SickSet <- (correctWithRules(setSickCorrection,heart.disase))$corrected


#usunięcie wierszy z NA z kolumny target 
## zmiejszenie bazy jeśli o wiersz w którym brak informacji czy zdrowy/chory(jeśli instnieją takie)

nrow(heart.disase.SickSet)
heart.disase.Better <- subset(heart.disase.SickSet,is.finite(target))
nrow(heart.disase.Better)

## sprawdzenie gdzie występują w pozostałych kolumnach lub wartośći nie zgadzają się z wytycznymi

E <- editset(c(
               "age >0.0","sex %in% c(0,1)","cp %in% c(1,2,3,4)","trestbps > 0",
               "chol > 0", "fbs %in% c(1,0)","restecg %in% c(0,1,2,3)","thalach > 0",
               "exang %in% c(0,1)", "oldpeak >=0","slope %in% c(1,2,3)","ca %in% c(0,1,2,3)",
               "thal %in% c(3,6,7)"
               ))

ve <- violatedEdits(E,heart.disase.Better)
ve

failed_rows <- data.frame(ve[which(apply(ve, 1, any)), ])
E
failed_rows

# mamy błedy w kolumnie ca i thal (wartości NA) reszta wymagań co do wartości w kolumnach została spełniona\

# użyłem kNN ponieważ w obu kolumnach przyjmowane so wartości z jakiegoś zakresu np.0, 1 lub 3.
# Uznałem że średnia nie będzie adakewatna co do pacjanta ale wynik kogoś podobnego do niego.
install.packages("VIM")
library(VIM)

heart.knn <- kNN(heart.disase.Better)

heart.knn[88,]

# obicięcie kolumn z informacjami gdzie znalezione błędy
heart.disase.ready <- heart.knn[,1:14]
nrow(heart.disase.ready)

#################   Punkt 3   ############################

## a #####################################################

## podział na grupę treningową i testową w stosunku 67/33 

ind <- sample(2, nrow(heart.disase.ready), replace=TRUE, prob=c(0.67, 0.33))
heart.disase.train <- heart.disase.ready[ind==1, 1:14]
heart.disase.test <- heart.disase.ready[ind==2, 1:14]
nrow(heart.disase.train)
nrow(heart.disase.test)

heart.disase.train$target
heart.disase.test$target

## b ##########################################################

install.packages("party")
library("party")

heart.disase.train.ctree <- heart.disase.train
heart.disase.train.ctree$target <- as.factor(heart.disase.train.ctree$target)

tpr.values <- NULL
fpr.values <- NULL

####################### Model drzewa ctree ######################
heart.disase.ctree <- ctree(target ~ age + sex + cp + trestbps + chol + fbs + restecg +
                              thalach + exang + oldpeak + slope + ca + thal, data=heart.disase.train.ctree)

#print(heart.disase.ctree)

#plot(heart.disase.ctree, type="simple")

predicted.ctree <- predict(heart.disase.ctree, heart.disase.test[,1:13])
trueSickValue <- heart.disase.test[,14]
conf.matrix.ctree <- table(heart.disase.test[,14],predicted.ctree)
conf.matrix.ctree
accuracy.ctree <- sum(diag(conf.matrix.ctree))/sum(conf.matrix.ctree)
accuracy.ctree

## obliczanie wartośći tpr i fpr #########################
tpr.values <- append(tpr.values, tpr(conf.matrix.ctree))
fpr.values <- append(fpr.values, fpr(conf.matrix.ctree))

sum(trueSickValue == 0)
length(predicted)

####################### kNN #####################################

library(class)
library(e1071)

# funkcja normalizująca dane 
normalize <- function(x) {
   ((x - min(x)) / (max(x) - min(x)))
}

# normalizacja. Do noirmalizacji wybrałem kolumny pomiarowe z dużymi liczbami 
#aby nie dawały dużych różnic a tylko ustandaryzowane
heart.disase.norm <- heart.disase.ready
heart.disase.norm$age <- unlist( normalize(heart.disase.norm[1]))
heart.disase.norm$trestbps <- unlist(normalize(heart.disase.norm[4]))
heart.disase.norm$chol <- unlist(normalize(heart.disase.norm[5]))
heart.disase.norm$thalach <- unlist(normalize(heart.disase.norm[8]))
#################################################################

################## dzilenie z normą ############################
heart.disase.norm.train <- heart.disase.norm[ind==1, 1:14]
heart.disase.norm.test <- heart.disase.norm[ind==2, 1:14]
nrow(heart.disase.norm.train)
nrow(heart.disase.norm.test)
################################################################

### Modele wyliczeniowe z normą danych i bez
compute.model.knn <- knn(heart.disase.train[,1:13], heart.disase.test[,1:13], cl=heart.disase.train[,14], k = 3, prob=FALSE)
compute.model.norm.knn <- knn(heart.disase.norm.train[,1:13], heart.disase.norm.test[,1:13], cl=heart.disase.norm.train[,14], k = 3, prob=FALSE)



predicted <- compute.model.norm.knn
#predicted <- compute.model.knn
real <- heart.disase.norm.test[,14]
conf.matrix.knn <- table(real,predicted)
conf.matrix.knn
accuracy.knn <- sum(diag(conf.matrix.knn))/sum(conf.matrix.knn)
accuracy.knn

## obliczanie wartośći tpr i fpr #########################
tpr.values <- append(tpr.values, tpr(conf.matrix.knn))
fpr.values <- append(fpr.values, fpr(conf.matrix.knn))
tpr(conf.matrix.knn)
fpr(conf.matrix.knn)
###############################################################

################ naiveBayes ###################################
install.packages("e1071")
library(e1071)

## columnę którajest naszą Klass trzeba przerboić na factor ###
#heart.disase.train.NB <- heart.disase.train
#heart.disase.train.NB$target <- as.factor(heart.disase.train.NB$target)

### z normalizacją
heart.disase.train.NB <- heart.disase.norm.train
heart.disase.train.NB$target <- as.factor(heart.disase.train.NB$target)
###############################################################

############### Model naiveBayes ###############################
modelHeart <- naiveBayes(target ~ ., data = heart.disase.train.NB)

#predicted.naiveBayes <- predict(modelHeart, newdata = heart.disase.test)
# z normalizacją
predicted.naiveBayes <- predict(modelHeart, newdata = heart.disase.norm.test)

#conf.matrix.naiveBayes <- table(predicted.naiveBayes,heart.disase.test$target)

# z normalizacją
conf.matrix.naiveBayes <- table(heart.disase.norm.test$target,predicted.naiveBayes)
conf.matrix.naiveBayes
accuracy.naiveBayes <- sum(diag(conf.matrix.naiveBayes))/sum(conf.matrix.naiveBayes)
accuracy.naiveBayes

## obliczanie wartośći tpr i fpr #########################
tpr.values <- append(tpr.values, tpr(conf.matrix.naiveBayes))
fpr.values <- append(fpr.values, fpr(conf.matrix.naiveBayes))

tpr(conf.matrix.naiveBayes)
fpr(conf.matrix.naiveBayes)
################### SVM #####################################

################# Model SVM #################################
model.SVM <- svm(target ~ ., data = heart.disase.train, type='C-classification',kernel='linear',
                 scale=FALSE)
# z normalizacją
model.SVM <- svm(target ~ ., data = heart.disase.norm.train, type='C-classification',kernel='linear',
                 scale=FALSE)

#print(model.SVM)
#summary(model.SVM)

# z normalizacją
predicted.SVM <- predict(model.SVM, newdata = heart.disase.norm.test)

conf.matrix.SVM <- table(heart.disase.norm.test$target,predicted.SVM)
conf.matrix.SVM
accuracy.SVM <- sum(diag(conf.matrix.SVM))/sum(conf.matrix.SVM)
accuracy.SVM

## obliczanie wartośći tpr i fpr #########################
tpr.values <- append(tpr.values, tpr(conf.matrix.SVM))
fpr.values <- append(fpr.values, fpr(conf.matrix.SVM))

tpr(conf.matrix.SVM)
fpr(conf.matrix.SVM)

# c ##########################################################

## wykres 

plot(fpr.values,tpr.values,t="b")

names <- c("ctree", "kNN", "NaiveBayes","SVM")

plot(fpr.values, tpr.values, main="ROC Space",
     xlab="FPR",
     ylab="TPR",
     col="blue",
     t="b")
text(fpr.values, tpr.values, labels=names, cex= 0.9, pos=c(2,4,4))

#### dokładność wykers
install.packages("plotrix")
library(plotrix)
accuracy.all <- c(accuracy.ctree,accuracy.knn,accuracy.naiveBayes,accuracy.SVM)
plot(accuracy.all,t="h")
barplot(accuracy.all,main="Dokładność metod",
        ylim = c(0,1),
        xlab="Klasyfikator",
        ylab="Dokładność",
        col = c('red','blue','yellow','green'),
        names.arg = names)




#......................

#################   Punkt 4   ############################

### k srednie
require(graphics)
heart.disase.logPrep <- heart.disase.norm[,1:13]
#
#heart.disase.logPrep <- subset(heart.disase.ready, select=-sex)
#heart.disase.logPrep <- subset(heart.disase.ready, select=-fbs)
#heart.disase.logPrep <- subset(heart.disase.ready, select=-restecg)
#heart.disase.logPrep <- subset(heart.disase.ready, select=-exang)
#heart.disase.logPrep <- subset(heart.disase.ready, select=-oldpeak)
#heart.disase.logPrep <- subset(heart.disase.ready, select=-ca)
#heart.disase.logPrep <- subset(heart.disase.ready, select=-target)

heart.disase.logPrep <- heart.disase.norm[,1:13]
heart.disase.log <- log(heart.disase.logPrep)
heart.disase.stand <- scale(heart.disase.logPrep, center=TRUE)
heart.disase.pca <- prcomp(heart.disase.stand)
heart.disase.final <- predict(heart.disase.pca)[,1:13]

#heart.disase.test <- as.data.frame(heart.disase.final)
#heart.disase.test$target <- heart.disase.ready$target

# Rysowaniegrupowania
predicted_plot <- function(x) {
  k <- kmeans(heart.disase.final, x)
  plot(heart.disase.final, col=k$cluster)
  points(k$centers, col=1:2, pch=8, cex=2)
  table(k[["cluster"]])
}
predicted_plot(2)


# może się przyda
k <- kmeans(heart.disase.final, 5)
clusterNew <- table(heart.disase.norm[,14],k$cluster)

sum(k$cluster == 3)

## wypisywanie wszytkich target(oczekiwany stan) danego klastra 
for(i in 1:length(k$cluster)){
  if(k$cluster[i] == 4){
    print(heart.disase.norm$target[i])
  }
}
clusterNew

# sumowanie elemntów klastra
for(j in 1:5){
  sum(k$cluster==1)
}
  
#################   Punkt 5   ############################
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
mat1 <- data.frame(
  age = as.character(heart.disase.ready[1:303,1:1])
  ,sex = as.character(heart.disase.ready[1:303,2:2])
  #,cp = as.character(heart.disase.ready[1:303,3:3])
  #,trestbps = as.character(heart.disase.ready[1:303,4:4])
  #,chol = as.character(heart.disase.ready[1:303,5:5])
  #,fbs = as.character(heart.disase.ready[1:303,6:6])
  #,restecg = as.character(heart.disase.ready[1:303,7:7])
  #,thalach = as.character(heart.disase.ready[1:303,8:8])
  #,exang = as.character(heart.disase.ready[1:303,9:9])
  #,oldpeak = as.character(heart.disase.ready[1:303,10:10])
  #,slope = as.character(heart.disase.ready[1:303,11:11])
  ,ca = as.character(heart.disase.ready[1:303,12:12])
  #,thal = as.character(heart.disase.ready[1:303,13:13])
  ,target = as.character(heart.disase.ready[1:303,14:14])
  ,stringsAsFactors = TRUE);

rules <- apriori(mat1,
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("target=0","target=1"), default="lhs"),
                 control = list(verbose=F)
)

rules.sorted <- sort(rules, by="lift")

subset.matrix <- is.subset(rules.sorted, rules.sorted)

subset.matrix[lower.tri(subset.matrix, diag=T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules.sorted[!redundant]

print(inspect(rules.pruned))

str(heart.disase.ready)
rules <- apriori(heart.disase.ready)
inspect(rules)


###########################
#Dodatkowe wykresy

### Badanie zachorowalnoście względem wieku ###############

woman <- subset(heart.disase.ready,sex == 0)
man <- subset(heart.disase.ready,sex == 1)
length(woman$sex)
length(man$sex)

min(woman$age)
max(woman$age)
min(man$age)
max(man$age)

woman.age.count <- vector("numeric", 100)
man.age.count <- vector("numeric", 100)

for(i in 1:length(woman$age)){
  if(woman$target[i] == 1){
    woman.age.count[woman$age[i]] = woman.age.count[woman$age[i]] + 1
  }
}
for(i in 1:length(man$age)){
  if(man$target[i] == 1){
  man.age.count[man$age[i]] = man.age.count[man$age[i]] + 1
  }
}

woman.age.countN <-normalize(woman.age.count)
man.age.countN <- normalize(man.age.count)

plot(25:85,man.age.count[25:85],t="l",xlab="Wiek",ylab="Ilość osób",main="Zachorowania według wieku",col="blue")
lines(25:85,woman.age.count[25:85],col="red")
legend(0, 10, legend=c("Mężczyźni", "Kobiety"),
       col=c("blue", "red"), lty=1:1, cex=1.3)

#######################################################################
### Badanie, która płeć ma większą szanse na zachorowanie #############
length(woman$age)
womanCunt <- sum(woman.age.count)
length(man$age)
manCount <- sum(man.age.count)

library(plotrix)
womanToPlot <- c(length(woman$age),womanCunt)

manToPlot <- c(length(man$age),manCount)

fan.plot(womanToPlot, labels = c("Kobiety","Zachorowania"), main = "Procentowa zachorowalność kobiet")
fan.plot(manToPlot, labels = c("Mężczyźni","Zachorowania"), main = "Procentowa zachorowalność mężczyzn")


########################################################################
# cukier fbs
length(heart.disase.ready$fbs)
people.fbs <- subset(heart.disase.ready, fbs == 1)
length(people.fbs$fbs)

data.fbs <- c(sum(people.fbs$target ==1),sum(people.fbs$target ==0))

plot(accuracy.all,t="h")
barplot(data.fbs,main="Wysoki cukier",
        ylim = c(0,30),
        ylab="Ilość osób",
        col = c('red','blue'),
        names.arg = c("Zachorowania","Zdrowi"))
legend(0.8,30, legend=c("Liczba osób z wysokim\n cukrem - 45\n")
       , lty=1:1, cex=1.3)



  
  