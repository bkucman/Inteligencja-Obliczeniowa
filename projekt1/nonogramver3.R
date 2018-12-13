install.packages("genalg")
library(genalg)
## nonogramy
#4x4
nonogramx <- list(rows = list(2L,c(1L,1L),c(1L, 1L),2L), 
                 cols= list(2L,c(1L,1L),c(1L,1L),2L))
#5x5
nonogramx <- list(rows = list(c(2L,2L), c(2L,2L), NULL, c(1L, 1L), 3L), 
                 cols= list(c(2L,1L), c(2L,1L),1L,c(2L,1L), c(2L,1L)))
#6x6
nonogramx <- list(rows = list(1, 2L,4L, c(2L,2L),c(1L, 1L),c(1L, 1L) ), 
                 cols= list(1L,4L,3L,2L,4L,1L))
#10x10
nonogramx <- list(cols = list(2L,4L,4L,8L,c(1L,1L),c(1L,1L),c(1L,1L,2L),
                             c(1L,1L,4L),c(1L,1L,4L),8L),
                 rows = list(4L,c(3L,1L),c(1L,3L),c(4L,1L),c(1L,1L),c(1L,3L),c(3L,4L),
                             c(4L,4L),c(4L,2L),2L))
#15x15
nonogramx <- list(cols = list(c(3L,2L),c(1L,2L,3L),c(2L,2L,5L),11L,14L,15L,c(3L,1L,2L,5L),c(2L,2L),
                             c(2L,2L,1L),c(2L,1L,1L,2L,1L),c(2L,1L,1L,1L),c(2L,2L,2L),c(2L,1L,2L,3L),c(3L,8L),9L),
                 rows = list(9L,11L,c(3L,2L),c(2L,2L,1L),c(1L,5L,2L,2L,1L),c(1L,3L,1L,1L,2L),c(2L,4L,2L),c(6L,2L,3L),
                             c(4L,3L),c(3L,2L),c(5L,1L,1L),c(5L,2L,2L),c(6L,2L),c(8L,2L),12L))

#20x20
nonogramx <- list(rwos = list(2L,2L,1L,1L,c(1L,3L),c(2,5),c(1L,7L,1L,1L),c(1L,8L,2L,2L),
                             c(1L,9L,5L),c(2L,16),c(1L,17L),c(7L,11L),c(5L,5L,3L),c(5L,4L),
                             c(3L,3L),c(2L,2L),c(2L,1L),c(1L,1L),c(2L,2L),c(2L,2L)),
                 cols = list(5L,c(5L,3L),c(2L,3L,4L),c(1L,7L,2L),8L,9L,9L,8L,7L,8L,9L,10L,
                             13L,c(6L,2L),4L,6L,6L,5L,6L,6L))

## oblicza karê w konkretnym wierszu. 1 pkt karny za jeden wiersz #############
penaltyRows <- function(foundFields,i){
  penalty = 0
  #print(znalezione)
  if (length(foundFields) != length(nonogramx$rows[[i]])){
    return(1)
  }
  
  if ( all(foundFields == nonogramx$rows[[i]])){
    return(penalty)
  } else if(sum(nonogramx$rows[[i]]) == sum(foundFields)){
    return(1)
  }
  return(1)
}

penaltyCols <- function(foundFields,i){
  penalty = 0
  #print(znalezione)
  if (length(foundFields) != length(nonogramx$cols[[i]])){
    return(1)
  }
  
  if ( all(foundFields == nonogramx$cols[[i]])){
    return(penalty)
  } else if(sum(nonogramx$cols[[i]]) == sum(foundFields)){
    return(1)
  }
  return(1)
}
#####################################################################

## drugwa  wersja obliczania kary za wiersze i kolumny ##############
penaltyRows2 <- function(foundFields,i){
  penalty = 0
  diference = length(foundFields) - length(nonogramx$rows[[i]])
  if(diference != 0){
    penalty = penalty + abs(diference)
  }else if(all(foundFields == nonogramx$rows[[i]])){
    return(0)
  }
  countFind = sum(foundFields)
  countNono = sum(nonogramx$rows[[i]])
  penalty = penalty + abs(countNono - countFind)
  
  return(penalty)
}

penaltyCols2<- function(foundFields,i){
  penalty = 0
  diference = length(foundFields) - length(nonogramx$cols[[i]])
  if(diference != 0){
    penalty = penalty + abs(diference)
  }else if(all(foundFields == nonogramx$cols[[i]])){
    return(0)
  }
  countFind = sum(foundFields)
  countNono = sum(nonogramx$cols[[i]])
  penalty = penalty + abs(countNono - countFind)
  
  return(penalty)
  
}
#############################################################################


## znajduje ci¹gi zamalaowanych pól w wierszach nonogramu
rowsFitnessNonogram3 <- function(chrNonogram){
  penalty = 0
  iterChr = 1
  
  ### wiersze
  for(i in 1:sqrt(length(chrNonogram))){ ## zmiana wiersza
    countBlackFields = 0
    foundBlackStrips <- NULL
    
    for(j in 1:sqrt(length(chrNonogram))){ ## iteracje po wierszu
      
      if(chrNonogram[iterChr] == 1){ ## sprawdzenie czy zamalowany i zliczanie ci¹gu zamalowanych
        countBlackFields = countBlackFields + 1
        
      } else if(chrNonogram[iterChr] == 0){ # jeœli 0(niepomalowany) to sprawdzam czy by³ jakiœ ci¹g pomalowanych
        if(countBlackFields > 0){
          
          foundBlackStrips <- append(foundBlackStrips, countBlackFields)
          countBlackFields = 0
        }
      }
      iterChr = iterChr + 1
    }
    if(countBlackFields > 0){ ## jeœli wiersz siê skoñczy³ sprawdzam czy by³ ci¹g zamalowany
      #print("tu")
      foundBlackStrips <- append(foundBlackStrips, countBlackFields)
      countBlackFields = 0
    }
    ### porównanie i pkt karne jakiœ for po nonogram{AxA}$row
    penalty = penalty + penaltyRows2(foundBlackStrips,i) ## obliczenie kary na podstawie wiersza
    #return(foundBlackStrips)
  }
  return(penalty)
}

## znajduje ci¹gi zamalaowanych pó³ w kolumnach nonogramu
colsFitnessNonogram3 <- function(chrNonogram){
  penalty = 0
  ### kolumny
  #print("tu")
  for(i in 1:sqrt(length(chrNonogram))){ ## zmiana wiersza
    countBlackFields = 0
    foundBlackStrips <- NULL
    iterChr = i
    for(j in 1:sqrt(length(chrNonogram))){ ## iteracje po wierszu
      
      if(chrNonogram[iterChr] == 1){ ## sprawdzenie czy zamalowany i zliczanie ci¹gu zamalowanych
        countBlackFields = countBlackFields + 1
        
      } else if(chrNonogram[iterChr] == 0){ # jeœli 0(niepomalowany) to sprawdzam czy by³ jakiœ ci¹g pomalowanych
        if(countBlackFields > 0){
          
          foundBlackStrips <- append(foundBlackStrips, countBlackFields)
          countBlackFields = 0
        }
      }
      iterChr = iterChr + sqrt(length(chrNonogram))
    }
    
    if(countBlackFields > 0){ ## jeœli wiersz siê skoñczy³ sprawdzam czy by³ ci¹g zamalowany
      foundBlackStrips <- append(foundBlackStrips, countBlackFields)
      countBlackFields = 0
    }
    ### porównanie i pkt karne jakiœ for po nonogram{AxA}$row
    penalty = penalty + penaltyCols2(foundBlackStrips,i) ## obliczenie kary na podstawie wiersza
  }
  return(penalty)
}

# funckja sumuj¹ca kary
fitnessNonogram3 <- function(chrNonogram) {
  kara = 0
  kara = kara + rowsFitnessNonogram3(chrNonogram)
  kara = kara + colsFitnessNonogram3(chrNonogram)
  
  if( !is.null(kara) && length(kara) > 0){
    return(kara)
  } else
    return(0)
}

## kod  poni¿ej u¿ywany do testowania . Tylko rêczna zmiana rozmiaru zagadaki poprzez ponowene podstawienie innej
nonogramGenA <- rbga.bin(size = 36, popSize = 200, iters = 100,
                            mutationChance = 0.08, elitism = T, evalFunc = fitnessNonogram3)

summary(nonogramGenA, echo=TRUE)
minEval = min(nonogramGenA$evaluations)

#test mutation
wyniki <- NULL
czasy <- NULL

## testowanie dzia³ania funckji fitness zale¿nie od mustacji i populacji
popsize = 100
it = 50
for( i in 1:6 ){
  czas <- system.time(a <- rbga.bin(size = 36, popSize = popsize, iters = it,
                           mutationChance = 0.08, elitism = T, evalFunc = fitnessNonogram3))
  wyniki <- append(wyniki,min(a$evaluations) )
  czasy <- append(czasy,czas[3])
  popsize = popsize + 50
  it = it + 50
}

czasyRozmiarow <- NULL

  czasR <- system.time(rbga.bin(size = 400 , popSize = 200, iters = 100,
                         mutationChance = 0.08, elitism = T, evalFunc = fitnessNonogram3))
  
  czasyRozmiarow <- append(czasyRozmiarow,czasR[3])
  rozmiarChromosomu <- c(16,25,36,100,225,400)
  
plot(type = "l", main="Czas dzia³ania wzglêdem rozmiaru",xlab = "Rozmiar chromosomu", ylab = "Czas", rozmiarChromosomu, czasyRozmiarow, col = "blue")