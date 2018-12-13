install.packages("genalg")
library(genalg)
#4x4
nonogram <- list(rows = list(2L,c(1L,1L),c(1L, 1L),2L), 
                    cols= list(2L,c(1L,1L),c(1L,1L),2L))
#5x5
nonogram <- list(rows = list(c(2L,2L), c(2L,2L), NULL, c(1L, 1L), 3L), 
                    cols= list(c(2L,1L), c(2L,1L),1L,c(2L,1L), c(2L,1L)))
#6x6
nonogram <- list(rows = list(1, 2L,4L, c(2L,2L),c(1L, 1L),c(1L, 1L) ), 
                    cols= list(1L,4L,3L,2L,4L,1L))
#10x10
nonogram <- list(cols = list(2L,4L,4L,8L,c(1L,1L),c(1L,1L),c(1L,1L,2L),
                                  c(1L,1L,4L),c(1L,1L,4L),8L),
                      rows = list(4L,c(3L,1L),c(1L,3L),c(4L,1L),c(1L,1L),c(1L,3L),c(3L,4L),
                                  c(4L,4L),c(4L,2L),2L))
#15x15
nonogram <- list(cols = list(c(3L,2L),c(1L,2L,3L),c(2L,2L,5L),11L,14L,15L,c(3L,1L,2L,5L),c(2L,2L),
                             c(2L,2L,1L),c(2L,1L,1L,2L,1L),c(2L,1L,1L,1L),c(2L,2L,2L),c(2L,1L,2L,3L),c(3L,8L),9L),
                 rows = list(9L,11L,c(3L,2L),c(2L,2L,1L),c(1L,5L,2L,2L,1L),c(1L,3L,1L,1L,2L),c(2L,4L,2L),c(6L,2L,3L),
                             c(4L,3L),c(3L,2L),c(5L,1L,1L),c(5L,2L,2L),c(6L,2L),c(8L,2L),12L))

#20x20
nonogram <- list(rwos = list(2L,2L,1L,1L,c(1L,3L),c(2,5),c(1L,7L,1L,1L),c(1L,8L,2L,2L),
                                  c(1L,9L,5L),c(2L,16),c(1L,17L),c(7L,11L),c(5L,5L,3L),c(5L,4L),
                                  c(3L,3L),c(2L,2L),c(2L,1L),c(1L,1L),c(2L,2L),c(2L,2L)),
                      cols = list(5L,c(5L,3L),c(2L,3L,4L),c(1L,7L,2L),8L,9L,9L,8L,7L,8L,9L,10L,
                                  13L,c(6L,2L),4L,6L,6L,5L,6L,6L))

#Sprawdza czy wiersz jest poprawny jak nie kara 1 
penaltyRow <- function(foundFields,i){
  penalty = 0
  #print(znalezione)
  if (length(foundFields) != length(nonogram$rows[[i]])){
    return(1)
  }
  if ( all(foundFields == nonogram$rows[[i]])){
    return(penalty)
  } else if(sum(nonogram$rows[[i]]) == sum(foundFields)){
    return(1)
  }
  return(1)
}
#Sprawdza czy kolumna jest poprawna jak nie kara 1 
penaltyCols <- function(znalezione,i){
  kara = 0
  #print(znalezione)
  if (length(znalezione) != length(nonogram$cols[[i]])){
    return(1)
  }
  
  if ( all(znalezione == nonogram$cols[[i]])){
    return(kara)
  } else if(sum(nonogram$cols[[i]]) == sum(znalezione)){
    return(1)
  }
  return(1)
}

rowsFitnessNonogram <- function(chrNonogram){
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
    penalty = penalty + penaltyCols(foundBlackStrips,i) ## obliczenie kary na podstawie wiersza
    #return(foundBlackStrips)
  }
  return(penalty)
}

colsFitnessNonogram <- function(chrNonogram){
  penalty = 0
  ### kolumny
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
    penalty = penalty + penaltyCols(foundBlackStrips,i) ## obliczenie kary na podstawie wiersza
  }
  return(penalty)
}

fitnessNonogram <- function(chrNonogram) {
  penalty = 0
  penalty = penalty + rowsFitnessNonogram(chrNonogram)
  penalty = penalty + colsFitnessNonogram(chrNonogram)
  
  if( !is.null(penalty) && length(penalty) > 0){
    return(penalty)
  } else
    return(0)
}

# 
nonogramGenAlgor <- rbga.bin(size = 25, popSize = 200, iters = 100,
                            mutationChance = 0.04, elitism = T, evalFunc = fitnessNonogram)

summary(nonogramGenAlgo, echo=TRUE)

