install.packages("genalg")
library(genalg)

nonogram5x5 <- list(rows = list(c(2L,2L), c(2L,2L), NULL, c(1L, 1L), 3L), 
  cols= list(c(2L,1L), c(2L,1L),1L,c(2L,1L), c(2L,1L)))

nonogram6x6 <- list(rows = list(1, 2L,4L, c(2L,2L),c(1L, 1L),c(1L, 1L) ), 
                    cols= list(1L,4L,3L,2L,4L,1L))

nonogram10x10 <- list(cols = list(2L,4L,4L,8L,c(1L,1L),c(1L,1L),c(1L,1L,2L),
                                  c(1L,1L,4L),c(1L,1L,4L),8L),
                      rows = list(4L,c(3L,1L),c(1L,3L),c(4L,1L),c(1L,1L),c(1L,3L),c(3L,4L),
                                  c(4L,4L),c(4L,2L),2L))

length(nonogram10x10$rows)

aa$row_clues[[3]][1] ##


#################
# chromosom dla 5x5 to 25 bitów
# przyk³adowy 5x5 wiersz po wierszu chr = length(c(1,1,1,0,1,1,1,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0))
chrExample = c(1,1,0,1,1,
               1,1,0,0,1,
               0,1,0,1,0,
               1,0,1,0,1,
               0,1,0,1,0)
sum(nonogram10x10$cols[[7]])
czyKaraWiersze <- function(znalezione,i,rodzaj){
  kara = 0
  #print(znalezione)
  if (length(znalezione) != length(nonogram5x5$rows[[i]])){
    return(1)
  }
  
  if ( all(znalezione == nonogram5x5$rows[[i]])){
    return(kara)
  } else if(sum(nonogram5x5$rows[[i]]) == sum(znalezione)){
    return(1)
  }
  return(1)
  ## druga wersja wiêcej karnych bêdzie
}
czyKaraKolumny <- function(znalezione,i){
  kara = 0
  #print(znalezione)
  if (length(znalezione) != length(nonogram5x5$cols[[i]])){
    return(1)
  }
  
  if ( all(znalezione == nonogram5x5$cols[[i]])){
    return(kara)
  } else if(sum(nonogram5x5$cols[[i]]) == sum(znalezione)){
    return(1)
  }
  return(1)
}

wierszeFitnessNonogram <- function(chrNonogram){
  kara = 0
  iterChr = 1
  
  ### wiersze
  for(i in 1:sqrt(length(chrNonogram))){ ## zmiana wiersza
    licznikZajetePola = 0
    znalezioneZajete <- NULL
    
    for(j in 1:sqrt(length(chrNonogram))){ ## iteracje po wierszu
      
      if(chrNonogram[iterChr] == 1){ ## sprawdzenie czy zamalowany i zliczanie ci¹gu zamalowanych
        licznikZajetePola = licznikZajetePola + 1
        
      } else if(chrNonogram[iterChr] == 0){ # jeœli 0(niepomalowany) to sprawdzam czy by³ jakiœ ci¹g pomalowanych
        if(licznikZajetePola > 0){
          
          znalezioneZajete <- append(znalezioneZajete, licznikZajetePola)
          licznikZajetePola = 0
        }
      }
      iterChr = iterChr + 1
    }
    if(licznikZajetePola > 0){ ## jeœli wiersz siê skoñczy³ sprawdzam czy by³ ci¹g zamalowany
      #print("tu")
      znalezioneZajete <- append(znalezioneZajete, licznikZajetePola)
      licznikZajetePola = 0
    }
    ### porównanie i pkt karne jakiœ for po nonogram{AxA}$row
    kara = kara + czyKaraWiersze(znalezioneZajete,i) ## obliczenie kary na podstawie wiersza
    #return(znalezioneZajete)
  }
  return(kara)
}

kolumnyFitnessNonogram <- function(chrNonogram){
  kara = 0
  ### kolumny
  #print("tu")
  for(i in 1:sqrt(length(chrNonogram))){ ## zmiana wiersza
    licznikZajetePola = 0
    znalezioneZajete <- NULL
    iterChr = i
    for(j in 1:sqrt(length(chrNonogram))){ ## iteracje po wierszu
      
      if(chrNonogram[iterChr] == 1){ ## sprawdzenie czy zamalowany i zliczanie ci¹gu zamalowanych
        licznikZajetePola = licznikZajetePola + 1
        
      } else if(chrNonogram[iterChr] == 0){ # jeœli 0(niepomalowany) to sprawdzam czy by³ jakiœ ci¹g pomalowanych
        if(licznikZajetePola > 0){
          
          znalezioneZajete <- append(znalezioneZajete, licznikZajetePola)
          licznikZajetePola = 0
        }
      }
      iterChr = iterChr + sqrt(length(chrNonogram))
    }
    
    if(licznikZajetePola > 0){ ## jeœli wiersz siê skoñczy³ sprawdzam czy by³ ci¹g zamalowany
      znalezioneZajete <- append(znalezioneZajete, licznikZajetePola)
      licznikZajetePola = 0
    }
    ### porównanie i pkt karne jakiœ for po nonogram{AxA}$row
    kara = kara + czyKaraKolumny(znalezioneZajete,i) ## obliczenie kary na podstawie wiersza
  }
  return(kara)
}

fitnessNonogram <- function(chrNonogram) {
  kara = 0
  kara = kara + wierszeFitnessNonogram(chrNonogram)
  kara = kara + kolumnyFitnessNonogram(chrNonogram)

  if( !is.null(kara) && length(kara) > 0){
    return(kara)
    } else
      return(0)
}

########## testowe
fitnessNonogram(chrExample)

nonogramGenAlgo <- rbga.bin(size =25, popSize = 200, iters = 100,
                         mutationChance = 0.04, elitism = T, evalFunc = fitnessNonogram)
summary(nonogramGenAlgo, echo=TRUE)

nonogramGenAlgo6x6 <- rbga.bin(size =36, popSize = 12000, iters = 300,
                            mutationChance = 0.04, elitism = T, evalFunc = fitnessNonogram)
summary(nonogramGenAlgo6x6, echo=TRUE)


nonogramGenAlg
aa <- NULL
bb <- NULL
aa <- append(aa,2)
aa <- append(aa,2)
bb <- append(bb,5)
bb <- append(bb,5)
length(aa)
if(identical(aa,nonogram5x5$rows[[1]])){
  print("tak")
}
if ( all(aa == nonogram5x5$rows[[3]])){
  print("tak")
}

printNono <- function(chrN){
  for(i in 1:sqrt(length(chrN))){
    print(chrN[])
  }
}         
          