image_dirNormal <- "..\\data\\chest_xray\\train\\NORMAL"
image_dirPneumonia <- "..\\data\\chest_xray\\train\\pneumonia"

install.packages("BiocManager")
BiocManager::install("EBImage")
library(EBImage)

exampleNormal <- readImage(file.path(image_dirNormal, "IM-0115-0001.jpeg"))
display(example)
examplePneumonia <- readImage(file.path(image_dirPneumonia, "person1_bacteria_1.jpeg"))
display(example)