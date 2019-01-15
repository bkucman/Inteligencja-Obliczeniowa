cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages("mxnet")

install.packages("pbapply")

image_dirNormal <- "..\\data\\chest_xray\\train\\NORMAL"
image_dirPneumonia <- "..\\data\\chest_xray\\train\\pneumonia"

install.packages("BiocManager")
BiocManager::install("EBImage")
library(EBImage)

exampleNormal <- readImage(file.path(image_dirNormal, "IM-0115-0001.jpeg"))
display(exampleNormal)
examplePneumonia <- readImage(file.path(image_dirPneumonia, "person1_bacteria_1.jpeg"))
display(examplePneumonia)

display(resize(examplePneumonia, w = width, h = height))

width <- 28
height <- 28
## pbapply is a library to add progress bar *apply functions
## pblapply will replace lapply
library(pbapply)
extract_feature <- function(dir_path, width, height, is_pneumonia = TRUE, add_label = TRUE) {
  img_size <- width*height
  ## List images in path
  images_names <- list.files(dir_path)
  if (add_label) {
    ## Select only cats or dogs images
    # images_names <- images_names[grepl(ifelse(is_cat, "cat", "dog"), images_names)]
    ## Set label, cat = 0, dog = 1
    ## labels pneumonia = 0, normal = 1
    label <- ifelse(is_pneumonia, 0, 1)
  }
  print(paste("Start processing", length(images_names), "images"))
  ## This function will resize an image, turn it into greyscale
  feature_list <- pblapply(images_names, function(imgname) {
    ## Read image
    img <- readImage(file.path(dir_path, imgname))
    ## Resize image
    img_resized <- resize(img, w = width, h = height)
    ## Set to grayscale
    grayimg <- channel(img_resized, "gray")
    ## Get the image as a matrix
    img_matrix <- grayimg@.Data
    ## Coerce to a vector
    img_vector <- as.vector(t(img_matrix))
    return(img_vector)
  })
  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, feature_list)
  feature_matrix <- as.data.frame(feature_matrix)
  ## Set names
  names(feature_matrix) <- paste0("pixel", c(1:img_size))
  if (add_label) {
    ## Add label
    feature_matrix <- cbind(label = label, feature_matrix)
  }
  return(feature_matrix)
}

pneumonia_data <- extract_feature(dir_path = image_dirPneumonia, width = width, height = height)
normal_data <- extract_feature(dir_path = image_dirNormal, width = width, height = height, is_pneumonia = FALSE)
dim(cats_data)

saveRDS(pneumonia_data, "pneumonia.rds")
saveRDS(normal_data, "normal.rds")

