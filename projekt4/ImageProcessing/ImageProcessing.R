cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)

install.packages("BiocManager")
BiocManager::install("EBImage")

install.packages("mxnet")
install.packages("pbapply")
install.packages("caret")
install.packages("randomForest")
install.packages("gbm")

library(mxnet)
library(pbapply)
library(EBImage)
library(caret)
library(randomForest)
library(gbm)
library(e1071)

# windows
image_dirNormal <- "..\\data\\chest_xray\\train\\NORMAL"
image_dirPneumonia <- "..\\data\\chest_xray\\train\\PNEUMONIA"
test_image_dirNormal <- "..\\data\\chest_xray\\test\\NORMAL"
test_image_dirPneumonia <- "..\\data\\chest_xray\\test\\PNEUMONIA"

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

# calculating train images to vector of pixels
pneumonia_data <- extract_feature(dir_path = image_dirPneumonia, width = width, height = height)
normal_data <- extract_feature(dir_path = image_dirNormal, width = width, height = height, is_pneumonia = FALSE)
dim(pneumonia_data)

# calculating test images to vector of pixels
pneumonia_test_data <- extract_feature(dir_path = test_image_dirPneumonia, width = width, height = height)
normal_test_data <- extract_feature(dir_path = test_image_dirNormal, width = width, height = height, is_pneumonia = FALSE)

# save ready matrix with vectors of images
saveRDS(pneumonia_data, "pneumonia.rds")
saveRDS(normal_data, "normal.rds")
saveRDS(pneumonia_test_data, "pneumonia_test.rds")
saveRDS(normal_test_data, "normal_test.rds")

## Bind test and trains rows in a single dataset
complete_set <- rbind(pneumonia_data, normal_data)
complete_set_test <- rbind(pneumonia_test_data, normal_test_data)

dim(complete_set)
dim(complete_set_test)

# save matrix as csv
#write.csv(complete_set, file = "comletedata.csv")
#write.csv(complete_set_test, file = "comletedatatest.csv")

####### Fix train and test datasets for CNN (mxnet library) #############
train_data <- data.matrix(complete_set)
train_x <- t(train_data[, -1]) # only parameters without label
train_y <- train_data[,1] # label only
train_array <- train_x
dim(train_array) <- c(28, 28, 1, ncol(train_x))

test_data <- data.matrix(complete_set_test)
test_x <- t(complete_set_test[,-1])
test_y <- complete_set_test[,1]
test_array <- test_x
dim(test_array) <- c(28, 28, 1, ncol(test_x))

#ncol(train_array)
#nrow(train_array)
#typeof(complete_set)
#typeof(train_data)
#typeof(train_array)
#length(train_array)

#### Convolutional Neural Network (CNN) ###################################

## Model based on toturial Image Classification in R: MXNet - Shikun Li
mx_data <- mx.symbol.Variable('data')
## 1st convolutional layer 5x5 kernel and 20 filters.
conv_1 <- mx.symbol.Convolution(data = mx_data, kernel = c(5, 5), num_filter = 20)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2,2 ))
## 2nd convolutional layer 5x5 kernel and 50 filters.
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5,5), num_filter = 50)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data = tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
## 1st fully connected layer
flat <- mx.symbol.Flatten(data = pool_2)
fcl_1 <- mx.symbol.FullyConnected(data = flat, num_hidden = 500)
tanh_3 <- mx.symbol.Activation(data = fcl_1, act_type = "tanh")
## 2nd fully connected layer
fcl_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 2)
## Output
NN_model <- mx.symbol.SoftmaxOutput(data = fcl_2)

## Set seed for reproducibility
mx.set.seed(100)

## Device used. 
device <- mx.cpu()


model_CNN <- mx.model.FeedForward.create(NN_model, X = train_array, y = train_y,
                                     ctx = device,
                                     num.round = 30,
                                     array.batch.size = 100,
                                     learning.rate = 0.05,
                                     momentum = 0.9,
                                     wd = 0.00001,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100))

## Test CNN classifier
predict_CNN <- predict(model, test_array)
predicted_labels_CNN <- max.col(t(predict_CNN)) - 1
table(test_data[, 1], predicted_labels_CNN)

sum(diag(table(test_data[, 1], predicted_labels_CNN)))/624


### Random Forest 

data_for_forest <- read.csv("comletedata.csv")
data_for_forest <- data_for_forest[,-1]

data_for_forest_test <- read.csv("comletedatatest.csv")
data_for_forest_test <- data_for_forest_test[,-1]

# Prepare labels to classifier
labels <- data_for_forest[,1]
labels <- as.character(labels)
labels <- as.factor(labels)
# data
data_ready <- data_for_forest[,-1]

model_randomForest = randomForest(data, labels, ntree = 30, maxnodes = 30)

predict_randomForest = predict(model_randomForest, data_for_forest_test[,-1], type = "class")
table = table(predict_randomForest , data_for_forest_test[,1])
sum(diag(table(data_for_forest_test[,1], predict_randomForest )))/624

## Gradient boosting

require(dplyr)

labels_GBM <- data_for_forest$label 
data_for_GBM <- select(data_for_forest, -label)

model_GBM <- gbm.fit(
  x = data_for_GBM,
  y = labels_GBM,
  distribution = "bernoulli",
  shrinkage=0.01,
  interaction.depth = 3,
  n.minobsinnode = 10,
  verbose=F,
  n.trees = 2000
)

predicted.GBM <- predict(model_GBM, newdata = data_for_forest_test[,-1], n.trees = 1000,type = "response")

predicted.GBM <- predict(object = model_GBM, newdata=data_for_forest_test[,-1],
                         n.trees = 2000, type = "response")

predicted.GBM_round <- round(predicted.GBM)

table.GBM <- table(predicted.GBM_round, data_for_forest_test[,1])
sum(diag(table(data_for_forest_test[,1], predicted.GBM_round)))/624

# SVM  ###### dobre

model.SVM <- svm(label ~ ., data = train_data, type='C-classification',kernel='linear',
                 scale=FALSE)

predicted.SVM <- predict(model.SVM, newdata = test_data)
conf.matrix.SVM <- table(test_data[,1],predicted.SVM)

sum(diag(table(test_data[, 1], predicted.SVM)))/624