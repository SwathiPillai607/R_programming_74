if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

if (!requireNamespace("EBImage", quietly = TRUE)) {
  BiocManager::install("EBImage")
}

if (!requireNamespace("keras", quietly = TRUE)) {
  install.packages("keras")
}

library(EBImage)
library(keras)
library(reticulate)

if (!dir.exists(path.expand("~/.virtualenvs/r-tensorflow"))) {
  virtualenv_create(
    envname = "r-tensorflow",
    python = NULL
  )
}

use_virtualenv(
  "~/.virtualenvs/r-tensorflow",
  required = TRUE
)

py_install(
  packages = c("numpy", "tensorflow"),
  pip = TRUE
)

library(tensorflow)

print(tf$constant("TensorFlow is working!"))

setwd("/Users/aryankulkarni/Downloads/drive-download-20260818T090153Z-1-001")

pics <- c(
  "p1.jpg", "p2.jpg", "p3.jpg",
  "p4.jpg", "p5.jpg", "p6.jpg",
  "c1.jpg", "c2.jpg", "c3.jpg",
  "c4.jpg", "c5.jpg", "c6.jpg"
)

mypic <- list()

for (i in 1:12) {
  mypic[[i]] <- readImage(pics[i])
}

print(mypic[[1]])
display(mypic[[8]])
summary(mypic[[1]])
hist(mypic[[2]])
str(mypic)

for (i in 1:12) {
  mypic[[i]] <- resize(mypic[[i]], 28, 28)
}

for (i in 1:12) {
  mypic[[i]] <- array_reshape(mypic[[i]], c(2352))
}

trainx <- NULL

for (i in 1:5) {
  trainx <- rbind(trainx, mypic[[i]])
}

for (i in 7:11) {
  trainx <- rbind(trainx, mypic[[i]])
}

str(trainx)

testx <- rbind(mypic[[6]], mypic[[12]])

trainy <- c(0,0,0,0,0,1,1,1,1,1)

testy <- c(0,1)

trainLabels <- tf$keras$utils$to_categorical(trainy, num_classes = 2)
testLabels <- tf$keras$utils$to_categorical(testy, num_classes = 2)

model <- keras_model_sequential()

model %>%
  layer_dense(
    units = 256,
    activation = "relu",
    input_shape = c(2352)
  ) %>%
  layer_dense(
    units = 128,
    activation = "relu"
  ) %>%
  layer_dense(
    units = 2,
    activation = "softmax"
  )

summary(model)

model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = c("accuracy")
  )

history <- model %>%
  fit(
    trainx,
    trainLabels,
    epochs = 30,
    batch_size = 32,
    validation_split = 0.2
  )

model %>%
  evaluate(
    trainx,
    trainLabels
  )

pred <- model %>%
  predict_classes(trainx)

table(
  Predicted = pred,
  Actual = trainy
)

prob <- model %>%
  predict_proba(trainx)

cbind(
  prob,
  Predicted = pred,
  Actual = trainy
)

model %>%
  evaluate(
    testx,
    testLabels
  )

test_pred <- model %>%
  predict_classes(testx)

table(
  Predicted = test_pred,
  Actual = testy
)

test_prob <- model %>%
  predict_proba(testx)

cbind(
  test_prob,
  Predicted = test_pred,
  Actual = testy
)