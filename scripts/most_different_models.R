library(caret)

tag <- read.csv("data/tag_data.csv", row.names = 1)
tag <- as.matrix(tag)

## Select only models for regression
classModels <- tag[tag[,"Classification"] == 1,]

all <- 1:nrow(classModels)
## Seed the analysis with the SVM model
start <- grep("(rf)", rownames(classModels), fixed = TRUE)
pool <- all[all != start]

## Select 4 model models by maximizing the Jaccard
## dissimilarity between sets of models
nextMods <- maxDissim(classModels[start,,drop = FALSE], 
                      classModels[pool, ], 
                      method = "Jaccard",
                      n = 10)

rownames(classModels)[c(start, nextMods)]

[1] "Random Forest (rf)"
[2] "Linear Distance Weighted Discrimination (dwdLinear)" (only 2 classes)
[3] "Bagged CART (treebag)"
[4] "Patient Rule Induction Method (PRIM)"
[5] "CART (rpart2)"
[6] "Sparse Linear Discriminant Analysis (sparseLDA)"
[7] "Quadratic Discriminant Analysis with Stepwise Feature Selection (stepQDA)"
[8] "Ordered Logistic or Probit Regression (polr)"
[9] "Regularized Linear Discriminant Analysis (rlda)"
[10] "Generalized Additive Model using Splines (bam)"
[11] "SIMCA (CSimca)"
