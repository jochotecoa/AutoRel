library(caret)

tag <- read.csv("data/tag_data.csv", row.names = 1)
tag <- as.matrix(tag)

## Select only models for regression
classModels <- tag[tag[,"Classification"] == 1 & tag[,"Two.Class.Only"] == 0,]

all <- 1:nrow(classModels)
## Seed the analysis with the SVM model
start <- grep("(rf)", rownames(classModels), fixed = TRUE)
pool <- all[all != start]

## Select 4 model models by maximizing the Jaccard
## dissimilarity between sets of models
nextMods <- maxDissim(classModels[start,,drop = FALSE], 
                      classModels[pool, ], 
                      method = "Jaccard",
                      n = 20)

rownames(classModels)[c(start, nextMods)]

[1] "Random Forest (rf)"
[2] "Penalized Multinomial Regression (multinom)"
# [3] "Least Squares Support Vector Machine with Polynomial Kernel (lssvmPoly)" error
# [4] "Regularized Linear Discriminant Analysis (rlda)" estimator=Moore-Penrose Pseudo-Inverse Error in predict(modelFit, newdata)$class :
#   $ operator is invalid for atomic vectors
[5] "Bagged CART (treebag)"
# [6] "Patient Rule Induction Method (PRIM)" Only two-class classification is supported.
[7] "Sparse Linear Discriminant Analysis (sparseLDA)"
[8] "CART (rpart2)"
[9] "Generalized Additive Model using Splines (bam)"
[10] "SIMCA (CSimca)"
[11] "Nearest Shrunken Centroids (pam)"
# [12] "Diagonal Discriminant Analysis (dda)"
# [13] "Boosted Tree (blackboost)" Two-class outcomes only. See ?mboost::Multinomial
# [14] "Gaussian Process (gaussprLinear)" model fit failed for Fold01: parameter=none Error in if (err < tol) break : missing value where TRUE/FALSE needed

# [15] "Robust Mixture Discriminant Analysis (rmda)" model fit failed for Fold02: K=4, model=VEV Error in colSums(P[cls == c, ]) :
#   'x' must be an array of at least two dimensions
[16] "Penalized Ordinal Regression (ordinalNet)"
[17] "eXtreme Gradient Boosting (xgbDART)"
# [18] "Multivariate Adaptive Regression Spline (earth)"
# [19] "Radial Basis Function Network (rbf)"
# [20] "Quadratic Discriminant Analysis with Stepwise Feature Selection (stepQDA)"
# [21] "Naive Bayes Classifier with Attribute Weighting (awnb)"

