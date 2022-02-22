set.seed(1010)
n = 1000
p = 100
nzc = trunc(p/10)
x = matrix(rnorm(n * p), n, p)
ly = rbinom(n = length(px), prob = px, size = 1)
fit = glmnet(x, ly, family = "binomial")
plot(fit, xvar = "lambda", label = TRUE)
cv.lasso = cv.glmnet(x, ly, family = "binomial")
plot(cv.lasso)
cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
# df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)
coeficients_best_lambda = as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min))
important_variables = a[a != 0,]
