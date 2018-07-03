library(faraway)
data(coagulation)
mod <- lm(coag ~ diet - 1, data=coagulation)

n <- dim(coagulation)[1]
m <- length(levels(coagulation$diet))
# manual construction; can ignore

# diet <- coagulation$diet
# X <- numeric()
# for (j in 1:(m)) {
#   X <- cbind(X, as.numeric(diet == levels(diet)[j]))
# }
# y <- coagulation$coag
# ybar <- mean(y)
# betahat <- solve(t(X) %*% X, t(X) %*% y)
# yhat <- X %*% betahat
# resid <- y - yhat

y <- coagulation$coag
ybar <- mean(y)
yhat <- fitted(mod)
resid <- resid(mod)

RSS <- sum(resid^2)
RSE <- sqrt(RSS / (n - m))
RSE

# !!! formulas for RegSS and TSS change
# see excellent post here:
# https://stats.stackexchange.com/a/26205

RegSS <- sum((yhat)^2) # !
TSS <- sum(y^2) # !
R2 <- RegSS / TSS
R2

# F-statistic tests for all four group means being zero

Fstat <- (RegSS / m) / (RSS / (n - m))
Fstat
Fstat <- ((TSS - RSS) / m) / (RSS / (n - m))
Fstat
Fstat <- R2 / (1 - R2) * (n-m) / m
Fstat


summary(mod)

