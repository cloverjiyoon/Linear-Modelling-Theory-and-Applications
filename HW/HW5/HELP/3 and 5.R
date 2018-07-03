library(DAAG)
data(frogs)
help(frogs)

frogs.glm <- glm(formula = pres.abs ~ log(distance) + log(NoOfPools) + meanmin,
                 family = binomial, data = frogs)
summary(frogs.glm)



X = model.matrix(frogs.glm)
W = diag(frogs.glm$fitted.values*(1 - frogs.glm$fitted.values))
solve(t(X) %*% W %*% X)


mean(frogs$pres.abs)

x1 = 265
x2 = 26 
x3 = 3.5
rrg = c(1, log(x1), log(x2), x3)
eta = sum(rrg*frogs.glm$coefficients)
prr = exp(eta)/(1 + exp(eta))
prr


data(spam7)
s = 0.001
M1 <- glm(yesno~ log(crl.tot) + log(dollar+s) + log(bang+s)
          +log(money+s) + log(n000+s) + log(make+s),
          family=binomial, data=spam7)
summary(M1)


x1 = 157
x2 = 0.868
x3 = 2.894
x4 = 0
x5 = 0
x6 = 0
rrg = c(1, log(x1), log(x2+s), log(x3+s), log(x4+s), log(x5+s), log(x6+s))
eta = sum(rrg*M1$coefficients)
prr = exp(eta)/(1 + exp(eta))
prr

M2 = glm(yesno ~ crl.tot + dollar + bang + money + n000 + make,
         family=binomial, data=spam7)
summary(M2)
