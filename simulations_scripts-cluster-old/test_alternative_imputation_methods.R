library(missMDA)
## Not run:
data(orange)
## First the number of components has to be chosen
## (for the imputation step)
## nb <- estim_ncpPCA(orange,ncp.max=5) ## Time consuming, nb = 2
## Imputation
res.comp <- imputePCA(orange,ncp=2)
completeOrange <- res.comp$completeObs
## End(Not run)

library(randomForestSRC)
data(pbc, package = "randomForestSRC")

pbc1.d <- impute(data = pbc)
View(pbc1.d)
