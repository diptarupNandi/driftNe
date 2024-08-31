n = 10000
inds <- paste0("ind", seq(from=1, to=n))

repInd <- vector()
for (i in 1:100){ 
  newGen <- sample(inds, replace = TRUE)
  repInd[i] <- length(unique(newGen))
}
mean(repInd); median(repInd); var(repInd); var(repInd)/mean(repInd)


expect_uniqDraws <- function(n,k = n){
  eUniq <- n*(1 - (1-(1/n))^k)
return(eUniq)
}

nInds <- seq(from =2, by= 4, to = 1000)
meanUniqInds <- expect_uniqDraws(nInds)
plot(x = nInds, y = meanUniqInds)
