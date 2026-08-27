#All functions used for SLiM analysis

#Ne for warped sexes
Ne_sex <- function(Sex_ratio){
  Male <- N * Sex_ratio
  Female <- N - Male
  Ne_sexual <- (4*Male*4*Female)/(4*Male + 4*Female)
  return(Ne_sexual)
}

#Ne from Heterozygosity
Ne_H <- function(H){
  return((H/(1-H)) * (1/mew4))
} 

#Expectation of Segregating sites
reciprocal_sum = sum(1/(1:N))
Seg_sites_expec = theta * reciprocal_sum * L