#Estimating Ne 

library(ggplot2)
library(tidyr)

setwd("C:/APU/Honors/Data/Sexual_pops/1k/")

N = 10000
mew4 = 4*1.2*10^-8
theta = mew4 * N
L = 10^6
Sex_ratio = 0.9

x = 'Generations'
data <- read.table("testLog0_9M.txt", header = TRUE, sep = "\t")

Ne_sex <- function(Sex_ratio){
  Male <- N * Sex_ratio
  Female <- N - Male
  Ne_sexual <- (4*Male*4*Female)/(4*Male + 4*Female)
  return(Ne_sexual)
}
#========================================================================
#Plotting Ne
data$het_Ne <- Ne_H(data$Heterozygosity)
data$theta_Ne <- data$Watt_theta/mew4 

Ne_data <- data %>% pivot_longer(cols = c(het_Ne,theta_Ne,ReproInds),                              names_to = 'Ne', values_to = 'Values')

line.data <- data.frame(x=data$cycle,y=rep(Ne_sex(0.9),length(data$cycle)))

ggplot(Ne_data, aes(cycle,Values, colour = Ne)) + geom_line() + geom_line(aes(x,y,color="Expected Ne"), data = line.data, linetype = "dashed") + labs(x="Generations", y="Ne") + ggtitle("Effective Population size") + geom_hline(yintercept = N, linetype = "dotted")

#===============
#Plotting He
ggplot(data, aes(cycle,Heterozygosity)) + geom_line() + scale_y_continuous(labels = scales::scientific) + labs(x=x)

#Plotting Watterson's Theta
ggplot(data, aes(cycle,Watt_theta)) + geom_line() + scale_y_continuous(labels = scales::scientific) + labs(x=x,y="Watterson's Theta")

#Plotting Seg sites
ggplot(data, aes(cycle,Seg_sites)) + geom_line() + labs(x=x,y="No. of Segregating sites")
