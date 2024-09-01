library(ggplot2)
library(tidyr)
library(dplyr)

setwd("C:/APU/Honors/SLiM_Output/Burn_Ins/0.1K/")

#Measuring variance: Rolling variance
data <- read.table("log.txt", sep = "\t", header = TRUE)


Het_roll_var <- c()
#No. of data points = 10k/10 = 1k. So, calc var every 10 gens
for (i in seq(11,length(data$Heterozygosity),by = 10)){
  Het_roll_var <- append(Het_roll_var, (var(c(data$Heterozygosity[i-10:i]))) )
}
plot(Het_roll_var)

Theta_roll_var <- c()
for (i in seq(11,length(data$Watt_theta),by = 10)){
  Theta_roll_var <- append(Theta_roll_var, (var(c(data$Watt_theta[i-10:i]))) )
}
plot(Theta_roll_var)

Seg_roll_var <- c()
for (i in seq(11,length(data$Seg_sites),by = 10)){
  Seg_roll_var <- append(Seg_roll_var, (var(c(data$Watt_theta[i-10:i]))) )
}
plot(Seg_roll_var)




