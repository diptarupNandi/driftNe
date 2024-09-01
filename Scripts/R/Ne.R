#Estimating Ne 

library(ggplot2)
library(tidyr)

setwd("C:/APU/Honors/SLiM_Output/Sexual_pops/1k/")

N = 1000
mew4 = 4*1.2*10^-8
theta = mew4 * N
L = 10^6

x = 'Generations'
data <- read.table("log2.txt", header = TRUE, sep = "\t")

Ne_H <- function(H){
  return((H/(1-H)) * (1/mew4))
} 

reciprocal_sum = sum(1/(1:N))
Seg_sites_expec = theta * reciprocal_sum * L

#========================================================================
#Plotting Ne
data$het_Ne <- Ne_H(data$Heterozygosity)
data$theta_Ne <- data$Watt_theta/mew4 

Ne_data <- data %>% pivot_longer(cols = c(het_Ne,theta_Ne,ReproInds),                              names_to = 'Ne', values_to = 'Values')
ggplot(Ne_data, aes(cycle,Values, colour = Ne)) + geom_line() + geom_hline(yintercept = N, linetype = 'dashed') + labs(x="Generations", y="Ne") + ggtitle("Effective Population size")

#========================================================================
#Plotting Heterozygosity

# Define lines
mean_val <- mean(data$Heterozygosity)
Expec <- theta

# Create the plot
ggplot(data, aes(x = cycle, y = Heterozygosity)) +
  geom_line(color = "black") +  # Change the main line color here
  geom_hline(yintercept = mean_val, linetype = 'dashed', colour = 'firebrick') +
  geom_hline(yintercept = Expec, linetype = 'twodash', colour = 'steelblue4') +
  geom_hline(aes(yintercept = mean_val, color = 'Mean'), linetype = 'dashed', show.legend = TRUE) +
  geom_hline(aes(yintercept = Expec, color = 'Expected'), linetype = 'twodash', show.legend = TRUE) +
  scale_color_manual(name = 'Legend', breaks = c('Mean', 'Expected'), values = c('Mean' = 'firebrick', 'Expected' = 'steelblue4')) +
  guides(color = guide_legend(override.aes = list(linetype = c('dashed', 'twodash')))) + labs(x=x)

#========================================================================
#Plotting Watterson's Theta

# Define lines
mean_val <- mean(data$Watt_theta)
Expec <- theta

# Create the plot
ggplot(data, aes(x = cycle, y = Watt_theta)) +
  geom_line(color = "black") +  # Change the main line color here
  geom_hline(yintercept = mean_val, linetype = 'dashed', colour = 'firebrick') +
  geom_hline(yintercept = Expec, linetype = 'twodash', colour = 'steelblue4') +
  geom_hline(aes(yintercept = mean_val, color = 'Mean'), linetype = 'dashed', show.legend = TRUE) +
  geom_hline(aes(yintercept = Expec, color = 'Expected'), linetype = 'twodash', show.legend = TRUE) +
  scale_color_manual(name = 'Legend', breaks = c('Mean', 'Expected'), values = c('Mean' = 'firebrick', 'Expected' = 'steelblue4')) +
  guides(color = guide_legend(override.aes = list(linetype = c('dashed', 'twodash')))) + labs(x=x,y="Watterson's Theta")

#========================================================================
#Plotting No. of segregating sites
# Define lines
mean_val <- mean(data$Seg_sites)
Expec <- Seg_sites_expec

# Create the plot
ggplot(data, aes(x = cycle, y = Seg_sites)) +
  geom_line(color = "black") +  # Change the main line color here
  geom_hline(yintercept = mean_val, linetype = 'dashed', colour = 'firebrick') +
  geom_hline(yintercept = Expec, linetype = 'twodash', colour = 'steelblue4') +
  geom_hline(aes(yintercept = mean_val, color = 'Mean'), linetype = 'dashed', show.legend = TRUE) +
  geom_hline(aes(yintercept = Expec, color = 'Expected'), linetype = 'twodash', show.legend = TRUE) +
  scale_color_manual(name = 'Legend', breaks = c('Mean', 'Expected'), values = c('Mean' = 'firebrick', 'Expected' = 'steelblue4')) +
  guides(color = guide_legend(override.aes = list(linetype = c('dashed', 'twodash')))) + labs(x=x,y="No. of Segregating sites")
