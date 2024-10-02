#Estimating Ne 
library(ggplot2)
library(tidyr)
setwd("C:/APU/Honors/Data/Sexual_pops/10k/")

N = 10000
mew4 = 4*1.2*10^-8
theta = mew4 * N
L = 10^6
Sex_ratio = 0.9

x = 'Generations'
data <- read.table("testLog0_9M.txt", header = TRUE, sep = "\t")

reciprocal_sum = sum(1/(1:N))
Seg_sites_expec = theta * reciprocal_sum * L

#========================================================================
#Plotting Ne
data$het_Ne <- Ne_H(data$Heterozygosity)
data$theta_Ne <- data$Watt_theta/mew4 

Ne_data <- data %>% pivot_longer(cols = c(het_Ne,theta_Ne,ReproInds),                              names_to = 'Ne', values_to = 'Values')

line.data <- data.frame(x=data$cycle,y=rep(Ne_sex(0.9),length(data$cycle)))

ggplot(Ne_data, aes(cycle,Values, colour = Ne)) + geom_line() + geom_line(aes(x,y,color="Expected Ne"), data = line.data, linetype = "dashed") + labs(x="Generations", y="Ne") + ggtitle("Effective Population size") + geom_hline(yintercept = N, linetype = "dotted") + scale_color_manual(values = c('red4','green','navy','purple3'))
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
