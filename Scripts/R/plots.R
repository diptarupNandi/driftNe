library(ggplot2)
library(tidyr)

setwd("C:/APU/Honors/SLiM_Output/Stand_runs-100k_gens/0.1 k/")


N = 100
mew4 = 4*1.2*10^-8
theta = mew4 * N

#Plotting Expected and Observed Heterozygosity
data <- read.table("log.txt", header = TRUE, sep = "\t")

ggplot(data, aes(cycle,Repro_inds)) + geom_line() + geom_hline(yintercept = mean(data$Repro_inds),color = 'red')

#Plotting Heterozygosity
ggplot(Het_data) + geom_line(aes(x=cycle,y=Values,colour = Heterozygosity)) + geom_hline(yintercept = 4.8e-05) + labs(x = "Generations", y="Heterozygosity") + scale_x_continuous(limits = c(0,N))

#Plotting Expected Heterozygosity
ggplot(data, aes(cycle,Heterozygosity)) + geom_line() + labs(x="Generations",y="Heterozygosity") + geom_hline(linetype = "dashed", yintercept = theta, color = "darkred")

#Plotting Watterson's Theta
#Taking Ne = N for expected theta(?)

ggplot(data, aes(cycle,Watt_theta)) + geom_line() + geom_hline(yintercept = theta,color = "red")+ labs(x = "Generations", y="Theta")

ggplot(data, aes(cycle, Watt_theta)) + geom_line() +  labs(x = "Generations", y="Theta")


#Plotting Heterozygosity calculated Ne
ggplot(data, aes(cycle,Het_Ne)) + geom_line() + labs(x="Generations",y='Ne from Het') + geom_hline(linetype = "dashed", yintercept = 1000, color = "darkred")

#Plotting No. of segregating sites
ggplot(data, aes(cycle, Seg_sites)) + geom_line() + labs(y = "No. of segregating sites", x="Generations")

#Plotting theta_Ne
ggplot(data, aes(cycle,theta_Ne)) + geom_line() + scale_y_continuous(breaks = seq(0,1200,200)) + geom_hline(yintercept = 100,color="darkred",linetype="dashed") + labs(x="Generations",y="Ne from theta")

#Plotting Heterozygosity and Watterson's theta together
Mix_data <- data %>% pivot_longer(cols = c(Expec_Het, Watt_theta),
                                  names_to = 'Measures',
                                  values_to = 'Genetic Diversity' )

ggplot(Mix_data, aes(cycle,`Genetic Diversity`,colour = `Measures`)) + geom_line()+ geom_hline(linetype = "dashed", yintercept = theta, color = "darkred")

