library(dplyr)
library(ggplot2)
library(tidyr)
setwd("C:/APU/Honors/Data/Sexual_pops/0.1k/")

#============
N = 100
mew4 = 4*1.2*10^-8
theta = mew4 * N
L = 10^6
Sex_ratio = 0.9

Equal <- read.table("Equal_Log2.txt", header = TRUE, sep="\t")
r6 <- read.table("testLog0_6M.txt", header = TRUE, sep = "\t")
r7 <- read.table("testLog0_7M.txt", header = TRUE, sep = "\t")
r8 <- read.table("testLog0_8M.txt", header = TRUE, sep = "\t")

#Code for generating different Ne values for all the data
All_data <- list("Equal" = Equal,"r6" = r6, "r7" = r7, "r8" = r8)

all_data_n <- list()
for(i in c(1:length(All_data))){
  
  # All_data[[i]]$Ne_het = Ne_H(All_data[[i]]$Heterozygosity)
  df_n <- All_data[[i]] %>% mutate(Ne_het = Ne_H(Heterozygosity), Ne_theta = Watt_theta/mew4)
  all_data_n[[i]] <- df_n
}
names(all_data_n) <- names(All_data)
list2env(all_data_n, envir = .GlobalEnv)
rm(All_data,all_data_n)

#=========
#Combined Heterozygosity
Total_het <- data.frame("Generations" = Equal$cycle,
                        "p5" = Equal$Heterozygosity,
                        "p6" = r6$Heterozygosity,
                        "p7" = r7$Heterozygosity,
                        "p8" = r8$Heterozygosity)
Total_het <- Total_het %>% pivot_longer(cols = c(p5,p6,p7,p8),
                                        names_to = 'Ratio', values_to = 'Heterozygosity')
ggplot(Total_het, aes(Generations,Heterozygosity,colour = Ratio)) + geom_line() + ggtitle("N = 100")

#=========
#Combined Watterson's Theta
Total_het <- data.frame("Generations" = Equal$cycle,
                        "p5" = Equal$Watt_theta,
                        "p6" = r6$Watt_theta,
                        "p7" = r7$Watt_theta,
                        "p8" = r8$Watt_theta)
Total_het <- Total_het %>% pivot_longer(cols = c(p5,p6,p7,p8),
                                        names_to = 'Ratio', values_to = "Watt_theta")
ggplot(Total_het, aes(Generations,Watt_theta,colour = Ratio)) + geom_line() + ggtitle("N = 100") + labs(y = "Watterson's Theta")

#=========
#Combined Seg_sites
Total_het <- data.frame("Generations" = Equal$cycle,
                        "p5" = Equal$Seg_sites,
                        "p6" = r6$Seg_sites,
                        "p7" = r7$Seg_sites,
                        "p8" = r8$Seg_sites)
Total_het <- Total_het %>% pivot_longer(cols = c(p5,p6,p7,p8),
                                        names_to = 'Ratio', values_to = "Seg_sites")
ggplot(Total_het, aes(Generations,Seg_sites,colour = Ratio)) + geom_line() + ggtitle("N = 100") + labs(y = "Segregating sites")
#Plotting Ne_het
Total_Ne_het <- data.frame("Generations" = Equal$cycle,
                           "p5" = Equal$Ne_het,
                           "p6" = r6$Ne_het,
                           "p7" = r7$Ne_het,
                           "p8" = r8$Ne_het)
Total_Ne_het <- Total_Ne_het %>% pivot_longer(cols = c(p5,p6,p7,p8),
                                              names_to = 'Ratio', values_to = 'Ne_het')
ggplot(Total_Ne_het, aes(Generations,Ne_het,colour = Ratio)) + geom_line() + ggtitle("N = 100, Ne from Heterozygosity") + labs(y="Ne") + scale_colour_manual(values = wes_palette("Cavalcanti1")) + theme_bw()

#=========
#Plotting Ne_theta
Total_Ne_theta <- data.frame("Generations" = Equal$cycle,
                             "p5" = Equal$Ne_theta,
                             "p6" = r6$Ne_theta,
                             "p7" = r7$Ne_theta,
                             "p8" = r8$Ne_theta)
Total_Ne_theta <- Total_Ne_theta %>% pivot_longer(cols = c(p5,p6,p7,p8),
                                                  names_to = 'Ratio', values_to = 'Ne_theta')
ggplot(Total_Ne_theta, aes(Generations,Ne_theta,colour = Ratio)) + geom_line() + ggtitle("N = 100, Ne from Watterson's Theta") + labs(y="Ne") + scale_colour_manual(values = wes_palette("Cavalcanti1")) + theme_bw()