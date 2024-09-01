library(ggplot2)
library(tidyr)
library(dplyr)
library(roll)
library(zoo)

setwd("C:/APU/Honors/SLiM_Output/Sexual_pops/10k")
data <- read.table("log2.txt", sep = "\t", header = TRUE)

Het_roll_var <- c()
#No. of data points = 10k/10 = 1k. So, calc var every 10 gens
for (i in seq(1000,length(data$Heterozygosity),by = 10)){
  
  Het_roll_var <- append(Het_roll_var, (var(c(data$Watt_theta[i-1000:i]))) )
}

gens <- 10*seq(1000,length(data$Heterozygosity),by=10)
df <- data.frame("Rolling Variance" = Het_roll_var,
                 "Generations" = gens)

ggplot(df, aes(Generations, Rolling.Variance)) + geom_point() + theme_bw() + ggtitle("Watterson's Theta")
#plot(gens,Het_roll_var,pch=20,
 #    xlab="Generations",
  #   ylab="Rolling variance",
   #  main = "Heterozygosity",
    # cex.axis=0.8)
head(Het_roll_var)
#Other non-working ways to plot rolling variance
#==================
#Plotting rolling variance
#Window size
win = 10
#Shift size
shift = 10
x=1
var_data <- data[x:(x+win),]
var_data <- subset(var_data, select = -c(cycle, ReproInds))
out_data <- data.frame( Gens = c(),
                        Het_var = c(),
                        Theta_var = c(),
                        Seg_var = c())
iter_max = (nrow(data) - win)/shift

for(i in c(1:iter_max)){
  temp_data <- data.frame(Gens = c(win+x),
                          Het_var = c(var(var_data$Heterozygosity)),
                          Theta_var = c(var(var_data$Watt_theta)),
                          Seg_var = c(var(var_data$Seg_sites)))
  out_data <- rbind(out_data,temp_data)
  x = shift*i
  var_data <- data[x:(x+shift),]
  var_data <- subset(var_data, select = -c(cycle, ReproInds))
}

print(dim(out_data))
head(out_data)

ggplot(out_data, aes(Gens, Het_var)) + geom_line()
#====================
# Using inbuilt rolling variance function
het_var <- roll_var(data$Heterozygosity, width = 1000)
plot(het_var)

#=================
# Load the necessary package
library(zoo)

# Example data
x <- data$Heterozygosity

# Define the window size
n <- 1000

# Define the step size (90% overlap means a 10% shift, so the step size is 1 for a window size of 3)
step_size <- ceiling(n * 0.1)

# Initialize an empty vector to store the rolling variances
rolling_variance <- c()

# Calculate the rolling variance with 90% overlap
for (i in seq(1, length(x) - n + 1, by = step_size)) {
  window <- x[i:(i + n - 1)]
  rolling_variance <- c(rolling_variance, var(window))
}

# Display the result
plot(rolling_variance)


zoo_data <- read.zoo(data)
zoo_var <- rollapply(zoo_data, width = 1000, var, by=10 )
plot(zoo_var$Heterozygosity)
