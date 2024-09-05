# Simulating 'random' change in allele frequencies over N generations
library(ggplot2)
drift = function(N){
  count <- N
  blah <- c(count)
  generations <- 1
  while(count < 2*N && count > 0){
    count <- count + rbinom(1,2*N,(blah[generations]/2*N))
    generations <- generations + 1
    blah[generations] <- count
  }
  data <- data.frame(freq = blah,
            gens = c(1:generations))
  ggplot(data, aes(gens,freq))+ geom_line()
}

drift(100)

#Simulating with rbinom
drift1 = function(N){
  count <- 0
  blah <- c(N)
  gens <- 1
  while(count > 0 && count < 2*N){
    count <- rbinom(1,2*N,(blah[gens]/2*N))
    gens <- gens + 1
    print(count)
  }
}

i <- 1
while(i<30 && i != 15){
  print(i)
  i <- i+rbinom(1,10,0.5)
}