library(ggplot2)
first_head = function(x){
  tosses <- c()
  for (i in 1:x){
    tails <- TRUE
    flips <- 0
    while(tails==TRUE){
      flips <- flips + 1
      toss <- sample(c(0,1), size = 1)
      if (toss ==0){
        tails <- FALSE
      } 
    }
    tosses[i] <- flips
  }
  return(tosses)
}

blah <- first_head(10000)
ggplot(mapping = aes(blah)) + geom_histogram(color = "black", fill = "steelblue", binwidth = 1) + theme_minimal() + scale_x_continuous(breaks = seq(1,15,1)) + labs(title="Histogram of First Heads in a Coin toss simulation", x="First Head", y= "Frequency")
