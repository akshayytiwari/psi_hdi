# Function to get Wilson score interval

fn_wilson <- function(prop, sample) {
  a <- prop + ((1.96^2) / (2 * sample))
  b <- 1.96 * sqrt(((prop * (1 - prop)) + (1.96^2 / (4 * sample))) / sample)
  c <- 1 + (1.96^2 / sample)
  
  C_l <- (a - b)/c
  C_u <- (a + b)/c
  
  return(c(C_l, C_u))
}
