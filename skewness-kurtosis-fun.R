# Functions to compute 3rd and 4th statistical moments (Skewness and Kurtosis)

# Third statistical moment 
m3 <- function(x) {
   n <- length(x)
   dif <- (x - mean(x))^3
   sum(dif)/n
}

# To compute standardized thrid statistical moment
m3_std <- function(x) {
   s <- sd(x)
   m3(x)/(s^3)
}

# Skewness interpreter

skewness_interpreter <- function(x) {
   if(x == 0) {
      return("symmetric (not skewed)")
   } else if (x > -0.5 & x < 0.5) {
      return("approximately symmetric")
   } else if (x <= -0.5 & x >= -1) {
      return("moderately (negatively) skewed")
   } else if (x >= 0.5 & x <= 1) {
      return("moderately (positively) skewed")
   } else if (x < -1 | x > 1) {
      if (x < -1) {
         return("highly negatively skewed")
      } else {
         return("highly positively skewed")
      }
   } else {
      return("Can't interpret that, I need one numerical value.")
   }
}

# Fourth moment (Kurtosis)
m4 <- function(x) {
   n <- length(x)
   m <- mean(x)
   sum((x - m)^4)/n
}

# Standardized kurtosis
m4_std <- function(x) {
   s <- sd(x)
   m4(x)/s^4
}

# Excess Kurtosis

excess_kurt <- function(x) {
   m4_std(x) - 3
}

# Excess interpreter

excess_interpreter <- function(x) {
   if(x == 0) {
      return("mesokurtic")
   } else if (x > -0.5 & x < 0.5) {
      return("approximately mesokurtic")
   } else if (x <= -0.5 & x > -1) {
      return("moderately platykurtic")
   } else if (x >= 0.5 & x < 1) {
      return("moderately leptokurtic")
   } else if (x <= -1) {
      return("platykurtic")
   } else {
      return("leptokurtic")
   }
}
