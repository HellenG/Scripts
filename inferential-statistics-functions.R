# A collection of functions for computing some inferential statistics
# Some of these functions are avaliable in base R or contributed packages (and will be noted), but aim for generating them is for learning purposes (understand underlying statistical concept)


# Function for computing raw z-score (values in a sample)
# Assumptions:
   # Independently sampled from a normal distribution
   # Population standard deviation is known
   # Sample size "n" is greater than 30
# If assumptions are not met, use T-scores
# Arguments:
   # x a numeric sample statistic
   # mu population mean
   # sigma population standard deviation
# Base R equivalent: scale() though it is used for vectors while z_score is for individual values

z_raw <- function(x, mu, sigma) {
      (x - mu)/sigma
}

# Example
set.seed(1)
a <- rnorm(31, 50)
z_raw(a[20], mean(a), sd(a)); scale(a, scale = TRUE)[20]

# Function for computing z-score for sampling distributions of a test statistic like sample mean
# Arguments:
   # X a numeric test statistic
   # xbar sample mean
   # sigma population standard deviation 
   # abs logical whether output should be an absolute value, defaults to FALSE

z_sample <- function(X, mu = NULL, pop.sd = NULL, n = NULL) {
   if(length(X) == 0) stop("Empty vector given")
   if(length(X) == 1) {
      if(is.null(mu)) stop("'mu' not given")
      if(is.null(pop.sd)) stop("'pop.sd' is not given")
      if(is.null(n)) stop("'n' is not given")
   } else {
      if(is.null(mu)) mu <-  mean(X)
      if(is.null(pop.sd)) pop.sd <- sd(X)
      if(is.null(n)) n <- length(x) 
   }
   se <- pop.sd/sqrt(n)
   (X - mu)/se
}


# T-test
# Uses: 
   # When "n" < 30
   # When sigma is unknown

t_test <- function(xbar, mu, s, n, abs = FALSE) {
   t <- (xbar - mu)/(s/sqrt(n))
   pt(t, n-1)
}

t_test(19850, 20500, 1084, 14)
a2 <- a[-c(30:31)]
t_test(a2[20], mean(a2), sd(a2), length(a2))
t.test(a2)


# Confidence Interval

ci <- function(x, cl = "95%", stdv = NULL, n = NULL, two.sided = TRUE, lower.tail = TRUE, ...) {
   if(length(x) > 1) {
      if(is.null(stdv)){
         stdv <- sd(x)
      }   
      if(is.null(n)){
         n <- length(x)
      }
   } else {
      if(is.null(stdv)) stop("No 'stdv' specified")
      if(is.null(n)) stop("No 'n' specified")
   } 
   cl <- as.integer(sub("(\\d+)%?", "\\1", cl))/100
   alpha <- 1 - cl
   if (two.sided) {
      cv <- qnorm(1-alpha/2, log.p = FALSE)
   } else {
      cv <- qnorm(1 - alpha, lower.tail = lower.tail, ...)
   }
   se <- stdv/sqrt(n)
   me <- cv * se
   if(length(x) > 1) {
      x <- mean(x)
   }
   c(x - me, x + me)
}

