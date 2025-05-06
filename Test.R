N <- 1000
z_iter <- c()
lmao <- c()

for (epoch in 1:400) 
{
  set.seed(epoch)  # Ensure reproducibility per epoch
  prob <- rep(1/6, 6)
  
  # Simulate N dice rolls
  x <- sample(1:6, N, replace = TRUE)
  
  # Reshape counts into 3x2 contingency table
  count <- matrix(tabulate(x, nbins = 6), nrow = 3, ncol = 2)
  
  # Custom Q function for chi-squared statistic
  Q <- function(occur) {
    sum_val <- 0
    count_row <- rowSums(occur)
    count_col <- colSums(occur)
    total <- sum(occur)
    
    for (i in 1:nrow(occur)) {
      for (j in 1:ncol(occur)) {
        E <- (count_row[i] * count_col[j]) / total
        sum_val <- sum_val + ((occur[i, j] - E)^2) / E
      }
    }
    return(sum_val)
  }
  
  z <- Q(count)
  ddl <- (nrow(count) - 1) * (ncol(count) - 1)  # Correct degrees of freedom
  alpha <- 0.05
  
  z_iter <- c(z_iter, z)
  lmao <- c(lmao, z <= qchisq(1 - alpha, ddl))
}

hist(z_iter, freq = FALSE, main = "Histogram of Chi-squared Statistics")
curve(dchisq(x, df = 2), add = TRUE, col = "red")  # Overlay theoretical density

barplot(table(lmao)/400, names.arg = c("Reject H0", "Fail to Reject H0"),
        ylab = "Proportion", main = "Test Outcomes")

