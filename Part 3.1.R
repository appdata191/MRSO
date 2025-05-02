# Instruction pour que tout le monde ait le m^eme al´ea de d´epart\
H0_list <- c()
N_list <- c(50, 100, 200, 500, 1000)
for (N in N_list){
  z_iter = c()
  lmao = c()
  for (epoch in 1:400)
  {
    RNGkind(sample.kind = "Rounding")
    prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
    P = cumsum(prob)
    x = (1:length(P))[runif(1)<P][1]
    
    x=rep(NA,N)
    for(i in 1:N) x[i] = (1:length(P))[runif(1)<P][1]
    data <- cbind(table(x)/N,prob)
    data <- t(data)
    
    rownames(data) <- c("Empirique","Théorique")
    
    
    count = c(sum(x==1), sum(x==2), sum(x==3), sum(x==4), sum(x==5), sum(x==6))
    
    dim(count) <- c(3,2)
    
    test = chisq.test(count, p =prob)
    
    Q <- function(occur)
    {
      sum = 0
      count_row = rowSums(occur)
      count_col = colSums(occur)
      for (i in 1:nrow(occur))
      {
        for (j in 1:ncol(occur))
        {
          E = (count_row[i] * count_col[j])/N
          sum = sum + ((occur[i,j] - E)^2)/E
        }
      }
      return (sum)
    }
    
    z = Q(count)
    
    
    ddl = 2
    alpha = 0.05
    
    
    z_iter = c(z_iter, z)
    lmao = c(lmao, z <= qchisq(1 - alpha, ddl))
  }
  

  H0_list <- cbind(H0_list,table(lmao)[1]/400)
  print(H0_list)
  print(N)
}


print(H0_list)


