# Instruction pour que tout le monde ait le m^eme al´ea de d´epart
RNGkind(sample.kind = "Rounding")
prob = c(1/4, 1/5, 1/6, 1/6, 1/10, 7/60)
P = cumsum(prob)
x = (1:length(P))[runif(1)<P][1]

N = 1000 ; x=rep(NA,N)
for(i in 1:N) x[i] = (1:length(P))[runif(1)<P][1]
  barplot(table(x)/N, col="cyan", cex = 2, cex.axis = 1.5, cex.lab=1.5,
          xlab="Valeurs de X", ylab = "Effectifs")
data <- cbind(table(x)/1000,prob)
data <- t(data)

rownames(data) <- c("Empirique","Théorique")

barplot(data, beside = TRUE, col=c("blue", "red"),
        legend.text = rownames((data)), args.legend = list(title="Comparaison des probabilités"))
print(dim(data))
View(data)

print(table(x))
Q <- function(occur, prob)
{
  return (sum(((occur - prob)^2)/prob))
}

z = Q(table(x), N*prob)

alpha = 0.05
ddl = 5
if (z <= qchisq(1 - alpha, ddl)){
  print("H0 n'est pas rejetté")
} else
{
  print("H0 est rejetté")
}
