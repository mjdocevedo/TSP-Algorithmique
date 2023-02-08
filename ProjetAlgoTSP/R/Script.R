##### Création d'une matrice aléatoire uniforme de taille N
N = 15
G = matrix(runif(N*N,min=10,max=50),nrow=N,ncol=N)
G = G %*% t(G) # rendre G symétrique.
diag(G) = 0 # annuler la diagonale de G.

##### Implementation du Branch and Bound sur Rcpp
#tic()
#BranchBound(G)
#toc()

## Compexité en temps des differentes méthodes
library(reshape2)
library(ggplot2)
n <- c(5,10,15,20,25,30,35)
naif <- c(factorial(n))
hk <- c((n*2)*(2**n))
bb <- c((n*3)*(log(n)**2))
x <- log(n)
sample_data <- data.frame(x, y)

dat <- data.frame(algoNaif = log(naif), algoHK = log(hk), algoBB = log(bb), villes = log(n))
dat2 <- melt(data = dat, id = "villes")
ggplot(data= dat2, aes(x = villes, y = value, col = variable)) +
  geom_line() +
  xlab("log(n=villes)") + ylab("log(temps)") + # Set axis labels
  ggtitle("Comparaison entre algorithmes")+
  theme_bw()
