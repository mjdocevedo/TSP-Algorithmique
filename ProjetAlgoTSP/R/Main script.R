##### Création d'une matrice aléatoire uniforme de taille N
N = 15
G = matrix(runif(N*N,min=10,max=50),nrow=N,ncol=N)
G = G %*% t(G) # rendre G symétrique.
diag(G) = 0 # annuler la diagonale de G.


##### Implementation du Naif sur R directemment sur script Tsp_naif

##### Implementation du Branch and Bound sur Rcpp
#tic()
#BranchBound(G)
#toc()

##### Implementation du Branch and Bound sur Rcpp
#tic()
#BranchBound(G)
#toc()

##### Implementation du Branch and Bound sur R directemment sur script Tsp_bb

##### Implementation de l'algorithme genetique
library(GA) #on utilise la librarie de l'algorithme genetique

#Fonction pour calculer la taille du tour
tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[,2:1]
  sum(distMatrix[route])
}

#Fitness à maximiser
tspFitness <- function(tour, ...) 1/tourLength(tour, ...)

#On applique l'algorithme genetique à notre matrice G crée au début du script
GA <- ga(type = "permutation", fitness = tspFitness, distMatrix = G,
         min = 1, max = N, popSize = 50, maxiter = 5000,
         run = 500, pmutation = 0.2)
summary(GA)
summary(GA)$solution

######################################################################################################

##### Compexité en temps des differentes méthodes
library(reshape2)
library(ggplot2)
n <- c(5,10,15,20,25,30,35)
naif <- c(factorial(n))
hk <- c((n*2)*(2**n))
bb <- c((n*3)*(log(n)**2))
gen <- c(log(n))
x <- log(n)
sample_data <- data.frame(x, y)

dat <- data.frame(algoNaif = log(naif), algoHK = log(hk), algoBB = log(bb), algoGen = log(gen),villes = log(n))
dat2 <- melt(data = dat, id = "villes")
ggplot(data= dat2, aes(x = villes, y = value, col = variable)) +
  geom_line() +
  xlab("log(n=villes)") + ylab("log(temps)") +
  ggtitle("Comparaison entre algorithmes")+
  theme_bw()
