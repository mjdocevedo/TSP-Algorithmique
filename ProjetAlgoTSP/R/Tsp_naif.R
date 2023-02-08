#' @name compute_distance
#' @title calcul de distance selon un ordre de villes en R.
#' @param G : la matrice des distances.
#' @param cities : vecteur des villes à visiter.
#' @param start_city : la ville de début du voyage.
#' @usage compute_distance(G,cities,start_city)
#' @return La distance totale parcourue en commençant par 'start_city' et visitant 'cities' et terminant en 'start_city'.
#' @examples
#' G = matrix(runif(4*4,min=10,max=50),nrow=4,ncol=4)
#' G = G %*% t(G) # rendre G symétrique.
#' diag(G) = 0 # annuler la diagonale de G.
#' ## la symétrie de G n'est pas nécessaire.
#' start_city = 1 # la ville de Départ.
#' cities = c(2,3,4)  # un ordre de ville à visiter.
#' distance = compute_distance(G,cities,start_city) # calcul de distance du tour.
compute_distance <- function(G,cities,start_city){
  n = length(cities)
  distance = G[start_city,cities[1]]
  for (i in 1:(n-1)){
    distance = distance + G[cities[i],cities[i+1]]
  }
  return(distance + G[cities[n],start_city])
}

#' @name  naive_method
#' @title Test de toutes les permutations possibles en R.
#' @param G : la matrice des distances.
#' @param cities : vecteur des villes à visiter.
#' @param start_city : la ville de début du voyage.
#' @usage naive_method(G,cities,start_city)
#' @return La séquence des villes qui donnent la moindre en distance.
#' @examples
#' G = matrix(runif(4*4 , min = 10, max = 50),nrow=4)
#' G = G %*% t(G) # rendre G symétrique (n'est pas nécessaire.).
#' diag(G) = 0 # annuler la diagonale de G.
#' start_city = 1 # la ville de Départ.
#' cities = c(2,3,4)  # un ordre de ville à visiter.
#' results = naive_method(G,cities,start_city)
#' results['path_opt'] # l'ordre des villes optimales.
#' results['dist_opt'] # la distance obtenue avec l'ordre des villes optimales.
naive_method <- function(G,cities,start_city){
  if (start_city %in% cities){
    stop(showNonASCII(paste("'start_city' :",start_city," ne doit pas \u00EAtre dans 'cities' :",toString(cities))))
  }
  if ( (!is.matrix(G)) || dim(G)[1] != dim(G)[2]){
    stop(showNonASCII(paste("'G' :",toString(G)," doit \u00EAtre une matrice carr\u00E9e.")))
  }
  n = length(cities)
  c = rep(1,n)
  i = 1
  dist_optimal = compute_distance(G,cities,start_city)
  path_optimal = cities
  while (i < n+1){
    if (c[i] < i){
      if (i %% 2 == 1 ){
        tmp = cities[1]
        cities[1] = cities[i]
        cities[i] = tmp
      }
      else{
        tmp = cities[c[i]]
        cities[c[i]] = cities[i]
        cities[i] = tmp
      }
      # On compare la nouvelle permutation avec la précédente.
      if ( compute_distance(G,cities,start_city) < dist_optimal){
        path_optimal = cities
        dist_optimal = compute_distance(G,path_optimal,start_city)
      }
      c[i] = c[i] + 1
      i = 1
    }
    else {
      c[i] = 1
      i = i+1
    }
  }
  return(list(path_optimal=path_optimal,dist_optimal=dist_optimal))
}

########################### ESSAYS #############################

maxsize = Inf
N = 15
G = matrix(runif(N*N,min=10,max=50),nrow=N,ncol=N)
G = G %*% t(G) # rendre G symétrique.
diag(G) = 0 # annuler la diagonale de G.

tic()
naive_method(G, c(1:N),1)
toc()
