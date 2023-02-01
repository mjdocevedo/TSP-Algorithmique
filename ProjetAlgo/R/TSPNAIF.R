############################### Algorithme Brute Force ###################
villes <- function(n){
  max_x <- 20
  max_y <- 20
  set.seed(9999779)
  villes <- data.frame(id = 1:n, x = runif(n, max = max_x), ## coordonnées maximales est 20, mais on peut avoir plus que 20 villes
                       y = runif(n, max = max_y))
  return(villes)
}

#villes(29)


library(ggplot2)
plot_villes<-function(n){ggplot(villes(n), aes(x, y)) + geom_point()}

## Fonction ’distance’ : calcule la matrice (symétrique) des distances entre les n villes :
library(dplyr)
distance <- function(n) {
  return(as.matrix(dist(select(villes(n), x, y), diag = TRUE, upper = TRUE)))
}
distance(9)
seq_along(5)
optimize<-function(cost_list,path_list){
  a=as.numeric(min( unlist(cost_list))) ## a est le minmum cost
  order=match(a,cost_list)  ### on cherche quel trajet correspond au minimum cost
  path=path_list[order] ### le trajet qui a le meilleur cout
  print(path)
  return(c(a,path)) ### we return a list, the  first indicate the minimum cost and the second term indicates lequel trajet est le mieux, son ordre
}

optimize(c(6,8,9), c(5,8))

#### vapply calcul la somme des elements dans une liste
dist_fun <- function(i, j){
  vapply(seq_along(i), function(k) distance(9)[i[k], j[k]], numeric(1))
}
## numeric(1) :type of data we expect to have as output
dist_fun(2,3)

#### on somme la distance du trajet total
cout_total<-function(p,dist_fun){
  c<-dist_fun(1,p[1])
  i=1
  l=list(dist_fun(1,p[1]))
  while (i<=length(p))
  {
    a=p[i]
    b=p[i+1]
    l<-c(l,list(dist_fun(a,b)))
    i<-i+1
  }
  l<-c(l,list(dist_fun(p[length(p)],1)))
  l=unlist(l[!is.na(l)])
  print(l)
  return(sum(l))
}
p=c(2,4,5)
cout_total(p,7)
seq_along(7)

cout_total(c(2,4,5,3), 6)

### NAIF ###
TSP_Naif_R<- function(N){
  dist_fun <- function(i, j) {vapply(seq_along(i), function(k) distance(N)
                                     [i[k], j[k]], numeric(1L))}
  perm_list=permutations(n = N-1,r=N-1, v = 2:N)
  cost_list=list()
  path_list=list()
  for (i in seq(1,dim(perm_list)[1])) {
    #progress(i)
    p=perm_list[i,]
    cost_list=c(cost_list,cout_total(p,dist_fun))
    path_list=c(path_list, list(c(1,p,1)))
  }
  res=optimize(cost_list,path_list)
  print(c('Le cout minimal est:',res[1]))
  print(c('Le chemin le plus court est :',res[2]))
  return(res)
}
