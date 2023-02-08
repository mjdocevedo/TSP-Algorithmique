#' @name  .get_subsets
#' @title (Fonction cachée) Génère toutes les sous partitions de taille p d'un groupe de taille n.
#' @param set : le groupe initial.
#' @param p : la taille des partitions qu'on veut extraire de 'set'.
#' @return L'ensemble des partitions de taille p du groupe 'set'.
.get_subsets <- function(set,p){
  subsets = list()
  for (i in 1:(2^length(set))){
    t = i
    tmp = c()
    for (j in 1:length(set)){
      if (bitwAnd(t, 1)){
        tmp = c(tmp,set[[j]])
      }
      t =t %/% 2
    }
    if (length(tmp)==p){
      subsets[[length(subsets)+1]]=tmp
    }
  }
  return(subsets)
}

#' @name .construct_C_S_k
#' @title (Fonction cachée) Construire le vecteur (la liste [C(S{k}, m) + G[m,k]] pour tout m dans 'Subset')
#' @param C : la matrice tel que C[S,k] le coût min du chemin à partir de 1 et se termine au
#' #          sommet k, passant les sommets de l'ensemble S exactement une fois.
#' @param Subset : Un sous groupe du groupe complet des villes.
#' @param k : La ville pour laquelle on veut calculer C[S-k,m] pour tout m dans Subset.
#' @param G : la matrice des distances.
#' @usage .construct_C_S_k(C,Subset,k,G)
#' @return (la liste [C(S{k}, m) + G[m,k]] pour tout m dans 'Subset')
.construct_C_S_k <- function(C,Subset,k,G){
  C_S_k = list()
  S_k  = Subset[Subset!=k]
  row_S_k = paste(unlist(S_k), collapse='')
  for (m in Subset){
    if (m!=k){
      C_S_k[as.character(m)] = C[row_S_k,m] + G[m,k]
    }
  }
  return(C_S_k)
}

#' @name  .search_min_C_S_k
#' @title (Fonction cachée) recherche du min dans un vecteur et son index.
#' @param C_S_k : Une liste construit par la fonction '.construct_C_S_k'.
#' @return Le minimum du vecteur et l'index du minimum.
.search_min_C_S_k <- function(C_S_k){
  m_0 = names(C_S_k)[1]
  for (m in names(C_S_k)){
    if (C_S_k[[m]]  < C_S_k[[m_0]]){
      m_0=m
    }
  }
  return(list(c_s_k=C_S_k[[m_0]],m=as.integer(m_0)))
}

#' @name  held_karp
#' @title L'algorithme de Held_karp en R.
#' @param G : la matrice des distances.
#' @param n : le nombre de villes.
#' @usage held_karp(G,n)
#' @return La séquence des villes qui donnent la moindre en distance et la distance optimale.
#' @examples
#' G = matrix(runif(4*4 , min = 10, max = 50),nrow=4)
#' G = G %*% t(G) # rendre G symétrique (n'est pas nécessaire.).
#' diag(G) = 0 # annuler la diagonale de G.
#' n = 4 # nombre de villes.
#' results = held_karp(G,n)
#' results['path_opt'] # l'ordre optimale des villes.
#' results['dist_opt'] # la distance totale optimale.
held_karp <- function(G,n){
  C = matrix(Inf,nrow=0,ncol=n)
  C = data.frame(C)
  pr = matrix(Inf,nrow=0,ncol=n)
  pr = data.frame(pr)
  for (k in 2:n){
    C[nrow(C) + 1,] = rep(Inf,n)
    C[nrow(C),k] = G[1,k]
    rownames(C)[nrow(C)] = paste( unlist(k), collapse='')
  }

  for (s in 2:(n-1)){
    #subSets = combn(2:n, s, simplify = FALSE)
    subSets = .get_subsets(2:n,s)
    for (S in subSets){
      C[nrow(C) + 1,] = rep(Inf,n)
      rownames(C)[nrow(C)]=paste(unlist(S), collapse='')
      pr[nrow(pr) + 1,] = rep(Inf,n)
      rownames(pr)[nrow(pr)]=paste(unlist(S), collapse='')
      for (k in S){
        C_S_k = .construct_C_S_k(C,S,k,G)
        tmp = .search_min_C_S_k(C_S_k)
        C[nrow(C),k] = tmp$c_s_k
        pr[nrow(pr),k] = tmp$m
      }
    }
  }
  C_S_1    = .construct_C_S_k(C,c(2:n),1,G)
  min_tmp  = .search_min_C_S_k(C_S_1)
  dist_opt = min_tmp$c_s_k
  path_opt = rep(0,(n-1))
  path_opt[1] = min_tmp$m
  subset_opt = c(2:n)
  for (k in 2:(n-1)){
    row_S_k = paste(unlist(subset_opt), collapse='')
    path_opt[k] = pr[row_S_k,path_opt[k-1]]
    subset_opt = subset_opt[subset_opt!=path_opt[k-1]]
  }
  return(list(dist_opt=dist_opt,path_opt=path_opt))
}

########################### ESSAYS #############################

maxsize = Inf
N = 15
G = matrix(runif(N*N,min=10,max=50),nrow=N,ncol=N)
G = G %*% t(G) # rendre G symétrique.
diag(G) = 0 # annuler la diagonale de G.

tic()
held_karp(G,N)
toc()
