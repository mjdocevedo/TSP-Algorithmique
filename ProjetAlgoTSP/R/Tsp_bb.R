# Traveling Salesman Problem utilisant
# Branch and Bound.

# Fonction pour copier la solution temporaire dans le vecteur de solution finale
copyToFinal <- function(curr_path){
  final_path <<- curr_path
  final_path[N+1] <<- curr_path[1]
}


# Fonction qui garde le cout minimal adjacent au noeud i
firstMin <- function(adj,i){
  min = maxsize
  for (k in 1:N)
    if(adj[i,k] < min & i!=k)
      min = adj[i,k]
    return(min)
}

# Fonction qui garde le deuxieme cout minimal adjacent au noeud i
secondMin <- function(adj,i){
  first = maxsize
  second = maxsize
  for (j in 1:N){
    if(i == j)
      next
    if(adj[i,j]<=first){ #il y a un mineur que le premier et le second
      second = first
      first = adj[i,j]
    }
    else if(adj[i,j] <= second & adj[i,j] != first) #il est mineur que le second mais pas que le premier
      second = adj[i,j]
  }
  return(second)
}

# fonction qui prend en entreee
# curr_bound -> seuil
# curr_weight-> cout present
# level-> niveau de l'arbre de decision qu'on est en train de visiter
# curr_path[] -> wvecteur ou l'on garde la solution jusqu'a present et qui sera garde en final_path[]
BranchBoundRec <- function(adj, curr_bound, curr_weight,level, curr_path, visited){
  # base case quand on arrive au niveau N, on a parcouru tous les niveaux de l'arbre
  if (level == N+1){
    # verifier qu'il existe un chemin entre le premier neud et le dernier
    if (adj[curr_path[level-1],curr_path[1]] != 0){
      # curr_res poids/cout total
      curr_res = curr_weight + adj[curr_path[level-1],curr_path[1]]
      if(curr_res < final_res){
        copyToFinal(curr_path)
        final_res <<- curr_res
      }
    }
    #return()
  }
  # on parcourut l'arbre de facon recursive
  for (i in 1:N){
    # On ne considere pas les valeurs dans la diagonale d'adj
    if (adj[curr_path[level-1],i] != 0 & visited[i] == FALSE){
      temp = curr_bound
      curr_weight = curr_weight + adj[curr_path[level-1],i]
      if (level == 2){
        curr_bound = curr_bound - ((secondMin(adj, curr_path[level-1]) + secondMin(adj, i))/ 2)
      } else{
        curr_bound = curr_bound - ((firstMin(adj, curr_path[level-1]) + secondMin(adj, i))/2)
      }
      # si le seuil est mineur, on continue l'exploration
      if (curr_bound + curr_weight < final_res){
        curr_path[level] = i
        visited[i] = TRUE
        #print(curr_path)
        #print(final_path)
        #print(curr_bound)
        #print(curr_weight)
        #print(final_res)
        #print(level)
        # on fait l'appel recursive
        BranchBoundRec(adj, curr_bound, curr_weight,level + 1, curr_path, visited)
      }
      # on enleve les chemins a ne pas suivre
      curr_weight = curr_weight - adj[curr_path[level-1],i]
      curr_bound = temp

      visited = c(rep(FALSE,length(visited)))
      for (j in 1:level)
        if (curr_path[j] != -1)
          visited[curr_path[j]] = TRUE
    }
  }
}

BranchBound <- function(adj){
  curr_bound = 0
  curr_path = c(rep(-1, N + 1))
  visited = c(rep(FALSE, N))

  # on calcule le seuil initiale
  for (i in 1:N)
    curr_bound = curr_bound + (firstMin(adj, i) + secondMin(adj, i))

  curr_bound = ceiling(curr_bound / 2)
  visited[1] = TRUE
  curr_path[1] = 1
  BranchBoundRec(adj, curr_bound, 0, 2, curr_path, visited)
}



########################### ESSAYS #############################

maxsize = Inf
N = 15


final_path = c(rep(NA,N+1))

visited = c(rep(FALSE, N ))

final_res = maxsize

# Adjacency matrix for the given graph
#adj = matrix(c(0, 10, 15, 20,
#               10, 0, 35, 25,
#               15, 35, 0, 30,
#               20, 25, 30, 0), nrow = N, ncol = N)
#adj = matrix(c(0, 10, 15,
#               10, 0, 35,
#               15, 35, 0), nrow = N, ncol = N)
#BranchBound(adj)
G = matrix(runif(N*N,min=10,max=50),nrow=N,ncol=N)
G = G %*% t(G) # rendre G symétrique.
diag(G) = 0 # annuler la diagonale de G.

tic()
BranchBound(G)
toc()
print(final_res)
print(final_path)

# This code is contributed by ng24_7
