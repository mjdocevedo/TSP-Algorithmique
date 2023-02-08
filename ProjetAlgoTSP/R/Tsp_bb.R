# Traveling Salesman Problem utilisant
# Branch and Bound.
# Function to copy temporary solution
# to the final solution
copyToFinal <- function(curr_path){
  final_path <<- curr_path
  final_path[N+1] <<- curr_path[1]
}


# Function to find the minimum edge cost
# having an end at the vertex i
firstMin <- function(adj,i){
  min = maxsize
  for (k in 1:N)
    if(adj[i,k] < min & i!=k)
      min = adj[i,k]
    return(min)
}

# function to find the second minimum edge
# cost having an end at the vertex i
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

# function that takes as arguments:
# curr_bound -> lower bound of the root node
# curr_weight-> stores the weight of the path so far
# level-> current level while moving
# in the search space tree
# curr_path[] -> where the solution is being stored
# which would later be copied to final_path[]
BranchBoundRec <- function(adj, curr_bound, curr_weight,level, curr_path, visited){
  # base case is when we have reached level N
  # which means we have covered all the nodes once
  if (level == N+1){
    # check if there is an edge from
    # last vertex in path back to the first vertex
    if (adj[curr_path[level-1],curr_path[1]] != 0){ ##### CHECK THE "level-1" in python implementation
      # curr_res has the total weight
      # of the solution we got
      curr_res = curr_weight + adj[curr_path[level-1],curr_path[1]]
      if(curr_res < final_res){
        copyToFinal(curr_path)
        final_res <<- curr_res
      }
    }
    #return()
  }
  # for any other level iterate for all vertices
  # to build the search space tree recursively
  for (i in 1:N){
    # Consider next vertex if it is not same
    # (diagonal entry in adjacency matrix and
    # not visited already)
    if (adj[curr_path[level-1],i] != 0 & visited[i] == FALSE){
      temp = curr_bound
      curr_weight = curr_weight + adj[curr_path[level-1],i]
      # different computation of curr_bound
      # for level 2 from the other levels
      if (level == 2){
        curr_bound = curr_bound - ((secondMin(adj, curr_path[level-1]) + secondMin(adj, i))/ 2)
      } else{
        curr_bound = curr_bound - ((firstMin(adj, curr_path[level-1]) + secondMin(adj, i))/2)
      }
      # curr_bound + curr_weight is the actual lower bound
      # for the node that we have arrived on.
      # If current lower bound < final_res,
      # we need to explore the node further
      if (curr_bound + curr_weight < final_res){
        curr_path[level] = i
        visited[i] = TRUE
        #print(curr_path)
        #print(final_path)
        #print(curr_bound)
        #print(curr_weight)
        #print(final_res)
        #print(level)
        # call BranchBoundRec for the next level
        BranchBoundRec(adj, curr_bound, curr_weight,level + 1, curr_path, visited)
      }
      # Else we have to prune the node by resetting
      # all changes to curr_weight and curr_bound
      curr_weight = curr_weight - adj[curr_path[level-1],i]
      curr_bound = temp

      # Also reset the visited array
      visited = c(rep(FALSE,length(visited)))
      for (j in 1:level)
        if (curr_path[j] != -1)
          visited[curr_path[j]] = TRUE
    }
  }
}

# This function sets up final_path
BranchBound <- function(adj){
  # Calculate initial lower bound for the root node
  # using the formula 1/2 * (sum of first min +
  # second min) for all edges. Also initialize the
  # curr_path and visited array
  curr_bound = 0
  curr_path = c(rep(-1, N + 1))
  visited = c(rep(FALSE, N))

  # Compute initial bound
  for (i in 1:N)
    curr_bound = curr_bound + (firstMin(adj, i) + secondMin(adj, i))
  # Rounding off the lower bound to an integer
  curr_bound = ceiling(curr_bound / 2)
  # We start at vertex 1 so the first vertex
  # in curr_path[] is 1
  visited[1] = TRUE
  curr_path[1] = 1
  # Call to BranchBoundRec for curr_weight
  # equal to 0 and level 1
  BranchBoundRec(adj, curr_bound, 0, 2, curr_path, visited)
}





########################### ESSAYS #############################

# Driver code
maxsize = Inf
N = 15

# final_path[] stores the final solution
# i.e. the // path of the salesman.
final_path = c(rep(NA,N+1))

# visited[] keeps track of the already
# visited nodes in a particular path
visited = c(rep(FALSE, N ))

# Stores the final minimum weight
# of shortest tour.
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
G = matrix(runif(15*15,min=10,max=50),nrow=15,ncol=15)
G = G %*% t(G) # rendre G symétrique.
diag(G) = 0 # annuler la diagonale de G.

tic()
BranchBound(G)
toc()
print(final_res)
print(final_path)

# This code is contributed by ng24_7
