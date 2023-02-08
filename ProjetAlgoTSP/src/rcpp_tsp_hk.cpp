#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void get_subsets(IntegerVector &subset, IntegerMatrix &result, int start, int n)
{
  result.push_back(subset);
  for (int i = start; i < n; i++)
  {
    subset.push_back(i + 1);
    get_subsets(subset, result, i + 1, n);
    subset.pop_back();
  }
}

// [[Rcpp::export]]
IntegerMatrix get_all_subsets(int n)
{
  IntegerMatrix result;
  IntegerVector subset;
  get_subsets(subset, result, 0, n);
  return result;
}

// [[Rcpp::export]]
IntegerVector construct_C_S_k(IntegerMatrix &C, IntegerVector &S, int k, IntegerMatrix &G) {
  IntegerVector C_S_k;
  for (int m : S) {
    if (m == k) {
      continue;
    }
    int C_m = C(m,k);
    int C_S_m = C(S,m);
    C_S_k.push_back(C_m + C_S_m);
  }
  return C_S_k;
}


int n = 5;  // number of cities
IntegerMatrix dist(5,5) = {{0, 2, 9, 10, 20},
{2, 0, 6, 10, 11},
{9, 6, 0, 2, 8},
{10, 10, 2, 0, 7},
{20, 11, 8, 7, 0}};  // distance matrix

std::map<std::pair<int, int>, int> C;  // C(S, k)
std::map<std::pair<int, int>, int> pr;  // previous node

// [[Rcpp::export]]
int search_min(int s, int k) {
  if (C.count({s, k}) > 0) {  // check if C(S, k) has been calculated
    return C[{s, k}];
  }

  int S = s - (1 << k);
  int ans = 0x3f3f3f3f;  // initialize answer to a large number
  int t = -1;  // initialize previous node
  for (int i = 0; i < n; i++) {
    if (S & (1 << i)) {  // check if i is in S
      int cur = dist(k,i) + search_min(S, i);
      if (cur < ans) {
        ans = cur;
        t = i;
      }
    }
  }
  C[{s, k}] = ans;
  pr[{s, k}] = t;
  return ans;
}






const int N=1<<15,INF=0x3f3f3f3f;
int n = INF;
int m = INF;
int and = INF;
dist(17,17);

int dp(int S,int u){
  if(S==(1<<n)-1&&u==0)return 0;
  int &ret=dist[u][S];
  if(ret)return ret;
  ret=INF;
  for(int v=0;v<n;v++)
    if(!(S&1<<v))
      ret=min(ret,dp(S|1<<v,v)+dist[u][v]);
    return ret;
}

int main(){
  std::cin>>n;
  for(int i=0;i<n;i++)
    for(int j=0;j<n;j++)
      std::cin>>dist[i][j];
  std::cout<<dp(0,0)<<std::endl;
  return 0;
}
