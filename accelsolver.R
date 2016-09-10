
######################################################################
#### SOLVES FLOWLOGO EQUATIONS USING RSERVE AND SPARSEM PACKAGES #####
######################################################################

#library(pcg)
#library(Matrix)
#library(gputools)

library(Rserve)
library(SparseM)

################################# SOLUTION USING 'sparseM' PACKAGE ##############################

#Cholesky decomposition of conductance matrix from GW flow equations
cholesky_A <- function(A,n)
{
  A <- matrix(unlist(A),nrow=n,ncol=n)
  A.csr <- as.matrix.csr(A)                   #transform to csr sparse format
  A.chol <<- chol(A.csr,nsubmax=50*n)            #perform cholseky decomp and store as global
}

### Solve GW flow equations for confined conditions ###
solveSparse <- function(A.chol,b,n)
  {
  b <- matrix(unlist(b),nrow=n,ncol=1)        #transform from NetLogo list to R matrix
  b.csr <- as.matrix.csr(b)                   #transform to csr sparse format
  x<-backsolve(A.chol,b)                      #solve using pre-computed Choleski decomposition
  x<-as.matrix(x)                             #csr to matric format
  x<<-as.data.frame(x)                        #save as global variable that NetLogo can retrieve
}













################################# OTHER METHODS ##############################

# solvePCG <- function(A_fl,b_fl,n)
#   {
#   A_r<-matrix(unlist(A_fl),nrow=n,ncol=n)
#   b_r<-matrix(unlist(b_fl),nrow=n,ncol=1)
#   x<-gpuSolve(A_r,b_r)
#   #x<-pcg(A_r,b_r,maxiter = 1e+02, tol = 1e-01)
#   x<<-as.data.frame(x)
#   }


# solveCHSPARSE <- function(A_fl,b_fl,n)
# {
#   A_r<-matrix(unlist(A_fl),nrow=n,ncol=n)
#   b_r<-matrix(unlist(b_fl),nrow=n,ncol=1)
#   x<-gpuSolve(A_r,b_r)
#   #x<-pcg(A_r,b_r,maxiter = 1e+02, tol = 1e-01)
#   x<<-as.data.frame(x)
# }