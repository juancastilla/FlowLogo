n <- 1000
x <- matrix(runif(n*n), n, n)
y <- runif(n)
b <- gpuSolve(x, y)
cat("Solution:\n")
print(b)
