# Find Matrix using eigenvalues & eigenvectors
# Matrix

A = matrix(c(12,0,10,1),2,2)

# use eigen() function to find both values and vectors

eg = eigen(A)

# formula for determining A is SVS^-1
S = eg$vectors
V = eg$values

A2 = S %*% diag(V) %*% solve(S^1)

all.equal(A,A2)

#--------------------------------------------
# Example 2
#--------------------------------------------

A = matrix(c(1,1,0,0,1,1,0,1,1),3,3)

# Using matrix to solve linear equations

# Solving systems of linear equations
# consider these two equations

#  x1 + 3x2 = 7
# 2x1 + 4x2 = 10

# The system can be written in matrix form
# |1 3||x1|   | 7|
# |2 4||x2| = |10|
#
# A^-1 A=I     |-2  1.5| | 7|  |1|
# x = A^-1 b = | 1 -0.5| |10|= |2|

A = matrix(c(1,2,3,4),ncol=2)
b = c(7,10)
(x = solve(A) %*% b)


# (A + B)^t = A^t + B^t
B = matrix(c(2,6,8,2),ncol=2)

all.equal(t(A + B),(t(A) + t(B)))

# In general AB <> BA
#> A %*% B
#      [,1] [,2]
#[1,]   20   14
#[2,]   28   24
#> B %*% A
#      [,1] [,2]
#[1,]   18   38
#[2,]   10   26

all.equal((A %*% B),(B %*% A))

# The Inverse of a 2 x 2 matrix

#     |a b|
# A = |c d|
#
# Then the inverse is
# A^-1 = 1/ad - bc |d -b|
#                  |-c a|
# under the assumption that ab-bc<>0. The number ab-bc is called the determinant
# of A, sometimes written |A|. If |A| = 0, then A has no inverse.

# One method for matrix inversion is called the Gauss-Seidels method. R has the solve() function.

A=matrix(c(2,2,3,3,5,9,5,6,7),ncol=3)

# We want to find the matrix B = A^-1. To start, we append to A the identity matrix and call
# the result AB

AB=cbind(A,diag(c(1,1,1)))

#      [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]    2    3    5    1    0    0
#[2,]    2    5    6    0    1    0
#[3,]    3    9    7    0    0    1
#
# On a matrix we allow ourselves to do the following three operations as often as we want
# 1. Multiply a row by a (non-zero) constant
# 2. Multiply a row by a (non-zero) constant and add the result to another row.
# 3. Interchange two rows.

# The aim is to perform such operations on AB in a way such that one ends up with
# a 3 x 6 matrix which has the identity matrix in the three leftmost columns.
# The three rightmost columns will then contain B = A^-1.
# Recall that writing AB[1,] extracts the entire first row of AB

# 1, First, we make sure that AB[1,1]=1. Then we subtract a constant times the 
#  first row from the second to obtain that AB[2,1]=0, and similarly for the third row:
 AB[1,] = AB[1,]/AB[1,1]
 AB[2,] = AB[2,] - 2* AB[1,]
 AB[3,] = AB[3,] - 3* AB[1,]
 AB
#       [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]    1  1.5  2.5  0.5    0    0
# [2,]    0  2.0  1.0 -1.0    1    0
# [3,]    0  4.5 -0.5 -1.5    0    1

# 2. Next we ensure that AB[2,2]=1. Afterwards we subtract a constant times the
 # second row from the third to obtain that AB[3,2]=0
 AB[2,] = AB[2,]/AB[2,2]
 AB[3,] = AB[3,] - 4.5 * AB[2,]
 
# 3. Next we rescale the third row such that AB[3,3]=1
 AB[3,] = AB[3,]/AB[3,3]
 AB
#       [,1] [,2] [,3]       [,4]      [,5]       [,6]
# [1,]    1  1.5  2.5  0.5000000 0.0000000  0.0000000
# [2,]    0  1.0  0.5 -0.5000000 0.5000000  0.0000000
# [3,]    0  0.0  1.0 -0.2727273 0.8181818 -0.3636364 
 
# Then AB has zeros below the main diagonal.
# 4. We then work our way up to obtain that AB has zeros above the main diagonal
 AB[2,] = AB[2,] - 0.5 * AB[3,]
 AB[1,] = AB[1,] - 2.5 * AB[3,]
 AB
#      [,1] [,2] [,3]       [,4]        [,5]       [,6]
# [1,]    1  1.5    0  1.1818182 -2.04545455  0.9090909
# [2,]    0  1.0    0 -0.3636364  0.09090909  0.1818182
# [3,]    0  0.0    1 -0.2727273  0.81818182 -0.3636364

 AB[1,] = AB[1,] - 1.5 * AB[2,]
 AB 
#      [,1] [,2] [,3]       [,4]        [,5]       [,6]
# [1,]    1    0    0  1.7272727 -2.18181818  0.6363636
# [2,]    0    1    0 -0.3636364  0.09090909  0.1818182
# [3,]    0    0    1 -0.2727273  0.81818182 -0.3636364 
 
# Now extract the three rightmost columns of AB into matrix B.
# We claim that B is the inverse of A, and this can be verified by
# a simple matrix multiplication
 B = AB[,4:6]
 A %*% B
 
#              [,1]         [,2]         [,3]
# [1,]  1.000000e+00 0.000000e+00 0.000000e+00
# [2,] -4.440892e-16 1.000000e+00 4.440892e-16
# [3,] -2.220446e-16 8.881784e-16 1.000000e+00



x = c(1.00,2.00,3.00,4.00,5.00)
y = c(3.7,4.2,4.9,5.7,6)
plot(x,y,col="red")

# least squares
X =cbind(c(1,1,1,1,1),x)

beta.hat = solve(t(X) %*% X) %*% t(X) %*% y
abline(beta.hat,col="green")



