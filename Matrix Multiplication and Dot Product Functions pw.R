#Practice with FOR Loops
#Programming for Analytics
#Paul Whitson, UChicago MScA Program

#The functions below calculate the dot product of two vectors
#and the product of two matrices.  Note that the matrix multiplication function
#requires the dot product function.

#Dot Product Function

dot <- function(a,b){
  if((is.vector(a) && is.vector(b)) == FALSE) print("Error! Arguments must be vectors.")
  if(length(a) != length(b)) print("Error! Vectors must be same length")
  x <- 0
  for(i in 1:length(a)){
    x <- x + a[i]*b[i]
  }
    return(x)
}

#Test function here
a <- c(1,2,3)
b <- c(4,5,6)
dot(a,b)


#Matrix Multiplication Function:

MatMult <- function(m1, m2) {
  
  if((is.matrix(m1) && is.matrix(m2)) == FALSE) print("Error! Arguments must be matrices.")
  if(ncol(m1) != nrow(m2)) print("Error! Number of columns in first matrix must match number of rows in second.")
  
  I <- nrow(m1)
  L <- ncol(m2)
  #Initialize result matrix with 0s
  result <- matrix(c(rep(0,nrow(m1)*ncol(m2))), nrow = nrow(m1), ncol = ncol(m2))
  
  #result[i,l] is dot product of ith row of m1 with lth column of m2:
  for(i in 1:I) {
    for(l in 1:L) {
        result[i,l] <- dot(m1[i,], m2[,l])
      }
    }
  return(result)
}


#Test matrix multiplication function:
Mat1<-matrix(c(1,2,3,4), nrow = 2, byrow = TRUE)
Mat2<-matrix(c(5,6,7,8), nrow = 2, byrow = TRUE)

MatMult(Mat1, Mat2)

Mat3 <- matrix(c(1:16), nrow = 4)
Mat4 <- matrix(c(2:17), nrow = 4)

MatMult(Mat3, Mat4)


