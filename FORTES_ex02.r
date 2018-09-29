#FORTES, PATRICIA JO B.
#CMSC 150 B-6L
#EXER NO 2: computing for the inverse of a matrix

#INPUT DATA-----------------------
matrix_a = matrix(c(1,-1,-1,-1,2,3,1,1,4), 3,3, byrow = TRUE)
#--------------------------------

#determines if a matrix is either square or not
isSquareMatrix <- function(matrix_a) {
  if(nrow(matrix_a) == ncol(matrix_a)) {
    return(TRUE)
  }else {
    return(FALSE)
  }
}

# returns the minor of the ith and jth element of the matrix
matrixMinor <- function(matrix_a, i,j) {
    if(isSquareMatrix(matrix_a) == "TRUE") {
        return(matrix_a[-i,-j])
    }
    
}

# computes for the cofactor given row and col and minor
matrixCofactor <- function(matrix_a, i,j) {
  minor = matrixMinor(matrix_a, i ,j)
  if((class(minor) == "numeric") || (class(minor) == "integer" )) {
    
    cofactor = (((-1) ^ (i+j)) * minor)
    
  }else {
   
    cofactor = (((-1) ^ (i+j)) * (det(minor)))
   
  }
 
  return(cofactor);
}

# creates another matrix wherein it is the transpose of matrix of cofactor
matrixAdjoint <- function(matrix_a) {
    if(isSquareMatrix(matrix_a) == "TRUE"){
        matrixSize = nrow(matrix_a)
        matrix_b = matrix(0,matrixSize,matrixSize)
        for(i in 1:matrixSize) {
            for(j in 1:matrixSize) {
                matrix_b[j,i] = matrixCofactor(matrix_a,i,j)
            }
        }
      
        return(matrix_b)
    }else {
        return("NA")
    }
}

# computes for the inverse of the original matrix
matrixInverse <- function(matrix_a) {
    if(isSquareMatrix(matrix_a) == "TRUE"){
        inverse = ((1 / det(matrix_a)) * (matrixAdjoint(matrix_a)))
        return(inverse)
    }else {
        return("NA")
    }
}
