#FORTES, PATRICIA JO B.
#B - 6L
#EXER 3: Creating Augmented Coefficient Matrix

#INPUT DATA ---------------------------->
E1 <- function(x, y, z) 3 * x + 3 * y + 3 * z + 12
E2 <- function(x, y, z) 4 * x + 4 * y + 4 * z + 13
E3 <- function(q, y, z) 5 * y + -5 * x + 5 * z + 14

system <- list(E1,E2,E3)
#--------------------------------------->

#FUNCTION: converting system to string and returns vectorVar
toStringVar <- function(system) {
  size = length(unlist(system))
  vectorVar = c()
 
  for(i in 1:size) {
   vectorVar[i] = (deparse(system[[i]])[1])
  }
 
  return(vectorVar)
}

#FUNCTION: converting system to string and returns vectorCoeff
toStringCoeff <- function(system) {
  size = length(unlist(system))
  vectorCoeff = c()
  
  for(i in 1:size) {
    vectorCoeff[i] = (deparse(system[[i]])[2])
  }
 
  return(vectorCoeff)
}

#FUNCTION: splitting vectorVar and returning list of variables
splitVar <- function(system) {
  vectorVar = toStringVar(system)
  size = length(unlist(vectorVar))
  varSubstr = c()
  variables = c()
  verifier = FALSE
  index = 1
  
  for(i in 1: size) {
   varSubstr[i] = (substring(vectorVar[i], 10))
  }
  for(i in 1:(size - 1)) {
    if(varSubstr[i] == varSubstr[i+1]) {
      verifier = 0
    }
    else{
      verifier = 1
    }
  }
  if(verifier == 0) {
    finalVar = unlist(strsplit(varSubstr[1],"[, (,)]"))
    
    for(i in 1:length(finalVar)) {
      
      if(finalVar[i] != "") {
        variables[index] = finalVar[i]
        index = index + 1
      }
    }
   (variables)
  }
}

#FOR MATRIX COEFFICIENT ----------------------------->
splitCoeff <- function(system) {
  vectorCoeff = toStringCoeff(system)
  row = length(unlist(vectorCoeff))
  col = length(unlist(toStringVar(system))) + 1
  vars = splitVar(system)
  rownames = c(1:row)
  colnames = c(vars,"RHS")
  augcoeffmatrix = matrix(0,row,col,TRUE,dimnames = list(rownames, colnames))
 
  
  for(i in 1: row) {
    coeffTemp = unlist(strsplit(vectorCoeff[i],"[*,+]"))
    size = length(coeffTemp)
    
    #placing constant on matrix
    augcoeffmatrix[i, col] = as.numeric((coeffTemp[7]))
    
    #placing coefficients on the matrix
    for(j in 1:6) {
     if(coeffTemp[j + 1] == vars[1]) {
       augcoeffmatrix[i, 1] = as.numeric((coeffTemp[j]))
      }
      if(coeffTemp[j + 1] == vars[2]) {
        augcoeffmatrix[i, 2] = as.numeric((coeffTemp[j]))
      }
      if(coeffTemp[j + 1] == vars[3])  {
        augcoeffmatrix[i, 3] = as.numeric((coeffTemp[j]))
      }
     
    }
  
  }
  results = list(variables = vars, augcoeffmatrix = augcoeffmatrix)
  return(results)
  
}
  


