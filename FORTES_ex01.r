#FORTES, PATRICIA JO B
#CMSC 150 B-6L
#EXER NO 1: evaluating a cosine function

#computes for the factorial in the series
factorial <- function(x) {
  i = 1;
  temp = 1;
  while(i != x){
    temp = temp * (i+1);
    i = i + 1;
  } 
  if(i == x) {
    return(temp);
  } 
}

#power in the numerator part
numerator <- function(x,n) {
  prod = x ^ n;
  return(prod);
}


cosineFunction <- function(x,n) {
  X <- vector(mode="numeric", length=n)
  if(n == 1) { #when n = 1
    X[1] = 1;
  } else {
      X[1] = 1;
      k = 1;
      step = 2;
      pSum = 1 - ((numerator(x,step)) / (factorial(step)));
      X[2] = pSum;
      k = k + 1;
      if(n > 2 ){ #for continuous term
        while (k != n) {
          step = step + 2;
          k = k + 1;
          if(k %% 2 != 0){
          pSum = pSum + ((numerator(x,step)) / (factorial(step)));
          X[k] = pSum;
  
          } else{
            pSum = pSum - ((numerator(x,step)) / (factorial(step)));
            X[k] = pSum;
          }
      
      }
    }
  }
  
  print(X)
 }
  
  
  





