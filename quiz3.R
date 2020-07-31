

baby.sapply = function(X, FUN) { for(i in seq_len(length(X))){ X[i] = FUN(X[i]); }; return(X);}

baby.tapply = function(X, INDEX, FUN) { v <- c(); for(i in levels(factor(INDEX))){ v <- c(v, FUN(X[INDEX == i])); }; return(v) }

sumEven = function(x) { return(sum(x[x%%2 == 0])) }

sumEven = function(x) { sum <- 0; for(i in seq_len(length(x))){ if(x[i] %% 2 == 0){ sum = sum + x[i]; } }; return(sum) }

linearSearch = function(x, target) { for(i in seq_len(length(x))){ if(x[i] == target){ return(i) } }; return(0) }

binarySearch = function(x, target) { left = 1; right = length(x); while(left<=right){ midpt = (left+right)%/%2; if(target == x[midpt]){return(midpt)} else if(target < x[midpt]){ right = midpt - 1;} else{left = midpt + 1;} }; return(0) }

ageCategoryFromUser = function() {
  repeat{
    cat("Please enter: ");
    decision = scan(what=character(), n=1, quiet=TRUE)
    return(decision);
    if ((decision != "child") | (decision != "youth") | (decision != "adult") | (decision != "senior")) {
      break;
    }
  }
}