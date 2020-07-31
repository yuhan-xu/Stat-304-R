isLowerVowel = function(letter) { if(letter %in% c('a','e','i','o','u')){ return(TRUE) }; return(FALSE) }

baby.min = function(x, y){ if(x<y){ return(x) } else { return(y) } }

ageCategory = function(age) { if(age >= 0 & age <= 14){ return('child') } else if (age > 14 & age <=24 ) { return('youth') } else if (age >24 & age <= 64) { return('adult') } else { return('senior') } }

baby.min.vector = function(x, y) { return(ifelse(test=(x < y), yes=x, no=y)) }