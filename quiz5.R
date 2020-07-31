rm(list = ls()) #initialization
a = array(data=1:12, dim=c(3, 2, 2));
a
a = array(data=1:12, dim=c(2,6));
a
# 1) 11

# 2) 22
a = array(data=1:12, dim=c(3, 2, 2));
a
sum(a[1,,])
#length(dim(a))

#3
a = array(data=1:12, dim=c(3, 2, 2));
a[,1,]
sum(a[,1,])

sum(a[,,1])

sum(a[(a %% 2) != 0])

b = matrix(data = c(c(1, 7), c(5,4)),nrow = 2, ncol = 2, byrow = TRUE);
b
b1 = cbind(b, 1:1)
b2 = rbind(1:1, b1)
b2
b3 = cbind(1:1,b2)
b4 = rbind(b3,1:1)
b4
sum(b4)

c1 = matrix(data = c(c(1, 7, 5), c(4,7,6),c(2,6,4)),nrow = 3, ncol = 3, byrow = TRUE);
c1
cdig = c1[row(c1) - col(c1) == 2 - 1]
sum(cdig)

cdig2 = c1[row(c1) + col(c1) == 3 + 2]
sum(cdig2)

A = matrix(data = c(c(1, 7,5), c(4,7,6)),nrow = 2, ncol = 3, byrow = TRUE);
B = matrix(data = c(c(2, 6), c(4,1),c(7,5)),nrow = 3, ncol = 2, byrow = TRUE);
C = A %*% B
C
sum(C)


vector.sum = function(x) { result = 0; for(i in x){ result = result + i; }; return(result) }

vector.sum(c(1,2,3,4,5))

matrix.sum = function(m) { result = 0; for(i in 1:nrow(m)){ for(j in 1:ncol(m)){if(is.null(j) == FALSE){ result = result + m[i,j];};}; }; return(result) }

c1 = matrix(data = c(c(1, 7, NULL), c(NULL,7,6),c(2,7,4)), byrow = TRUE);
matrix.sum(c1)

array.sum = function(a) { result = 0; v <- dim(a); if(is.null(a)){ return(0) }; for(i in 1:v[1]){ for(j in 1:v[2]){ for(k in 1:v[3]){ if(!is.null(a[i,j,k])){ result = result + a[i,j,k];};};};}; return(result) }

d1 = array(data = (1:24), dim = c(3,4,2));
#const1 = dim(d1)[1]
#const1
#print(d1[1,2,2])
array.sum(d1)


setMatrixValues = function(A, rows, columns, values) { for(i in seq_len(length(rows))){ A[rows[i],columns[i]] = values[i]; }; return(A) }
setMatrixValues(A=matrix(data=1:6, nrow=2, ncol=3),rows=c(2, 1), columns=c(3, 2), values=c(7, 8))


