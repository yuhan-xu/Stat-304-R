# Name: Yuhan Xu
# Email: yxu329@wisc.edu

rm(list = ls())

# Implement Connect Four in the same manner that we
# implemented tic-tac-toe in lecture. Start by implementing
# the helper functions, below, and testing them by running
#   source("HW2test.R")
# Then write code for the game itself.
#
# We'll test your code by running
#   source("HW2.R")
# We might also play Connect Four, read your code, and do other tests.

# Returns TRUE if vector v (of character strings) contains
# (at least) four in a row of player (character string). e.g.
#   four.in.a.row("X", c("O","X","X","X","X","O"))
# is TRUE, while
#   four.in.a.row("O", c("O","X","X","X","X","O"))
# is FALSE.
four.in.a.row = function(player, v, debug=FALSE) {
  if (debug) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  
  if (length(v) < 4) {
    return(FALSE)
  } else {
    count = 1
    i = 2
    
    while (i <= length(v)) {
      if (v[i] == v[i - 1]) {
        count = count + 1
      } else {
        count = 1
      }
      
      if ((count >= 4) & (v[i] == player)) {
        return(TRUE)
      }
      
      i = i + 1
    }
    
    return(FALSE)
  }
  
}

# Returns TRUE if (matrix) board (of character strings)
# contains at least four in a row of (string) player, who
# just played in position (r, c). (Here "r" means "row" and
# "c" means "column").
#
# Hint: this function should call four.in.a.row() four times, once
# each for the current row, column, diagonal, and reverse diagonal.
won = function(player, board, r, c, debug=FALSE) {
  if (debug) {
    cat(sep="", "won(player=", player, ", board=\n")
    print(board)
    cat(sep="", ", r=", r, ", c=", c, ")\n")
  }
  # call four.in.a.row() four times to check if there are four in a row, in a column, in diagonal, and in reverse diagonal
  return(four.in.a.row(player, board[r,]) | four.in.a.row(player, board[,c]) | four.in.a.row(player, board[row(board) - col(board) == r - c]) | 
           four.in.a.row(player, board[row(board) + col(board) == r + c]))
}

# Returns largest index of an empty position in column col
# of (matrix) board. If there is no such empty position in
# board, return value is NULL.
largest.empty.row = function(board, col, debug=FALSE) {
  if (debug) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  v = board[, col]
  count = 0
  for (i in 1:6) {
    if (v[i] == "E") 
      count = count + 1
  }
  return(count)
}

source("HW2test.R") # Run tests on the functions above.

# ... your code to implement Connect Four using the
# functions above ...


# Hint: this program is modeled on the tic-tac-toe program we did in
# lecture, so studying the latter program is worthwhile.

# Note that a user click in a column indicates that a checker should
# go to that column's lowest empty row (unless the column is full).

# Note that you should implement a computer player. At the least, it
# should choose randomly from among the non-full columns. (Feel free
# to do much more!)

par(pty = "s") # generate a square plotting region
x = rep(1:7, each = 6)
y = rep(1:6, times = 7)
symbols(x = x, y = y, squares = rep(1, 42), inches = FALSE, xlim = c(0, 8), 
        ylim = c(8, 0))

board = matrix(rep("E", 42), nrow = 6, ncol = 7) # E means empty, nothing in that place
player = "X"

for (j in 1:42) {
  if (player == "X") { # human player
    repeat {
      index = identify(x, y, n = 1, plot = FALSE)
      col = x[index]
      row = y[largest.empty.row(board, col)]
      
      if(largest.empty.row(board, col) == 0){
        text(x=4,y=7.6,labels="The column is already full!",col="red");
      } else{
        if (board[row, col] == "E") {
          break
        }
        
      }

    }
  } else { # computer player
    indices.of.open.squares =  which(c(board) == "E")
    n.open = length(indices.of.open.squares)
    index = indices.of.open.squares[sample.int(n = n.open,size = 1)]
    col = x[index]
    row = y[largest.empty.row(board, col)]
  }
  
  board[row, col] = player
  text(x=col, y=row, labels=player)
  
  print(board)
  
  # print the message if player player wins
  if (won(player, board, row, col,debug=FALSE)) {
    text(x=4,y=0.2,labels=paste(player," won!"),col="red")
    break
  }
  
  player = ifelse(test=(player == "X"), yes="O", no="X")
}
