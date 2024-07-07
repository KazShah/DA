#Assessment #3
#Q2: Factory Profit problem

library(lpSolveAPI)
#As the problem has 9 decision variables and 9 constraints 9x9 matrix is defined (6 proportion constraints and 3 demand constraints)

lprec<-make.lp(9,9)

#As we need to maximize the profit so sense is defined as maximize
lp.control(lprec,sense="maximize")

#Obejctive function defintion
set.objfn(lprec,c(25,21,25,10,6,10,5,1,5))

#Defining Constraints
set.row(lprec,1,c(1,1,1),indices=c(1,4,7))
set.row(lprec,2,c(1,1,1),indices=c(2,5,8))
set.row(lprec,3,c(1,1,1),indices=c(3,6,9))
set.row(lprec,4,c(0.45,-0.55,-0.55),indices=c(1,4,7))
set.row(lprec,5,c(0.7,-0.3,-0.3),indices=c(1,4,7))
set.row(lprec,6,c(0.55,-0.45,-0.45),indices=c(2,5,8))
set.row(lprec,7,c(0.6,-0.4,-0.4),indices=c(2,5,8))
set.row(lprec,8,c(0.7,-0.3,-0.3),indices=c(3,6,9))
set.row(lprec,9,c(-0.5,0.5,-0.5),indices=c(3,6,9))

set.rhs(lprec,c(4800,3000,3500,0,0,0,0,0,0))

set.constr.type(lprec,c("<=","<=","<=",">=",">=",">=",">=",">=",">="))

set.type(lprec,c(1:9),"real")

set.bounds(lprec,rep(0,9),upper=rep(Inf,9))

solve(lprec)

objvalue<-get.objective(lprec)

objvalue

solution<-get.variables(lprec)

solution

sum(solution[c(1,4,7)])

sum(solution[c(2,5,8)])

sum(solution[c(3,6,9)])
get.constraints(lprec)
################################################################
###############################################################
#Q3 Two player zero sum game
#Player-1 Game
library(lpSolveAPI)

lprec <- make.lp(0, 8)

lp.control(lprec, sense= "maximize")  

set.objfn(lprec, c(0, 0, 0,0,0,0,0, 1))

add.constraint(lprec, c(-1,-2,-1,0,-1,-2,-1,1), "<=", 0)

add.constraint(lprec, c( 0,-1,-2,-2,-2,-1,0,1), "<=", 0)

add.constraint(lprec, c(0,0,-2,-4,-2,0,0,1), "<=", 0)

add.constraint(lprec, c(0,-1,-2,-2,-2,-1,0,1), "<=", 0)

add.constraint(lprec, c(-1,-2,-1,0,-1,-2,-1,1), "<=", 0)

add.constraint(lprec, c(1,1,1,1,1,1,1,0), "=", 1)

set.bounds(lprec, lower = c(0, 0, 0,0,0,0,0, -Inf))

RowNames <- c("Row1", "Row2", "Row3","Row4","ROW5","ROW6")

ColNames <- c("x1", "x2", "x3","x4","x5","x6","x7", "v")

dimnames(lprec) <- list(RowNames, ColNames)

solve(lprec) 

get.objective(lprec)

get.variables(lprec)

get.constraints(lprec)

lprec
#######################################################
#Player-2 Game

library(lpSolveAPI)

lprec <- make.lp(0, 6)

lp.control(lprec, sense= "minimize")  

set.objfn(lprec, c(0,0,0,0,0,1))

add.constraint(lprec, c(-1,0,0,0,-1,1), ">=", 0)

add.constraint(lprec, c( -2,-1,0,-1,-2,1), ">=", 0)

add.constraint(lprec, c(-1,-2,-2,-2,-1,1), ">=", 0)

add.constraint(lprec, c(0,-2,-4,-2,0,1), ">=", 0)

add.constraint(lprec, c(-1,-2,-2,-2,-1,1), ">=", 0)

add.constraint(lprec, c(-2,-1,0,-1,-2,1), ">=", 0)

add.constraint(lprec, c(-1,0,0,0,-1,1), ">=", 0)

add.constraint(lprec, c(1,1,1,1,1,0), "=", 1)

set.bounds(lprec, lower = c(0,0,0,0,0, -Inf))

RowNames <- c("Row1", "Row2", "Row3","Row4","ROW5","ROW6","ROW7","ROW8")

ColNames <- c("y1", "y2", "y3","y4","y5","v")

dimnames(lprec) <- list(RowNames, ColNames)

solve(lprec) 

get.objective(lprec)

get.variables(lprec)

get.constraints(lprec)

lprec

