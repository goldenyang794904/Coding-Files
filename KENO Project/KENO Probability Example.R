####################################################################
##
## Keno Lottery (part 5/5): R Software Calculations and Simulations
##
####################################################################


rm(list=ls())

Prize.1<-c(0,2)
Prize.2<-c(0,0,10)
Prize.3<-c(0,0,2,25)
Prize.4<-c(0,0,1,5,60)
Prize.5<-c(0,0,0,2,20,330)
Prize.6<-c(0,0,0,1,6,55,1000)
Prize.7<-c(0,0,0,1,2,15,100,5000)
Prize.8<-c(0,0,0,0,2,6,75,550,10000)
Prize.9<-c(0,0,0,0,1,5,20,125,3000,30000)
Prize.10<-c(5,0,0,0,0,2,10,45,300,5000,100000)
Prize.1b=c(0,50)
Prize.2b=c(0,15,62)
Prize.3b=c(0,8,17,125)
Prize.4b=c(0,5,11,25,300)
Prize.5b=c(0,5,4,12,80,930)
Prize.6b=c(0,5,3,6,31,155,3500)
Prize.7b=c(0,5,2,4,12,75,500,12500)
Prize.8b=c(0,5,2,2,7,26,200,1800,50000)
Prize.9b=c(0,5,2,2,6,15,60,525,8000,80000)
Prize.10b=c(0,5,2,2,3,7,35,145,1300,25000,300000)
Prize.1db=c(0,0)
Prize.2db=c(0,0,155)
Prize.3db=c(0,0,43,313)
Prize.4db=c(0,0,28,63,750)
Prize.5db=c(0,0,10,30,200,2325)
Prize.6db=c(0,0,8,15,78,388,8750)
Prize.7db=c(0,0,5,10,30,188,1250,31250)
Prize.8db=c(0,0,5,5,18,65,500,4500,125000)
Prize.9db=c(0,0,5,5,15,38,150,1313,20000,200000)
Prize.10db=c(0,0,5,5,8,18,88,363,3250,62500,1000000)

Mult<-c(1,2,3,4,5,10)
Mult.Prob<-c(32,34,5,5,3,1)/80

### No bullseye, no multiplier
reps <- 100000
N<-4
tmp.prize <- get(paste0("Prize.",N))
ticket <- sample(1:80,N)
win.prob <- 0
winnings <- 0
for(i in 1:reps) {
  keno.nums <- sample(1:80,20)
  match <- sum(1*(ticket %in% keno.nums))
  win.prob <- win.prob + 1*(tmp.prize[match+1]>0)
  winnings <- winnings + tmp.prize[match+1]
}
win.prob/reps
winnings/reps
reps/win.prob


### No bullseye, Yes multiplier
reps <- 50000
N<-7
tmp.prize <- get(paste0("Prize.",N))
ticket <- sample(1:80,N)
win.prob <- 0
winnings <- 0
for(i in 1:reps) {
  keno.nums <- sample(1:80,20)
  match <- sum(1*(ticket %in% keno.nums))
  win.prob <- win.prob + 1*(tmp.prize[match+1]>0)
  winnings <- winnings + tmp.prize[match+1]*sample(Mult,1,prob=Mult.Prob)
}
win.prob/reps
winnings/reps
reps/win.prob



## bulls-eye simulation
reps <- 50000
N<-5
tmp.prize.b <- get(paste0("Prize.",N,"b"))
tmp.prize <- get(paste0("Prize.",N))
ticket <- sample(1:80,N)
win.prob <- matrix(0,nrow=2,ncol=N+1)
winnings <- 0
for(i in 1:reps) {
  keno.nums <- sample(1:80,20)
  bullseye <- sample(keno.nums,1)
  match <- sum(ticket %in% keno.nums)
  match.bull <- bullseye %in% ticket
  win.prob[1*match.bull+1,match+1]<-win.prob[1*match.bull+1,match+1] + 1
  winnings <- winnings + if(match.bull) tmp.prize.b[match+1] else tmp.prize[match+1]
}

win.prob/reps
winnings/reps
sum(win.prob[1,]*(tmp.prize>0)/reps + win.prob[2,]/reps)
1/sum(win.prob[1,]*(tmp.prize>0)/reps + win.prob[2,]/reps)
sum(win.prob[2,])/reps



## double bulls-eye simulation
reps <- 10000
N<-3
tmp.prize.db <- get(paste0("Prize.",N,"db"))
tmp.prize.b <- get(paste0("Prize.",N,"b"))
tmp.prize <- get(paste0("Prize.",N))
ticket <- sample(1:80,N)
win.prob <- matrix(0,nrow=3,ncol=N+1)
winnings <- 0
for(i in 1:reps) {
  keno.nums <- sample(1:80,20)
  bullseye <- sample(keno.nums,2)
  match <- sum(ticket %in% keno.nums)
  match.bull <- sum(bullseye %in% ticket)
  win.prob[match.bull+1,match+1]<-win.prob[match.bull+1,match+1] + 1
  winnings <- winnings + switch(match.bull+1,tmp.prize[match+1],tmp.prize.b[match+1],tmp.prize.db[match+1])
}

winnings/reps
sum(win.prob[1,]*(tmp.prize>0)/reps + win.prob[2,]*(tmp.prize.b>0)/reps + win.prob[3,]*(tmp.prize.db>0)/reps)
1/sum(win.prob[1,]*(tmp.prize>0)/reps + win.prob[2,]*(tmp.prize.b>0)/reps + win.prob[3,]*(tmp.prize.db>0)/reps)
1/c(sum(win.prob[1,]*(tmp.prize>0))/reps,sum(win.prob[2,]*(tmp.prize.b>0))/reps,sum(win.prob[3,]*(tmp.prize.db>0))/reps)
rowSums(win.prob)/reps
