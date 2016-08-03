add<- function(a,b)
{
  c<- a +b
  return(c)
}
add(6,5)

add_n<- function(n)
{
  d<- (n*(n+1))/2
  return(d)
}
add_n(4)

sum_numbers<- function(g){
  total<-0
  for(i in 1:g)
    total<-total+i
return(total)
}
sum_numbers(4)

vector_numbers<- function(f){
  total<-0
  for(i in f)
    total<-total+i
  return(total)
}
vector_numbers(c(1,3,5,7))

add_odd <- function(p)
{
  total<-0
  for ( i in 1:p)
  {
    if(i%%2 != 0 )
    {
      total<- total+i
    }
  }
  return(total)
}
add_odd(5)

circle<- function(r)
{
  area<- pi*r*r
  circumfrence<- 2*pi*r
  return(c(area,circumfrence))
  
}
circle(4)
options(digits=4)

temp_conversion<- function(fah)
{
  centrigrade<- ((5)*(fah-32))/9
  return(centrigrade)
}

temp_conversion(12)

add_even <- function(q)
{
  total<-0
  for ( i in 1:q)
  {
    if(i%%2 == 0 )
    {
      total<- total+i
    }
  }
  return(total)
}
add_even

x<--1
if(x>0){
  print("positive number")
}else if(x<0){
  print("negative number")
}else{
  print("ZERO")
}

#repeat
x<-1
repeat{
  print(x)
       x<-x+1
       if(x==6){
         break
       }
}
       
sqr<- seq(1,100,2)
squared<- NULL
for(i in 1:50)
{
  squared[i]<- sqr[i]^2
}

#Assignmant Questions 

compound_interest<- function(P,R,N,Q)    
{
  interest<- P*((1+(R/(Q*100)))^(N*Q))
  return(interest)
}
# Q is frequency of compounding
# N is number of years
# P is principal
# R is rate of interest
calFactorial<- function(N)
{
  if ( N==0){
    return(1)
  }else{
    if(N<0){
    print("does not exist")
  }else{
  product<- 1
  for(i in 1:N){
    product<- product*i
  }
  }
}
return(product)
}

sharperatio<- function(vect)
{
  ratio<- (mean(vect)/sd(vect))*sqrt(252)
  return(ratio)
}
vect<- c(-171.47,37.24,265.2,-393.14,54.65,-183.08,116.95,214.19,356.28,300.74,144.74,-270.43,243.06,188.60,373.49)

sum_any<- function(n)
{
  total<- 0 
  for(r in 1:n)
  {
    for(s in 1:r)
    {
      total<- total+ (s*s)/(10+4*r*r*r)
    }
  }
return(total)
}
# write full function 
quadratic_roots<- function(a,b,c)
{
  
  x1<- (-b+ sqrt(b^2 - 4*a*c))/2*a
  x2<- (-b- sqrt(b^2 - 4*a*c))/2*a
return(c(x1,x2))
}
 
#Plots
#1.
x<- 1:10
y1<- (x*x)- 3*x +2
y2<- (x*x)- 2*x +3
plot(y1)
lines(y1)
lines(y2,col=2)
lines(density(y1),col=4)

#2.
hist(airquality$Ozone,breaks=4)
hist(airquality$Solar.R)
hist(airquality$Wind)
hist(airquality$Temp,freq = F,ylim = c(0,0.05))
lines(density(airquality$Temp),col=6,lwd=4,lty=6)
plot(airquality$Ozone,airquality$Solar.R)
abline(lm(airquality$Solar.R~airquality$Ozone))
cor(airquality$Ozone,airquality$Solar.R)
cor(airquality)
cov(airquality)

#3.
data("painters")
summary(painters)
str(painters)
levels(painters$School)
count<- table(painters$School)
count
barplot(count,main="arplot for Grades", ylab = "Grades", xlab = "Frequency",horiz = T,col=colors()[seq(1,16,2)],space=0,beside = T,width = c(1,0.6,0.7,0.8,0.9,1.0,1.1,4,1,1),axisnames = T,names.arg = c("a","b","c","d","e","f","g","h"))

#5.
par(mfrow=c(4,4))
boxplot(mtcars$wt,mtcars$cyl)
levels(mtcars$cyl)
table(mtcars$cyl)
boxplot(mtcars$gear,mtcars$drat)
stripchart(mtcars$disp)
coplot(mtcars$wt~mtcars$disp|mtcars$am)
#4
par(mfrow=c(1,2))
data_pieVec <- c(6,1,9,4,3,5,2)
pie(data_pieVec,col = colors())
box()
v<- c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","saturday")
legend("right",v,fill = colors())
rm(list = ls())
#6.
plot(BOD$Time,BOD$demand,ylim = c(0,20),main="Demand vs Time",xlab="days",ylab = "BOD",pch=1, col=24)
abline(lm(BOD$demand~BOD$Time),col=2)

#7.
x<- c(seq(0,2*pi,0.01595))
y<- c(seq(0,1,0.01),seq(1.1,0,-0.01),seq(-0.1,-1,-0.01),seq(-0.9,0,0.01))
plot(x,y)
legend("top","sine function")

#8.
S <- c((0.1,0.2,0.5,1,2,5,10,20)*1e-6)
Km <- 2e-6
Vmax <-10
v <- Vmax *S/(Km +S)
par(mfrow=c(2,2))
plot(v,S)

chicago<- read.csv(file.choose(),sep = ",")
library(dplyr)
library(lubridate)
chicago<- arrange(chicago,date)
chicago<- arrange(chicago,desc(date))
chicago<- rename(chicago,dewpoint=dptp)
chicago<- mutate(chicago, pm25trend=pm25tmean2-mean(pm25tmean2,na.rm = T))
chicago<- mutate(chicago, year= year(dmy(date)))li
years<- group_by(chicago,year)
summarise(years , pm25tmean2 =mean(pm25tmean2,na.rm = T))

mutate(chicago,month=month(dmy(date)))%>%
group_by(month)%>%
summarize(month,pt25=mean(pt25tmean2,na.rm=T))

x<- c( 1,2,3,4)
sd(x)
var(x)

rnorm(100,0,1)
plot(density(rnorm(1000000,0,1)))
plot(density(rnorm(1000000,10,0.5)))
