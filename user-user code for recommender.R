#inputing the values
recommend <- read.csv(file.choose())
recommend
#inputing the NA values with 0
recommend[is.na(recommend)]<- 0
recommend

#user-user similarity matrix
recommend.similarity<- matrix(NA,nrow=nrow(recommend),ncol = nrow(recommend))
rownames(recommend.similarity)<- recommend[,1]
colnames(recommend.similarity)<- recommend[,1]

#cosine function
cosi<- function(x,y)
{
  # slecting only those values where one of them is not equal to zero
  x1<- x[!y==0]
  w<-x1[!x1==0]
  y1<- y[!x==0]
  u<-y1[!y1==0]
  cosine= sum(w*u)/(sqrt(sum(w*w))*sqrt(sum(u*u)))
  return(cosine)
}


# writing the calculated similarity values into the matrix
for(i in 1:ncol(recommend.similarity))
{
  for(j in 1:nrow(recommend.similarity))
  {
    recommend.similarity[i,j]=cosi(recommend[i,2:ncol(recommend)],recommend[j,2:ncol(recommend)])
  }
}
# using the function to calculate the user specific rating
recom<- function(usernumber, itemnumber)
{
  l<-recommend.similarity[,usernumber]
  q<-recommend[,itemnumber+1]
  r<-l[!q==0]
  recom<- sum(l*q)/sum(r)
  recommend[usernumber,itemnumber+1]<- recom
  return(recommend[usernumber,])
}

recom(3,3)
recom(3,5)
