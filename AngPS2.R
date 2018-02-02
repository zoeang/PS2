###Benford's Law; calculating violations
# how do I make r take the significant number?
#xi<-proportion instances/total instances

#Create some toy data to test the loop
a<-c(548,265489,16514,651,864,31,54,231,648,23)
b<-NULL
xi<-NULL


leemis<-function(a){
  #argument a will be a vector of total votes
  b<-as.numeric(substr(a, start=1, stop=1)) #This will take the first signficant digit of each element of the fed vector
  proportions<-NULL
  for (i in 1:9){ #This loop creates x_i, a proportion of occurrence of each significant digit
    y<-sum(b==i)/length(a)
    proportions<-c(proportions,y)
  }
  xi<-proportions
  argument<-NULL
  for(i in 1:9){ #This loop calculates the inside of the max function for Leemis m
    element<-(xi[i]-log((1+1/i), base=10))
    argument<-c(argument,element)
  }
  argmax<-max(argument) #This is m
  return(argmax)
}
leemis(a)




if(leemis()>=1.212){
  print("Significant when $\alpha=0.01")
  if
}