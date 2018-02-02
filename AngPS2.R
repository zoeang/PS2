###Benford's Law; calculating violations
# how do I make r take the significant number?
#xi<-proportion instances/total instances

#Create some toy data to test the loop
a<-c(548,265489,16514,651,864,31,54,231,648,23)
b<-NULL
xi<-NULL

###Leemis
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

###Cho-Gains
#argument a will be a vector of total votes
ChoGains<-function(a){
  b<-as.numeric(substr(a, start=1, stop=1)) #This will take the first signficant digit of each element of the fed vector
  proportions<-NULL
  for (i in 1:9){ #This loop creates x_i, a proportion of occurrence of each significant digit
    y<-sum(b==i)/length(a)
    proportions<-c(proportions,y)
  }
  xi<-proportions
  elemsum<-NULL
  for (i in 1:9){ #This loop will calculate ChoGains' d
    element<-((xi[i]-log((1+1/i), base=10))^2)
    elemsum<-c(elemsum,element)
    elemsumvec<-sum(elemsum)
    d<-sqrt(elemsumvec)
  }
return(d)
}

ChoGains(a)


##################################################################
#### Function that does the things

Parti<-function(a,m,d){
  #a is the vector
  #if m is true, Leemis m will be calculated; if false, it won't
  #if d is true, Cho Gains will be calculated; if false, it won't
  #argument a will be a vector of total votes
  b<-as.numeric(substr(a, start=1, stop=1)) #This will take the first signficant digit of each element of the fed vector
  proportions<-NULL
  for(i in 1:9){ #This loop creates x_i, a proportion of occurrence of each significant digit
    y<-sum(b==i)/length(a)
    proportions<-c(proportions,y)
  }
  xi<-proportions
#Leemis
  if(m==T){
      argument<-NULL
      for(i in 1:9){ #This loop calculates the inside of the max function for Leemis m
        element<-(xi[i]-log((1+1/i), base=10))
        argument<-c(argument,element)
      }
      output1<-max(argument) #This is m
  } else{
    output1<-NA
  }
#Cho Gain's
  if(d==T){
  elemsum<-NULL
  for (i in 1:9){ #This loop will calculate ChoGains' d
    element<-((xi[i]-log((1+1/i), base=10))^2)
    elemsum<-c(elemsum,element)
    elemsumvec<-sum(elemsum)
    d<-sqrt(elemsumvec)
    }
    output2<-d
  } else{
    output2<-NA
  }
#distribution
digitdistribution<-table(xi)
#Final Output
  output <- list(output1, output2, digitdistribution)
  names(output)<-c("Leemis' m", "Cho-Gains' d", "Digit Distribution")
  return(output)
}

Parti(c(1:100), FALSE, TRUE)
##################################################################

#####
if(leemis()>=1.212){
  print("Significant when $\alpha=0.01")
  if
}