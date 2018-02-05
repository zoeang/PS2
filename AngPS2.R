###Benford's Law; calculating violations


#Create some toy data to test the loop
#a<-c(548,265489,16514,651,864,31,54,231,648,23)
a<-c(10:13)
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
  xi
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
digitdistribution<-table(as.numeric(substr(a, start=1, stop=1)))
#Final Output
  output <- list(output1, output2, digitdistribution)
  names(output)<-c("Leemis' m", "Cho-Gains' d", "Significant Digit Distribution")
  return(output)
}

Parti(c(1:100), FALSE, TRUE)
##################################################################

#####Question 2
print.benfords<-function(a,m,d){
L<-Parti(a, m, d)[[1]] 
D<-Parti(a, m, d)[[2]] 

L1=NULL
LM<-(if(L>=.851 & L<.967){
  L1<-paste(round(L,3),"*", sep="")
} else if (L>=.967 & L<1.212){
  L1<-paste(round(L,3),"**", sep="")
} else if (L>=1.212){
  L1<-paste(round(L,3),"***", sep="")
} else {
    print(round(L,3)) #this is printing L
  })

D1=NULL
CGD<-(if(D>=1.212 & D<1.330){
  D1<-paste(round(D,3),"*", sep="")
} else if (D>=1.330 & L<1.569){
  D1<-paste(round(L,3),"**", sep="")
} else if (D>=1.569){
  D1<-paste(round(D,3),"***", sep="")
} else {
  print(round(D,3)) #This is printing D
})

Stattable<-matrix(c(LM,CGD),byrow = T)
row.names(Stattable)<-c("Leemis' m", "Cho-Gains' d")
colnames(Stattable)<-c("Result")
Significance<-"* p<.10; ** p<.05; *** p<.01"
print(Stattable)
cat(Significance)

}
print.benfords(c(1:100), T,T)
###########
exportfun <- function(a,m,d){
  sink(file="Benfords.csv")
  print.benfords(a,m,d)
  sink()
}###THis will not work because the arguments cannot be specified

exportfun

sink()
print.benfords(c(1:50), T,T)
getwd()

rbind(Stattable, Significance)
