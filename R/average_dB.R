#################################
##### average_dB            #####
##### By: Marloes Eeftens   #####
##### Last edit: 21/12/2017 #####
#################################

#Function average_dB:
average_dB=function(x,vars){

  #0) Set defaults & parameters:
  if(missing(x)){stop("Please specify a dataframe or numeric vector for x...")}
  if(is.data.frame(x)&missing(vars)){stop("Please specify which columns within the dataframe should be averaged...")}

  #1) Average variables indicated for averaging:
  if(is.data.frame(x)){
    x_avg<-c()
    for(i in vars){
      #Remove and missing values if present
      x_new<-x[which(names(x)==i)]
      x_new<-x_new[!is.na(x_new)]
      #Calculate a logarithmic mean of the sounds levels
      xi_avg<-10*log10((1/length(x_new))*sum(10^(0.1*x_new)))
      names(xi_avg)<-i
      x_avg<-c(x_avg,xi_avg)
    }
  }
  if(is.vector(x)){
    #Remove and missing values if present
    x_new<-x[!is.na(x)]
    #Calculate a logarithmic mean of the sounds levels
    x_avg<-10*log10((1/length(x_new))*sum(10^(0.1*x_new)))
    }

  #2) Return converted data:
  return(x_avg)
  }
