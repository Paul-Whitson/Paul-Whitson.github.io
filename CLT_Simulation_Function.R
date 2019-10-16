#CLT Illustration

#Paul Whitson, University of Chicago, October 2019

#This script defines a function that illustrates the Central Limit Theorem. It is intended for 
#those who are teaching or studying statistics.

#Users may select one of several distribution types and parameters, as well as various sample sizes.
#The function simulates and creates a histogram historgram of the individual values in the parent population
#as well as a histogram of the sample means.  As the sample size increases, the shape of the 
#distribution of the sample means will approach that of a Gaussian / normal distribution

#SampleSize is the number of samples that will be used to calculate EACH sample mean
#NumberOfMeans is the number of sample means that will be calculated

CLTDemonstration <- function(NumIndiv = 10000, DistType = "Normal", DistParam1 = 5, DistParam2 = 0.5, 
                           SampleSize = 10, NumberOfMeans = 1000) {

  #Note that some distributions, like Poisson and Exponential, require only one DistParam value.
  #Note that distribution names ("Normal", "Beta" are capitalized and case-sensitive)
  #Generate individual data points
   
  if(DistType == "Normal") {IndivData <- c(rnorm(NumIndiv, mean = DistParam1, sd = DistParam2))} else
  if(DistType == "Uniform") {IndivData <- c(runif(NumIndiv, min = DistParam1, max = DistParam2))} else
  if(DistType == "Beta") {IndivData <- c(rbeta(NumIndiv, shape1 = DistParam1, shape2 = DistParam2))} else
  if(DistType == "Gamma") {IndivData <- c(rgamma(NumIndiv, shape = DistParam1, rate = DistParam2))} else
  if(DistType == "Exponential") {IndivData <- c(rexp(NumIndiv, rate = DistParam1))} else
  if(DistType == "Poisson") {IndivData <- c(rpois(NumIndiv, lambda = DistParam1))} else
  if(DistType == "Binomial") {IndivData <- c(rbinom(NumIndiv, size = DistParam1, prob = DistParam2))} 
   else {print("Error! Check distribution type.  Only Normal, Uniform, Beta, Gamma, Exponential, Poisson and Binomial are supported")}
 
#Generate "NumberOfMeans" samples of "SampleSize" each from the parent population data and calculate sample means; 
#store sample means in MeanData 
  MeanData <- vector(mode = "numeric", length = NumberOfMeans)
  for(i in 1:NumberOfMeans) MeanData[i] <- mean(SampleData <- c(sample(IndivData, SampleSize, replace = TRUE)))
  
  #plot individuals and sample means
  par(mfrow=c(2,1))
  IndividualHistogram <- hist(IndivData, main = "Histogram of Individual Data Points", sub = paste("Parent Population Distribution = ", DistType))
  #set axis scale of second graph to be same as for first graph
  xMin <- IndividualHistogram$breaks[1]
  xMax <- IndividualHistogram$breaks[length(IndividualHistogram$breaks)]
  hist(MeanData, xlim = c(xMin, xMax), main = "Histogram of Sample Means", sub = paste("Sample Size for Each Mean = ", SampleSize))
}

 #Examples:  To run, uncomment and run each line separately:
 
#CLTDemonstration(NumIndiv = 10000, DistType = "Beta", DistParam1 = 0.5, DistParam2 = 0.5, SampleSize = 100, NumberOfMeans = 1000)

#CLTDemonstration(NumIndiv = 10000, DistType = "Poisson", DistParam1 = 5, SampleSize = 30, NumberOfMeans = 1000) 

#CLTDemonstration(NumIndiv = 10000, DistType = "Gamma", DistParam1 = 10, DistParam2 = 3, SampleSize = 10, NumberOfMeans = 1000) 

