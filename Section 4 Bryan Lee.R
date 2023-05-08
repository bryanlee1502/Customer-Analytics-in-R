#install.packages("") of the packages that u haven't installed
#install.packages("tidyverse")

#install.packages("mlogit")
#install.packages("gridExtra")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("ggthemes")
#install.packages("extrafont")
#install.packages("ggrepel")
rm(list=ls())

library(tidyverse)
library(mlogit)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(extrafont)
library(ggrepel)
#stats::filter() #use this possibly
setwd("/Users/lym/Library/Mobile Documents/com~apple~CloudDocs/Winter 2023/MGT 100/R /Section 4 Final/Data")
load("/Users/lym/Library/Mobile Documents/com~apple~CloudDocs/Winter 2023/MGT 100/R /Section 4 Final/Data/final_exam_datasets.RData")

#20

d4 <- d4 %>% mutate(A_t = cumsum(NFTAs)-NFTAs)

plot(A_t ~ month, data=d4)

out_lm <- lm(NFTAs ~ A_t + I(A_t^2), data=d4)
coef(out_lm)

# function to convert regression coefficients to M, p, q
bassfun <- function(params) {
  a <- params[1]
  b <- params[2]
  c <- params[3]
  
  p <- (sqrt(b^2-4*a*c) - b)/2
  q <- (sqrt(b^2-4*a*c) + b)/2
  M <- -q/c
  
  res <- c(M, p, q)
  names(res) <- c("M", "p", "q")
  return(res)
}

# get M,p,q
result1 <- bassfun(coef(out_lm))
round(result1, 4)

#21
#q measures the magnitude/degree of people that purchases the solar panels not 
#because they personally want/need it, but because of the environment 
#around them (social factors that influences them to make this purchase). 
#This behavior exists among consumers for at-home solar panels because 
#renewable energy is the trending/rising form of energy for households with 
#lots of subsidies for it in the US, therefore, there must be some households 
#that don't necessarily need it or actually value the technology, but 
#their getting subsidies and their neighbors are probably doing it (so there is also peer/social pressure).

#22
#CBCV is valuing a company using the relationship(retention) 
#and acquisition that a company has relative to it's customers. 
#It is more expensive to get new customers than to retain 
#them (as used in the professor's example during class where he 
#was only navigating to the manage subscription page and not even 
#clicking on cancelling subscription button yet and the website 
#already offers him a discount), therefore, CBCV captures
#the relative importance of getting and most importantly keeping 
#customers whcih is what older approaches fail to do.





















































