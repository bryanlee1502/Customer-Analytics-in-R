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
setwd("/Users/lym/Library/Mobile Documents/com~apple~CloudDocs/Winter 2023/MGT 100/R /Section 3 Final/Data")
load("/Users/lym/Library/Mobile Documents/com~apple~CloudDocs/Winter 2023/MGT 100/R /Section 3 Final/Data/final_exam_datasets.RData")

#13 MNL


View(as.data.frame(d3))

n <- nrow(d3)

dat_list <- vector(mode = "list", length = n)
pb <- txtProgressBar(min = 1, max = n, style = 3)


out <- mlogit(choice ~ seat + cargo + eng + price | 0, data = d3)

summary(out)

#14 likelihood ratio

ll_ratio <- function(data, model) {
  N <- nrow(model$probabilities)
  J <- ncol(model$probabilities)
  ll0 <- N * log(1 / J)   # this is our null model for comparison
  ll1 <- as.numeric(model$logLik)   # this is lnL(beta) from slides
  1 - ll1 / ll0
}

ll_ratio(d3, out)

#15

#6 suv, scenario 1

attrib <- list(seat  = c("6", "7", "8"),
               cargo = c("2ft", "3ft"),
               eng   = c("gas", "hyb", "elec"), 
               price = c(30, 35, 40))

newdat1 <- expand.grid(attrib)[c(8, 1, 3, 41, 49, 26), ]

#adding 7th suv, scenario 2

newdat2 <- expand.grid(attrib)[c(8, 8, 1, 3, 41, 49, 26), ]

#predict

pred_mkt_shr <- function(model, data) {
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[ , -1]
  utility <- data.model %*% model$coef
  share <- exp(utility)/sum(exp(utility))
  cbind(share, data)
}

pred_mkt_shr(out, newdat1)

pred_mkt_shr(out, newdat2)

#because the predicted market share is equal between the 7 seat, 2ft, hyb, price 30, SUV, when we 
#predicted the market share for when there is 2 of the same SUV, this assumes IIA

#16



#out1 <- mlogit(choice ~ seat + cargo + eng + price + carpool | 0, data = d3)

#17

#We are omitting a relevant variable that affects the price term, hence,
#it is an omitted variable bias

#18

d3<-d3|> mutate(fold = rep(1:3, each=3000))

fold1 <- d3 |> filter(fold == 1)
fold2 <- d3 |> filter(fold == 2)
fold3 <- d3 |> filter(fold == 3)




fold12 <- rbind(fold1, fold2)
fold13 <- rbind(fold1, fold3)
fold23 <- rbind(fold2, fold3)

out12 <- mlogit(choice ~ seat + cargo + eng + price | 0, data=fold12)
out13 <- mlogit(choice ~ seat + cargo + eng + price | 0, data=fold13)
out23 <- mlogit(choice ~ seat + cargo + eng + price | 0, data=fold23)


pred3<-predict(out12, newdata=fold3)
pred2<-predict(out13, newdata=fold2)
pred1<-predict(out23, newdata=fold1)


pred_3_vec <- as.vector(t(pred3))
pred_2_vec <- as.vector(t(pred2))
pred_1_vec <- as.vector(t(pred1))




#calculate squared error

predicted_prob <- rbind(pred_3_vec,pred_2_vec,pred_1_vec)

choice <- as.data.frame(d3)$choice

squared_error<-(choice-predicted_prob)^2

mean(squared_error)













































































































