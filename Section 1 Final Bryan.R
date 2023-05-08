#install.packages("") of the packages that u haven't installed
#install.packages("tidyverse")

#install.packages("mlogit")
#install.packages("gridExtra")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("ggthemes")
#install.packages("extrafont")
rm(list=ls())

library(tidyverse)
library(mlogit)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(extrafont)
#stats::filter() #use this possibly
setwd("/Users/lym/Library/Mobile Documents/com~apple~CloudDocs/Winter 2023/MGT 100/R /Section 1 Final/Data")
load("/Users/lym/Library/Mobile Documents/com~apple~CloudDocs/Winter 2023/MGT 100/R /Section 1 Final/Data/final_exam_datasets.RData")

#1
type_sum(d1$product_id) #int
type_sum(d1$category) #fct
type_sum(d1$price1) #dbl
type_sum(d1$has_fakes) #lgl

#2

d1 |> filter(category == "Electronics")   |> ggplot() + geom_histogram(aes(price1), binwidth = 1) + ggtitle("")+
  xlab("")+ylab("")
d1 |> filter(category == "Home Products")   |> ggplot() + geom_histogram(aes(price1), binwidth = 1) + ggtitle("")+
  xlab("")+ylab("")
d1 |> filter(category == "Books")   |> ggplot() + geom_histogram(aes(price1), binwidth = 1) + ggtitle("")+
  xlab("")+ylab("")
d1 |> filter(category == "Toys")   |> ggplot() + geom_histogram(aes(price1), binwidth = 1) + ggtitle("")+
  xlab("")+ylab("")
d1 |> filter(category == "Pet Supplies")   |> ggplot() + geom_histogram(aes(price1), binwidth = 1) + ggtitle("")+
  xlab("")+ylab("")


#or
d1 |>ggplot() +geom_histogram(aes(y=price1)) + facet_wrap(~category)


#3
A<-d1 |> group_by(category) |> summarize(change_in_ratings = mean(ratings2-ratings1))

#4 Visual
#Bar plot

#5 No coding needed.















