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
setwd("/Users/lym/Library/Mobile Documents/com~apple~CloudDocs/Winter 2023/MGT 100/R /Section 2 Final/Data")
load("/Users/lym/Library/Mobile Documents/com~apple~CloudDocs/Winter 2023/MGT 100/R /Section 2 Final/Data/final_exam_datasets.RData")

#7
sub <- d2 %>% select(print_ads, digital_ads, tv_ads, urban_pop)
sub
scl <- sub %>% scale() %>% as_tibble()
scl
scl %>% summarize_all(mean) %>% round(3) 
scl %>% summarize_all(sd) 
out <- kmeans(scl, centers=4, nstart=10)
K <- 4
D <- 10

set.seed(1234)
out <- kmeans(scl, centers=K, nstart=D)
str(out)
sub <- sub %>% mutate(cluster = factor(out$cluster))

sub %>% count(cluster)
out$size

centers <- as_tibble(out$centers) 
centers

SD   <- sub %>% select(print_ads, digital_ads, tv_ads, urban_pop) %>% summarize_all(sd)
MEAN <- sub %>% select(print_ads, digital_ads, tv_ads, urban_pop) %>% summarize_all(mean)
SD
MEAN
SD   <- SD   %>% unlist() %>% rep(K) %>% matrix(nrow=K, ncol=4, byrow=T)
MEAN <- MEAN %>% unlist() %>% rep(K) %>% matrix(nrow=K, ncol=4, byrow=T)

SD
MEAN
centers <- centers*SD + MEAN
round(centers, 1)
res <- vector(length=4)
for(i in 1:4) {
  # run k means
  out <- kmeans(scl, centers=i)
  
  # grab the WSS value, store it in the i'th position of res
  res[i] <- out$tot.withinss
}
ggplot(data.frame(x=1:4, y=res), aes(x,y)) + 
  geom_line(color="grey") + 
  geom_point(size=3) + 
  xlab("Number of Clusters (K)") + 
  ylab("Within-Group Sum of Squares (WSS)") + 
  theme_minimal()

#8

sub <- d2 %>% select(print_ads, digital_ads, tv_ads, urban_pop)
sub
scl <- sub %>% scale() %>% as_tibble()
scl
scl %>% summarize_all(mean) %>% round(3) 
scl %>% summarize_all(sd) 
out <- kmeans(scl, centers=5, nstart=10)
K <- 5
D <- 10

set.seed(1234)
out <- kmeans(scl, centers=K, nstart=D)
str(out)
sub <- sub %>% mutate(cluster = factor(out$cluster))

sub %>% count(cluster)
out$size

centers <- as_tibble(out$centers) 
centers

SD   <- sub %>% select(print_ads, digital_ads, tv_ads, urban_pop) %>% summarize_all(sd)
MEAN <- sub %>% select(print_ads, digital_ads, tv_ads, urban_pop) %>% summarize_all(mean)
SD
MEAN
SD   <- SD   %>% unlist() %>% rep(K) %>% matrix(nrow=K, ncol=4, byrow=T)
MEAN <- MEAN %>% unlist() %>% rep(K) %>% matrix(nrow=K, ncol=4, byrow=T)

SD
MEAN
centers <- centers*SD + MEAN
round(centers, 1)
res <- vector(length=4)
for(i in 1:4) {
  # run k means
  out <- kmeans(scl, centers=i)
  
  # grab the WSS value, store it in the i'th position of res
  res[i] <- out$tot.withinss
}
ggplot(data.frame(x=1:4, y=res), aes(x,y)) + 
  geom_line(color="grey") + 
  geom_point(size=3) + 
  xlab("Number of Clusters (K)") + 
  ylab("Within-Group Sum of Squares (WSS)") + 
  theme_minimal()

#open sub and d2 and see which cluster New York is in. New York is number 32 in d2. Number 32 in sub is cluster 4.
#Massachusetts is 21, 21 is cluster 3.
#Rhode Island is 39, 39 is cluster 3.
#Connecticut is 7, 7 is cluster 3.
#New Jersey is 30, 30 is cluster 3.
#New Hampshire is 29, 29 is cluster 2.
#Vermont is 45, 45 is cluster 2.
#Maryland is 20, 20 is cluster 4.
#Pennsylvania is 38, 38 is cluster 3.
#Answer is Maryland.


#9 We will use the details from question 8 for the plot

ggplot() + 
  geom_point(data=sub,     aes(x=digital_ads, y=urban_pop,  color=cluster)) + 
  geom_point(data=centers, aes(x=digital_ads, y=urban_pop), size=4) + 
  ggtitle("Kmeans cluster membership and centroids") + 
  theme_minimal()

#Because the size of the sample is not large enough (only 50, from 50 states, states are way too broad/generic for kmeans clustering on different ads). 
#Another reason is that print ads, digital ads, and tv ads are not different enough, which would mean that customers from various states might respond just the same to more than one of the ads.

#10 PCA



sub1 <- d2 |> 
  select(state, print_ads, digital_ads, tv_ads, urban_pop) |> 
  arrange(state, print_ads, digital_ads, tv_ads, urban_pop) |> 
  distinct()
sub1


pca_out1 <- sub1 |> ungroup() |> select(print_ads, digital_ads, tv_ads, urban_pop) |> prcomp()


summary(pca_out1)

pcs1 <- as_tibble(pca_out1$x)

ggplot(pcs1, aes(x=PC1, y=PC2)) + 
   
  geom_point(size=2) + 
  geom_hline(yintercept=0) + 
  geom_text_repel(aes(label=d2$state)) + 
  theme_bw()

#11

summary(pca_out1)




































