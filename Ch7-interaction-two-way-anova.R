# Load libraries
library(tidyverse)
library(ggplot2)
library(patchwork)

# Main Effect and Interaction effect
Cide<-c(1,1,2,2)
Fert<-c(1,2,1,2)
y<-c(15,25,5,18)

fertpest1<-data.frame(Cide = Cide, Fert = Fert, y=y)

# Main effect
A1<-fertpest1 %>% group_by(Cide) %>%
  summarize(meanY = mean(y)) %>%
  ggplot(aes(Cide, meanY)) +
  geom_point(size = 3) +
  geom_line(aes(group = 1)) +
  geom_point(data=fertpest1, aes(Cide,y))

A2<-fertpest1 %>% group_by(Fert) %>%
  summarize(meanY = mean(y)) %>%
  ggplot(aes(Fert, meanY))+
  geom_point(size = 3)+
  geom_line(aes(group = 1))+
  geom_point(data= fertpest1, aes(Fert, y))

A1+A2 # load pacthwork library to do this

# Interaction
A1<-fertpest1 %>% ggplot(aes(Cide, y, group = Fert)) +
  geom_point(size=3)+
  geom_line()

A2<-fertpest1 %>% ggplot(aes(Fert, y, group = Cide)) +
  geom_point(size=3)+
  geom_line()

A1 + A2

# Example 2
A<-c(1,1,1,1,2,2,2,2)
B<-c(1,1,2,2,1,1,2,2)
Y<-c(5,8,8,10,10,14,3,4)
fertpest2<-data.frame(A=A,B=B,Y=Y)

# Interaction effect
A1=fertpest2 %>% group_by(A) %>%
  summarize(meanY = mean(Y)) %>%
  ggplot(aes(A, meanY)) +
  geom_point(size = 3) +
  geom_line(aes(group = 1)) +
  geom_point(data = fertpest2, aes(A, Y, color = B))

A2=fertpest2 %>% group_by(A, B) %>%
  summarize(meanY = mean(Y)) %>%
  ggplot(aes(A, meanY)) +
  geom_point(size = 3) +
  geom_line(aes(color = B, group = B)) +
  geom_point(data = fertpest2, aes(A, Y, color = B))

A1+A2

