library(ggplot2)
library(dplyr)
library(tidypaleo)
library(patchwork)
library(grid)
library(ggthemes)
library(ggh4x)


df <- data.frame(z=c(rep("R-squared",15),rep("p",15),rep("beta",15)),
                 x=c(runif(15),exp(-runif(15,1,10)),rnorm(15,1,0.5)),
                 y=rep(letters[1:15],3))

p1 <- ggplot(filter(df, z == "beta"), aes(x, y)) +
  geom_point()+
  theme_tufte()+
  theme(axis.line = element_line(colour = "black"),
        plot.margin = margin(0, 0, 0, 0, "cm"))+
  guides(x = "axis_truncated", y = "axis_truncated")

p2 <- ggplot(filter(df, z == "p"), aes(x, y)) +
  geom_point(colour = 'red') +
  scale_x_reverse(position = 'top') +
  theme_tufte()+
  guides(x = "axis_truncated")+
  theme(axis.title.y = element_blank(), 
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(), plot.margin = margin(0, 0, 0, 0, "cm"), 
        axis.line.x = element_line(colour = "black"))

p3 <- ggplot(filter(df, z == "R-squared"), aes(x, y)) +
  scale_x_continuous(limits = c(0,1))+
  geom_point() +
  theme_tufte()+
  guides(x = "axis_truncated")+
  theme(axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(), plot.margin = margin(0, 0, 0, 0, "cm"), 
        axis.line.x = element_line(colour = "black"))

p1 + p2 + p3
grid.draw(linesGrob(x = unit(c(0.06, 0.98), "npc"), y = unit(c(0.277, 0.277), "npc")))
