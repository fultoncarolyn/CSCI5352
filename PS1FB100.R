## Problem Set 1 FB100 data
library(igraph)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(hrbrthemes)

# Extract Data frm .txt files into one dataframe with labeled college
filelist = list.files(path = "./facebook100txt", pattern = "*.txt", full.names = TRUE)
data <- lapply(filelist, function(x)cbind.data.frame(read.delim(x, header=FALSE, sep = "\t"), name=x))
datadf <- as.data.frame(do.call(rbind, data))
names(datadf) <- c('cxn', 'node', 'filepath')

# Sample used for building
## exfilelist = list.files(path = "./facebook100txt", pattern = "*0.txt", full.names = TRUE)
## exdata <- lapply(exfilelist, function(x)cbind.data.frame(read.delim(x, header=FALSE, sep = "\t"), name=x))
## exdatadf <- as.data.frame(do.call(rbind, exdata))
## names(exdatadf) <- c('cxn', 'node', 'filepath')

# Sort for each college their corresponding nodes and compute the degree of each node
# Square values for use in part C
sortdeg <- datadf %>%
  group_by(filepath,node) %>%
  summarise(degreevalue = length(unique(cxn)), sqrdegree = degreevalue^2)

# Compute necessary values from the previous dataframe
avgdeg <- sortdeg %>%
  group_by(filepath) %>%
  summarise(sumdeg = sum(degreevalue), totnode = max(node), meandeg = sumdeg/totnode, sqrsum = sum(sqrdegree), sqrmean = sqrsum/totnode, frndrat = sqrmean/(meandeg)^2)

# Plot histogram of mean degree
p1 <- avgdeg %>%
  ggplot(aes(x=meandeg)) +
  geom_histogram(color="darkblue", fill = "lightblue", binwidth = 5) + 
  #theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Mean Degree") +
  ylab("Number of Schools (Nodes)") +
  ggtitle(paste("Mean Degree Distribution of Facebook100 Networks"))

# Plot scatter plot of friendship paradox ratio
p2 <- avgdeg %>%
  ggplot(aes(x=meandeg, color = filepath)) +
  geom_point(aes(y=frndrat)) + 
  annotate('text', x = c(39.11019,89.05399,116.13174,65.47977,74.32916), y = c(1.820164,1.505527,1.924520,2.815012,2.247126), vjust = 1.5, hjust = 0.6, label = c("Reed", "Colgate", "Mississippi","Virginia","UC Berkeley")) +
  geom_line(aes(y=1), color = "red") +
  #theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Mean Degree") +
  ylab("Ratio of Mean Neighbor Degree and Mean Degree") +
  ggtitle(paste("Friendship Paradox of Facebook100 Networks"))

print(p2)