rm(list=ls())
library(tidyverse)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)

groceries = read.transactions("groceries.txt", format = "basket", sep = ",")

# Now run the 'apriori' algorithm
# Look at rules with support > .005 & confidence >.1 & length (# items) <= 5
groc = apriori(groceries, parameter=list(support=.005, confidence=.1, maxlen=5))
# warning is fine
inspect(groc) #rules

str(groc)
str(groceries)
summary(groc)

## Choose a subset
inspect(subset(groc, subset=lift > 4))
inspect(subset(groc, subset=confidence > 0.6))
inspect(subset(groc, subset=lift > 3.5 & confidence > 0.3))
#subset on lift and confidence
#stricter constraints, fewer rules you get

# plot all the rules in (support, confidence) space
# notice that high lift rules tend to have low support
plot(groc)

# can swap the axes and color scales
plot(groc, measure = c("support", "lift"), shading = "confidence")

# "two key" plot: coloring is by size (order) of item set
plot(groc, method='two-key plot')

# can now look at subsets driven by the plot
inspect(subset(groc, support > 0.035))
inspect(subset(groc, confidence > 0.6))
# no formal way to describe thresholds, figure on lift and support
# if you only want a few rules, be strict
# if you want a dense graph, be lax about thresholds

# graph-based visualization
sub1 = subset(groc, subset=confidence > 0.03 & support > 0.03)
summary(sub1)
plot(sub1, method='graph')

plot(head(sub1, 100, by='lift'), method='graph')

# export set of decision rules 
saveAsGraph(head(groc, n = 100, by = "lift"), file = "groc.s:0.03.graphml")
