# Load the libraries
require(arules)
require(arulesViz)

## Read in CSV file to transactions - format basket
mydata<- read.transactions("MB1GOOD.csv" , format = "basket" ,  rm.duplicates = TRUE, sep= "," )

## Create an item frequency plot for the top 20 items
itemFrequencyPlot(mydata,topN=20,type="absolute", col="steelblue")

## Get apriori rules - adjust confidence and support
rules <- apriori(mydata, parameter = list(supp = 0.001, conf = 0.4, minlen=2))

## Sort descending
rules<-sort(rules, by="lift", decreasing=TRUE)

## Summary of rules and groupings
summary(rules)

## Inspect rules
options(digits=2)
inspect(rules[1:20])

## Remove redundancies
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

## Write to CSV file
write(rules, file = "data.csv",  sep = ",")

## Create different visualizations
plot(rules, method="graph", main="", control=list(layout=igraph::with_graphopt()))
plot(rules, method="paracoord")
plot(rules, method="grouped")
