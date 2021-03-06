# Load the libraries
require(arules)
require(arulesViz)

## Read in CSV file to transactions - format basket
mydata<- read.transactions("MB1GOOD.csv" , format = "basket" ,  rm.duplicates = TRUE, sep= "," )

## If file in longform format by BIN - convert to transactions
## trans <- as(split(mydata[,"Category"], mydata[,"BIN"]), "transactions")
## Write all transactions to CSV file
## write(mydata, file = "all_data.csv", sep = ",")

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

## Write rules to CSV file
write(rules, file = "data.csv",  sep = ",")



## Create different visualizations
plot(rules, method="graph", main="", control=list(layout=igraph::with_graphopt()))
plot(rules, method="paracoord")
plot(rules, method="grouped")
## plot(rules,method="graph",interactive=TRUE,shading=NA) 
##In plot.rules(rules, method = "graph", interactive = TRUE, shading = NA) :
##The parameter interactive is deprecated. Use engine='interactive' instead.
plot(rules,method="graph",engine='interactive',shading=NA)
