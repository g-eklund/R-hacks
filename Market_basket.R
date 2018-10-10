# Read CSV data file
mydata <-MBA


# Split data
dt <- split(mydata$Products, mydata$ID)

# Loading arules package
if(!require(arules)) install.packages("arules")


dt2 = as(dt,"transactions")


# Most Frequent Items
itemFrequency(dt2, type = "relative")
itemFrequencyPlot(dt2,topN = 5)

# aggregated data
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8))
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8, minlen = 3))
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8, maxlen = 4))

#Convert rules into data frame
rules3 = as(rules, "data.frame")
# write(rules, "C:\\Users\\Deepanshu\\Downloads\\rules.csv", sep=",")

# Show only particular product rules
inspect( subset( rules, subset = rhs %pin% "Product H" ))

# Show the top 10 rules
options(digits=2)
inspect(rules[1:10])

# Get Summary Information
summary(rules)

# Sort by Lift
rules<-sort(rules, by="lift", decreasing=TRUE)

# Remove Unnecessary Rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules[!redundant]
rules<-rules.pruned

#Clean Rules
rules3$rules=gsub("\\{", "", rules3$rules)
rules3$rules=gsub("\\}", "", rules3$rules)
rules3$rules=gsub("\"", "", rules3$rules)

#Split the rule
library(splitstackshape)
Rules4=cSplit(rules3, "rules","=>")
names(Rules4)[names(Rules4) == 'rules_1'] <- 'LHS'
Rules5=cSplit(Rules4, "LHS",",")
Rules6=subset(Rules5, select= -c(rules_2))
names(Rules6)[names(Rules6) == 'rules_3'] <- 'RHS'

# What are customers likely to buy before they purchase "Product A"
rules<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="Product A"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])
plot(rules,method="graph",interactive=TRUE,shading=NA)

library(arulesViz)
#plotting the rules
plot(rules,method="graph",interactive=TRUE,shading=NA)
inspect(rules)
plot(rules,method="graph",control=list(type="items"), interactive=TRUE,shading=NA)
plot(rules,method="paracoord",control=list(reorder=TRUE))
itemFrequencyPlot(dt2, support=0.1,  col=rainbow(14))
image(dt2)
length(dt2)
plot(rules, method = "grouped", control = list(k= 18)) #K=18 due to there is 18 rules
plot(rules, method="grouped")
plot(rules, method = NULL, measure = "support", shading = "lift", interactive = T)
plot(rules, measure = c("support","lift"), shading = "confidence")
image(dt2)

subrules <- rules[quality(rules)$confidence > 0.9]
subrules
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))
plot(rules, method="grouped", control=list(k=18))
subrules2<-head(sort(rules,by="lift"), 10)
subrules2
plot(subrules2, method="graph")
plot(subrules2, method="graph", control=list(type="itemsets"))
plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control=list(reorder=TRUE))
