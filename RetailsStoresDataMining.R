# Install above packages
require(ggplot2)
require(arulesViz)
require(plyr)

setwd("/Users/eirinimitsopoulou/Desktop") # Change directory

groceries <- read.csv(file="GroceriesInitial.csv",header=TRUE,sep=",") # Read data
View(groceries)

### ΑΣΚΗΣΗ 1

#keep only 13 certain products
product_names <- levels(unlist(groceries[,4:35])) 
citrus_fruit <- which(product_names == "citrus fruit")
tropical_fruit <- which(product_names == "tropical fruit")  
whole_milk <- which(product_names == "whole milk") 
other_vegetables <- which(product_names == "other vegetables") 
rolls_buns <- which(product_names == "rolls/buns")  
chocolate <- which(product_names == "chocolate")  
bottled_water <- which(product_names == "bottled water") 
yogurt <- which(product_names == "yogurt")  
sausage <- which(product_names == "sausage") 
root_vegetables <- which(product_names == "root vegetables")  
pastry <- which(product_names == "pastry")  
soda <- which(product_names == "soda") 
cream <- which(product_names == "cream") 

product_names <- product_names[c(citrus_fruit, tropical_fruit, whole_milk, other_vegetables, rolls_buns, chocolate, bottled_water, yogurt, sausage, root_vegetables, pastry, soda, cream)]

# Create binary table
products <- as.data.frame(t(apply(groceries[,4:35],1,  function(x) (product_names) %in% as.character(unlist(x))))) 
names(products) <- product_names 
groceries_binary <- cbind(groceries[,1:3],products) 
View(groceries_binary)

# Discretize continouous variables 
groceries_discrete <- groceries_binary
cut_points <- quantile(groceries_discrete$basket_value, probs = c(0,0.33, 0.66,1), na.rm = TRUE,names = FALSE)
groceries_discrete$basket_value_bin <- cut(groceries_discrete$basket_value,breaks = cut_points,labels=c("Low","Medium","High"),include.lowest = TRUE)
table(groceries_discrete$basket_value_bin)

#Produce binary format of discretized variables
binarize <-function(data_columns,extra_columns=NULL){
  column_names <- levels(unlist(data_columns))
  cat(column_names)
  blank <- which(column_names == "")
  if (length(blank) !=0)
    column_names <- column_names[-c(blank)]
  binary_result <- as.data.frame(t(apply(data_columns,1,  function(x) column_names %in% as.character(unlist(x)))))
  names(binary_result) <- column_names
  if (is.null(extra_columns)==FALSE)
    binary_result<- cbind(extra_columns,binary_result)
  return(binary_result)
}

# Converts basket value to binary format
groceries_discrete <- binarize(as.data.frame(groceries_discrete$basket_value_bin),groceries_discrete) 
groceries_discrete <- groceries_discrete[,-c(which(colnames(groceries_discrete)=="basket_value_bin"))]
View(groceries_discrete)


### ΑΣΚΗΣΗ 2

# α
min <- c(0.001, 0.002, 0.005, 0.01, 0.02) #vector with minimum values for support 
#execute apriori with different values for minimum support
for(m in min) {
  rules <- apriori(groceries_discrete[,4:ncol(groceries_discrete)], parameter = list(minlen=2, supp=m, conf=1))
  rules.sorted <- sort(rules, by="support")
  inspect(rules.sorted)
}


# β
rules <- apriori(groceries_discrete[,4:16],parameter = list(minlen=2, supp=0.001, conf=0.8))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted[1:20])
#plot(rules.sorted[1:20], method="paracoord", control=list(reorder=TRUE))
#plot(rules.sorted[1:20], method="graph", control=list(type="items"))

# γ
rules <- apriori(groceries_discrete[,4:ncol(groceries_discrete)],parameter = list(minlen=2, supp=0.01, conf=1))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted[1:20])
#plot(rules.sorted[1:20], method="paracoord", control=list(reorder=TRUE))
#plot(rules.sorted[1:20], method="graph", control=list(type="items"))

### ΑΣΚΗΣΗ 3

# α
# K-Means
groceries_kmeans <- groceries_discrete[,2:3]
groceries_kmeans$Cluster <- kmeans(scale(groceries_kmeans), 5)$cluster 
# get cluster means
groceries_clusters <- ddply(groceries_kmeans, c("Cluster"), colwise(mean))
groceries_clusters
#plot(groceries_clusters[,2:3])


# β
# number of items in clusters
table(data_kmeans$Cluster)
# Get standard deviation
standard_deviation <- ddply(groceries_kmeans, c("Cluster"), colwise(sd)) 
standard_deviation
#plot(standard_deviation[,2:3])


#γ
# Convertes Clusters to binary format
groceries_discrete$Cluster <- as.character(groceries_kmeans$Cluster)
groceries_discrete <- binarize(as.data.frame(groceries_discrete$Cluster),groceries_discrete) 
groceries_discrete <- groceries_discrete[,-c(which(colnames(groceries_discrete)=="Cluster"))]
View(groceries_discrete)

### ΑΣΚΗΣΗ 4

columns <- c(4:16,20:24)
rules <- apriori(groceries_discrete[,columns], parameter = list(minlen=2, supp=0.01, conf=0.1), 
                 appearance = list(rhs=c("1","2","3","4","5"), default="lhs"))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted[1:20])
#plot(rules.sorted[1:20], method="paracoord", control=list(reorder=TRUE))
#plot(rules.sorted[1:20], method="graph", control=list(type="items"))


