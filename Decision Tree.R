# DecisionTree
install.packages("rpart.plot", dependencies = T)
ir <- iris
head(ir)
str(ir)
library(rpart)
tree <- rpart(Species~., data=ir)
tree
library(rpart.plot)
prp(tree, type = 4, extra = 2) # type=tree type, extra=node ratio(%)
tree$cptable
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt,"CP"]
prune.c <- prune(tree, cp = cp)
plotcp(tree)

