concrete_data <- read.csv(file.choose(), header=TRUE, stringsAsFactors = FALSE)
str(concrete_data)
norm_dist <- function(x) { 
  + return((x-min(x)) / (max(x) - min(x))) }
concrete_norm_dist <- as.data.frame(lapply(concrete_data, norm_dist))
summary(concrete_norm_dist$Concrete.compressive.strength.MPa..megapascals..)
#data is correctly correlated for ANN algorithm 
str(concrete_data)
#1030 variables, must split data for machine learning i.e test/train 
concrete_data_train <- concrete_norm_dist[1:773, ]
concrete_data_test <- concrete_norm_dist[774:1030, ]
install.packages("neuralnet")
library(neuralnet)
concrete_ANNmodel <- neuralnet(Concrete.compressive.strength.MPa..megapascals.. ~ Cement..component.1..kg.in.a.m.3.mixture. + 
                                 Blast.Furnace.Slag..component.2..kg.in.a.m.3.mixture. + Water...component.4..kg.in.a.m.3.mixture. + 
                                 Fly.Ash..component.3..kg.in.a.m.3.mixture. + Superplasticizer..component.5..kg.in.a.m.3.mixture. + 
                                 Coarse.Aggregate...component.6..kg.in.a.m.3.mixture. + Fine.Aggregate..component.7..kg.in.a.m.3.mixture. + 
                                 Age..day., data=concrete_data_train)
plot(concrete_ANNmodel)
model_testresults <- compute(concrete_ANNmodel, concrete_data_test[1:8])
strength_prediction <- model_testresults$net.result
cor(strength_prediction, concrete_data_test$Concrete.compressive.strength.MPa..megapascals..)
#strength_prediction = .698, decide to refine model further, expect hidden layers. 
model2 <- neuralnet(Concrete.compressive.strength.MPa..megapascals.. ~ Cement..component.1..kg.in.a.m.3.mixture. + 
                      Blast.Furnace.Slag..component.2..kg.in.a.m.3.mixture. + 
                      Water...component.4..kg.in.a.m.3.mixture. + Water...component.4..kg.in.a.m.3.mixture. + 
                      Fly.Ash..component.3..kg.in.a.m.3.mixture. + Superplasticizer..component.5..kg.in.a.m.3.mixture. + 
                      Coarse.Aggregate...component.6..kg.in.a.m.3.mixture. + Fine.Aggregate..component.7..kg.in.a.m.3.mixture. + 
                      Age..day., data=concrete_data_train, hidden=5)
#5 hidden layers yielded the best results
model2_results <- compute(model2, concrete_data_test[1:8])
strength_prediction2 <- model2_results$net.result 
cor(strength_prediction2, concrete_data_test$Concrete.compressive.strength.MPa..megapascals..)
#cor=.81 


