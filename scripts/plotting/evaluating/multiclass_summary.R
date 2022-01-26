final_rpart2 <- data.frame(obs = test_data$significance,
                           pred = predict(model_rpart2, newdata = test_data), 
                           predict(model_rpart2, newdata = test_data,
                                   type = "prob"),
                           row.names = row.names(test_data))

final_kknn <- data.frame(obs = test_data$significance,
                         pred = predict(model_kknn, newdata = test_data), 
                         predict(model_kknn, newdata = test_data,
                                 type = "prob"),
                         row.names = row.names(test_data))

multiClassSummary(final, levels(test_data$significance))
