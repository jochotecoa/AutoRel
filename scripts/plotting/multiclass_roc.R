result.roc <- roc(gsub('dubious', 'nonsignificant', as.character(test_data$significance)), final$significant) # Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")                                             

final <- data.frame(obs = test_data$significance,
                    pred = predict(model_kknn, newdata = test_data), 
                    predict(model_kknn, newdata = test_data,
                            type = "prob"),
                    row.names = row.names(test_data))

multiClassSummary(final, levels(test_data$significance))


