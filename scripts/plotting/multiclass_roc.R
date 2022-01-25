result.roc <- roc(gsub('dubious', 'nonsignificant', as.character(final$actual)), final$significant) # Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")                                             

