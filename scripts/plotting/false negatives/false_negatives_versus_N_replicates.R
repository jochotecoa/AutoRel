library(dplyr)
library(magrittr)
library(ggplot2)

a = rnorm(n = 2000, mean = 1000, sd = 500)
b = rnorm(n = 2000, mean = 1500, sd = 500)

d = c(a, b) %>% data.frame(values = .)
d$groups = c(rep('a', 2000), rep('b', 2000))

ggplot(d, aes(x = values)) +
  geom_density(aes(color = groups))

false_negatives = NULL


for (j in 3:30) {
  print(j)
  ttst_res_3 = data.frame()
  for (i in 1:2000) {
    ttst_res_i = t.test(sample(a, j), sample(b, j)) %>% 
      unlist() %>% 
      as.data.frame() %>% 
      t()
    ttst_res_3 = rbind.data.frame(ttst_res_3, ttst_res_i)
  }
  
  ttst_res_3$p.value %<>% as.numeric()
  hist(ttst_res_3$p.value, breaks = seq(0, 1, 0.05)) %>%  plot()
  false_negative = table(ttst_res_3$p.value >= 0.05) / nrow(ttst_res_3)
  false_negatives = rbind(false_negatives, false_negative)
  rownames(false_negatives)[nrow(false_negatives)] = j
}

false_negatives[false_negatives[,2] == 1] = 0
plot(x = rownames(false_negatives), y = false_negatives[, 2], xlab = 'N replicates', ylab = 'False Negative Ratio')
abline(b = 0, a = 0.05)
