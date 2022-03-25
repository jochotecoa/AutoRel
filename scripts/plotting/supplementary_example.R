a = rnorm(n = 2000, mean = 1000, sd = 500)
b = rnorm(n = 2000, mean = 1500, sd = 500)

d = c(a, b) %>% data.frame(values = .)
d$groups = c(rep('a', 2000), rep('b', 2000))

ggplot(d, aes(x = values)) +
  geom_density(aes(color = groups))

ttst_res_3 = data.frame()

for (i in 1:2000) {
  ttst_res_i = t.test(sample(a, 3), sample(b, 3)) %>% 
                        unlist() %>% 
                        as.data.frame() %>% 
                        t()
  ttst_res_3 = rbind.data.frame(ttst_res_3, ttst_res_i)
}

ttst_res_3$p.value %<>% as.numeric()
hist(ttst_res_3$p.value, breaks = seq(0, 1, 0.05))
table(ttst_res_3$p.value >= 0.05) / nrow(ttst_res_3)


ttst_res_9 = data.frame()

for (i in 1:2000) {
  ttst_res_i = t.test(sample(a, 9), sample(b, 9)) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    t()
  ttst_res_9 = rbind.data.frame(ttst_res_9, ttst_res_i)
}

ttst_res_9$p.value %<>% as.numeric()
hist(ttst_res_9$p.value, breaks = seq(0, 1, 0.05))
table(ttst_res_9$p.value >= 0.05) / nrow(ttst_res_9)


ttst_res_21 = data.frame()

for (i in 1:2000) {
  ttst_res_i = t.test(sample(a, 21), sample(b, 21)) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    t()
  ttst_res_21 = rbind.data.frame(ttst_res_21, ttst_res_i)
}

ttst_res_21$p.value %<>% as.numeric()
hist(ttst_res_21$p.value, breaks = seq(0, 1, 0.05))
table(ttst_res_21$p.value >= 0.05) / nrow(ttst_res_21)

