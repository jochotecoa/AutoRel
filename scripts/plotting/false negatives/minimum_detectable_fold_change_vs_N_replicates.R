
res = NULL
for (delta in 1:3100) {
  res = c(res, power.t.test(delta = delta, power = 0.95, sd = 500)$n)
}
a = (1000+(1:3100))/1000

df = data.frame(N_Replicates = res, minimum_detectable_fold_change = a)
plot(df, xlim = c(1, 30))
abline(b = 0, a = 3)
abline(v = 3)
