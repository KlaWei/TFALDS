set.seed(20)
#x = seq(-5,5, by=0.02)
curve(pnorm(x), from = -5, to=5)
par(mfrow = c(1,5), mgp=c(1.3,0.5,0), mar=c(5,4,4,2)-1.5)
curve(pt(x, df=1), from = -10, to = 10, ylab = "", main = "df = 1")
curve(pt(x, df=3), from = -10, to = 10, ylab = "", main = "df = 3", col = "blue")
curve(pt(x, df=5), from = -10, to = 10, ylab = "", main = "df = 3", col = "red")
curve(pt(x, df=10), from = -10, to = 10, ylab = "", main = "df = 3", col = "red")
curve(pt(x, df=100), from = -10, to = 10, ylab = "", main = "df = 3", col = "red")

space_palette = c("#005b6e", "#04668c", "#3c6ca7", "#726eb7", "#a86bba", "#da66ac", "#ff6792")
greens = c("#88deb0", "#69c6af", "#4eadaf", "#377a98", "#244a80", "#152069", "#140951")
forest = c("#ffffab", "#b9e49f", "#79c59b", "#3da597", "#00838c", "#00617a", "#003f5f")
sunset = c("#ffff5d", "#ffc447", "#ff8a55", "#f1576c", "#bc377e", "#742e83", "#002a75")
blues_purples = c("#6ec2de", "#79a7e8", "#7e8cf1", "#7e71f9", "#7957fb", "#6448dc")
library(ggplot2)


values = c(1,3,5,10,100)
colours = c(palette("Set2")[2:6]);palette()

ggplot(data.frame(x=c(-10, 10)), aes(x)) + 
  mapply(function(col, df) {
    stat_function(fun = pt, args = list(df = df), colour = col)}, 
    df = values, col = colours) 

colours = c(palette("Okabe-Ito")[2:7])


# Exercise 1

ggplot(data.frame(x=c(-10, 10)), aes(x)) +
  stat_function(fun= pt, args = list(df=1),aes(colour="df = 1"))+
  stat_function(fun= pt, args = list(df=3),aes(colour="df = 3"))+
  stat_function(fun= pt, args = list(df=5),aes(colour="df = 5"))+
  stat_function(fun= pt, args = list(df=10),aes(colour="df = 10"))+
  stat_function(fun= pt, args = list(df=100),aes(colour="df = 100"))+
  stat_function(fun= pnorm,aes(colour="normal"))+
  scale_colour_brewer(palette="Dark2")


# size = 10^6
# k = 1:size
# gk = sqrt(2*log(k))
# 
# sampl = cummax(rnorm(size))
# data4 = cbind(k, gk, 0)
# data4 = rbind(data4, cbind(k, sampl, 1))
# colnames(data4) = c("Argument", "Value", "Type")
# data5 = cbind(k, sampl/gk, 1)
# colnames(data5) = c("Argument", "Value", "Sample")
# 
# x = tail(data5[,c("Argument")], -1)
# y = tail(data5[,c("Value")], -1)
# plot(x, y, type = "l")
# 
# for (i in 2:5)
# {
#   sampl = cummax(rnorm(size))
#   data4 = rbind(data4, cbind(k, sampl, i))
#   data5 = rbind(data5, cbind(k, sampl/gk, i))
# }
# 
# data4 = as.data.frame(data4)
# data5 = as.data.frame(data5)



# Exercise 2

standardized_pchisq = function(x, df) {
  return((pchisq(x, df) - df)/sqrt(2*df))
}

standardized_pchisq = function(x, df) {
  return(pchisq(x*sqrt(2*df) + df, df))
}

ggplot(data.frame(x=c(-10, 20)), aes(x)) +
  stat_function(fun= standardized_pchisq, args = list(df=1),aes(colour="df = 1"))+
  stat_function(fun= standardized_pchisq, args = list(df=3),aes(colour="df = 3"))+
  stat_function(fun= standardized_pchisq, args = list(df=5),aes(colour="df = 5"))+
  stat_function(fun= standardized_pchisq, args = list(df=10),aes(colour="df = 10"))+
  stat_function(fun= standardized_pchisq, args = list(df=100),aes(colour="df = 100"))+
  stat_function(fun= pnorm,aes(colour="normal"))+
  ggtitle("Standardized chi-square dist.") +
  scale_colour_brewer(palette="Dark2")


ggplot(data.frame(x=c(-1, 20)), aes(x)) +
  stat_function(fun= pchisq, args = list(df=1),aes(colour="df = 1"))+
  stat_function(fun= pchisq, args = list(df=3),aes(colour="df = 3"))+
  stat_function(fun= pchisq, args = list(df=5),aes(colour="df = 5"))+
  stat_function(fun= pchisq, args = list(df=10),aes(colour="df = 10"))+
  stat_function(fun= pchisq, args = list(df=100),aes(colour="df = 100"))+
  stat_function(fun= pnorm,aes(colour="normal"))+
  ggtitle("Chi-square dist.") +
  scale_colour_brewer(palette="Dark2")


## Exercise 3 a)
lambda_H0 = 5
n = 100
pois_sample_H0 = rpois(n = n, lambda = lambda_H0)
ppois(sum(pois_sample_H0), lambda = n*lambda_H0, lower.tail = FALSE)

## Exercise 3 b)
get_p_val = function(x, lambda = 5) {
  pois_sample = rpois(n = n, lambda = lambda)
  ppois(sum(pois_sample), lambda = n*lambda_H0, lower.tail = FALSE)
}

get_p_val_needle = function(x, lambda = 5) {
  pois_sample = rpois(n = n, lambda = lambda)
  ppois(sum(pois_sample), lambda = n*lambda_H0, lower.tail = FALSE)
}

p_vals = sapply(1:1000, get_p_val)

# BONFERRONI

# return TRUE if we reject the null
bonferroni = function(p){
  return (min(p) <= alpha/length(p))
}

fishtest = function(p) {
  return (-2*sum(log(p)) > qchisq(1-alpha, 2*length(p)))
}



n=100
n_samples=1000
alpha=0.05

pval = function(x){
  ppois(x, lambda = n*5, lower.tail = FALSE)
}

bonferroni = function(p){
  return (min(p) <= alpha/length(p))
}

fishtest = function(p) {
  return (-2*sum(log(p)) > qchisq(1-alpha, 2*length(p)))
}

# return 1000 p-vals for a single bonf/fisher test
get_p_vals = function(k, lambda) {
  X2 = matrix(rpois(n_samples*n, lambda = lambda), nrow = n, ncol = n_samples)
  X2 = apply(X2, 2, sum)
  pval(X2)
}

Xa2 = sapply(1:k, get_p_vals, lambda = 5)

type_i_err = function(x, k) {
  # 1000 values of a sum statistic in each column
  Xa = matrix(rpois(n_samples*k, lambda = 5*n), n_samples, k)
  Xa = pval(Xa)
  bonf_res = mean(apply(Xa, 2, bonferroni))
  fish_res= mean(apply(Xa, 2, fishtest))
  return(c(bonf_res, fish_res))
}

type_i_err = function(x, k) {
  # 1000 p-values in each column
  Xa = sapply(1:k, get_p_vals, lambda = 5)
  print(dim(Xa))
  bonf_res = mean(apply(Xa, 2, bonferroni))
  fish_res= mean(apply(Xa, 2, fishtest))
  return(c(bonf_res, fish_res))
}

res_type_i_err50 = sapply(1:5, type_i_err, k=50)
res_type_i_err200 = sapply(1:5, type_i_err, k=200)
res_type_i_err500 = sapply(1:5, type_i_err, k=500)
res_type_i_err1000 = sapply(1:5, type_i_err, k=1000)

df_type_i_err_bonf = data.frame(res_type_i_err50[1,], res_type_i_err200[1,],  
                                res_type_i_err500[1,],  res_type_i_err1000[1,])

colnames(df_type_i_err_bonf) = c("k=50", "k=200", "k=500", "k=1000")

df_type_i_err_fish = data.frame(res_type_i_err50[2,], res_type_i_err200[2,],  
                                res_type_i_err500[2,],  res_type_i_err1000[2,])

colnames(df_type_i_err_fish) = c("k=50", "k=200", "k=500", "k=1000")

X_pvals = rpois(n_samples, lambda = 5*n)
X_pvals = pval(X_pvals)
hist(X_pvals)

ggplot(data.frame(X_pvals), aes(x=X_pvals)) + 
  geom_histogram(bins=10, fill ="blue", alpha = 0.6, color = "black")


# ----------------------


# ---------------------------------------------------------------------------------------

n=100
n_samples=1000
alpha=0.05

# return 1000 p-vals for a single bonf/fisher test
get_p_vals_needle = function(k, lambda, needle) {
  # 1 sample in each column
  X2 = matrix(rpois(n_samples*n, lambda = lambda), nrow = n, ncol = n_samples)
  #print(X2)
  # the first sample has E(X) = needle
  X2[,1] = rpois(n, lambda = needle)
  #print(X2)
  X2 = apply(X2, 2, sum)
  X2 = pval(X2)
  #print(X2)
  X2
}

Xa = sapply(1:3, get_p_vals_needle, lambda = 5, needle = 7)
mean(apply(Xa, 2, bonferroni))

needle_fun = function(x, k) {
  # 1000 p-values in each column
  Xa = sapply(1:k, get_p_vals_needle, lambda = 5, needle = 7)
  bonf_res = mean(apply(Xa, 2, bonferroni))
  fish_res= mean(apply(Xa, 2, fishtest))
  return(c(bonf_res, fish_res))
}


res_needle50 = sapply(1:5, needle_fun, k=50)
res_needle200 = sapply(1:5, needle_fun, k=200)
res_needle500 = sapply(1:5, needle_fun, k=500)

df_needle_bonf = data.frame(res_needle50[1,], res_needle200[1,],  
                                res_needle500[1,])

colnames(df_needle_bonf) = c("k=50", "k=200", "k=500")

df_needle_fish = data.frame(res_needle50[2,], res_needle200[2,],  
                                res_needle500[2,])

colnames(df_needle_fish) = c("k=50", "k=200", "k=500")



## Exercise 3 c)

bonf_needle_small_eff_prob = function(x, k) {
  p_vals_M_needle = sapply(1:(k*1000), get_p_val)
  # k p-value lists, each list in a column
  p_vals_M_needle = matrix(p_vals_M_needle, ncol = k)
  p_vals_small_eff = p_vals_M_needle
  # First p-value of each of the p-value lists should be from E(X) = 7
  p_vals_M_needle[1,] = sapply(1:k, get_p_val, lambda = 7)
  p_vals_small_eff[1:100,] = sapply(1:(k*100), get_p_val, lambda = 5.5)
  # perform k bonferroni tests
  bonf_res_needle = apply(p_vals_M_needle, 2, bonferroni, alpha = 0.05)
  bonf_res_small_eff = apply(p_vals_small_eff, 2, bonferroni, alpha = 0.05)
  # empirical prob = number of rejections/total number of trials
  return(list(power_needle = sum(bonf_res_needle)/length(bonf_res_needle),
              power_small_eff = sum(bonf_res_small_eff)/length(bonf_res_needle)))
}


res50_needle_small_eff = unlist(lapply(1:5, bonf_needle_small_eff_prob, k = 50))

get_p_val = function(x, lambda = 5) {
  pois_sample = rpois(n = n, lambda = lambda)
  ppois(sum(pois_sample), lambda = n*lambda_H0, lower.tail = FALSE)
}

p_vals_needle = function(x) {
  n = 100
  n_samples=1000
  # 1000 samples of size 100 
  X = matrix((rpois(n = n*n_samples, lambda = 5)), nrow = 100)
  X[,1] = rpois(n = n, lambda = 7)
  X = apply(X, 2, sum)
  p_vals = ppois(X, lambda = n*5, lower.tail = FALSE)
  p_vals
}

p_vals_small_eff = function(x) {
  n = 100
  n_samples=1000
  # 1000 samples of size 100  100X1000
  X = matrix((rpois(n = n*n_samples, lambda = 5)), nrow = 100)
  X[,1:100] = rpois(n = n*100, lambda = 5.2)
  X = apply(X, 2, sum)
  p_vals = ppois(X, lambda = n*5, lower.tail = FALSE)
  p_vals
}


p_vals_M_needle = lapply(1:800, p_vals_needle)
bonf_res_needle = unlist(lapply(p_vals_M_needle, bonferroni, alpha=.05))
sum(bonf_res_needle)/length(bonf_res_needle)


p_vals_M_small_eff = lapply(1:800, p_vals_small_eff)
bonf_res_se = unlist(lapply(p_vals_M_small_eff, bonferroni, alpha=.05))
sum(bonf_res_se)/length(bonf_res_se)


p_vals_M_needle = lapply(1:800, p_vals_needle)
fish_res_needle = unlist(lapply(p_vals_M_needle, fishtest, alpha=.05))
sum(fish_res_needle)/length(fish_res_needle)


p_vals_M_small_eff = lapply(1:800, p_vals_small_eff)
fish_res_se = unlist(lapply(p_vals_M_small_eff, fishtest, alpha=.05))
sum(fish_res_se)/length(fish_res_se)



###### 

n=10000
n_samples=1000
alpha=0.05

pval = function(x){
  ppois(x, lambda = n*5, lower.tail = FALSE)
}

bonferroni = function(p){
  return (min(p) <= alpha/length(p))
}

fishtest = function(p) {
  return (-2*sum(log(p)) > qchisq(1-alpha, 2*length(p)))
}

type_i_err = function(x, k) {
  Xa = matrix(rpois(n_samples*k, lambda = 5*n), n_samples, k)
  Xa = pval(Xa)
  bonf_res = mean(apply(Xa, 2, bonferroni))
  fish_res= mean(apply(Xa, 2, fishtest))
  return(c(bonf_res, fish_res))
}

res_type_i_err50 = sapply(1:5, type_i_err, k=50)
res_type_i_err200 = sapply(1:5, type_i_err, k=200)
res_type_i_err500 = sapply(1:5, type_i_err, k=500)
res_type_i_err1000 = sapply(1:5, type_i_err, k=5000)

df_type_i_err_bonf = data.frame(res_type_i_err50[1,], res_type_i_err200[1,],  
                                res_type_i_err500[1,],  res_type_i_err1000[1,])

colnames(df_type_i_err_bonf) = c("k=50", "k=200", "k=500", "k=1000")
colnames(df_type_i_err) = c("Bonferroni", "Fisher")

needle = function(x, k) {
  Xa = matrix(rpois(n_samples*k, lambda = 5*n), n_samples, k)
  # First sample comes from E(7) distribution
  Xa[1,] = rpois(k, lambda = 7*n)
  Xa = pval(Xa)
  bonf_res = mean(apply(Xa, 2, bonferroni))
  fish_res= mean(apply(Xa, 2, fishtest))
  return(c(bonf_res, fish_res))
}

small_effects = function(x, k) {
  Xb = matrix(rpois(n_samples*k, lambda = 5*n), n_samples, k)
  Xb[1:100, ] = rpois(100*k, lambda = 5.5*n)
  Xb = pval(Xb)
  bonf_res = mean(apply(Xb, 2, bonferroni))
  fish_res= mean(apply(Xb, 2, fishtest))
  return(c(bonf_res, fish_res))
}


res_se = sapply(1:10, small_effects, k=800)
df_se = data.frame(res_se[1,], res_se[2,])
colnames(df_se) = c("Bonferroni", "Fisher")
df_se


# Exercise 4

size = 10^5
i = 2:size
#cumulative max
nsample_cummax = cummax(abs(rnorm(size-1)))
R = nsample_cummax/sqrt(2*log(i))
df = data.frame(i, R, 1)
colnames(df) = c("i", "R_i", "sample")
ggplot(data = df, aes(x=i, y=R_i)) +
  geom_line(size=1.2, alpha=.8, aes(color = sample)) 


for (k in 2:10) {
  s = cummax(abs(rnorm(size-1)))
  df2 = data.frame(i, s/sqrt(2*log(i)), k)
  colnames(df2) = c("i", "R_i", "sample")
  df = rbind(df, df2)
}

ggplot(data = df, aes(x=i, y=R_i, group=as.factor(sample), color=as.factor(sample))) +
  geom_line(size=0.7, alpha=.8) +
  scale_color_brewer(palette="Spectral")


# Exercise 5

eps = 0.1
n = c(10^3, 10^4, 10^5)

get_l = function(rep, n) {
  equal = 0
  y = rnorm(n)
  c = sqrt(2*log(n))
  gamma = (1+eps)*c
  expr = exp(y*gamma - (gamma^2)/2)
  l = mean(expr)
  l_tilde = mean(expr*(y<c))
  if(l == l_tilde) equal = 1
  return(c(l, l_tilde, equal))
}

calc_and_plot = function(n) {
  rep = 1000
  res = lapply(1:rep, get_l, n = n)
  # matrix with each column equal to (l, l_tilde) (res[1, ] - l, res[2, ] - l_tilde)
  res = matrix(unlist(res), nrow = 3)
  prob = sum(res[3,])/rep
  
  df1 = data.frame(res[1, ]); colnames(df1) = c("val")
  df2 = data.frame(res[2, ]); colnames(df2) = c("val")
  df1 = cbind(df1, "L"); colnames(df1) = c("val", "type")
  df2 = cbind(df2, "L tilde"); colnames(df2) = c("val", "type")
  
  dat = data.frame(rbind(df1, df2))
  p = ggplot(dat, aes(x=val, fill = as.factor(type))) +
    guides(fill = guide_legend(override.aes= list(alpha = 0.4))) +
    scale_x_log10() +
    geom_histogram(data = subset(dat, type == "L"), alpha = 0.5, binwidth =0.3) +
    geom_histogram(data = subset(dat, type == "L tilde"), alpha = 0.5, binwidth = 0.3)  +
    scale_fill_manual(name = "type", values = c("red", "blue"), labels = c("L", "L_tilde")) 
  
  var_vec = apply(res[1:2,], 1, var)
  return(list(vars = var_vec, prob = prob, plot = p))
}

calc_and_plot = function(n) {
  rep = 1000
  res = lapply(1:rep, get_l, n = n)
  # matrix with each column equal to (l, l_tilde) (res[1, ] - l, res[2, ] - l_tilde)
  res = matrix(unlist(res), nrow = 3)
  prob = sum(res[3,])/rep
  
  df1 = data.frame(res[1, ]); colnames(df1) = c("val")
  df2 = data.frame(res[2, ]); colnames(df2) = c("val")
  df1 = cbind(df1, "L"); colnames(df1) = c("val", "type")
  df2 = cbind(df2, "L tilde"); colnames(df2) = c("val", "type")
  
  hist(res[1,])
  hist(res[2, ])
  
  dat = data.frame(rbind(df1, df2))
  p = ggplot(dat, aes(x=val, fill = as.factor(type), color = as.factor(type))) + 
    geom_histogram(data = subset(dat, type == "L"), alpha = 0.5, fill = "red", binwidth =5) +
    theme(legend.position = "bottom")
  
  p2 = ggplot(dat, aes(x=val, fill = as.factor(type),  color = as.factor(type))) + 
    geom_histogram(data = subset(dat, type == "L tilde"), alpha = 0.5, fill = "blue", color = "blue", binwidth =5) 
  
  var_vec = apply(res[1:2,], 1, var)
  return(list(vars = var_vec, prob = prob, plot = p, plotL_tilde = p2))
}

res10_3 = calc_and_plot(1000)
res10_3$plot
res10_3$plotL_tilde
res10_4 = calc_and_plot(10^4)
res10_5 = calc_and_plot(10^5)



vardf = cbind(res10_3$vars, res10_4$vars, res10_5$vars)
colnames(vardf) = c("n = 10^3", "n = 10^4", "n = 10^5")
rownames(vardf) = c("L", "L_tilde")

probdf = cbind(res10_3$prob, res10_4$prob, res10_5$prob)
colnames(probdf) = c("n = 10^3", "n = 10^4", "n = 10^5")


# Exercise 6
bonferroni = function(p, alpha){
  return (min(p) <= alpha/length(p))
}

# critical value for L
L = function(X, eps) {
  p = length(X)
  m = (1+eps)*sqrt(2*log(p))
  mean(exp(X*m - m^2/2))
}

rep = 100
# one sample in each row
M500 = matrix(rnorm(rep*500), nrow = rep)
M5000 = matrix(rnorm(rep*5000), nrow = rep)
M50000 = matrix(rnorm(rep*50000), nrow = rep)

l1.05_1 = sapply(1:rep, function(i) L(M500[i,], 0.05))
l1.05_2 = sapply(1:rep, function(i) L(M5000[i,], 0.05))
l1.05_3 = sapply(1:rep, function(i) L(M50000[i,], 0.05))

l1.2_1 = sapply(1:rep, function(i) L(M500[i,], 0.2))
l1.2_2 = sapply(1:rep, function(i) L(M5000[i,], 0.2))
l1.2_3 = sapply(1:rep, function(i) L(M50000[i,], 0.2))

results2 = data.frame("1.05" = c(quantile(l1.05_1, probs=0.95), 
                                 quantile(l1.05_2, probs=0.95), quantile(l1.05_3, probs=0.95)),
                      "1.2" = c(quantile(l1.2_1, probs=0.95), 
                                quantile(l1.2_2, probs=0.95), quantile(l1.2_3, probs=0.95)),
                      row.names = c("n=500", "n=5000", "n=50000"))

colnames(results2) = c("1.05", "1.2")
results2


# powers of tests

n = c(500, 5000, 50000)
k=800 # number of simulations
alpha=0.05
mi_1 = sapply(n, function(x) 1.05*sqrt(2*log(x))) 
mi_2 = sapply(n, function(x) 1.2*sqrt(2*log(x)))

# p-value for a two-sided alternative
pval_norm = function(x){
  return (2*pnorm(-abs(x)))
}


bonf_power_1.05 = vector(mode = "numeric", length = 3)
bonf_power_1.2 = vector(mode = "numeric", length = 3)

for (i in 1:3) {
  # n values of test statistics for one global test in each column
  X = matrix(rnorm(n[i]*k), nrow = n[i], k)
  X1 = X2 = X
  # first test statistic in each column has a different mean
  X1[1,] = X[i,] + mi_1[i]
  X1 = pval_norm(X1)
  
  X2[1,] = X[i,] + mi_2[i]
  X2 = pval_norm(X2)
  
  bonf_power_1.05[i] = mean(apply(X1, 2, bonferroni, alpha = alpha))
  bonf_power_1.2[i] = mean(apply(X2, 2, bonferroni, alpha = alpha))
}


NP_power = function(k, n, eps, critical_val) {
  m = (1+eps)*sqrt(2*log(n))
  # n values of test statistics in each column
  Z = matrix(rnorm(n*k), nrow = n, ncol = k)
  Z[1, ] = Z[1, ] + m
  mean(sapply(1:k, function(i) L(Z[,i], eps) > critical_val))
}

k=100
NP_powers1.05 = c(NP_power(k, n[1], 0.05, l1.05_1), NP_power(k, n[2], 0.05, l1.05_2), 
                  NP_power(k, n[3], 0.05, l1.05_3))
NP_powers1.05


NP_powers1.2 = c(NP_power(k, n[1], 0.2, l1.2_1), NP_power(k, n[2], 0.2, l1.2_2), 
                  NP_power(k, n[3], 0.2, l1.2_3))
NP_powers1.2

rownames(results3) = c("p=5000", "p=50000")
colnames(results3) = c("NP 1.2", "NP 0.8", "Bonf 1.2", "Bonf 0.8")

res = cbind(NP_powers1.05, NP_powers1.2, bonf_power_1.05, bonf_power_1.2)
colnames(res) = c("NP 1.05", "NP 1.2", "Bonf 1.05", "Bonf 1.2")







