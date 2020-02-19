set.seed(40)
rnorm_fixed = function(N, mu = 0, sd = 1)
  scale(rnorm(N)) * sd + mu

N = 20  # Number of samples per group
D = data.frame(
  value = c(rnorm_fixed(N, 0), rnorm_fixed(N, 1), rnorm_fixed(N, 0.5)),
  group = rep(c('a', 'b', 'c'), each = N),
  
  # Explicitly add indicator/dummy variables
  # Could also be done using model.matrix(~D$group)
  #group_a = rep(c(1, 0, 0), each=N),  # This is the intercept. No need to code
  group_b = rep(c(0, 1, 0), each = N),
  group_c = rep(c(0, 0, 1), each = N)
)  # N of each level

View(D)# Crossing factor
###############################################################################
# modeling

a = car::Anova(aov(value ~ group, D))  # Dedicated ANOVA function
b = lm(value ~ 1 + group_b + group_c, data = D)  # As in-your-face linear model
summary(b)

test1 = lm(value ~ 1 + group_b + group_c, data=D)
test2 = lm(value ~ 1 + group_c, data=D)
anova(test1,test2)

test3 = lm(value ~ 1 + group_b + group_c, data=D)
test4 = lm(value ~ 1 + group_b, data=D)
anova(test3,test4)

test5 = lm(value ~ 1 + group_b + group_c, data=D)
test6 = lm(value ~ 1, data=D)
anova(test5,test6)
