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

#View(D)# Crossing factor

D$mood = c('happy', 'sad')

# Dummy coding
D$mood_happy = ifelse(D$mood == 'happy', 1, 0)  # 1 if mood==happy. 0 otherwise.
#D$mood_sad = ifelse(D$mood == 'sad', 1, 0)  # Same, but we won't be needing this
D$age = D$value + rnorm_fixed(nrow(D), sd = 3)  # Correlated to value
 
#------------------------------------------------------------------------------------#
#a = car::Anova(aov(value ~ mood * group, D), type='II')  # Normal notation. "*" both multiplies and adds main effects
b = car::Anova(aov(value ~ mood + group + mood:group + age, D))  # Identical but more verbose about main effects and interaction

# two-way Ancova for mood (f-value)
full3 = lm(value ~ 1 + group_b + group_c + mood_happy + age, D)  # Full model
null3 = lm(value ~ 1 + group_b + group_c + age, D)   
f_mood = anova(null3, full3) 

# two-way Ancova for group (f-value)
full2 = lm(value ~ 1 + group_b + group_c + mood_happy + age, D)  # Full model
null2 = lm(value ~ 1 + mood_happy + age, D)  # Without interaction
f_group = anova(null2, full2)  # whoop whoop, same F, p, and Dfs

# two-way Ancova for mood:group (f-value)
full = lm(value ~ 1 + group_b + group_c + mood_happy + group_b:mood_happy + group_c:mood_happy + age, D)  # Full model
null = lm(value ~ 1 + group_b + group_c + mood_happy + age, D)  
f_moodgroup = anova(null, full)  

# two-way Ancova for age (f-value)
full4 = lm(value ~ 1 + group_b + group_c + mood_happy + group_b:mood_happy + group_c:mood_happy + age, D)  # Full model
null4 = lm(value ~ 1 + group_b + group_c + mood_happy + group_b:mood_happy + group_c:mood_happy, D)  # Without interaction
f_age = anova(null4, full4)  

# Testing for Regression F-value (null check, where f-value came from?)

overall = lm(value ~ 1 + group_b + group_c + mood_happy + group_b:mood_happy + group_c:mood_happy + age, D)  # Full model
nulll = lm(value ~ 1, D)  # Without interaction
overallt = anova(nulll, overall) 


b
f_mood
f_group
f_age
f_moodgroup
summary(full)
overallt

#regression group_b coeffi
group_b1 = lm(value ~ 1 + group_b + group_c + mood_happy + group_b:mood_happy + group_c:mood_happy + age, D)  # Full model
group_b2 = lm(value ~ 1 + group_c + mood_happy + group_b:mood_happy + group_c:mood_happy + age, D)  # Without interaction
group_b = anova(group_b2 , group_b1)
group_b
