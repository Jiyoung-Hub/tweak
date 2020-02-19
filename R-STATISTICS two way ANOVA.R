
# two-way ANOVA Test

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

#------------------------------------------------------------------------------------#
# Normal notation. "*" both multiplies and adds main effects
#a = car::Anova(aov(value ~ mood * group, D), type='II')  

# Identical but more verbose about main effects and interaction
model = car::Anova(aov(value ~ mood + group + mood:group, D))  
model

# Anova for 'mood' (f-value)
full3 = lm(value ~ 1 + group_b + group_c + mood_happy, D)  # Full model
null3 = lm(value ~ 1 + group_b + group_c, D)  # Without 'mood'
f_mood = anova(null3, full3)  # same F, p, and Dfs
f_mood

# Anova for group (f-value)
full2 = lm(value ~ 1 + group_b + group_c + mood_happy, D)  # Full model
null2 = lm(value ~ 1 + mood_happy, D)  # Without 'group'
f_group = anova(null2, full2)  # same F, p, and Dfs
f_group

# Anova for mood:group (f-value)
full = lm(value ~ 1 + group_b + group_c + mood_happy + group_b:mood_happy + group_c:mood_happy, D)  # Full model
null = lm(value ~ 1 + group_b + group_c + mood_happy, D)  # Without interaction
f_moodgroup = anova(null, full)  # same F, p, and Dfs
f_moodgroup

# Testing for Regression F-value (null check)
# summary(full)
overall = lm(value ~ 1 + group_b + group_c + mood_happy + group_b:mood_happy + group_c:mood_happy, D)  # Full model
nulll = lm(value ~ 1, D)  # Without interaction
f_overall = anova(nulll, overall) 
f_overall




