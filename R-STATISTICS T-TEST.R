
# T-TEST and POWER T-TEST

# read and look at the data
setwd("C:\\Users\\hahas\\Desktop\\BA")
data <- read.csv("BMI.CSV")
View(data)
summary(data$BMI)

# Let's check randomization
t.test(gender~magicpill,data=data)
t.test(height~magicpill,data=data)
t.test(weightmagicpill~magicpill,data=data)

#using t-test
t.test(BMI~magicpill, data=data)
lm(BMI ~ 1+I(data$magicpill==0), data=data) #the same result

# gender t-test
data_gender0 <- data[data$gender==0,]
data_gender1 <- data[data$gender==1,]
t.test(BMI~magicpill, data=data_gender0)
t.test(BMI~magicpill, data=data_gender1)

b = lm(BMI ~ gender+weightmagicpill+ height+gender*weightmagicpill+height*gender+weightmagicpill*height, data=data)
summary(b)

#sandy model vs my model
lm(BMI ~ gender + height + height*gender) # t-test + lm regression
lm(BMI ~ gender, data=data) # t-test (a special form of lm regression)

#test
a = lm(BMI ~ gender, data = data)
t.test(BMI ~ gender, data=data)
summary(a)

# Power_t_test
library(MESS)
summary(data$BMI)
power_t_test(n=200,type=c("two.sample"),alternative="two.sided",power=0.8,sig.level=0.02,delta=NULL)
power_t_test(n=NULL,type=c("two.sample"),alternative="two.sided",power=0.8,sig.level=0.02,delta=0.08)

# add a BMI prior and BMI difference
data$BMI_prior <- data$weightmagicpill*703/data$height/data$height
data$BMI_diff <- (data$BMI-data$BMI_prior)/data$BMI_prior*100
t.test(BMI_diff~magicpill, data=data)

"""

1. Evaluate its expected validity in terms of experimental design 
* Randomization checks: To check if the two groups have different features, I tested the statistical difference for gender, height,
  and weight_magic_pill. Each p-value was 0.5484, 0.7598 and 3.509e-05 each. Except 'weight_magic_pill' feature, 
  the groups seem to be randomized properly.
* Potential Issues: We might not be able to say that there is a causal effect with current experiment. First, there might be omitted 
  variables. Even though we can verify that the treated group shows decreased BMI, that can be due to other external factors. Also, 
  there might be the selection issue. It is possible that the treated group has more people who have lower BMI in the first place. 
  These issues can be threats to causal inference. For example, if we assume that 'weight_magic_pill' is the weight of each participant 
  before the experiment, the p-value is 0.50. There is no difference between treatment and control group in terms of a percent decrease 
  of BMI before and after the treatment. 

2. Did the pill work?
  If we run the t-test for BMI of two sample groups, p-value is 5.482e-05, which means that we can reject the null hypothesis. In this
  case, it means that the difference in BMI mean of two groups is not zero; thus, implying that treated and control group have different
  BMI mean and it is statistically significant. However, we can't jump into the conclusion that the pill worked in real since we do not 
  know the prior BMI distribution of each group as well as valid control information. 

3. Is there a different effect on men vs women? Would you recommend marketing the pill to both?
  Within gender group 0: the mean difference between magic pill treatment is 2.88 and p-value = 0.01617. The difference is statistically 
  significant at alpha 5% within the group. The treated group has lower BMI by 2.88 
  
  Within gender group 1: the mean difference between magic pill treatment is 5.21 and p-value = 0.001632. The difference is statistically
  significant at alpha 5% within the group. The treated group has lower BMI by 5.21 
  However, we can't say that this difference is derived by taking the magic pill as mentioned above. Under the assumption that the BMI at
  the end of the trial represents the effect of the pill, I would recommend marketing the pill to both as both gender group showed a 
  decrease in BMI at a statistically significant level. 

4. FDA will only consider a pill "effective" if you can show 8% improvement with 98% confidence. Will this experiment be enough or will another one be needed?
  Another experiment will be needed. If we want to show 8% improvement with 98% confidence, we need to have randomized two groups, before
  and after BMI, and external factor information. However, we lack the information about before and after BMI and external factor controls.
  Therefore, we need to do this experiment again. In terms of the sample size, according to the power test, we need 3137.612 sample size to 
  ensure 8% improvement with 98% confidence. According to current setting, we can detect 0.31 difference, so the sample size is not sufficient. 
  But in general, we need to not only collect more participants but redesign the experiment.

"""
