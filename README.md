# ONE-WAY-ANOVA--
attach(antisemitism_07)

# defeine factors
str(antisemitism_07)
liedetection<- as.factor(liedetection)
#label factors
liedetection<- factor(liedetection,c(0,1),labels = c("Without","With"))

# 1. Compute summary statistics by groups - count, mean, sd:
library(dplyr)
group_by(antisemitism_07, suffering) %>%
  summarise(
    count = n(),
    mean = mean(prejudice, na.rm = TRUE),
    sd = sd(prejudice, na.rm = TRUE)
  )
# 1. # get descriptives
library(psych)
## Warning: package 'psych' was built under R version 3.2.2
describeBy(data$score, data$animal, mat=TRUE)





# 2  图
# 2.1 ggboxplot
library("ggpubr")
ggboxplot(antisemitism_07, x = "suffering", y = "prejudice", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2")
          ylab = "Weight", xlab = "Treatment")

# 2.2 library("ggpubr")
ggline(antisemitism_07, x = "suffering", y = "prejudice", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")
# Add error bars: mean_se


# 3   ANOVA
#Tip: our dataset is unbalanced, since the analysis of variance is performed in R by fitting a linear model 
#created from indicator variables for the levels of the factor.  
#so this validity of this approach does not depend on balance in the data. 


#Compute the analysis of variance
res.aov <- aov(weight ~ gender, data = my_data)
#Summary of the analysis
summary(res.aov)


# 4 Tukey
> TukeyHSD(res.aov)
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

# 5 check model assumptions
# 5.1 Homogeneity of variances
plot(res.aov, 1)
#Points 17, 15, 4 are detected as outliers, which can severely affect normality and homogeneity of variance. 
#It can be useful to remove outliers to meet the test assumptions.

#Levene’s test
library(car)
leveneTest(weight ~ group, data = my_data)
#p-value is not less than the significance level of 0.05. 
#This means that there is no evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can assume the homogeneity of variances in the different treatment groups.

# 5.2 Normality  

plot(my_anova1, 2)
#As all the points fall approximately along this reference line, we can assume normality.

#Shapiro-Wilk test on the ANOVA residuals
#Extract the residuals
aov_residuals <- residuals(object = my_anova1)
#Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )


# 6 Multiple comparisons 
#install.packages("multcomp")
# 6.1 
glht(model, lincft)
summary(glht(res.aov, linfct = mcp(group = "Tukey")))

# 6.2
pairwise.t.test(my_data$weight, my_data$gender,
                 p.adjust.method = "BH")

