# Loading the dataset
mbcd <- read.csv("mbcd.csv", header = TRUE)
attach(mbcd)

# Two-way ANOVA with interaction
mbcd.anova <- aov(Value ~ as.factor(Group) * as.factor(Species))

# Testing for normality
# shapiro.test(mbcd.anova$residuals)

# 	Shapiro-Wilk normality test

# data:  mbcd.anova$residuals
# W = 0.89811, p-value = 0.01964

# ANOVA normality assumption not met, using a non-parametric test
kruskal.test(Value ~ Group)

# 	Kruskal-Wallis rank sum test

# data:  Value by as.factor(Group)
# Kruskal-Wallis chi-squared = 13.42, df = 3, p-value = 0.003811

# Post-hoc test
pairwise.wilcox.test(Value, Group, exact=F)

# 	Pairwise comparisons using Wilcoxon rank sum test 

# data:  Value and Group 

#     0.25 0.5  1   
# 0.5 0.81 -    -   
# 1   0.38 0.38 -   
# 2   0.03 0.03 0.18

# P value adjustment method: holm 
