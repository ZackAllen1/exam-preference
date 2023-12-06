library(dplyr)
library(MASS)
library(ggplot2)

# load data
surveyData <- read.csv("./exam_preferences.csv")

# convert cols to factor
surveyData$EducationStatus <- as.factor(surveyData$EducationStatus)
surveyData$FieldOfStudy <- as.factor(surveyData$FieldOfStudy)
surveyData$IR_Format <- as.factor(surveyData$IR_Format)
surveyData$NR_Format <- as.factor(surveyData$NR_Format)

# make IR/NR negative if format B is selected
# create cip and high/low
surveyData <- surveyData %>% mutate(IR = ifelse(IR_Format == "Format A", IR, -1*IR),
                                    NR = ifelse(NR_Format == "Format A", NR, -1*NR),
                                    cip = NR-IR,
                                    highlow = as.factor(ifelse(EducationStatus %in% c("1st Year Undergraduate",
                                                                                      "2nd Year Undergraduate",
                                                                                      "3rd Year Undergraduate"), "low", "high")))

# Plot cip values for highlow
stripchart(cip ~ highlow, data = surveyData, method = "jitter", col = "black", pch = 16, vertical = TRUE)
overall_mean <- mean(surveyData$cip)
abline(h = overall_mean, col = "red")
group_means <- tapply(surveyData$cip, surveyData$highlow, mean)

# Add blue shorter lines for each group mean
for (i in seq_along(group_means)) {
  segments(i - 0.2, group_means[i], i + 0.2, group_means[i], col = "blue", lty = 2, lwd = 2)
}


# Plot cip values for FieldOfStudy
stripchart(cip ~ FieldOfStudy, data = surveyData, method = "jitter", col = "black", pch = 16, vertical = TRUE)
overall_mean <- mean(surveyData$cip)
abline(h = overall_mean, col = "red")
group_means <- tapply(surveyData$cip, surveyData$FieldOfStudy, mean)

# Add blue shorter lines for each group mean
for (i in seq_along(group_means)) {
  segments(i - 0.2, group_means[i], i + 0.2, group_means[i], col = "blue", lty = 2, lwd = 2)
}



## Single Factor Group Mean Analysis

# plot residuals for cip ~ highlow
par(mar = c(2,4,1,2))
tm <- aov(cip ~ highlow, data=surveyData)
qqnorm(resid(tm))
qqline(resid(tm))
hist(resid(tm))

# repeat for cip ~ FieldOfStudy
par(mar = c(2,4,1,2))
tm2 <- aov(cip ~ FieldOfStudy, data=surveyData)
qqnorm(resid(tm2))
qqline(resid(tm2))
hist(resid(tm2))

# Bartlett Test for Equal Variances
bartlett.test(cip ~ highlow, data = surveyData)
bartlett.test(cip ~ FieldOfStudy, data = surveyData)


# Kruskal-Wallis Test (equal medians)
kruskal.test(cip ~ highlow, data = surveyData)
kruskal.test(cip ~ FieldOfStudy, data = surveyData)


## Unbalanced 2-Factor Study

# create full model m1
options(contrasts=c("contr.sum", "contr.poly"))
m1 <- aov(cip ~ highlow * FieldOfStudy, data=surveyData)
anova(m1)

# Perform Tests for Main and Interaction Effects
drop1(m1, .~., test="F")










