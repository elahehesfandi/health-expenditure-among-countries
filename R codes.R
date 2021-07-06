#data entry
#data entry
library(readxl)
data <- read_excel("C:/Users/SONY/Documents/unimi.sta/data.xlsx", 
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"))
#data preperation
library(dplyr)
glimpse(data)
head(data)
tail(data)
str(data)
summary(data)
summary(data$`Current_health_expenditure `)
qqnorm(data$`Current_health_expenditure `)

#normality test
shapiro.test(data$`Current_health_expenditure `)
library(ggpubr)
ggqqplot(data$`Current_health_expenditure `)

library(purrr)
library(tidyr)
library(ggplot2)
data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
#######

summary(data$`Current_health_expenditure `)
m1 <-mean(data$`Current_health_expenditure `,na.rm = TRUE)
v1 <- var(data$`Current_health_expenditure `,na.rm = TRUE)
Current_health_expenditure.s <- (data$`Current_health_expenditure `- m1)/sqrt(v1) # standardization
mean(Current_health_expenditure.s,na.rm = TRUE)  
var(Current_health_expenditure.s,na.rm = TRUE)
######

summary(data$Public_Health_expenditure)
m2 <-mean(data$Public_Health_expenditure,na.rm = TRUE)
v2 <- var(data$Public_Health_expenditure,na.rm = TRUE)
Public_Health_expenditure.s <- (data$Public_Health_expenditure - m2)/sqrt(v2) # standardization
mean(Public_Health_expenditure.s,na.rm = TRUE)  
var(Public_Health_expenditure.s,na.rm = TRUE)

######

summary(data$Out_of_pocket_Health_expenditure)
m3 <-mean(data$Out_of_pocket_Health_expenditure,na.rm = TRUE)
v3 <- var(data$Out_of_pocket_Health_expenditure,na.rm = TRUE)
Out_of_pocket_Health_expenditure.s <- (data$Out_of_pocket_Health_expenditure - m3)/sqrt(v3) # standardization
mean(Out_of_pocket_Health_expenditure.s,na.rm = TRUE)  
var(Out_of_pocket_Health_expenditure.s,na.rm = TRUE)

#####

summary(data$External_health_expenditure)
m4 <-mean(data$External_health_expenditure,na.rm = TRUE)
v4 <- var(data$External_health_expenditure,na.rm = TRUE)
External_health_expenditure.s <- (data$External_health_expenditure - m4)/sqrt(v4) # standardization
mean(External_health_expenditure.s,na.rm = TRUE)  
var(External_health_expenditure.s,na.rm = TRUE)

#####

summary(data$rate_of_Physicians)
m5 <-mean(data$rate_of_Physicians,na.rm = TRUE)
v5 <- var(data$rate_of_Physicians,na.rm = TRUE)
rate_of_Physicians.s <- (data$rate_of_Physicians - m5)/sqrt(v5) # standardization
mean(rate_of_Physicians.s,na.rm = TRUE)  
var(rate_of_Physicians.s,na.rm = TRUE)

#####

summary(data$rate_of_Nurses_and_midwives)
m6 <-mean(data$rate_of_Nurses_and_midwives,na.rm = TRUE)
v6 <- var(data$rate_of_Nurses_and_midwives,na.rm = TRUE)
rate_of_Nurses_and_midwives.s <- (data$rate_of_Nurses_and_midwives - m6)/sqrt(v6) # standardization
mean(rate_of_Nurses_and_midwives.s,na.rm = TRUE)  
var(rate_of_Nurses_and_midwives.s,na.rm = TRUE)

#####

summary(data$rate_of_Specialist_surgical_workforce)
m7 <-mean(data$rate_of_Specialist_surgical_workforce,na.rm = TRUE)
v7 <- var(data$rate_of_Specialist_surgical_workforce,na.rm = TRUE)
rate_of_Specialist_surgical_workforce.s <- (data$rate_of_Specialist_surgical_workforce - m7)/sqrt(v7) # standardization
mean(rate_of_Specialist_surgical_workforce.s,na.rm = TRUE)  
var(rate_of_Specialist_surgical_workforce.s,na.rm = TRUE)

#####

summary(data$rate_of_Completeness_of_birth_registration)
m8 <-mean(data$rate_of_Completeness_of_birth_registration,na.rm = TRUE)
v8 <- var(data$rate_of_Completeness_of_birth_registration,na.rm = TRUE)
rate_of_Completeness_of_birth_registration.s <- (data$rate_of_Completeness_of_birth_registration - m8)/sqrt(v8) # standardization
mean(rate_of_Completeness_of_birth_registration.s,na.rm = TRUE)  
var(rate_of_Completeness_of_birth_registration.s,na.rm = TRUE)

#####

summary(data$rate_of_Completeness_of_death_registration)
m9 <-mean(data$rate_of_Completeness_of_death_registration,na.rm = TRUE)
v9 <- var(data$rate_of_Completeness_of_death_registration,na.rm = TRUE)
rate_of_Completeness_of_death_registration.s <- (data$rate_of_Completeness_of_death_registration - m9)/sqrt(v9) # standardization
mean(rate_of_Completeness_of_death_registration.s,na.rm = TRUE)  
var(rate_of_Completeness_of_death_registration.s,na.rm = TRUE)

#####

data.stand <- scale(data[,-1])  # To standarize the variables
data.stand <- as.data.frame(data.stand)
rownames(data.stand) <- data$country

######
library(tidyr)
library(ggplot2)
data.stand %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()#Splitting Data
library(caTools)
set.seed(150)
split = sample.split(data$country, SplitRatio = 0.70)
# Create training and testing sets
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)
dim(train); dim(test)
str(train,)
head(train,)
str(test,)
head(test,)
sapply(train, class)

#Compute the correlation matrix between the variables, which variables are more correlated?
library(tidyverse)
library(dplyr)
mdata<-train %>% select(`Current_health_expenditure `, Public_Health_expenditure,, Out_of_pocket_Health_expenditure,External_health_expenditure)
str(mdata)
head(mdata)
res<-cor(mdata, use = "complete.obs")
round(res, 2)
symnum(res, abbr.colnames = FALSE)
library("Hmisc")
rcorr(as.matrix(mdata))
# change format 
# flattenCorrMatrix
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res1<-rcorr(as.matrix(mdata))
flattenCorrMatrix(res1$r, res1$P)
#Scatter plot matrix
library(car)
scatterplotMatrix(mdata, regLine = TRUE)
library(corrplot)
corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 45)
library("PerformanceAnalytics")
chart.Correlation(mdata, histogram=TRUE, pch=19)

##first model

first.model <- lm(`Current_health_expenditure ` ~., data = mdata)
summary(first.model)

#check the improvement of using logarithm in first model

lm_log.model = lm(log1p(`Current_health_expenditure `) ~ log1p(Public_Health_expenditure)+log1p(Out_of_pocket_Health_expenditure)+log1p(External_health_expenditure), data = mdata)
summary(lm_log.model)



##second model

library(dplyr)
m2data<-train %>% select(`Current_health_expenditure `,rate_of_Completeness_of_birth_registration,rate_of_Completeness_of_death_registration,rate_of_Nurses_and_midwives,rate_of_Physicians,rate_of_Specialist_surgical_workforce)
str(m2data)
head(m2data)
res2<-cor(m2data, use = "complete.obs")
round(res2, 2)
symnum(res, abbr.colnames = FALSE)
library("Hmisc")
rcorr(as.matrix(m2data))
# change format 
# flattenCorrMatrix
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res2<-rcorr(as.matrix(m2data))
flattenCorrMatrix(res2$r, res2$P)
#Scatter plot matrix
library(car)
scatterplotMatrix(m2data, regLine = TRUE)
library(corrplot)
corrplot(res2, type = "upper", 
         tl.col = "black", tl.srt = 45)
library("PerformanceAnalytics")
chart.Correlation(m2data, histogram=TRUE, pch=19)

##second model
second.model <- lm(`Current_health_expenditure `~., data = m2data)
summary(second.model)


#check the improvement of using logarithm in first model
m2data<-na.omit(m2data)
lm_log.model2 = lm(log1p(`Current_health_expenditure `) ~ log1p(rate_of_Completeness_of_birth_registration)+log1p(rate_of_Completeness_of_death_registration)+log1p(rate_of_Nurses_and_midwives)+log1p(rate_of_Physicians)+log1p(rate_of_Specialist_surgical_workforce), data = m2data)
summary(lm_log.model2)

##Check multicollinarity, using Variance Inflaction Factor

vif(lm_log.model2) # variance inflation factors 
sqrt(vif(lm_log.model2)) > 2 # problem?

##apply the steptwise regression approach

library(MASS)

step.model <- stepAIC(lm_log.model2, direction = "both", 
                      trace = FALSE, k=3)
summary(step.model)

par(mfrow=c(2,2))

plot(step.model)


##Check all the diagnostics of the model!
##Collinarity

vif(step.model) # variance inflation factors 
sqrt(vif(step.model)) > 2 # problem?
##Standardized and Studentized Residual Distribution

summary(step.model$residuals)
shapiro.test(step.model$residuals)
ggqqplot(step.model$residuals)
m2data$fit<-step.model$fitted.values
m2data$res<-step.model$residuals
ggdensity(m2data, x = "res", fill = "lightgray", title = "Residuals") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
library(olsrr) 
ols_plot_resid_stud(step.model)
ols_plot_resid_stud_fit(step.model)
ols_plot_resid_lev(step.model)
par(mfrow=c(1,2))
plot(step.model,4)
plot(step.model,5)
library(olsrr) 
ols_plot_cooksd_bar(step.model)

library(tidyverse)
library(broom)
theme_set(theme_classic())
model.diag.metrics <- augment(step.model)
head(model.diag.metrics)
model.diag.metrics %>%
  top_n(5, wt = .cooksd)
ols_plot_dfbetas(step.model)

ols_plot_dffits(step.model)

summary(step.model)


# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(step.model)

library(lmtest)
library(sandwich)
bptest(step.model)
coeftest(step.model, vcov = vcovHC(step.model, "HC1"))  

################################################################################

##Polynomials
library(tidyverse)
library(caret)
theme_set(theme_classic())

ggplot(m2data, aes(rate_of_Specialist_surgical_workforce, `Current_health_expenditure `) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

lm(`Current_health_expenditure `~ +poly(rate_of_Specialist_surgical_workforce, 2, raw = TRUE), data = train) %>%summary()

poly1<-lm(`Current_health_expenditure `~ +poly(rate_of_Specialist_surgical_workforce, 2, raw = TRUE), data = train)
poly1
ggplot(m2data, aes(rate_of_Specialist_surgical_workforce, `Current_health_expenditure `) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))


###test the models
library(mgcv)
library(purrr)
# Make predictions

predictions1 <-predict(lm_log.model, test)
predictions2 <-predict(step.model, test)
predictions3 <-predict(poly1, test)

# Model performance
data.frame(
  RMSE = RMSE(predictions, test$`Current_health_expenditure `),
  R2 = R2(predictions, test$`Current_health_expenditure `)
)

library(modelr)
data.frame(
  R2 = rsquare(lm_log.model, data = mdata),
  RMSE = rmse(lm_log.model, data = mdata),
  MAE = mae(lm_log.model, data = mdata)
)


##Assessing model quality

# Metrics for model 1
glance(lm_log.model) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

# Metrics for model 2
glance(step.model) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

# Metrics for model 3
glance(poly1) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)


library(matlib)
###model 1

predictions1<-na.omit(predictions1)
predictions1t <- as.data.frame(predictions1t)
predictions1t

testf<-test$`Current_health_expenditure `
testf<-na.omit(testf)
testft<- as.data.frame(testf)

confusionMatrix(
  factor(predictions1t, levels = 1:2),
  factor(testft, levels = 1:2)
)
### model 2

predictions2<-na.omit(predictions2)
predictions2t <- as.data.frame(predictions2)
predictions2t

testf<-test$`Current_health_expenditure `
testf<-na.omit(testf)
testft<- as.data.frame(testf)

confusionMatrix(
  factor(predictions2t, levels = 1:2),
  factor(testft, levels = 1:2)
)


### model 3

predictions3<-na.omit(predictions3)
predictions3t <- as.data.frame(predictions3)
predictions3t

testf<-test$`Current_health_expenditure `
testf<-na.omit(testf)
testft<- as.data.frame(testf)

confusionMatrix(
  factor(predictions3t, levels = 1:2),
  factor(testft, levels = 1:2)
)
