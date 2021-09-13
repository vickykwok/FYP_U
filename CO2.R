############################################################
#    The R program is aim to built the regression model,   #
#     and compare the performance with the SAS output      #
############################################################
############################## Preparation ####################################

dat=read.table("/Users/yukkeikwok/Desktop/FYP/CO2.txt",
               header=T)
#install.packages("olsrr")
library(olsrr)

####################### Model 1: Interim Model ###########################
Interim=lm(dat$CO2~I(dat$Traffic)+dat$Hour:dat$Wind,dat)

### Normality Checking of residuals
shapiro.test(Interim$res)
### Visual inspection of QQ plot and histogram for normality
ols_plot_resid_qq(Interim)
ols_plot_resid_hist(Interim)
### Generate ANOVA table
anova(Interim)
library(olsrr)
ols_plot_cooksd_chart(Interim)
ols_plot_resid_fit(Interim)
ols_plot_resid_box(Interim)
ols_plot_resid_qq(Interim)
ols_plot_resid_hist(Interim)
ols_plot_resid_lev(Interim)
ols_plot_resid_pot(Interim)
ols_plot_resid_stand(lm6)
ols_plot_resid_stud(Interim)
ols_plot_resid_stud_fit(Interim)
par(mfrow=c(2,2))

############################## Part 1 ####################################

#Fit a multiple linear regression model to the data with linear predictors#
lm1=lm(dat$CO2~.,dat)
summary(lm1)
lm2=step(lm1)
summary(lm2)

### Normality Checking of residuals
shapiro.test(lm2$res)
### Visual inspection of QQ plot and histogram for normality
ols_plot_resid_qq(lm2)
ols_plot_resid_hist(lm2)

### Pairwise QQ plots for response variable and predictors
### To discover some special relationships between variables
### For example, linear relationship, 
### periodic relationships (sine and/or cosine)
pairs(dat)

############################## Part 2 ####################################
### Fit a multiple regression model with nonlinear terms
### in predictors AND harmonics to reflect the periodic
### (sine and/or cosine) relationships for predictors 
lm3=lm(dat$CO2~dat$Traffic+I(dat$Traffic^2)+I(dat$Traffic^3)
       +dat$Wind+dat$Wind^2+dat$Wind^3
       +dat$Wind:dat$Traffic+dat$Hour:dat$Traffic+dat$Wind:dat$Hour
       +sin((2*pi/24)*dat$Hour)+cos((2*pi/24)*dat$Hour)
       +sin((4*pi/24)*dat$Hour)+cos((4*pi/24)*dat$Hour)
       +sin((6*pi/24)*dat$Hour)+cos((6*pi/24)*dat$Hour)
       +sin((8*pi/24)*dat$Hour)+cos((8*pi/24)*dat$Hour)
       +sin((10*pi/24)*dat$Hour)+cos((10*pi/24)*dat$Hour)
       +sin((12*pi/24)*dat$Hour)+cos((12*pi/24)*dat$Hour)
       +sin((14*pi/24)*dat$Hour)+cos((14*pi/24)*dat$Hour)
       +sin((16*pi/24)*dat$Hour)+cos((16*pi/24)*dat$Hour)
       +sin((18*pi/24)*dat$Hour)+cos((18*pi/24)*dat$Hour)
       +sin((20*pi/24)*dat$Hour)+cos((20*pi/24)*dat$Hour)
       +sin((22*pi/24)*dat$Hour)+cos((22*pi/24)*dat$Hour)
       +sin((24*pi/24)*dat$Hour)+cos((24*pi/24)*dat$Hour)
       +sin((26*pi/24)*dat$Hour)+cos((26*pi/24)*dat$Hour)
       +sin((28*pi/24)*dat$Hour)+cos((28*pi/24)*dat$Hour),dat)
lm4=step(lm3)
summary(lm4)
### Generate the ANOVA table
anova(lm4)

############################## Model 2 ####################################
lm5=lm(dat$CO2 ~ I(dat$Traffic^2) + I(dat$Traffic^3)
       + dat$Wind+cos((6*pi/24)*dat$Hour)+sin((6*pi/24)*dat$Hour),dat)
### Normality Checking of residuals
shapiro.test(lm5$res)
### Visual inspection of QQ plot and histogram for normality
ols_plot_resid_qq(lm5)
ols_plot_resid_hist(lm5)
### Generate ANOVA table
anova(lm5)

####################### Model 3: Output from SAS ##########################
lm6=lm(dat$CO2~I(dat$Traffic^2)+I(dat$Traffic^3)
       +I(dat$Wind^2)
       +sin((6*pi/24)*dat$Hour)+cos((6*pi/24)*dat$Hour)
       +cos((8*pi/24)*dat$Hour)
       +cos((12*pi/24)*dat$Hour)
       +sin((16*pi/24)*dat$Hour)+cos((18*pi/24)*dat$Hour),dat)
### Normality Checking of residuals
shapiro.test(lm6$res)
### Visual inspection of QQ plot and histogram for normality
ols_plot_resid_qq(lm6)
ols_plot_resid_hist(lm6)
### Generate ANOVA table
anova(lm6)

#################### Model Comparison ########################
#install.packages("performance")
library(performance)
compare_performance(Interim,lm5,lm6)


library(car)
vif(lm6)
plot(vif(lm6),type = "h")
##baselin 10





