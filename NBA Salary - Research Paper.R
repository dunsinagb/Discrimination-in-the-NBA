library(tidyverse)
library(wooldridge)

# load necessary packages for importing the function

install.packages('RCurl')
library(RCurl)

# import the function from repository
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

data(nbasal)

#Select for Data for the Data
nbasal_select <- nbasal
nbasal_select <- nbasal%>% 
  select(wage,lwage, black, guard, forward, center, marr, marrblck, minutes, exper)

#----Creating a NBA Salary Model ----
nbasal_model_standard <- lm(lwage~black + forward + center + marr + marrblck + minutes + exper, data = nbasal_select)
sum1<- summary(nbasal_model_standard)
sum1

# ------ Testing for Heteroskedacity - Alternative White Test ----
nbasal_select$uhat = residuals(nbasal_model_standard)
nbasal_select$uhatsq = (nbasal_select$uhat)^2
nbasal_select$yhat = predict(nbasal_model_standard)
nbasal_select$yhatsq = (nbasal_select$yhat)^2

nbasal_model_AWT <- lm(uhatsq~yhat + yhatsq, data = nbasal_select)
summary(nbasal_model_AWT)

#-----#Solution for Heteroskedacity ----
library(lmtest)
library(sandwich)
nbasal_model_rob <- coeftest(nbasal_model_standard, 
         vcov = vcovHC(nbasal_model_standard, type = "HC1"))
summary(nbasal_model_standard, robust = T)

#---- Better Representation of Regression Summary ----
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(nbasal_model_standard,  
          show.se = TRUE,
          collapse.se = TRUE,
          pred.labels = c("Intercept", "Black", "Position (Forward)", "Position(Center)", "Married", "Married x Black", "Minutes played Per Game",
                          "Years Played Professionally"),
          dv.labels = c("Model on the Wage (Log) - Standard", "Model on the Wage (Log) - Robust"),
          string.se = "SE", 
          show.fstat = TRUE)

library(stargazer)
stargazer(nbasal_model_standard, 
          type = 'html',
          out = 'regression.html', 
          se = c(0.212, .320492))

#---- Sum of Squared Total ----
sse = sum((fitted(nbasal_model_standard)-mean(nbasal$lwage))^2)
ssr = sum((fitted(nbasal_model_standard)- nbasal$lwage)^2)
sst = sse + ssr

r_squared = sse/sst
