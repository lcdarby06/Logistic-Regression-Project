## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.


## reading in the data 
NH11 <- readRDS("NatHealth2011.rds")

## checking out structure
str(NH11)

## checking to see which variables have N/As; it appears that the dependent variable everwrk does
apply(NH11, 2, function(x) any(is.na(x)))

## dealing with missing values in everwrk
pmiss <- function(x){sum(is.na(x))/length(x)*100}
apply(NH11, 2, pmiss)


## subsetting the data to remove N/As
NH11_new <- subset(NH11, !is.na(everwrk))

## estimating logistic regression model
mod1 <- glm(everwrk~age_p + r_maritl, data = NH11_new, family = "binomial")

## displaying model results
coef(summary(mod1))

## making results more interpretable
mod1.table <- coef(summary(mod1))
mod1.table[, "Estimate"] <- exp(coef(mod1))
mod1.table

## 2. Predict the probability of working for each level of marital status
plot(allEffects(mod1))
allEffects(mod1)

## for married - spouse in household p = 0.1093, married - spouse not in household p = 0.1176, widowed p = 0.1962, divorced p = 0.0558, separated p = 0.0986, never married p = 0.1491, living w/partner p = 0.0729, unknown marital status p = 0.1742

