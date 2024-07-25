setwd("C:\\Users\\Hajar\\Downloads\\Project Data Files Zip File (1)")
Dat <- read.csv('816627_project.csv', header = TRUE)
str(Dat)

M_E <- lm(Y ~ E1+E2+E3+E4, data=Dat)
summary(M_E)
summary(M_E)$adj.r.squared

M_raw <- lm(Y ~ (E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20)^2, data=Dat)
plot(resid(M_raw)~ fitted(M_raw), main ='Residual Plot')

library(MASS)
boxcox(M_raw)

M_trans <- lm(I(Y^2) ~ (E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20)^2, data=Dat)
summary(M_raw)$adj.r.square
summary(M_trans)$adj.r.square

plot(resid(M_trans) ~ fitted(M_trans), main='New Residual Plot')

# Load the leaps library
library(leaps)

# Use the regsubsets() function from leaps to perform forward stepwise variable selection for your regression model
M <- regsubsets( model.matrix(M_trans)[,-1], I((Dat$Y)^2),
                 nbest = 1 , nvmax=5, 
                 method = 'forward', intercept = TRUE )
# Assign the summary of this forward selection stepwise regression object to the variable 'temp'
temp <- summary(M)


# Load the knitr library
library(knitr)

# Assign the names of all predictors and pairwise interactions to the variable 'Var'
Var <- colnames(model.matrix(M_trans))

# Use the apply() function to obtain the proposed models from the regsubsets() function based on stepwise selection
M_select <- apply(temp$which, 1, 
                  function(x) paste0(Var[x], collapse='+'))

# Output the table of these proposed models based on stepwise selection, along with each proposed model's
# adjusted R^2 and BIC
kable(data.frame(cbind( model = M_select, adjR2 = temp$adjr2, BIC = temp$bic)),
      caption='Model Summary')

# Create a regression model which contains all the predictors but with no interaction terms
M_main <- lm( I(Y^2) ~ E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20, data=Dat)
temp <- summary(M_main)

# Create a table that outputs only the terms from the regression model M_main which have p-values less than 0.001
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='Sig Coefficients')

# The lines of code below create and then provide the summary values for the chosen final model
# Note how to include an interaction term in your model you use a colon ( : ) between the two interaction variables
M_final <- lm(I(Y^2) ~ E1 + E2 + E3 + E4, data = Dat)
summary(M_final)
