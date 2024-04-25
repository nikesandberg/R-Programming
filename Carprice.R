library(readxl)
library(MASS)  
library(leaps)  
library(car)   
library(Metrics)
library(dplyr)
install.packages("fastDummies")
library(fastDummies)

file_path <- "C:/Users/niksan2/Downloads/cardatablocket.xlsx"
car_data <- read_excel(file_path)

View(car_data)

dim(car_data)
head(car_data)
summary(car_data)




spec = c(train = .6, validate = .2, test = .2)

set.seed(123)
g = sample(cut(
  seq(nrow(car_data)), 
  nrow(car_data)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(car_data, g)

cd_train <- res$train
cd_val <- res$validate
cd_test <- res$test

# Creating 3 Models -------------------------------------------------------

# 1. First approach, first model
lm_1 <- lm(Price ~ Mileage + Year, data = cd_train)
summary(lm_1)

par(mfrow = c(2, 2))
plot(lm_1)

vif(lm_1)

# 2. Second approach, starting from the full model
lm_2 <- lm(Price~., data = cd_train)
summary(lm_2)

par(mfrow = c(1, 1))
plot(lm_2)

vif(lm_2)


vif(lm_2, type = "predictor")

?vif

#Removing the Model predictor as it causes multicolinearity

cd_train_filtered <- subset(cd_train, select = -Model)
cd_val_filtered <- subset(cd_val, select = -Model)
cd_test_filtered <- subset(cd_test, select = -Model)
summary(cd_train_filtered)

lm_2 <-lm(Price~., data = cd_train_filtered)

vif(lm_2)

summary(lm_2)

plot(lm_2)


# 3. Third approach: Best subset regression

#Examining the number of coefficent --> number of variables -1 (due to intercept)
num_coef <- length(coef(lm_2))
num_coef

lm_3 <- regsubsets(Price ~., data = cd_train_filtered, nvmax = 29)
lm_3_summary <- summary(lm_5)

lm_3_summary$rsq
lm_3_summary$adjr2

plot(lm_3_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R Square")
which.max(lm_3_summary$adjr2)
points(29, lm_3_summary$adjr2[29], col = "red", cex = 2)
grid()

plot(lm_3_summary$rss, xlab = "Number of Variables", ylab = "RSS")
which.min(lm_3_summary$rss)
points(29, lm_3_summary$rss[29], col = "red", cex = 2)
grid()


#---------- Which variables are the best for a model containing 15 variables?
coef(lm_3, 15)

plot(lm_3, scale = "adjr2")


# Create dummy variables
data <- dummy_cols(data, 
                   select_columns = c("Brand", "Fuel", "Gearbox"), 
                   remove_first_dummy = TRUE)


data_val <- cd_val_filtered
data_val <- dummy_cols(data_val, 
                     select_columns = c("Brand", "Fuel", "Gearbox"), 
                     remove_first_dummy = TRUE)

data_test <- cd_test_filtered
data_test <- dummy_cols(data_test, 
                   select_columns = c("Brand", "Fuel", "Gearbox"), 
                   remove_first_dummy = TRUE)

?dummy_cols

print(data)


#Changing the dummy variable name to Mercedes for simplicity
colnames(data)[15] <- "Mercedes"
colnames(data_val)[14] <- "Mercedes"
colnames(data_test)[14] <- "Mercedes"

data[15]

lm_3 <- lm(Price~ Brand_BMW + Brand_Kia + Mercedes + Brand_MG + Brand_Nissan + Brand_Peugeot + Brand_Renault + Brand_Toyota + Brand_Volvo + Year + Fuel_Diesel + Fuel_El + Fuel_Hybrid + Mileage + Gearbox_Manuell + Year:Mileage, data = data)


plot(lm_3)


summary(lm_3)


##Fixing problem of heteroskedacity
data$Price_log <- log(data$Price)
lm_transformed <- lm(Price_log ~ Brand_BMW + Brand_Kia + Mercedes + Brand_MG + Brand_Nissan + Brand_Peugeot + Brand_Renault + Brand_Toyota + Brand_Volvo + Year + Fuel_Diesel + Fuel_El + Fuel_Hybrid + Mileage + Gearbox_Manuell, data = data)
plot(lm_transformed)
summary(lm_transformed)

vif(lm_transformed)


# Evaluation --------------------------------------------------------------

val_pred_m1 <- predict(lm_1, newdata = cd_val_filtered)
val_pred_m3 <- predict(lm_2, newdata = cd_val_filtered)
val_pred_m6 <- exp(predict(lm_transformed, newdata = data_val))


results <- data.frame(
  Model = c("2 Pred LM", "All Predictors", "BSS 15 Predictors"),
  RMSE_val_data = c(rmse(cd_val_filtered$Price, val_pred_m1),
                    rmse(cd_val_filtered$Price, val_pred_m3), 
                    rmse(data_val$Price, val_pred_m6)),
  Adj_R_squared = c(summary(lm_1)$adj.r.squared,
                    summary(lm_2)$adj.r.squared,
                    summary(lm_transformed)$adj.r.squared),
  BIC = c(BIC(lm_1), BIC(lm_2), BIC(lm_transformed))
)

results

# Evaluating our chosen model (lm_transformed) on the test data.
test_pred_m3 <- exp(predict(lm_transformed, newdata = data_test))
rmse(data_test$Price, test_pred_m3)


# Inference for model 3 ---------------------------------------------------


summary(lm_transformed)  
confint(lm_transformed)

# New data for prediction
new_data <- data.frame(
  Brand = c("Renault", "Volvo", "BMW"),
  Year = c(2016, 2015, 2017),
  Fuel = c("El", "Diesel", "Diesel"), 
  Mileage = c(5590, 20485, 13455),
  Gearbox = c("Automat", "Automat", "Automat"),
  Brand_BMW = c(0, 0, 1), 
  Brand_Kia = c(0, 0, 0),
  Mercedes = c(0, 0, 0), 
  Brand_MG = c(0, 0, 0), 
  Brand_Nissan = c(0, 0, 0), 
  Brand_Peugeot = c(0, 0, 0), 
  Brand_Renault = c(1, 0, 0),
  Brand_Toyota = c(0, 0, 0),
  Brand_Volvo = c(0, 1, 0),
  Fuel_Diesel = c(0, 1, 1),
  Fuel_El = c(1, 0, 0),
  Fuel_Hybrid = c(0, 0, 0),
  Gearbox_Manuell = c(0, 0, 0)
  )


# Create CI & PI for predictions
confidence_intervals <- exp(predict(lm_transformed, newdata = new_data, interval = "confidence", level = 0.95))
prediction_intervals <- exp(predict(lm_transformed, newdata = new_data, interval = "prediction", level = 0.95))


confidence_intervals
prediction_intervals


#True values of the new data
true_values <- c(115000.0, 140000.0, 379900.0)

true_values - exp(predict.lm(lm_transformed, newdata = new_data)) 
