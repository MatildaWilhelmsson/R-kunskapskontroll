
##Regressionsmodellering Kunskapskontroll


# Install packages

install.packages("tidyverse")
library(tidyverse)
install.packages("car")
library(car)
install.packages("glmnet")
library(glmnet)
install.packages("glmnetUtils")
library(glmnetUtils)
install.packages("plotmo")
library(plotmo)
install.packages("caret")
library(caret)
install.packages("DAAG")
library(DAAG)
install.packages("rsample")
library(rsample)


# Pipeline: loading data, deleting the NAs, filter price, delete whitespace

car_ads <- read.csv("car_ads_data_02.csv") |> 
  dplyr::filter(Pris >= 20000 & Pris <= 500000) |>
  mutate(across(everything(), str_trim)) |>
  mutate(across(everything(), ~ na_if(.x, "NA"))) |>
  mutate(Miltal = str_replace_all(Miltal, "\\D", "")) |>
  mutate(across(c(Modell, Bränsle, Växellåda, Biltyp, Drivning, Färg, Region), as.factor)) |>
  mutate(across(c(Modellår, Miltal, HK, Motorstorlek, Pris), as.integer)) |>
  mutate(Datum_i_trafik = as.Date(Datum.i.trafik)) |>
  mutate(Ålder = year(Sys.Date())-Modellår) |>
  dplyr::filter(year(Datum_i_trafik) >= 1999) |>
  select(!c(Motorstorlek, Modell, Datum_i_trafik, Datum.i.trafik, Färg)) |>
  na.omit() |>
  as_tibble()

view(car_ads)


# Gruppera märken med färre än 50 datapunkter till ny kategori "Other"

car_ads <- rows_update(
  car_ads, car_ads |>
   group_by(Märke) |>
    mutate(count = n()) |>
    dplyr::filter(count < 50) |>
    mutate(Märke = "Other") |>
    select(-count),
  by = "Id")

car_ads <- car_ads |> mutate(Märke = as.factor(Märke))


# Delar upp datan i Train och Test

set.seed(42)

car_ads <- car_ads |> mutate(row_nbr = row_number())

car_train <- car_ads |> sample_frac(0.7)
car_test <- anti_join(car_ads, car_train, by = "row_nbr")

train <- car_train |> select(-row_nbr, -Id)
test <- car_test |> select(-row_nbr, -Id)


# Creating linear model with Price as y. 

lm_1 <- lm(Pris ~., data = train)


summary(lm_1)
# Adjusted R-squared 0.8043

par(mfrow = c(2, 2))
plot(lm_1)
plotres(lm_1)

ggplot(data = df_train, aes(x = lm_1$residuals)) +
  geom_histogram(bins = 50, fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals lm_1', x = 'Residuals', y = 'Frequency')


# Skapar en Ridgemodell

df_train <- as.data.frame(train)

ctrl <- trainControl(method = "cv", number = 5)
formula <- as.formula("Pris ~ .")
lm_2 <- train(formula, data = df_train, method = "glmnet", trControl = ctrl, 
                     tuneGrid = expand.grid(alpha = 0, lambda = seq(0.01, 1, length = 100)))


# Skapar en Lassomodell

df_train <- as.data.frame(train)

ctrl <- trainControl(method = "cv", number = 5)
formula <- as.formula("Pris ~ .")
lm_3 <- train(formula, data = df_train, method = "glmnet", trControl = ctrl, 
              tuneGrid = expand.grid(alpha = 1, lambda = seq(0.01, 1, length = 100)))


# Testa modellerna på testdata --------------------------------------------------------------

predictions <- predict(lm_1, newdata = test)
pred <- as.data.frame(predictions)
column_to_convert <- test$Pris
testy <- as.numeric(column_to_convert)
rmse <- sqrt(mean((predictions - testy)^2))
print(rmse)


# RMSE lm_1 46093.41

# RMSE lm_2 46789.82

# RMSE lm_3 46106.11





