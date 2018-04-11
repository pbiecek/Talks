library("DALEX")
head(apartments)

# create a linear model
apartments_lm_model <- lm(m2.price ~ construction.year + surface + floor + 
                            no.rooms + district, data = apartments)
summary(apartments_lm_model)

# root mean square
predicted_mi2_lm <- predict(apartments_lm_model, apartmentsTest)
sqrt(mean((predicted_mi2_lm - apartmentsTest$m2.price)^2))

# create a random forest model
library("randomForest")
set.seed(59)

apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor + 
                                      no.rooms + district, data = apartments)
apartments_rf_model

# root mean square
predicted_mi2_rf <- predict(apartments_rf_model, apartmentsTest)
sqrt(mean((predicted_mi2_rf - apartmentsTest$m2.price)^2))

# 1. To use DALEX you need an explainer

explainer_lm <- explain(apartments_lm_model, 
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
explainer_lm

explainer_rf <- explain(apartments_rf_model, 
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
explainer_rf

# 2. Model performance

mp_lm <- model_performance(explainer_lm)
mp_lm

mp_rf <- model_performance(explainer_rf)
mp_rf

plot(mp_lm, mp_rf, geom = "boxplot")
plot(mp_lm, mp_rf)

# 3. Variable importance

vi_rf <- variable_importance(explainer_rf, loss_function = loss_root_mean_square)
vi_rf

vi_lm <- variable_importance(explainer_lm, loss_function = loss_root_mean_square)
vi_lm

plot(vi_lm, vi_rf)

# 4. Variable effect
## for construction.year

sv_rf  <- variable_response(explainer_rf, 
            variable =  "construction.year", type = "pdp")
plot(sv_rf)

sv_lm  <- variable_response(explainer_lm, 
            variable =  "construction.year", type = "pdp")
plot(sv_rf, sv_lm)

## for surface

sv_rf  <- variable_response(explainer_rf, 
             variable =  "surface", type = "pdp")
sv_lm  <- variable_response(explainer_lm, 
             variable =  "surface", type = "pdp")
plot(sv_rf, sv_lm)

## for district

svd_rf  <- single_variable(explainer_rf, 
             variable = "district", type = "factor")
svd_lm  <- single_variable(explainer_lm, 
             variable = "district", type = "factor")

plot(svd_rf, svd_lm)

# 5. Outlier detection

mp_rf <- model_performance(explainer_rf)

library("ggplot2")
ggplot(mp_rf, aes(observed, diff)) + geom_point() + 
  xlab("Observed") + ylab("Predicted - Observed") + 
  ggtitle("Diagnostic plot for the random forest model") + theme_mi2()

# 6. break Down

which.min(mp_rf$diff)
## 1161
new_apartment <- apartmentsTest[which.min(mp_rf$diff), ]
new_apartment


new_apartment_rf <- single_prediction(explainer_rf, 
                        observation = new_apartment)
new_apartment_lm <- single_prediction(explainer_lm, 
                        observation = new_apartment)
plot(new_apartment_lm, new_apartment_rf)


# New better model

library("DALEX")
apartments_lm_model_improved <- lm(m2.price ~ 
                       I(construction.year < 1935 | construction.year > 1995) + 
                       surface + floor + 
                       no.rooms + district, data = apartments)

# root mean square
predicted_mi2_lmi <- predict(apartments_lm_model_improved, apartmentsTest)
sqrt(mean((predicted_mi2_lmi - apartmentsTest$m2.price)^2))


