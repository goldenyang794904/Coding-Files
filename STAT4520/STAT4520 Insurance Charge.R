library(ggplot2)
library(lme4)
library(dplyr)
library(gt)
library(tidyr)
library(pbkrtest)
library(RLRsim)

set.seed(123)

data<-read.csv("/Users/antonyang/Downloads/Past Courses Note/STAT4520/medical_insurance.csv")

data <- data %>%
  mutate(across(c(sex, smoker, children, region), as.factor))

explanatory_variables<-data.frame(
  Variables = c("age", "sex", "bmi", "children", "smoker", "region", "charges"),
  Descriptions = c("Age of the person", "Gender of the person", "Body Mass Index", "Number of children", "Smoker or Non-smoker", "Region like northeast, northwest, southeast, southwest", "Yearly insurance price")
)

explanatory_variables%>%
  gt()%>%
  tab_header("Explanatory Variables")

ggplot(data, aes(x = charges))+
  geom_histogram(binwidth = 1000, fill = "skyblue", alpha = 0.7)+
  labs(
    title = "Distribution of Insurance Price",
    x = "Insurance Price"
  )+
  theme_minimal()

data_long <- data %>%
  pivot_longer(
    cols = -c(charges, age, bmi), 
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(data_long, aes(x = Value, y = charges)) +
  geom_boxplot(fill = 4) +  
  facet_wrap(~ Variable, scales = "free_x") +  
  labs(
    title = "Boxplot of Categorical Variables",
    x = "Value of Variable",
    y = "Yearly Insurance Prices"
  ) +
  theme_minimal()

ggplot(data, aes(x = charges)) +
  geom_histogram(binwidth = 1000, alpha = 0.7, fill = "skyblue") +
  facet_wrap(~ smoker, scales = "free_y") +  
  labs(
    title = "Distribution of Insurance Prices by Smoking Status",
    x = "Insurance Price",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 12)  
  )

ggplot(data, aes(x = charges)) +
  geom_histogram(binwidth = 1000, alpha = 0.7, fill = "skyblue") +
  facet_wrap(~ region, scales = "free_y") +  
  labs(
    title = "Distribution of Insurance Prices by Regions",
    x = "Insurance Price",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 12)  
  )

ggplot(data, aes(x = charges)) +
  geom_histogram(binwidth = 1000, alpha = 0.7, fill = "skyblue") +
  facet_wrap(~ as.factor(children), scales = "free_y") +  
  labs(
    title = "Distribution of Insurance Prices by Number of Children",
    x = "Insurance Price",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 12)  
  )

ggplot(data, aes(x = charges)) +
  geom_histogram(binwidth = 1000, alpha = 0.7, fill = "skyblue") +
  facet_wrap(~ sex, scales = "free_y") +  
  labs(
    title = "Distribution of Insurance Prices by Gender",
    x = "Insurance Price",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 12)  
  )

data_long2 <- data %>%
  pivot_longer(
    cols = c(age, bmi), 
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(data_long2, aes(x = Value, y = charges, color = smoker)) +
  geom_point(alpha = 0.3) +  
  facet_wrap(~ Variable, scales = "free_x") +  
  labs(
    title = "Distribution of Insurance Prices by Numeric Variables with Smoker Status",
    x = "Value of Variable",
    y = "Insurance Prices"
  ) +
  theme_minimal()

set.seed(123)
data$children<-as.numeric(data$children)
training_indices<-sample(nrow(data), size = 0.8 * nrow(data), replace = FALSE)
training_set<-data[training_indices,]
test_set<-data[-training_indices,]

model_fixed<-lm(charges ~., data = training_set)
final_model_fixed<-step(model_fixed, trace = F, direction = "both")
model_summary_fixed<-summary(final_model_fixed)

p_values_df <- data.frame(
  Variable = rownames(model_summary_fixed$coefficients),
  Coefficient = model_summary_fixed$coefficients[, 1],
  PValue = model_summary_fixed$coefficients[, 4]
)

p_values_df %>%
  gt() %>%
  tab_header("LM Coefficients (AIC = 44922.99)")


plot(final_model_fixed, which = 1)

plot(final_model_fixed, which = 2)

glm_model<-glm(charges~., data = training_set, family = Gamma)
final_glm_model<-step(glm_model, trace = F, direction = "both")
glm_model_summary<-summary(final_glm_model)

p_values_df_glm <- data.frame(
  Variable = rownames(glm_model_summary$coefficients),
  Coefficient = glm_model_summary$coefficients[, 1],
  PValue = glm_model_summary$coefficients[, 4]
)

p_values_df_glm %>%
  gt() %>%
  tab_header("GLM Coefficients (AIC = 44501.21)")

par(mfrow = c(1, 2))  

plot(final_glm_model, which = 1)

plot(final_glm_model, which = 2)

### Kenward to test whether which fixed effect is significant. Found out that sex is not significant
model1<-lmer(charges ~ smoker + children + age +bmi + region + (1|smoker), data = training_set)
model2<-lmer(charges ~ children + smoker + age + bmi + region + sex + (1|smoker), data = training_set)
KRmodcomp(model1, model2)

model_random1<-lmer(charges ~. + (1|smoker), data = training_set)

model_random1_summary<-summary(model_random1)

random_effects<-VarCorr(model_random1)

random_effects_df<-data.frame(
  Group = c("smoker", "Residuals"),
  Std_Dev = c(attr(random_effects$smoker, "stddev"), attr(random_effects, "sc"))
)

fixed_effects_df <- data.frame(
  Variable = rownames(model_random1_summary$coefficients),
  Coefficient = model_random1_summary$coefficients[, 1]
)  
random_effects_df %>%
  gt() %>%
  tab_header("Random Effects (AIC = 44924.99)")

fixed_effects_df %>%
  gt() %>%
  tab_header("Fixed Effects")

set.seed(123)
model_random2<-lmer(charges ~. + (sex + children + region + age + bmi|smoker), data = training_set)

model_random2_summary<-summary(model_random2)

smoker_coef <- t(coef(model_random2)$smoker[2,]) 
non_smoker_coef <- t(coef(model_random2)$smoker[1,])

model_random2_coef<-data.frame(
  Parameters = rownames(model_random2_summary$coefficients),
  Smoker = smoker_coef,
  "Non-smoker" = non_smoker_coef
)

model_random2_coef %>%
  gt() %>%
  tab_header("Mixed Effect Model 2 Coefficient by Smoker Status")

random_effects2<-VarCorr(model_random2)

random_effects_df2<-data.frame(
  Group = c("smoker", "", "", "", "", "", "", "", "Residuals"),
  Name = c("Intercept", "sexmale", "children", "regionnorthwest", "regionsoutheast", "regionsouthwest", "age", "bmi", ""),
  Std_Dev = c(attr(random_effects2$smoker, "stddev"), attr(random_effects2, "sc"))
)

fixed_effects_df2 <- data.frame(
  Variable = rownames(model_random2_summary$coefficients),
  Coefficient = model_random2_summary$coefficients[, 1]
)  
random_effects_df2 %>%
  gt() %>%
  tab_header("Random Effects (AIC = 44001.11)")

fixed_effects_df %>%
  gt() %>%
  tab_header("Fixed Effects")

plot(model_random2, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs. Fitted")

anova_table<-anova(model_random1, model_random2)

anova_table_df<-data.frame(
  Model = c("model_random1", "model_random2"),
  npar = c(anova_table$npar),
  AIC = c(anova_table$AIC),
  Deviance = c(anova_table$deviance),
  Chisq = c(anova_table$Chisq),
  P_value = c(anova_table$`Pr(>Chisq)`)
)

anova_table_df %>%
  gt() %>%
  tab_header("Anova Table")

prediction1 <- predict(final_model_fixed, newdata = test_set)
prediction2 <- predict(final_glm_model, newdata = test_set, type = "response")
prediction3 <- predict(model_random2, newdata = test_set)

mae1<-mean(abs(test_set$charges - prediction1))
mae2<-mean(abs(test_set$charges - prediction2))
mae3<-mean(abs(test_set$charges - prediction3))

mae_table<-data.frame(
  Models = c("Ordinary Linear Model", "Generalized Linear Model with Gamma Distribution", "Mixed Effect Model with Smoker as Random Effect"),
  "Mean Absolute Error" = c(mae1, mae2, mae3)
)

mae_table %>%
  gt() %>%
  tab_header("Mean Absolute Error of each Model")





