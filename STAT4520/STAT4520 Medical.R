library(ggplot2)
library(dplyr)
library(lme4)
library(lmerTest)

set.seed(123)
data<-read.csv("/Users/antonyang/Downloads/Past Courses Note/STAT4520/Medicalpremium.csv")

data <- data %>%
  mutate(across(c(Diabetes, BloodPressureProblems, AnyTransplants, AnyChronicDiseases, KnownAllergies, HistoryOfCancerInFamily, NumberOfMajorSurgeries), as.factor))

data_long <- data %>%
  pivot_longer(
    cols = -c(PremiumPrice, Age, Weight, Height), 
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(data_long, aes(x = Value, y = PremiumPrice)) +
  geom_boxplot(fill = 4) +  
  facet_wrap(~ Variable, scales = "free_x") +  
  labs(
    x = "Value of Variable",
    y = "Premium Prices"
  ) +
  theme_minimal()

data_long2 <- data %>%
  pivot_longer(
    cols = c(Age, Weight, Height), 
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(data_long2, aes(x = Value, y = PremiumPrice)) +
  geom_point(color = 4) +  
  facet_wrap(~ Variable, scales = "free_x") +  
  labs(
    x = "Value of Variable",
    y = "Premium Prices"
  ) +
  theme_minimal()

data <- data %>%
  mutate(across(c(Diabetes, BloodPressureProblems, AnyTransplants, AnyChronicDiseases, KnownAllergies, HistoryOfCancerInFamily, NumberOfMajorSurgeries), as.numeric))

training_indices<-sample(nrow(data), size = 0.8 * nrow(data), replace = FALSE)
training_set<-data[training_indices,]
test_set<-data[-training_indices,]

model_fixed<-lm(PremiumPrice ~., data = training_set)
final_model_fixed<-step(model_fixed, trace = F)
summary(final_model_fixed)

model_random<-lmer(PremiumPrice ~ Age + Weight + Diabetes + AnyTransplants + AnyChronicDiseases + HistoryOfCancerInFamily + (Age|NumberOfMajorSurgeries), data = training_set)
summary(model_random)


prediction1<-predict(final_model_fixed, newdata = test_set)
prediction2<-predict(model_random, newdata = test_set)

mae1<-mean(abs(prediction1 - test_set$PremiumPrice))
mae2<-mean(abs(prediction2 - test_set$PremiumPrice))

print(mae1)
print(mae2)




