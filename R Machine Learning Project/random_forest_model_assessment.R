set.seed(555)
library(MASS)
library(fastICA)
library(np)

trials<-1

kernelexample1_R0mean<-rep(NA,trials)
kernelexample1_R1mean<-rep(NA,trials)
kernelexample1_R2mean<-rep(NA,trials)
kernelexample1_R3mean<-rep(NA,trials)
kernelexample1_optimalRmean<-rep(NA,trials)

result_list <- list()

for (example_sample in c(8,9,10)){
  n=2^example_sample
  for (example1 in 1:trials){
    print(example1)
    training_example1 <- data.frame(matrix(NA,   
                                           nrow = n,
                                           ncol = 50),
                                    'A' = sample(c(0, 1, 2), n, replace = TRUE),
                                    'R0' = 0,
                                    'error' = rnorm(n))
    
    for (i in 1:50) {
      training_example1[, i] <- runif(n, -1, 1)
    }
    
    training_example1_optfunction <- ((1 + 0.5 * training_example1$X1 + 0.5 * training_example1$X2) - training_example1$A)^2
    
    training_example1$R0 <- 8 + (4 * training_example1$X1) - (2 * training_example1$X2) - (2 * training_example1$X3) - (25 * training_example1_optfunction) + training_example1$error
    
    # Select predictor variables
    predictors <- training_example1[, -which(names(training_example1) %in% c('R0','A', 'error'))]
    
    target <- training_example1$R0
    
    # Create a random forest model
    rf_model <- randomForest(
      x = predictors,
      y = target,
      ntree = 1000,  # Number of trees in the forest
      mtry = sqrt(ncol(predictors)),  # Number of variables to consider at each split
      importance = TRUE  # Compute variable importance
    )
    
    importance_score<-importance(rf_model)
    
    node_score<-rf_model$importance[,'IncNodePurity']
    
    score <- (node_score)[order(-node_score)]
    
    num_components<-7
    
    variables<-names(score[1:num_components])
    
    training_formula_str <- paste("R0 ~ ", paste0(variables[1:num_components], collapse = " + "), " + ordered(A)")
    
    training_bw_formula <- as.formula(training_formula_str)
    
    # Perform kernel regression using npregbw
    bw_example1 <- npregbw(training_bw_formula,data=training_example1)
    
    kernel_model<-npreg(bw_example1)
    
    group_example1<-data.frame(matrix(NA,   
                                      nrow = 10000,
                                      ncol = 50),
                               'A0'=0,
                               'A1'=1,
                               'A2'=2,
                               'R0'=0,
                               'error'=rnorm(10000))
    
    for (i in 1:50) {
      group_example1[, i] <- runif(10000, -1, 1)
    }
    
    group_example1_matrix<-as.matrix(group_example1[,variables])
    
    newgroup_example1_A0 <- data.frame(group_example1_matrix, A = group_example1$A0)
    
    group_example1$Q0 <- predict(kernel_model, newdata = newgroup_example1_A0)
    
    newgroup_example1_A1 <- data.frame(group_example1_matrix, A = group_example1$A1)
    
    group_example1$Q1 <- predict(kernel_model, newdata = newgroup_example1_A1)
    
    newgroup_example1_A2 <- data.frame(group_example1_matrix, A = group_example1$A2)
    
    group_example1$Q2 <- predict(kernel_model, newdata = newgroup_example1_A2)
    
    group_example1$Treatment<-NA
    
    group_example1$Treatment <- max.col(group_example1[, c("Q0", "Q1", "Q2")]) - 1
    
    group_example1$R0<-8+(4*group_example1$X1)-(2*group_example1$X2)-(2*group_example1$X3)-(25*((1+0.5*group_example1$X1+0.5*group_example1$X2)-group_example1$Treatment)^2)+group_example1$error
    group_example1$R1<-8+(4*group_example1$X1)-(2*group_example1$X2)-(2*group_example1$X3)-(25*((1+0.5*group_example1$X1+0.5*group_example1$X2)-group_example1$A0)^2)+group_example1$error
    group_example1$R2<-8+(4*group_example1$X1)-(2*group_example1$X2)-(2*group_example1$X3)-(25*((1+0.5*group_example1$X1+0.5*group_example1$X2)-group_example1$A1)^2)+group_example1$error
    group_example1$R3<-8+(4*group_example1$X1)-(2*group_example1$X2)-(2*group_example1$X3)-(25*((1+0.5*group_example1$X1+0.5*group_example1$X2)-group_example1$A2)^2)+group_example1$error
    
    group_example1$optimal <- NA
    
    group_example1$optimal <- max.col(group_example1[, c("R1", "R2", "R3")]) - 1
    
    group_example1$optimalR<-8+(4*group_example1$X1)-(2*group_example1$X2)-(2*group_example1$X3)-(25*((1+0.5*group_example1$X1+0.5*group_example1$X2)-group_example1$optimal)^2)+group_example1$error
    
    kernelexample1_R0mean[example1]<-mean(group_example1$R0)
    kernelexample1_R1mean[example1]<-mean(group_example1$R1)
    kernelexample1_R2mean[example1]<-mean(group_example1$R2)
    kernelexample1_R3mean[example1]<-mean(group_example1$R3)
    kernelexample1_optimalRmean[example1]<-mean(group_example1$optimalR)
  }
  finalkernel_threevariables_result_50<-data.frame('Kernel'=kernelexample1_R0mean,
                                                   'Kernel_No_Treatment'=kernelexample1_R1mean,
                                                   'Kernel_Some_Treatment'=kernelexample1_R2mean,
                                                   'Kernel_Full_Treatment'=kernelexample1_R3mean,
                                                   'Kernel_Optimal'=kernelexample1_optimalRmean
                                                   
  )
  result_list[[example_sample - 4]] <- finalkernel_threevariables_result_50
}

finalkernel_threevariables_result_50 <- do.call(rbind, result_list)

# Split the data frame into separate data frames for each sample size
sample_size_8_data <- finalkernel_threevariables_result_50[1:trials, ]
sample_size_9_data <- finalkernel_threevariables_result_50[(trials + 1):(2 * trials), ]
sample_size_10_data <- finalkernel_threevariables_result_50[(2 * trials + 1):(3 * trials), ]

final_kernel_result<-data.frame('Sample 256 Kernel'=sample_size_8_data$Kernel,
                                'Sample 256 No Treatment'=sample_size_8_data$Kernel_No_Treatment,
                                'Sample 256 Some Treatment'=sample_size_8_data$Kernel_Some_Treatment,
                                'Sample 256 Full Treatment'=sample_size_8_data$Kernel_Full_Treatment,
                                'Sample 256 Optimal Treatment'=sample_size_8_data$Kernel_Optimal,
                                'Sample 512 Kernel'=sample_size_9_data$Kernel,
                                'Sample 512 No Treatment'=sample_size_9_data$Kernel_No_Treatment,
                                'Sample 512 Some Treatment'=sample_size_9_data$Kernel_Some_Treatment,
                                'Sample 512 Full Treatment'=sample_size_9_data$Kernel_Full_Treatment,
                                'Sample 512 Optimal Treatment'=sample_size_9_data$Kernel_Optimal,
                                'Sample 1024 Kernel'=sample_size_10_data$Kernel,
                                'Sample 1024 No Treatment'=sample_size_10_data$Kernel_No_Treatment,
                                'Sample 1024 Some Treatment'=sample_size_10_data$Kernel_Some_Treatment,
                                'Sample 1024 Full Treatment'=sample_size_10_data$Kernel_Full_Treatment,
                                'Sample 1024 Optimal Treatment'=sample_size_10_data$Kernel_Optimal
)



