install.packages(c("dplyr","ggplot2",'knitr','manipulate' ,"rmarkdown",
                   "patchwork","LaTeX","GGally","Hmisc","pastecs",
                   "MASS","broom","tidyverse","survival","lattice"))

library(dplyr)
library(ggplot2)
library(knitr)
library(manipulate)
library(datasets)
library(patchwork)
library(Hmisc)
library(pastecs)
library(MASS)
library(broom)
library(tidyverse)
data("mtcars")
?mtcars
str(mtcars)
require(stats); require(graphics);require(ggplot2);require(GGally)
#1. exploratory 

#exploratory data
  head(mtcars)
  str(mtcars)
  summary(mtcars)
  describe(mtcars)
  g<- ggpairs(subset(mtcars),
               lower = list(continuous ="smooth", method = "loess"),
               title = "Raw Data MTCARS."
  )
  g
  
  #2. models 
  #create table number of significant predictors, model 
  
  #raw analysis (all variables)
  fit<- lm(mpg~.,data = mtcars)
  rawSummary<-summary(fit) #all variables
  rawCoef<-rawSummary$coefficients
  
  #analyze data - raw- Graph raw data/graph -all
  # Plot the coefficients using ggplot2-
    #plot showing the coefficients of the model along with their error bars
  tidy_fit <- tidy(fit)
  
  g1 <- ggplot(tidy_fit, aes(x = term, y = estimate)) +
    geom_point() +
    geom_errorbar(
      aes(ymin = estimate - std.error, ymax = estimate + std.error),
      width = 0.2) +
    labs(title = "Coefficients Plot", x = "Term", y = "Estimate") +
    theme_minimal()
  g1
  
  # Open a PDF graphics device
  pdf("Coefficients Plot.pdf")
  # Generate the diagnostic plots
  par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
  plot(fit,main="Coefficients Plot")
  
  # Close the graphics device
  dev.off()
  

  #summary report 
  summary_df <- data.frame(
    Model = character(),
    Adj_R_Squared=numeric(),
    AIC=numeric(),
    BIC=numeric(),
    SignificantPredictors = numeric())
  
  
  
  
  # model: mpg~cyl
  
  fitCyl<- lm(mpg~cyl,data = mtcars)
  SummaryCyl<-summary(fitCyl) #all variables
  SummaryCyl$coefficients
  
  
  summary_df<-rbind(summary_df,
                    data.frame(Model= "mpg~cyl",
                               Adj_R_Squared= SummaryCyl$adj.r.squared,
                               AIC=AIC(fitCyl),
                               BIC=BIC(fitCyl),
                              SignificatPredictors=1))
  
  
  
  

  MTCARSdataColNames <- names(mtcars)  # Get column names of the mtcars dataset
  
  # Loop through columns 3 to 10
  for (i in 3:10) {
    #single variable models
    print(paste("Single Model: ", paste0("mpg~",MTCARSdataColNames[i])))
    singleModelFit<-lm(as.formula(paste0("mpg~",MTCARSdataColNames[i])),data=mtcars)
    SummarySingle<-summary(singleModelFit)
    coefSingleModel<-SummarySingle$coefficients
    summary_df<-rbind(summary_df,data.frame(
      Model= paste0("mpg~",MTCARSdataColNames[i]),
      Adj_R_Squared=SummarySingle$adj.r.squared,
      AIC=AIC(singleModelFit),
      BIC=BIC(singleModelFit),
        SignificatPredictors=1))
    
    
    #if (coefSingleModel[2,4] <= 0.05) {}
    print("----------------------------------------------------------")
    #multiple variable models
    model <- paste0("+", paste(MTCARSdataColNames[3:i], collapse = " + "))
    model2<-paste0("mpg~ cyl",model)
    fit<-lm(as.formula(model2),data=mtcars)
    coef<-summary(fit)$coefficients
    rsqr<-summary(fit)$adj.r.squared
    #print("Model mpg~.")
    #print(rawCoef)
    report<-0
    
    for (p_value in coef[, 4]) {
      if (p_value <= 0.05) {
        report <- report + 1
      }
    }
    summary_df<-rbind(summary_df,data.frame(Model= model2,
                                            Adj_R_Squared=rsqr, 
                                            AIC=AIC(fit),
                                            BIC=BIC(fit),
                                            SignificatPredictors=report))
    
    print(paste("Number of significant predictors:", report))
    print(paste("Significant Model:", model2))
    print(paste0("Model"," ",model2))
    print(coef)
    
    print("--------------------------------------------------------")
  }
  
  
#3.Comparing Models: 
  "how to pick the corect model?
  A. # Compare models using AIC
      AIC(model1, model2, model3)

   B. # Compare nested models using ANOVA
    anova(model1, model2, model3)
  notes:
  - Adjusted R-squared: Indicates the proportion of the variance 
      in the dependent variable explained by the independent variables, 
      adjusted for the number of predictors. Higher values indicate
      a better model.
  - AIC/BIC: Lower values indicate a better model.
  - Residual Plots: Should show no patterns (random scatter), 
      indicating that assumptions of linearity, independence, 
      and homoscedasticity are met.
  - VIF (Variance Inflation Factor): Values above 10 indicate high 
      multicollinearity, which should be addressed.
  - RMSE: Lower values indicate better predictive performance on the test set."
  
  
  
   " 
   -anova(model1, model2, model3) 
   #analysis of variance tables for one or more fitted model objects
   - Residual Plots: Should show no patterns (random scatter), 
    indicating that assumptions of linearity, independence, 
    and homoscedasticity are met.
    - VIF (Variance Inflation Factor): Values above 10 indicate high 
    multicollinearity, which should be addressed.
    - RMSE: Lower values indicate better predictive performance on the test set."
    
    
  #4. Model Diagnostics:
  #best models based on adjusted R2
  maxSigPredictors<-top_n(summary_df,n=4,Adj_R_Squared)
  max(summary_df$Adj_R_Squared)
  
  for(n in 1:length(maxSigPredictors$Model)){
    modelName<-paste0("modelF",n)
    modelF<-lm(as.formula(maxSigPredictors$Model[n]),data = mtcars)
    assign(modelName, modelF)
    assign(paste0(modelName,"VIF"),vif(modelF))
    assign(paste0(modelName,"RMSE"),rmse(modelF,mtcars))
    #plot --ggplot
    # Tidy the model fit
    tidyFit <- tidy(modelF)
    
    plotFinal <- ggplot(tidyFit, aes(x = term, y = estimate)) +
      geom_point() +
      geom_errorbar(
        aes(ymin = estimate - std.error, ymax = estimate + std.error),
        width = 0.2) +
      labs(title = paste("Plot: ",n), x = "Term", y = "Estimate") +
      theme_minimal()
    
    assign(paste0(modelName,"_GGPlot"),plotFinal)
    #plot()
    modelName2<-paste0("modelDiagnostics_",n,".pdf")
    
    # Open a PDF graphics device
    pdf(modelName2)
    # Generate the diagnostic plots
    par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
    plot(modelF,main=modelName2)
    
    # Close the graphics device
    dev.off()
    
    #5. Validation:
    # Split the data into training and testing sets
    set.seed(123)
    train_indices <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))
    train_data <- mtcars[train_indices, ]
    test_data <- mtcars[-train_indices, ]
    
    # Fit the model on the training set
    train_model <- lm(modelF, data = train_data)
    
    # Predict on the test set
    predictions <- predict(train_model, newdata = test_data)
    
    # Calculate RMSE (Root Mean Squared Error)
    rmse <- sqrt(mean((test_data$mpg - predictions)^2))
    assign(paste0("ValidationRMSE",modelName),rmse)

  }
  #anova
  anova(modelF1,modelF2,modelF3,modelF4)
  c(modelF1VIF, modelF2VIF,modelF3VIF,modelF4VIF)
  c(modelF1RMSE,modelF2RMSE,modelF3RMSE,modelF4RMSE)
    
  #6. corrections to Model Diagnostics: 
    # remove cyl and displesment and compare with first set 
  #best models based on adjusted R2
  maxSigPredictors<-top_n(summary_df,n=4,Adj_R_Squared)
  max(summary_df$Adj_R_Squared)
  
  for(n in 1:length(maxSigPredictors$Model)){
    modelName<-paste0("model2F_",n)
    modelString <- as.character(maxSigPredictors$Model[n])
    modelString <- sub("cyl\\+disp \\+ ", "", modelString)  # Corrected the regex pattern
    print(modelString)
    print(modelString)
    modelF<-lm(as.formula(modelString),data = mtcars)
    assign(modelName, modelF)
    assign(paste0(modelName,"VIF"),vif(modelF))
    assign(paste0(modelName,"RMSE"),rmse(modelF,mtcars))
    #plot --ggplot
    # Tidy the model fit
    tidyFit <- tidy(modelF)
    
    plotFinal <- ggplot(tidyFit, aes(x = term, y = estimate)) +
      geom_point() +
      geom_errorbar(
        aes(ymin = estimate - std.error, ymax = estimate + std.error),
        width = 0.2) +
      labs(title = paste("Plot: ",n), x = "Term", y = "Estimate") +
      theme_minimal()
    
    assign(paste0(modelName,"_GGPlot"),plotFinal)
    #plot()
    modelName2<-paste0("model2Diagnostics_",n,".pdf")
    
    # Open a PDF graphics device
    pdf(modelName2)
    # Generate the diagnostic plots
    par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
    plot(modelF,main=modelName2)
    
    # Close the graphics device
    dev.off()
    
    #5. Validation:
    # Split the data into training and testing sets
    set.seed(123)
    train_indices <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))
    train_data <- mtcars[train_indices, ]
    test_data <- mtcars[-train_indices, ]
    
    # Fit the model on the training set
    train_model <- lm(modelF, data = train_data)
    
    # Predict on the test set
    predictions <- predict(train_model, newdata = test_data)
    
    # Calculate RMSE (Root Mean Squared Error)
    rmse <- sqrt(mean((test_data$mpg - predictions)^2))
    assign(paste0("ValidationRMSE",modelName),rmse)
    
    
  }
  #anova
  anova(model2F_1,model2F_2,model2F_3,model2F_4)
  c(model2F_1VIF, model2F_2VIF,model2F_3VIF,model2F_4VIF)
  c(model2F_1RMSE,model2F_2RMSE,model2F_3RMSE,model2F_4RMSE)
  
  g2 <- ggpairs(mtcars,
                lower = list(continuous = wrap("smooth", method = "loess")),
                aes(colour=factor(am)))
  g2
  
  proposedLM1=lm(mpg~(qsec+hp), data=mtcars)
  proposedLM2=lm(mpg~hp,data=mtcars)
  anova(model2F_1,model2F_2,model2F_3,model2F_4,proposedLM1,proposedLM2)
  
  TukeyHSD(anova(model2F_1,model2F_2,model2F_3,model2F_4,proposedLM1,proposedLM2))
  c(model2F_1VIF, model2F_2VIF,model2F_3VIF,model2F_4VIF)
  c(model2F_1RMSE,model2F_2RMSE,model2F_3RMSE,model2F_4RMSE)
  
  manualTransmision<-filter(mtcars,am==1)
  automaticTransmition<-filter(mtcars,am==0) 
  lm(mpg ~ (hp + qsec),data=manualTransmision)
  lm(mpg ~ (hp + qsec),data=automaticTransmition)
  
  summary(lm(mpg ~ (hp + qsec),data=manualTransmision))
  summary(lm(mpg ~ (hp + qsec),data=automaticTransmition))
  
  
  
  
  ?Comparison of Results:
   

  
  
  
  
  notes: 
- manual show more mpg in every category 
- manual shows more power 
  because by equation of power,
  velocity is manipulated by a factor of force. wh
  ich to make it the change smooth needs
  more gears and that is shown in the graph 
- automatic show more speed 
  more weight more displacement 
  with the same number of cylinders 
  and consume less gasoline 
  
- comparing final model 1 and 2, where model 1 represents mpg~hp+qsec shows 
  a strong strong evidence against the null hypothesis. The proposed hypotesis
  for this result is based on the power equation Power=force*velocity. 
  therefore the factor of velocity influences the model 

- “Is an automatic or manual transmission better for MPG”
  
#4. Model Diagnostics:
" # Residual plots for model diagnostics:
    -Residual Plots: Should show no patterns (random scatter), 
    indicating that assumptions of linearity, independence, and 
    homoscedasticity are met.
    
    par(mfrow = c(2, 2))
     plot(model3)
  # Check for multicollinearity using VIF
    library(car)
    vif(model3)  "
  

  "#5. Validation:
      # Split the data into training and testing sets
    set.seed(123)
    train_indices <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))
    train_data <- mtcars[train_indices, ]
    test_data <- mtcars[-train_indices, ]
    
    # Fit the model on the training set
    train_model <- lm(mpg ~ wt + hp + qsec, data = train_data)
    
    # Predict on the test set
    predictions <- predict(train_model, newdata = test_data)
    
    # Calculate RMSE (Root Mean Squared Error)
    rmse <- sqrt(mean((test_data$mpg - predictions)^2))
    rmse"
      
  
  
  
#_______________________________________________________________________________
#plots
  # Create the coefficients plot
  linearModel <- lm(as.formula(row$Model), data = mtcars)
  # Tidy the model fit
  tidyFit <- tidy(linearModel)
  
  plotFinal <- ggplot(tidyFit, aes(x = term, y = estimate)) +
    geom_point() +
    geom_errorbar(
      aes(ymin = estimate - std.error, ymax = estimate + std.error),
      width = 0.2) +
    labs(title = paste("Plot: ",row$Model), x = "Term", y = "Estimate") +
    theme_minimal()
  plotFinal
  # Assign the plot to a variable with a dynamic name
  plot_name <- paste0("plot",as.character(gsub("[~+]", "_",
                                               row$Model)))
  assign(plot_name, plotFinal)
  
 
#--------------------------------------------------------------------------
  
  
  
  

  

  
  
   # Creating a list with filtered data and descriptions removing "am" and "vs"
    data_list <- list(
      vA = list(
        data = filter(mtcars, am == 0 & vs == 0)[,-8:-9], 
        description = "V Shaped Engine/Automatic Transmission"
      ),
      vM = list(
        data = filter(mtcars, am == 1 & vs == 0)[,-8:-9], 
        description = "V Shaped Engine/Manual Transmission"
      ),
      lA = list(
        data =filter(mtcars, am == 0 & vs == 1)[,-8:-9], 
        description = "L Shaped Engine/Automatic Transmission"
      ),
      lM = list(
        data = filter(mtcars, am == 1 & vs == 1)[,-8:-9], 
        description = "L Shaped Engine/Manual Transmission"
      )
    )
      #2.1 exploration of new data 
      for (name in names(data_list)) {
        #exploratory data
        cat("\n\nData Frame:", name)
        cat("\nDescription:", data_list[[name]]$description)
        cat("\nFirst few rows of the data frame:\n")
        print(head(data_list[[name]]$data))
        df <- data_list[[name]]$data
        plot <- ggpairs(df, 
                        columns = names(df),
                        lower = list(continuous = "smooth", 
                                     combo = "facetdensity", 
                                     method = "loess"),
                        title = data_list[[name]]$description)
        print(plot)
        # Save the plot to a variable
        assign(paste0("g_", name), plot)
        #get the coefficients 
          #raw analysis (all variables)
          fit1<- lm(mpg~.,data = df)
          rawSummary1<-summary(lm(fit1,data = df)) #all variables
          assign(paste0("FitModel_",name),rawSummary1$coefficients)
          rawCoef1<-as.data.frame( rawSummary1$coefficients )
          assign(paste0("Coefficients",data_list[[name]]$description), rawCoef1)
          print(rawCoef1)
          #plot coefficients
          #plot_object_name <- paste0("CoefPlot", name)
          
          try_result <- try({
            assign(paste0("CoefPlot", name), plot(fit,
                                          main=data_list[[name]]$description))
          }, silent = FALSE)
          
          # Check if the plot creation and assignment were successful
          
          tryCatch({
            plot(fit, main=data_list[[name]]$description)
          }, error = function(e) {
            cat("Error in plotting:", e$message, "\n")
          })
        
      }

#_____________________________________________________________________________
    # possible categorical data detected on vs and am 
    
    #unique values in each variable 
    MTCARSdataColNames <- names(subset(mtcars))
    uniqueValuesperColumn<- list()
    for (x in MTCARSdataColNames) {
      print(paste("Column:", x))  # Print the column name
      # Use unique() to find unique values in the column
      mtcarsCol <- unique(mtcars[[x]])
      print(mtcarsCol)
      uniqueValuesperColumn[[x]]<-mtcarsCol
      print("---------------------------------")  # Separator for clarity
    }
    uniqueValuesperColumn
    # categorical data detected (vs and am ) #create 4 groups bases on categorical
    
  
    

#create table to compare the coefficients 
# Create a list of data frames to compare


# Iterate over the list and print the names
for (y in data_list) {
  print(y)
  # Initialize a list to store coefficients
  paste("coef_list",deparse(substitute(y)))<- list()
  

# Extract column names from mtcars dataset
dataColNames <- names(subset(y, select = -c(vs, am)))
      #print(dataColNames)

# Loop through all columns except the first one
for (x in dataColNames[-1]) {
  
  # Create a linear model using the first column as the response variable and x as the predictor
  linearModel <- lm(as.formula(paste(dataColNames[1], "~", x)),
                    data = y)
  
  # Extract coefficients from the linear model
  modelCoef <- summary(linearModel)$coefficients
  
  # Print the coefficients of the predictor (the second row)
  #print(modelCoef)
  
  # Store the coefficients in the list, using the predictor name as the key
  try(coef_list[[x]] <- modelCoef[2,],silent = TRUE)
  if (inherits(coef_list[[x]], "try-error")) {
    coef_list[[x]]<-modelCoef[1,]
  } else {
    #coef_list[[x]]<-modelCoef[1,]
    print(paste("else",x))
    print(modelCoef)
  }
}
# Convert the list of coefficients to a data frame for easier viewing
paste("coef_df",deparse(substitute(y))) <- do.call(rbind, coef_list)
print(paste("Coefficients",x))
print(paste("coef_df",deparse(substitute(y))))


}



#create a ned df and separate manual from automatic transmition!
  #data contains binary data for variables vs and am making the coefficients 
    #look big
