## Model 1: Calorie

library(decisionSupport)

# Make variables

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("Calorie.csv")))

Calorie_function <- function(x, varnames){
  
  # Calculate BMR for men using the Mifflin-St Jeor equation
  
  bmr_male <- vv(((10*weight_male) + (6.25*height_male) - (5*age_male) + 5),
                var_CV, n_year)
  
  
  # Calculate BMR for women using the Mifflin-St Jeor equation
  
  bmr_female <- vv(((10*weight_female) + (6.25*height_female) - (5*age_female) - 161),
                   var_CV, n_year)
  
################################################################################
  
  ## Normal training days
  
  # Calculate daily calorie target for men to maintain current weight
  
  daily_kcal_tar_nor_male <-vv(bmr_male * paf_normal,
                           var_CV, n_year)
  
  
  # Calculate daily calorie target for women to maintain current weight
  
  daily_kcal_tar_nor_female <- vv(bmr_female * paf_intensive,
                              var_CV, n_year)
  

################################################################################  
  
  ## Intensive training days
  
  # Calculate daily calorie target for men to maintain current weight
  
  daily_kcal_tar_int_male <- vv(bmr_male * paf_intensive,
                                var_CV, n_year)
  
  
  # Calculate daily calorie target for women to maintain current weight
  
  daily_kcal_tar_int_female <- vv(bmr_female * paf_intensive,
                                  var_CV, n_year)
  
################################################################################  
  
  ## Extra calorie needs for underweight athletes
  
  # Calculate BMI for male athletes
  
  
  # Number of underweight male athletes
  
  
  # Extract weight range of underweight male athletes
  
  
  # Range of extra weight gain needed (kg) for male athletes
  
  
  # Range of weeks needed to meet target weights
  
  
  # Range of extra calorie needed for male athletes for total target weeks
  
  
  # Calculate BMI for female athletes
  
  
  # Number of underweight female athletes
  
  # Number of underweight female athletes
  
  
  # Extract weight range of underweight female athletes
  
  
  # Range of extra weight gain needed (kg) for female athletes ()
  
  
  # Range of weeks needed to meet target weights
  
  
  # Range of extra calorie needed for female athletes for total target weeks
  
################################################################################ 
  
  ## Deduct calories for overweight athletes
  
  # Calculate fat mass of male athletes (weight in kg * (% body fat)/100)
  
  
  # Calculate lean body mass of male athletes (total body weight - fat mass)
  
  
  # Calculate minimum target weight for male athletes
  
  
  # Calculate target desired weight for male athletes
  
  
  # Calculate realistic weight range for male athletes
  
  
  # Extract number of male athletes with percent body fat above threshold range
  
  
  # Extract range of male percent body fat above threshold range
  
  # Extract weight range of male athletes with percent body fat above threshold range
  
  # Range of weight loss needed (kg) for male athletes with percent body fat above threshold range
  
  
  # Range of weeks needed to meet target weights 
  
  
  # Range of calorie deduction needed for male athletes for total target weeks
  
  
  
  
  # Calculate fat mass of female athletes (weight in kg * (% body fat)/100)
  
  
  # Calculate lean body mass of female athletes (total body weight - fat mass)
  
  
  # Calculate minimum target weight for female athletes
  
  
  # Calculate target desired weight for female athletes
  
  
  # Calculate realistic weight range for female athletes
  
  
  # Extract number of female athletes with percent body fat above threshold range
  
  
  # Extract range of female percent body fat above threshold range
  
  # Extract weight range of female athletes with percent body fat above threshold range
  
  # Range of weight loss needed (kg) for female athletes with percent body fat above threshold range
  
  
  # Range of weeks needed to meet target weights 
  
  
  # Range of calorie deduction needed for female athletes for total target weeks
  
################################################################################ 
  
  ## Total yearly calorie needs 
  
  # Calculate total yearly calorie needs for male athletes
  
  
  # Calculate total yearly calorie needs for female athletes
  
  
  # Calculate overall yearly calorie needs
  
  
################################################################################ 
  
  ## Calculate yearly extra calorie provided during intervention (to calculate cost)
  
  # Calculate current yearly calorie intake for male athletes
  
  # Calculate extra yearly calorie provided for male athletes
  
  # Calculate current yearly calorie intake for female athletes
  
  # Calculate extra yearly calorie provided for female athletes
  
  
  
  
  
  


  
  
  
  
  
  
}
