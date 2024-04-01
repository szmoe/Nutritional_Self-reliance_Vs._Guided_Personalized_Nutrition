## Model 1: Calorie

library(decisionSupport)

# Make variables

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("Calorie.csv")))

Calorie_function <- function(x, varnames){
  
  ### For First Year
  
  # Calculate BMR for men using the Mifflin-St Jeor equation
  
  bmr_male <- c(((10*weight_male) + (6.25*height_male) - (5*age_male) + 5),
                rep(0, 1))
  
  
  # Calculate BMR for women using the Mifflin-St Jeor equation
  
  bmr_female <- c(((10*weight_female) + (6.25*height_female) - (5*age_female) - 161),
                   rep(0, 1))
  
################################################################################
  
  ## Normal training days
  
  # Calculate daily calorie target for men to maintain current weight
  
  daily_kcal_tar_nor_male <-c(bmr_male * paf_normal,
                              rep(0, 1))
  
  
  # Calculate daily calorie target for women to maintain current weight
  
  daily_kcal_tar_nor_female <- c(bmr_female * paf_intensive,
                                 rep(0, 1))
  

################################################################################  
  
  ## Intensive training days
  
  # Calculate daily calorie target for men to maintain current weight
  
  daily_kcal_tar_int_male <- c(bmr_male * paf_intensive,
                               rep(0, 1))
  
  
  # Calculate daily calorie target for women to maintain current weight
  
  daily_kcal_tar_int_female <- c(bmr_female * paf_intensive,
                                 rep(0, 1))
  
################################################################################  
  
  ## Extra calorie needs for underweight athletes
  
  # Calculate BMI for male athletes
  
  bmi_male <- c(weight_male/(height_male*0.01)^2,
                rep(0,  1))
  
  # Number of underweight male athletes
  
  underweight_male <- c(per_underweight_male * number_male,
                        rep(0, 1))
  
  # Extract weight range of underweight male athletes
  
  underweight_bmi_male <- c(ifelse(bmi_male < bmi_healthy_cutoff,
                                   bmi_male, 0),
                            rep(0, 1))
  
  ## here we estimate weights for all possible low bmi for all heights
  wt_under_male <- c(underweight_bmi_male * (height_male * 0.01)^2,
                     rep(0, 1))
  
  # Range of extra weight gain needed (kg) for male athletes
  
  wt_healthy_male <- bmi_healthy_cutoff * (height_male * 0.01)^2
                  
  
  extra_wt_male <- c(wt_healthy_male - wt_under_male,
                     rep(0, 1))
  
  
  # Range of weeks needed to meet target weights
  
  wt_gain_week_male <- c(extra_wt_male/weekly_weight_gain,
                    rep(0, 1))
  
  # Range of extra calorie needed for male athletes for total target weeks
  
  extra_kcal_male <- c(weekly_extra_kcal * wt_gain_week_male,
                       rep(0, 1))
  
  # Total extra calorie needed for all underweight male athletes
  
  total_extra_kcal_male <- c(extra_kcal_male * underweight_male,
                             rep(0, 1))
  
  # Calculate BMI for female athletes
  
  bmi_female <- c(weight_female/(height_female*0.01)^2,
                rep(0,  1))
  
  # Number of underweight female athletes
  
  underweight_female <- c(per_underweight_female * number_female,
                        rep(0, 1))
  
  # Extract weight range of underweight female athletes
  
  underweight_bmi_female <- c(ifelse(bmi_female < bmi_healthy_cutoff,
                                   bmi_female, 0),
                            rep(0, 1))
  
  ## here we estimate weights for all possible low bmi for all heights
  wt_under_female <- c(underweight_bmi_female * (height_female * 0.01)^2,
                     rep(0, 1))
  
  
  # Range of extra weight gain needed (kg) for female athletes 
  
  wt_healthy_female <- bmi_healthy_cutoff * (height_female * 0.01)^2
  
  
  extra_wt_female <- c(wt_healthy_female - wt_under_female,
                     rep(0, 1))
  
  
  # Range of weeks needed to meet target weights
  
  wt_gain_week_female <- c(extra_wt_female/weekly_weight_gain,
                         rep(0, 1))
  
  
  # Range of extra calorie needed for female athletes for total target weeks
  
  extra_kcal_female <- c(weekly_extra_kcal * wt_gain_week_female,
                       rep(0, 1))
  
  # Total extra calorie needed for all underweight female athletes
  
  total_extra_kcal_female <- c(extra_kcal_female * underweight_female,
                             rep(0, 1))
  
################################################################################ 
  
  ## Deduct calories for overweight athletes
  # watch out for training intensity- kcal deduction will be less if they increase training
  
  # Calculate fat mass of male athletes (weight in kg * (% body fat)/100)
  
  fat_mass_male <- c(weight_male * (fat_perc_male/100),
                     rep(0, 1))
  
  
  # Calculate lean body mass of male athletes (total body weight - fat mass)
  
  lean_mass_male <- c(weight_male - fat_mass_male,
                      rep(0, 1))

  
  # Calculate target weight for male athletes
  # Since input table allows range, I just use one calculation to generate range-no need for step-cal
  
  target_wt_male <- c(lean_mass_male/(1-(target_fat_perc_male/100)),
                      rep(0, 1))
  
  
  # Extract number of male athletes with percent body fat above threshold range
  
  number_overweight_male <- c(per_overweight_male * number_male,
                              rep(0, 1))
  
  
  # Extract range of male percent body fat above threshold range
  
  fat_above_threshold_male <- c(ifelse(fat_perc_male > target_fat_perc_male,
                                       fat_perc_male, 0),
                                rep(0, 1))
  
  # Extract weight range of male athletes with percent body fat above threshold range
  
  wt_over_male <- c(fat_mass_male/(fat_above_threshold_male/100),
                    rep(0, 1))
  
  # Range of weight loss needed (kg) for male athletes with percent body fat above threshold range
  
  wt_normal_male <- c(fat_mass_male/(target_fat_perc_male/100),
                      rep(0, 1))
  
  wt_deduct_male <- c(wt_over_male - wt_normal_male,
                      rep(0, 1))
  
  # Range of weeks needed to meet target weights 
  
  wt_loss_week_male <- c(wt_deduct_male/weekly_weight_loss,
                           rep(0, 1))
  
  
  # Range of calorie deduction needed for male athletes for total target weeks
  
  kcal_deduct_male <- c(weekly_extra_kcal * wt_loss_week_male,
                         rep(0, 1))
  
  # Total extra calorie deducted for all overweight male athletes
  
  total_deduct_kcal_male <- c(kcal_deduct_male * number_overweight_male,
                               rep(0, 1))
  
  
  # Calculate fat mass of female athletes (weight in kg * (% body fat)/100)
  
  fat_mass_female <- c(weight_female * (fat_perc_female/100),
                     rep(0, 1))
  
  
  # Calculate lean body mass of female athletes (total body weight - fat mass)
  
  lean_mass_female <- c(weight_female - fat_mass_female,
                      rep(0, 1))
  
  
  # Calculate target weight for female athletes
  
  target_wt_female <- c(lean_mass_female/(1-(target_fat_perc_female/100)),
                      rep(0, 1))
  
  
  
  # Extract number of female athletes with percent body fat above threshold range
  
  number_overweight_female <- c(per_overweight_female * number_female,
                              rep(0, 1))
  
  
  # Extract range of female percent body fat above threshold range
  
  fat_above_threshold_female <- c(ifelse(fat_perc_female > target_fat_perc_female,
                                         fat_perc_female, 0),
                                  rep(0, 1))
  
  
  # Extract weight range of female athletes with percent body fat above threshold range
  
  wt_over_female <- c(fat_mass_female/(fat_above_threshold_female/100),
                    rep(0, 1))
  
  # Range of weight loss needed (kg) for female athletes with percent body fat above threshold range
  
  wt_normal_female <- c(fat_mass_female/(target_fat_perc_female/100),
                      rep(0, 1))
  
  wt_deduct_female <- c(wt_over_female - wt_normal_female,
                      rep(0, 1))
  
  
  # Range of weeks needed to meet target weights 
  
  wt_loss_week_female <- c(wt_deduct_female/weekly_weight_loss,
                         rep(0, 1))
  
  
  # Range of calorie deduction needed for female athletes for total target weeks
  
  kcal_deduct_female <- c(weekly_extra_kcal * wt_loss_week_female,
                        rep(0, 1))
  
  # Total extra calorie deducted for all overweight female athletes
  
  total_deduct_kcal_female <- c(kcal_deduct_female * number_overweight_female,
                              rep(0, 1))
  
################################################################################ 
  
  ## Total yearly calorie needs 
  
  # Calculate total yearly calorie needs for male athletes for first year
  
  
  # Calculate total yearly calorie needs for female athletes for first year
  
  
  # Calculate overall yearly calorie needs for first year
  
  
################################################################################ 
  
  ## Calculate first year extra calorie provided during intervention (to calculate cost) 
  
  # Calculate current yearly calorie intake for male athletes
  
  # Calculate extra yearly calorie provided for male athletes for first year
  
  # Calculate current yearly calorie intake for female athletes
  
  # Calculate extra yearly calorie provided for female athletes for first year
  

################################################################################ 
  
  ## For following years (except first year)
  
  # I calculate again for following years because the assumption is after first year
  # all athletes will be within normal weight range- so not the same weight as in 
  # the input table
 
 
  
  
  


  
  
  
  
  
  
}
