## Model 1: Calorie and Macro-nutrients

library(decisionSupport)

# Make variables

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("Calorie.csv")))

Calorie_function <- function(x, varnames){
  
  ### For First Year
  
  # Calculate BMR for men using the Mifflin-St Jeor equation
  
  bmr_male <- vv(((10*weight_male) + (6.25*height_male) - (5*age_male) + 5),
                var_CV, n = 1)
  
  
  # Calculate BMR for women using the Mifflin-St Jeor equation
  
  bmr_female <- vv(((10*weight_female) + (6.25*height_female) - (5*age_female) - 161),
                   var_CV, n = 1)
  
################################################################################
  
  ## Normal training days
  
  # Calculate daily calorie target for men to maintain current weight
  
  daily_kcal_tar_nor_male <-vv(bmr_male * paf_normal,
                               var_CV, n = 1)
  
  
  # Calculate daily calorie target for women to maintain current weight
  
  daily_kcal_tar_nor_female <- vv(bmr_female * paf_normal,
                                 var_CV, n = 1)
  

################################################################################  
  
  ## Intensive training days
  
  # Calculate daily calorie target for men to maintain current weight
  
  daily_kcal_tar_int_male <- vv(bmr_male * paf_intensive,
                               var_CV, n = 1)
  
  
  # Calculate daily calorie target for women to maintain current weight
  
  daily_kcal_tar_int_female <- vv(bmr_female * paf_intensive,
                                 var_CV, n = 1)
  
################################################################################  
  
  ## Extra calorie needs for underweight athletes
  
  # Calculate BMI for male athletes
  
  bmi_male <- vv(weight_male/(height_male*0.01)^2,
                var_CV, n = 1)
  
  # Number of underweight male athletes
  
  underweight_male <- vv(per_underweight_male * number_male,
                         var_CV, n = 1)
  
  # Extract weight range of underweight male athletes
  
  underweight_bmi_male <- vv(ifelse(bmi_male < bmi_low_cutoff,
                                   bmi_male, 0),
                             var_CV, n = 1)
  
  ## here we estimate weights for all possible low bmi for all heights
  wt_under_male <- vv(underweight_bmi_male * (height_male * 0.01)^2,
                      var_CV, n = 1)
  
  # Range of extra weight gain needed (kg) for male athletes
  
  wt_healthy_male <- bmi_healthy_cutoff * (height_male * 0.01)^2
                  
  
  extra_wt_male <- vv(wt_healthy_male - wt_under_male,
                      var_CV, n = 1)
  
  
  # Range of weeks needed to meet target weights
  
  wt_gain_week_male <- vv(extra_wt_male/weekly_weight_gain,
                          var_CV, n = 1)
  
  # Range of extra calorie needed for male athletes for total target weeks
  
  extra_kcal_male <- vv(weekly_extra_kcal * wt_gain_week_male,
                        var_CV, n = 1)
  
#####################
  
  # Calculate BMI for female athletes
  
  bmi_female <- vv(weight_female/(height_female*0.01)^2,
                   var_CV, n = 1)
  
  # Number of underweight female athletes
  
  underweight_female <- vv(per_underweight_female * number_female,
                           var_CV, n = 1)
  
  # Extract weight range of underweight female athletes
  
  underweight_bmi_female <- vv(ifelse(bmi_female < bmi_low_cutoff,
                                   bmi_female, 0),
                               var_CV, n = 1)
  
  ## here we estimate weights for all possible low bmi for all heights
  wt_under_female <- vv(underweight_bmi_female * (height_female * 0.01)^2,
                        var_CV, n = 1)
  
  
  # Range of extra weight gain needed (kg) for female athletes 
  
  wt_healthy_female <- bmi_healthy_cutoff * (height_female * 0.01)^2
  
  
  extra_wt_female <- vv(wt_healthy_female - wt_under_female,
                        var_CV, n = 1)
  
  
  # Range of weeks needed to meet target weights
  
  wt_gain_week_female <- vv(extra_wt_female/weekly_weight_gain,
                            var_CV, n = 1)
  
  
  # Range of extra calorie needed for female athletes for total target weeks
  
  extra_kcal_female <- vv(weekly_extra_kcal * wt_gain_week_female,
                          var_CV, n = 1)
  

################################################################################ 
  
  ## Deduct calories for overweight athletes
  # watch out for training intensity- kcal deduction will be less if they increase training
  
  # Calculate fat mass of male athletes (weight in kg * (% body fat)/100)
  
  fat_mass_male <- vv(weight_male * (fat_perc_male/100),
                      var_CV, n = 1)
  
  
  # Calculate lean body mass of male athletes (total body weight - fat mass)
  
  lean_mass_male <- vv(weight_male - fat_mass_male,
                       var_CV, n = 1)

  
  # Calculate target weight for male athletes
  # Since input table allows range, I just use one calculation to generate range-no need for step-cal
  
  target_wt_male <- vv(lean_mass_male/(1-(target_fat_perc_male/100)),
                       var_CV, n = 1)
  
  
  # Extract number of male athletes with percent body fat above threshold range
  
  number_overweight_male <- vv(per_overweight_male * number_male,
                               var_CV, n = 1)
  
  
  # Extract range of male percent body fat above threshold range
  
  fat_above_threshold_male <- vv(ifelse(fat_perc_male > target_fat_perc_male,
                                       fat_perc_male, 0),
                                 var_CV, n = 1)
  
  # Extract weight range of male athletes with percent body fat above threshold range
  
  wt_over_male <- vv(lean_mass_male/(1-(fat_above_threshold_male/100)),
                     var_CV, n = 1)
  
  
  # Range of weight loss needed (kg) for male athletes with percent body fat above threshold range
  
  wt_deduct_male <- vv(wt_over_male - target_wt_male,
                       var_CV, n = 1)
  
  # Range of weeks needed to meet target weights 
  
  wt_loss_week_male <- vv(wt_deduct_male/weekly_weight_loss,
                          var_CV, n = 1)
  
  
  # Range of calorie deduction needed for male athletes for total target weeks
  
  kcal_deduct_male <- vv(weekly_extra_kcal * wt_loss_week_male,
                         var_CV, n = 1)
  
#######################
  
  
  # Calculate fat mass of female athletes (weight in kg * (% body fat)/100)
  
  fat_mass_female <- vv(weight_female * (fat_perc_female/100),
                        var_CV, n = 1)
  
  
  # Calculate lean body mass of female athletes (total body weight - fat mass)
  
  lean_mass_female <- vv(weight_female - fat_mass_female,
                         var_CV, n = 1)
  
  
  # Calculate target weight for female athletes
  
  target_wt_female <- vv(lean_mass_female/(1-(target_fat_perc_female/100)),
                         var_CV, n = 1)
  
  
  
  # Extract number of female athletes with percent body fat above threshold range
  
  number_overweight_female <- vv(per_overweight_female * number_female,
                                 var_CV, n = 1)
  
  
  # Extract range of female percent body fat above threshold range
  
  fat_above_threshold_female <- vv(ifelse(fat_perc_female > target_fat_perc_female,
                                         fat_perc_female, 0),
                                   var_CV, n = 1)
  
  
  # Extract weight range of female athletes with percent body fat above threshold range
  
  wt_over_female <- vv(lean_mass_female/(1- (fat_above_threshold_female/100)),
                       var_CV, n = 1)

  
  # Range of weight loss needed (kg) for female athletes with percent body fat above threshold range
  
  wt_deduct_female <- vv(wt_over_female - target_wt_female,
                         var_CV, n = 1)
  
  
  # Range of weeks needed to meet target weights 
  
  wt_loss_week_female <- vv(wt_deduct_female/weekly_weight_loss,
                            var_CV, n = 1)
  
  
  # Range of calorie deduction needed for female athletes for total target weeks
  
  kcal_deduct_female <- vv(weekly_extra_kcal * wt_loss_week_female,
                           var_CV, n = 1)
  
  
################################################################################ 
  
  ### Total yearly calorie needs 
  
  ## Total first-year calorie needs for athletes within healthy weight range
  
  # Calculate number of male athletes within healthy weight range
  
  number_healthy_male <- number_male - (underweight_male + number_overweight_male)
  
  # Calculate total calorie needs for healthy weight range athletes for the first year
  
  bmr_healthy_male <- vv(((10*target_wt_male) + (6.25*height_male) - (5*age_male) + 5),
                         var_CV, n = 1)
  
  daily_kcal_tar_nor_healthy_male <-vv(bmr_healthy_male * paf_normal,
                                       var_CV, n = 1)
  
  
  daily_kcal_tar_int_healthy_male <- vv(bmr_healthy_male * paf_intensive,
                                        var_CV, n = 1)
  
  first_year_kcal_healthy_male <- vv((daily_kcal_tar_nor_healthy_male*normal_training) +
                                      (daily_kcal_tar_int_healthy_male*intensive_training),
                                     var_CV, n = 1)
  
  total_first_year_kcal_healthy_male <- vv(first_year_kcal_healthy_male*number_healthy_male,
                                           var_CV, n = 1)
  
  ##############
  
  # Calculate number of female athletes within healthy weight range
  
  number_healthy_female <- number_female - (underweight_female + number_overweight_female)
  
  # Calculate total calorie needs for healthy weight range athletes for the first year
  
  bmr_healthy_female <- vv(((10*target_wt_female) + (6.25*height_female) - (5*age_female) - 161),
                           var_CV, n = 1)
  
  daily_kcal_tar_nor_healthy_female <-vv(bmr_healthy_female * paf_normal,
                                         var_CV, n = 1)
  
  
  daily_kcal_tar_int_healthy_female <- vv(bmr_healthy_female * paf_intensive,
                                          var_CV, n = 1)
  
  first_year_kcal_healthy_female <- vv((daily_kcal_tar_nor_healthy_female*normal_training) +
                                        (daily_kcal_tar_int_healthy_female*intensive_training),
                                       var_CV, n = 1)
  
  total_first_year_kcal_healthy_female <- vv(first_year_kcal_healthy_female*number_healthy_female,
                                             var_CV, n = 1)
  
  
  #####################################################
  
  ## Total first-year calorie needs for male athletes below healthy BMI range
  
  # Change weight gain weeks for male athletes into days
  
  wt_gain_week_male_in_days <- wt_gain_week_male * 7
  
  ## Intensive training period (training before games) comes after normal period
  # Calculate total days where underweight athletes will do normal or intensive training during weight gain period
  
  wt_gain_int_training_days_male <- abs(wt_gain_week_male_in_days - normal_training)
  wt_gain_nor_training_days_male <- ifelse(normal_training >= wt_gain_week_male_in_days,
                                           wt_gain_week_male_in_days, 
                                           (wt_gain_week_male_in_days - 
                                              wt_gain_int_training_days_male))
  
  
  # Base calorie needs for an underweight male athlete during weight gain period
  
  base_kcal_need_during_wt_gain_male <- ifelse(wt_gain_week_male_in_days <= normal_training,
                                               (daily_kcal_tar_nor_male*wt_gain_week_male_in_days),
                                               ((daily_kcal_tar_nor_male*wt_gain_nor_training_days_male)+
                                                  daily_kcal_tar_int_male*wt_gain_int_training_days_male))
  
  # Total calorie needs for an underweight male athlete during weight gain period
  
  total_kcal_wt_gain_male <- base_kcal_need_during_wt_gain_male + extra_kcal_male
  
  ####################
  
  # Total calorie needs for an underweight male (now normal weight) after weight gain period
  
  total_training_days <- normal_training + intensive_training
  training_days_after_wt_gain_male <- total_training_days - wt_gain_week_male_in_days
  nor_training_days_after_wt_gain_male <- ifelse(training_days_after_wt_gain_male > intensive_training,
                                                 (training_days_after_wt_gain_male - 
                                                    intensive_training), 0)
  
  
  int_training_days_after_wt_gain_male <- ifelse(training_days_after_wt_gain_male > intensive_training,
                                                 (training_days_after_wt_gain_male -
                                                    nor_training_days_after_wt_gain_male),
                                                 training_days_after_wt_gain_male)
  
  first_year_kcal_after_wt_gain_male <- ((daily_kcal_tar_nor_healthy_male*
                                            nor_training_days_after_wt_gain_male) +
                                           (daily_kcal_tar_int_healthy_male*
                                              int_training_days_after_wt_gain_male))
  
  # Total first year calorie needs for all underweight male athletes during and after weight gain
  
  total_first_year_kcal_wt_gain_male <- vv((total_kcal_wt_gain_male + 
                                             first_year_kcal_after_wt_gain_male)*
                                            underweight_male,
                                           var_CV, n = 1)
  
  #########################################################
  
  ## Total first-year calorie needs for female athletes below healthy BMI range
  
  # Change weight gain weeks for female athletes into days
  
  wt_gain_week_female_in_days <- wt_gain_week_female * 7
  
  ## Intensive training period (training before games) comes after normal period
  # Calculate total days where underweight athletes will do normal or intensive training during weight gain period
  
  wt_gain_int_training_days_female <- abs(wt_gain_week_female_in_days - normal_training)
  wt_gain_nor_training_days_female <- ifelse(normal_training >= wt_gain_week_female_in_days,
                                             wt_gain_week_female_in_days, 
                                             (wt_gain_week_female_in_days - 
                                                wt_gain_int_training_days_female))
  
  
  # Base calorie needs for an underweight female athlete during weight gain period
  
  base_kcal_need_during_wt_gain_female <- ifelse(wt_gain_week_female_in_days <= normal_training,
                                                 (daily_kcal_tar_nor_female*wt_gain_week_female_in_days),
                                                 ((daily_kcal_tar_nor_female*wt_gain_nor_training_days_female)+
                                                    daily_kcal_tar_int_female*wt_gain_int_training_days_female))
  
  # Total calorie needs for an underweight male athlete during weight gain period
  
  total_kcal_wt_gain_female <- base_kcal_need_during_wt_gain_female + extra_kcal_female
  
  ####################
  
  # Total calorie needs for an underweight female (now normal weight) after weight gain period
  
  total_training_days <- normal_training + intensive_training
  training_days_after_wt_gain_female <- total_training_days - wt_gain_week_female_in_days
  nor_training_days_after_wt_gain_female <- ifelse(training_days_after_wt_gain_female > intensive_training,
                                                   (training_days_after_wt_gain_female - 
                                                      intensive_training), 0)
  
  
  int_training_days_after_wt_gain_female <- ifelse(training_days_after_wt_gain_female > intensive_training,
                                                   (training_days_after_wt_gain_female -
                                                      nor_training_days_after_wt_gain_female),
                                                   training_days_after_wt_gain_female)
  
  first_year_kcal_after_wt_gain_female <- ((daily_kcal_tar_nor_healthy_female*
                                              nor_training_days_after_wt_gain_female) +
                                             (daily_kcal_tar_int_healthy_female*
                                                int_training_days_after_wt_gain_female))
  
  
  # Total first year calorie needs for all underweight female athletes during and after weight gain
  
  total_first_year_kcal_wt_gain_female <- vv((total_kcal_wt_gain_female + 
                                               first_year_kcal_after_wt_gain_female)*
                                              underweight_female,
                                             var_CV, n = 1)
  
  ########################################################
  
  ## Total calorie needs for male athletes above body fat % threshold for the first year
  # This is calculated assuming their training regimen remains unchanged- any additional
  # workout done by the athletes to reduce weight should be reported and considered
  
  # Change weight loss weeks for male athletes into days
  wt_loss_week_male_in_days <- wt_loss_week_male * 7
  
  # Calculate total days where overweight athletes will do normal or intensive training during weight loss period
  wt_loss_int_training_days_male <- abs(wt_loss_week_male_in_days - normal_training)
  wt_loss_nor_training_days_male <- ifelse(wt_loss_week_male_in_days <= normal_training,
                                           wt_loss_week_male_in_days,
                                           (wt_loss_week_male_in_days - 
                                              wt_loss_int_training_days_male))
  
  # Base calorie needs for an overweight male athlete during weight loss period
  base_kcal_need_during_wt_loss_male <- ifelse(wt_loss_week_male_in_days <= normal_training,
                                               (daily_kcal_tar_nor_male *
                                                  wt_loss_week_male_in_days),
                                               ((daily_kcal_tar_nor_male*
                                                   wt_loss_nor_training_days_male) +
                                                  (daily_kcal_tar_int_male *
                                                     wt_loss_int_training_days_male)))
  
  # Total calorie needs for an overweight male athlete during weight loss period
  total_kcal_wt_loss_male <- base_kcal_need_during_wt_loss_male - kcal_deduct_male
  
  ####################
  
  # Total calorie needs for an overweight male (now normal weight) after weight loss period
  total_training_days <- normal_training + intensive_training
  training_days_after_wt_loss_male <- total_training_days - wt_loss_week_male_in_days
  nor_training_days_after_wt_loss_male <- ifelse(training_days_after_wt_loss_male > intensive_training,
                                                 (training_days_after_wt_loss_male -
                                                    intensive_training), 0)
  
  
  int_training_days_after_wt_loss_male <- ifelse(training_days_after_wt_loss_male > intensive_training,
                                                 (training_days_after_wt_loss_male -
                                                    nor_training_days_after_wt_loss_male),
                                                 training_days_after_wt_loss_male)
  
  first_year_kcal_after_wt_loss_male <- ((daily_kcal_tar_nor_healthy_male*
                                            nor_training_days_after_wt_loss_male) +
                                           (daily_kcal_tar_int_healthy_male*
                                              int_training_days_after_wt_loss_male))
  
  # Total first year calorie needs for all overweight male athletes during and after weight loss
  
  total_first_year_kcal_wt_loss_male <- vv((total_kcal_wt_loss_male + 
                                             first_year_kcal_after_wt_loss_male)*
                                            number_overweight_male,
                                           var_CV, n = 1)
  
  ############################################
  
  ## Total calorie needs for female athletes above body fat % threshold for the first year
  # This is calculated assuming their training regimen remains unchanged- any additional
  # workout done by the athletes to reduce weight should be reported and considered
  
  # Change weight loss weeks for female athletes into days
  wt_loss_week_female_in_days <- wt_loss_week_female * 7
  
  # Calculate total days where overweight athletes will do normal or intensive training during weight loss period
  wt_loss_int_training_days_female <- abs(wt_loss_week_female_in_days - normal_training)
  wt_loss_nor_training_days_female <- ifelse(wt_loss_week_female_in_days <= normal_training,
                                             wt_loss_week_female_in_days,
                                             (wt_loss_week_female_in_days - 
                                                wt_loss_int_training_days_female))
  
  # Base calorie needs for an overweight female athlete during weight loss period
  base_kcal_need_during_wt_loss_female <- ifelse(wt_loss_week_female_in_days <= normal_training,
                                                 (daily_kcal_tar_nor_female *
                                                    wt_loss_week_female_in_days),
                                                 ((daily_kcal_tar_nor_female*
                                                     wt_loss_nor_training_days_female) +
                                                    (daily_kcal_tar_int_female *
                                                       wt_loss_int_training_days_female)))
  
  # Total calorie needs for an overweight female athlete during weight loss period
  total_kcal_wt_loss_female <- base_kcal_need_during_wt_loss_female - kcal_deduct_female
  
  ####################
  
  # Total calorie needs for an overweight female (now normal weight) after weight loss period
  total_training_days <- normal_training + intensive_training
  training_days_after_wt_loss_female <- total_training_days - wt_loss_week_female_in_days
  nor_training_days_after_wt_loss_female <- ifelse(training_days_after_wt_loss_female > intensive_training,
                                                   (training_days_after_wt_loss_female -
                                                      intensive_training), 0)
  
  
  int_training_days_after_wt_loss_female <- ifelse(training_days_after_wt_loss_female > intensive_training,
                                                   (training_days_after_wt_loss_female -
                                                      nor_training_days_after_wt_loss_female),
                                                   training_days_after_wt_loss_female)
  
  first_year_kcal_after_wt_loss_female <- ((daily_kcal_tar_nor_healthy_female*
                                              nor_training_days_after_wt_loss_female) +
                                             (daily_kcal_tar_int_healthy_female*
                                                int_training_days_after_wt_loss_female))
  
  # Total first year calorie needs for all overweight female athletes during and after weight loss
  
  total_first_year_kcal_wt_loss_female <- vv((total_kcal_wt_loss_female + 
                                               first_year_kcal_after_wt_loss_female)*
                                              number_overweight_female,
                                             var_CV, n = 1)
  
  
  ##############################################
  
  # Calculate total yearly calorie needs for male athletes for first year
  
  total_first_year_kcal_need_male <- vv(total_first_year_kcal_healthy_male + 
                                         total_first_year_kcal_wt_gain_male + 
                                         total_first_year_kcal_wt_loss_male,
                                        var_CV, n = 1)
  
  # Calculate total yearly calorie needs for female athletes for first year
  
  total_first_year_kcal_need_female <- vv(total_first_year_kcal_healthy_female +  
                                           total_first_year_kcal_wt_gain_female + 
                                           total_first_year_kcal_wt_loss_female,
                                          var_CV, n = 1)
  
  
  # Calculate overall yearly calorie needs for first year
  
  overall_first_year_kcal_need <- vv(total_first_year_kcal_need_male +
                                      total_first_year_kcal_need_female,
                                     var_CV, n = 1)
  
  
################################################################################ 
  
  
  ## For following years (except first year)
  
  # I calculate again for following years because the assumption is after first year
  # all athletes will be within normal weight range- so not the same weight as in 
  # the input table
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
################################################################################
  
  ## Calculate extra calorie provided during intervention (to calculate cost) 
  
  # Calculate current yearly calorie intake for male athletes
  
  # Calculate extra yearly calorie provided for male athletes 
  
  # Calculate current yearly calorie intake for female athletes
  
  # Calculate extra yearly calorie provided for female athletes 
  
  # Calculate overall yearly extra calorie provided
  
  
################################################################################ 
  
  #### Protein
  
  
  
  
################################################################################ 
  
  ### Carbohydrate
  
  
  
  
################################################################################ 
  
  ### Fat
  
  
  
  
################################################################################ 
 
  
  
  


  
  
  
  
  
  
}
