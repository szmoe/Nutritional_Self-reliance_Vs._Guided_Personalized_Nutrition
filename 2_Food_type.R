
## Model 2: Food and Micronutrients

# Here I don't try to change the food type and match with the timing for intervention.
# I just follow the simplified current intake format because I will need to key in 
# a large food database otherwise. So, the model focus more on the quantity of food, not on
# quality and timing (model limitations).

library(decisionSupport)

# Make variables

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("2_Food_type.csv"))) # ignore warnings here
# model doesn't allow 0 in the input range. So, I changed them to 0.001. 
# The values should be constant but I pool different items, so they became like a range.
# To be a real range, I need to estimate them myself- food databases only report one value, not range. 
# This is also a limitation. Pooling food items is not a good idea- coz blank range can exist if outliers are present.
# Should use constant throughout (if use range, it should be narrow) and separate the food items- 
# dataset will be REALLY big though.

## Fixed the warnings coz model won't run. The nutrient values are increased to make the model work.
## Another reason not to pool the food items (even same food but different species) together unless 
## we are sure there's no outliers. 
## I also remove outliers value for Fe and some foods have 0 value for Fe but model won't accept. 
## So, not the best solution to increase Fe value for 0 and remove outliers. Better not group different food together.

Food_function <- function(x, varnames){
  
  ## Change 100g value to value per serve 
  
  # Chicken
  
  chicken_raw_per_serve_kcal <- chicken_raw_100g_kcal * chicken_gram_per_serve/100
  
  chicken_raw_per_serve_protein <- chicken_raw_100g_protein * chicken_gram_per_serve/100
  
  chicken_raw_per_serve_CHO <- chicken_raw_100g_CHO * chicken_gram_per_serve/100
  
  chicken_raw_per_serve_fat <- chicken_raw_100g_fat * chicken_gram_per_serve/100
  
  chicken_raw_per_serve_Ca <- chicken_raw_100g_Ca * chicken_gram_per_serve/100
  
  chicken_raw_per_serve_Fe <- chicken_raw_100g_Fe * chicken_gram_per_serve/100
  
  
  # Pork
  
  pork_raw_per_serve_kcal <- pork_raw_100g_kcal * pork_gram_per_serve/100
  
  pork_raw_per_serve_protein <- pork_raw_100g_protein * pork_gram_per_serve/100
  
  pork_raw_per_serve_CHO <- pork_raw_100g_CHO * pork_gram_per_serve/100
  
  pork_raw_per_serve_fat <- pork_raw_100g_fat * pork_gram_per_serve/100
  
  pork_raw_per_serve_Ca <- pork_raw_100g_Ca * pork_gram_per_serve/100
  
  pork_raw_per_serve_Fe <- pork_raw_100g_Fe * pork_gram_per_serve/100
  
                              
  # Beef
  
  beef_raw_per_serve_kcal <- beef_raw_100g_kcal * beef_gram_per_serve/100
  
  beef_raw_per_serve_protein <- beef_raw_100g_protein * beef_gram_per_serve/100
  
  beef_raw_per_serve_CHO <- beef_raw_100g_CHO * beef_gram_per_serve/100
  
  beef_raw_per_serve_fat <- beef_raw_100g_fat * beef_gram_per_serve/100
  
  beef_raw_per_serve_Ca <- beef_raw_100g_Ca * beef_gram_per_serve/100
  
  beef_raw_per_serve_Fe <- beef_raw_100g_Fe * beef_gram_per_serve/100
  
  
  # Fish
  
  fish_raw_per_serve_kcal <- fish_raw_100g_kcal * fish_gram_per_serve/100
  
  fish_raw_per_serve_protein <- fish_raw_100g_protein * fish_gram_per_serve/100
  
  fish_raw_per_serve_CHO <- fish_raw_100g_CHO * fish_gram_per_serve/100
  
  fish_raw_per_serve_fat <- fish_raw_100g_fat * fish_gram_per_serve/100
  
  fish_raw_per_serve_Ca <- fish_raw_100g_Ca * fish_gram_per_serve/100
  
  fish_raw_per_serve_Fe <- fish_raw_100g_Fe * fish_gram_per_serve/100
  
  # Prawn
  
  prawn_raw_per_serve_kcal <- prawn_raw_100g_kcal * prawn_gram_per_serve/100
  
  prawn_raw_per_serve_protein <- prawn_raw_100g_protein * prawn_gram_per_serve/100
  
  prawn_raw_per_serve_CHO <- prawn_raw_100g_CHO * prawn_gram_per_serve/100
  
  prawn_raw_per_serve_fat <- prawn_raw_100g_fat * prawn_gram_per_serve/100
  
  prawn_raw_per_serve_Ca <- prawn_raw_100g_Ca * prawn_gram_per_serve/100
  
  prawn_raw_per_serve_Fe <- prawn_raw_100g_Fe * prawn_gram_per_serve/100
  
  
  # Vegetable/Mushroom
  
  vegetable_mushroom_raw_per_serve_kcal <- vegetable_mushroom_raw_100g_kcal * vegetable_mushroom_gram_per_serve/100
  
  vegetable_mushroom_raw_per_serve_protein <- vegetable_mushroom_raw_100g_protein * vegetable_mushroom_gram_per_serve/100
  
  vegetable_mushroom_raw_per_serve_CHO <- vegetable_mushroom_raw_100g_CHO * vegetable_mushroom_gram_per_serve/100
  
  vegetable_mushroom_raw_per_serve_fat <- vegetable_mushroom_raw_100g_fat * vegetable_mushroom_gram_per_serve/100
  
  vegetable_mushroom_raw_per_serve_Ca <- vegetable_mushroom_raw_100g_Ca * vegetable_mushroom_gram_per_serve/100
  
  vegetable_mushroom_raw_per_serve_Fe <- vegetable_mushroom_raw_100g_Fe * vegetable_mushroom_gram_per_serve/100
  
  
  # Bean/Nut 
  # (I use same serving size for bean as nut because beans was rarely on the menu- upset stomach risk?)
  
  bean_nut_raw_per_serve_kcal <- bean_nut_raw_100g_kcal * bean_nut_gram_per_serve/100
  
  bean_nut_raw_per_serve_protein <- bean_nut_raw_100g_protein * bean_nut_gram_per_serve/100
  
  bean_nut_raw_per_serve_CHO <- bean_nut_raw_100g_CHO * bean_nut_gram_per_serve/100
  
  bean_nut_raw_per_serve_fat <- bean_nut_raw_100g_fat * bean_nut_gram_per_serve/100
  
  bean_nut_raw_per_serve_Ca <- bean_nut_raw_100g_Ca * bean_nut_gram_per_serve/100
  
  bean_nut_raw_per_serve_Fe <- bean_nut_raw_100g_Fe * bean_nut_gram_per_serve/100
  
  
  # Egg
  
  egg_whole_per_serve_kcal <- egg_whole_100g_kcal * egg_whole_gram_per_serve/100
  
  egg_whole_per_serve_protein <- egg_whole_100g_protein * egg_whole_gram_per_serve/100
  
  egg_whole_per_serve_CHO <- egg_whole_100g_CHO * egg_whole_gram_per_serve/100
  
  egg_whole_per_serve_fat <- egg_whole_100g_fat * egg_whole_gram_per_serve/100
  
  egg_whole_per_serve_Ca <- egg_whole_100g_Ca * egg_whole_gram_per_serve/100
  
  egg_whole_per_serve_Fe <- egg_whole_100g_Fe * egg_whole_gram_per_serve/100
  
  
  # Dairy
  
  dairy_per_serve_kcal <- dairy_100g_kcal * dairy_ml_per_serve/100
  
  dairy_per_serve_protein <- dairy_100g_protein * dairy_ml_per_serve/100
  
  dairy_per_serve_CHO <- dairy_100g_CHO * dairy_ml_per_serve/100
  
  dairy_per_serve_fat <- dairy_100g_fat * dairy_ml_per_serve/100
  
  dairy_per_serve_Ca <- dairy_100g_Ca * dairy_ml_per_serve/100
  
  dairy_per_serve_Fe <- dairy_100g_Fe * dairy_ml_per_serve/100
  
  
  # Fruit
  
  fruit_per_serve_kcal <- fruit_100g_kcal * fruit_gram_per_serve/100
  
  fruit_per_serve_protein <- fruit_100g_protein * fruit_gram_per_serve/100
  
  fruit_per_serve_CHO <- fruit_100g_CHO * fruit_gram_per_serve/100
  
  fruit_per_serve_fat <- fruit_100g_fat * fruit_gram_per_serve/100
  
  fruit_per_serve_Ca <- fruit_100g_Ca * fruit_gram_per_serve/100
  
  fruit_per_serve_Fe <- fruit_100g_Fe * fruit_gram_per_serve/100
  
  
  # Rice/Noodle
  
  rice_noodle_cooked_per_serve_kcal <- rice_noodle_cooked_100g_kcal * rice_noodle_gram_per_serve/100
  
  rice_noodle_cooked_per_serve_protein <- rice_noodle_cooked_100g_protein * rice_noodle_gram_per_serve/100
  
  rice_noodle_cooked_per_serve_CHO <- rice_noodle_cooked_100g_CHO * rice_noodle_gram_per_serve/100
  
  rice_noodle_cooked_per_serve_fat <- rice_noodle_cooked_100g_fat * rice_noodle_gram_per_serve/100
  
  rice_noodle_cooked_per_serve_Ca <- rice_noodle_cooked_100g_Ca * rice_noodle_gram_per_serve/100
  
  rice_noodle_cooked_per_serve_Fe <- rice_noodle_cooked_100g_Fe * rice_noodle_gram_per_serve/100
  

  # Sandwich
  
  sandwich_per_serve_kcal <- sandwich_100g_kcal * sandwich_gram_per_serve/100
  
  sandwich_per_serve_protein <- sandwich_100g_protein * sandwich_gram_per_serve/100
  
  sandwich_per_serve_CHO <- sandwich_100g_CHO * sandwich_gram_per_serve/100
  
  sandwich_per_serve_fat <- sandwich_100g_fat * sandwich_gram_per_serve/100
  
  sandwich_per_serve_Ca <- sandwich_100g_Ca * sandwich_gram_per_serve/100
  
  sandwich_per_serve_Fe <- sandwich_100g_Fe * sandwich_gram_per_serve/100
  
  
  # Oil/fat
  
  oil_fat_per_serve_kcal <- oil_fat_100g_kcal * oil_fat_gram_per_serve/100
  
  oil_fat_per_serve_protein <- oil_fat_100g_protein * oil_fat_gram_per_serve/100
  
  oil_fat_per_serve_CHO <- oil_fat_100g_CHO * oil_fat_gram_per_serve/100
  
  oil_fat_per_serve_fat <- oil_fat_100g_fat * oil_fat_gram_per_serve/100
  
  oil_fat_per_serve_Ca <- oil_fat_100g_Ca * oil_fat_gram_per_serve/100
  
  oil_fat_per_serve_Fe <- oil_fat_100g_Fe * oil_fat_gram_per_serve/100
  
  
#######################################################################################
  
  ## Calculate macronutrient distribution of current intake
  
  
  # Calculate total number of main meals 
  
  total_training_days_per_year <- normal_training_days + intensive_training_days
  
  total_main_meals_per_year_per_person <- vv(number_meal_per_day * total_training_days_per_year,
                                             var_CV, n_year)
  
  
  # Calculate total nutrient distributions for everyday food
  
  total_current_everyday_food_kcal_per_year_per_person <- vv(((current_bean_nut_intake_serve_per_day * 
                                                       bean_nut_raw_per_serve_kcal) +
                                                      (current_egg_intake_serve_per_day *
                                                         egg_whole_per_serve_kcal) +
                                                      (current_dairy_intake_serve_per_day *
                                                         dairy_per_serve_kcal) +
                                                      (current_fruit_intake_serve_per_day *
                                                         fruit_per_serve_kcal) +
                                                      (current_oil_fat_intake_serve_per_day *
                                                         oil_fat_per_serve_kcal) +
                                                      (sports_drink_bot_per_day * 
                                                         sports_drink_kcal) +
                                                      (current_sandwich_intake_serve_per_day *
                                                         sandwich_per_serve_kcal)) * 
                                                       total_training_days_per_year,
                                                    var_CV, n_year)
  
  total_current_everyday_food_protein_per_year_per_person <- vv(((current_bean_nut_intake_serve_per_day * 
                                                       bean_nut_raw_per_serve_protein) +
                                                      (current_egg_intake_serve_per_day *
                                                         egg_whole_per_serve_protein) +
                                                      (current_dairy_intake_serve_per_day *
                                                         dairy_per_serve_protein) +
                                                      (current_fruit_intake_serve_per_day *
                                                         fruit_per_serve_protein) +
                                                      (current_oil_fat_intake_serve_per_day *
                                                         oil_fat_per_serve_protein) +
                                                      (sports_drink_bot_per_day * 
                                                         sports_drink_protein) +
                                                      (current_sandwich_intake_serve_per_day *
                                                         sandwich_per_serve_protein))*
                                                        total_training_days_per_year,
                                                    var_CV, n_year)
  
  total_current_everyday_food_CHO_per_year_per_person <- vv(((current_bean_nut_intake_serve_per_day * 
                                                       bean_nut_raw_per_serve_CHO) +
                                                      (current_egg_intake_serve_per_day *
                                                         egg_whole_per_serve_CHO) +
                                                      (current_dairy_intake_serve_per_day *
                                                         dairy_per_serve_CHO) +
                                                      (current_fruit_intake_serve_per_day *
                                                         fruit_per_serve_CHO) +
                                                      (current_oil_fat_intake_serve_per_day *
                                                         oil_fat_per_serve_CHO) +
                                                      (sports_drink_bot_per_day * 
                                                         sports_drink_CHO) +
                                                      (current_sandwich_intake_serve_per_day *
                                                         sandwich_per_serve_CHO))*
                                                      total_training_days_per_year,
                                                    var_CV, n_year)
  
  total_current_everyday_food_fat_per_year_per_person <- vv(((current_bean_nut_intake_serve_per_day * 
                                                       bean_nut_raw_per_serve_fat) +
                                                      (current_egg_intake_serve_per_day *
                                                         egg_whole_per_serve_fat) +
                                                      (current_dairy_intake_serve_per_day *
                                                         dairy_per_serve_fat) +
                                                      (current_fruit_intake_serve_per_day *
                                                         fruit_per_serve_fat) +
                                                      (current_oil_fat_intake_serve_per_day *
                                                         oil_fat_per_serve_fat) +
                                                      (current_sandwich_intake_serve_per_day *
                                                         sandwich_per_serve_fat))*
                                                      total_training_days_per_year,
                                                    var_CV, n_year)
  
  total_current_everyday_food_Ca_per_year_per_person <- vv(((current_bean_nut_intake_serve_per_day * 
                                                      bean_nut_raw_per_serve_Ca) +
                                                     (current_egg_intake_serve_per_day *
                                                        egg_whole_per_serve_Ca) +
                                                     (current_dairy_intake_serve_per_day *
                                                        dairy_per_serve_Ca) +
                                                     (current_fruit_intake_serve_per_day *
                                                        fruit_per_serve_Ca) +
                                                     (current_oil_fat_intake_serve_per_day *
                                                        oil_fat_per_serve_Ca) +
                                                     (current_sandwich_intake_serve_per_day *
                                                        sandwich_per_serve_Ca))*
                                                     total_training_days_per_year,
                                                   var_CV, n_year)
  
  total_current_everyday_food_Fe_per_year_per_person <- vv(((current_bean_nut_intake_serve_per_day * 
                                                      bean_nut_raw_per_serve_Fe) +
                                                     (current_egg_intake_serve_per_day *
                                                        egg_whole_per_serve_Fe) +
                                                     (current_dairy_intake_serve_per_day *
                                                        dairy_per_serve_Fe) +
                                                     (current_fruit_intake_serve_per_day *
                                                        fruit_per_serve_Fe) +
                                                     (current_oil_fat_intake_serve_per_day *
                                                        oil_fat_per_serve_Fe) +
                                                     (current_sandwich_intake_serve_per_day *
                                                        sandwich_per_serve_Fe))*
                                                     total_training_days_per_year,
                                                   var_CV, n_year)
  
  
  ## Calculate total nutrient distributions for protein food
  
  # Calculate number of meals for each protein food
  
   number_meals_chicken_per_year_per_person <- vv(total_main_meals_per_year_per_person *
                                                    chicken_frequency,
                                                  var_CV, n_year)
   
   number_meals_pork_per_year_per_person <- vv(total_main_meals_per_year_per_person *
                                                    pork_frequency,
                                                  var_CV, n_year)
   
   number_meals_beef_per_year_per_person <- vv(total_main_meals_per_year_per_person *
                                                    beef_frequency,
                                                  var_CV, n_year)
   
   number_meals_fish_per_year_per_person <- vv(total_main_meals_per_year_per_person *
                                                    fish_frequency,
                                                  var_CV, n_year)
   
   number_meals_prawn_per_year_per_person <- vv(total_main_meals_per_year_per_person *
                                                    prawn_frequency,
                                                  var_CV, n_year)
   
   
   # Calculate nutrient distributions for all protein food and vegetables group
   
   total_current_protein_food_kcal_per_year_per_person <- vv((number_meals_chicken_per_year_per_person*
                                                                current_chicken_intake_serve_per_meal*
                                                                chicken_raw_per_serve_kcal) +
                                                               (number_meals_pork_per_year_per_person*
                                                                  current_pork_intake_serve_per_meal*
                                                                  pork_raw_per_serve_kcal) +
                                                               (number_meals_beef_per_year_per_person*
                                                                  current_beef_intake_serve_per_meal*
                                                                  beef_raw_per_serve_kcal) +
                                                               (number_meals_fish_per_year_per_person*
                                                                  current_fish_intake_serve_per_meal*
                                                                  fish_raw_per_serve_kcal) +
                                                               (number_meals_prawn_per_year_per_person*
                                                                  current_prawn_intake_serve_per_meal*
                                                                  prawn_raw_per_serve_kcal) +
                                                               (current_vegetable_mushroom_intake_serve_per_meal*
                                                                  vegetable_mushroom_raw_per_serve_kcal*
                                                                  total_main_meals_per_year_per_person),
                                                             var_CV, n_year)
   
   
   total_current_protein_food_protein_per_year_per_person <- vv((number_meals_chicken_per_year_per_person*
                                                                current_chicken_intake_serve_per_meal*
                                                                chicken_raw_per_serve_protein) +
                                                               (number_meals_pork_per_year_per_person*
                                                                  current_pork_intake_serve_per_meal*
                                                                  pork_raw_per_serve_protein) +
                                                               (number_meals_beef_per_year_per_person*
                                                                  current_beef_intake_serve_per_meal*
                                                                  beef_raw_per_serve_protein) +
                                                               (number_meals_fish_per_year_per_person*
                                                                  current_fish_intake_serve_per_meal*
                                                                  fish_raw_per_serve_protein) +
                                                               (number_meals_prawn_per_year_per_person*
                                                                  current_prawn_intake_serve_per_meal*
                                                                  prawn_raw_per_serve_protein) +
                                                                 (current_vegetable_mushroom_intake_serve_per_meal*
                                                                    vegetable_mushroom_raw_per_serve_protein*
                                                                    total_main_meals_per_year_per_person),
                                                             var_CV, n_year)
   
   
   total_current_protein_food_CHO_per_year_per_person <- vv((number_meals_chicken_per_year_per_person*
                                                                current_chicken_intake_serve_per_meal*
                                                                chicken_raw_per_serve_CHO) +
                                                               (number_meals_pork_per_year_per_person*
                                                                  current_pork_intake_serve_per_meal*
                                                                  pork_raw_per_serve_CHO) +
                                                               (number_meals_beef_per_year_per_person*
                                                                  current_beef_intake_serve_per_meal*
                                                                  beef_raw_per_serve_CHO) +
                                                               (number_meals_fish_per_year_per_person*
                                                                  current_fish_intake_serve_per_meal*
                                                                  fish_raw_per_serve_CHO) +
                                                               (number_meals_prawn_per_year_per_person*
                                                                  current_prawn_intake_serve_per_meal*
                                                                  prawn_raw_per_serve_CHO) +
                                                              (current_vegetable_mushroom_intake_serve_per_meal*
                                                                 vegetable_mushroom_raw_per_serve_CHO*
                                                                 total_main_meals_per_year_per_person),
                                                             var_CV, n_year)
   
   total_current_protein_food_fat_per_year_per_person <- vv((number_meals_chicken_per_year_per_person*
                                                                current_chicken_intake_serve_per_meal*
                                                                chicken_raw_per_serve_fat) +
                                                               (number_meals_pork_per_year_per_person*
                                                                  current_pork_intake_serve_per_meal*
                                                                  pork_raw_per_serve_fat) +
                                                               (number_meals_beef_per_year_per_person*
                                                                  current_beef_intake_serve_per_meal*
                                                                  beef_raw_per_serve_fat) +
                                                               (number_meals_fish_per_year_per_person*
                                                                  current_fish_intake_serve_per_meal*
                                                                  fish_raw_per_serve_fat) +
                                                               (number_meals_prawn_per_year_per_person*
                                                                  current_prawn_intake_serve_per_meal*
                                                                  prawn_raw_per_serve_fat)+
                                                              (current_vegetable_mushroom_intake_serve_per_meal*
                                                                 vegetable_mushroom_raw_per_serve_fat*
                                                                 total_main_meals_per_year_per_person),
                                                             var_CV, n_year)
   
   total_current_protein_food_Ca_per_year_per_person <- vv((number_meals_chicken_per_year_per_person*
                                                               current_chicken_intake_serve_per_meal*
                                                               chicken_raw_per_serve_Ca) +
                                                              (number_meals_pork_per_year_per_person*
                                                                 current_pork_intake_serve_per_meal*
                                                                 pork_raw_per_serve_Ca) +
                                                              (number_meals_beef_per_year_per_person*
                                                                 current_beef_intake_serve_per_meal*
                                                                 beef_raw_per_serve_Ca) +
                                                              (number_meals_fish_per_year_per_person*
                                                                 current_fish_intake_serve_per_meal*
                                                                 fish_raw_per_serve_Ca) +
                                                              (number_meals_prawn_per_year_per_person*
                                                                 current_prawn_intake_serve_per_meal*
                                                                 prawn_raw_per_serve_Ca) +
                                                             (current_vegetable_mushroom_intake_serve_per_meal*
                                                                vegetable_mushroom_raw_per_serve_Ca*
                                                                total_main_meals_per_year_per_person),
                                                            var_CV, n_year)
   
   total_current_protein_food_Fe_per_year_per_person <- vv((number_meals_chicken_per_year_per_person*
                                                               current_chicken_intake_serve_per_meal*
                                                               chicken_raw_per_serve_Fe) +
                                                              (number_meals_pork_per_year_per_person*
                                                                 current_pork_intake_serve_per_meal*
                                                                 pork_raw_per_serve_Fe) +
                                                              (number_meals_beef_per_year_per_person*
                                                                 current_beef_intake_serve_per_meal*
                                                                 beef_raw_per_serve_Fe) +
                                                              (number_meals_fish_per_year_per_person*
                                                                 current_fish_intake_serve_per_meal*
                                                                 fish_raw_per_serve_Fe) +
                                                              (number_meals_prawn_per_year_per_person*
                                                                 current_prawn_intake_serve_per_meal*
                                                                 prawn_raw_per_serve_Fe) +
                                                             (current_vegetable_mushroom_intake_serve_per_meal*
                                                                vegetable_mushroom_raw_per_serve_Fe*
                                                                total_main_meals_per_year_per_person),
                                                            var_CV, n_year)
   


  # Adjust calorie using rice 
  # I didn't estimate rice intake but use rice to adjust the model, because athletes may eat
  # food not provided by the kitchen (buy outside food, soft drinks, etc.). Food stalls in the vicinity
  # of MFF aren't known for nutritious foods, so I pool them all together as rice. So, any extra calorie
  # after deducting all calorie would be calorie from rice group. This works because I only want to know
  # if their macronutrient distribution is a problem or not- no need to go detail on their actual intake of nutrients.
   
  # Calculate total calorie for adjusting (calorie of rice/noodle group)
   
   total_current_rice_noodle_kcal_per_year_per_person <- vv(((current_kcal_intake_yearly_female+
                                            current_kcal_intake_yearly_male)/number_athlete) - 
                                           (total_current_everyday_food_kcal_per_year_per_person +
                                              total_current_protein_food_kcal_per_year_per_person),
                                         var_CV, n_year)
  

  # Change gram of rice into serve of rice
  
  rice_serve <- vv(total_current_rice_noodle_kcal_per_year_per_person/ rice_noodle_cooked_per_serve_kcal,
                   var_CV, n_year)
  
  # Calculate nutrient distribution for rice/noodle intake
  
  total_current_rice_noodle_protein_per_year_per_person <- vv(rice_serve*
                                                             rice_noodle_cooked_per_serve_protein,
                                                           var_CV, n_year )
  
  total_current_rice_noodle_CHO_per_year_per_person <- vv(rice_serve*
                                                             rice_noodle_cooked_per_serve_CHO,
                                                           var_CV, n_year )
  
  total_current_rice_noodle_fat_per_year_per_person <- vv(rice_serve*
                                                             rice_noodle_cooked_per_serve_fat,
                                                           var_CV, n_year )
  
  total_current_rice_noodle_Ca_per_year_per_person <- vv(rice_serve*
                                                             rice_noodle_cooked_per_serve_Ca,
                                                           var_CV, n_year )
  
  total_current_rice_noodle_Fe_per_year_per_person <- vv(rice_serve*
                                                             rice_noodle_cooked_per_serve_Fe,
                                                           var_CV, n_year )
  
  
  # Calculate total kcal, carb, protein, fat, calcium and iron for one person
  
  total_current_kcal_intake_per_year_per_person <- vv(total_current_everyday_food_kcal_per_year_per_person +
                                                        total_current_protein_food_kcal_per_year_per_person +
                                                        total_current_rice_noodle_kcal_per_year_per_person,
                                                      var_CV, n_year)
  
  total_current_protein_intake_per_year_per_person <- vv(total_current_everyday_food_protein_per_year_per_person +
                                                        total_current_protein_food_protein_per_year_per_person +
                                                        total_current_rice_noodle_protein_per_year_per_person,
                                                      var_CV, n_year)
  
  total_current_CHO_intake_per_year_per_person <- vv(total_current_everyday_food_CHO_per_year_per_person +
                                                        total_current_protein_food_CHO_per_year_per_person +
                                                        total_current_rice_noodle_CHO_per_year_per_person,
                                                      var_CV, n_year)
  
  total_current_fat_intake_per_year_per_person <- vv(total_current_everyday_food_fat_per_year_per_person +
                                                        total_current_protein_food_fat_per_year_per_person +
                                                        total_current_rice_noodle_fat_per_year_per_person,
                                                      var_CV, n_year)
  
  total_current_Ca_intake_per_year_per_person <- vv(total_current_everyday_food_Ca_per_year_per_person +
                                                        total_current_protein_food_Ca_per_year_per_person +
                                                        total_current_rice_noodle_Ca_per_year_per_person,
                                                      var_CV, n_year)
  
  total_current_Fe_intake_per_year_per_person <- vv(total_current_everyday_food_Fe_per_year_per_person +
                                                        total_current_protein_food_Fe_per_year_per_person +
                                                        total_current_rice_noodle_Fe_per_year_per_person,
                                                      var_CV, n_year)
  
  
  
  # Calculate total kcal, carb, protein, fat for all athletes
  
  total_current_kcal_intake_per_year_all_athlete <- vv(total_current_kcal_intake_per_year_per_person*
                                                         number_athlete,
                                                       var_CV, n_year)
  
  total_current_protein_intake_per_year_all_athlete <- vv(total_current_protein_intake_per_year_per_person*
                                                         number_athlete,
                                                       var_CV, n_year)
  
  total_current_CHO_intake_per_year_all_athlete <- vv(total_current_CHO_intake_per_year_per_person*
                                                         number_athlete,
                                                       var_CV, n_year)
  
  total_current_fat_intake_per_year_all_athlete <- vv(total_current_fat_intake_per_year_per_person*
                                                         number_athlete,
                                                       var_CV, n_year)

  
  # Model check 
  
  model_check_current_kcal <- vv(current_kcal_intake_yearly_female +
                                   current_kcal_intake_yearly_male,
                                 var_CV, n_year)
  
  total_protein_need_gram <- vv(total_protein_need_gram,
                                var_CV, n_year)
  
  total_carb_need_gram <- vv(total_carb_need_gram,
                             var_CV, n_year)
  
  total_fat_need_gram <- vv(total_fat_need_gram,
                            var_CV, n_year)
  
  total_kcal_need_yearly <- vv(total_kcal_need_female_yearly +
                                 total_kcal_need_male_yearly,
                               var_CV, n_year)
  
  
  # Yearly RDI amount for calcium and iron
  
  yearly_Ca_rdi <- vv(RDI_Ca * total_training_days_per_year,
                      var_CV, n_year)
  
  yearly_Fe_rdi <- vv(RDI_Fe * total_training_days_per_year,
                           var_CV, n_year)

  
  
################################################################################ 
  
  ### Calculate servings per food to provide based on macronutrient needs 
  
  ## Calculate serving of food to meet protein needs
  
  # Total protein needs for before and after training 
  # If there is two training sessions per day
  
  two_trainings_per_day <- chance_event(chance_two_trainings_day,
                                        value_if = 1,
                                        value_if_not = 0)
  
  before_training_protein_gram_per_year_per_person <- if(two_trainings_per_day == 1){
    before_training_protein_g * total_training_days_per_year * 2
  } else{
    before_training_protein_g * total_training_days_per_year
  }
  
  after_training_protein_gram_per_year_per_person <- if(two_trainings_per_day == 1){
    after_training_protein_g_per_kg * weight_athlete * total_training_days_per_year * 2
  } else{
    after_training_protein_g_per_kg * weight_athlete * total_training_days_per_year
  }
  
  total_before_after_training_protein_gram_per_year_per_person <- vv(before_training_protein_gram_per_year_per_person +
                                                                       after_training_protein_gram_per_year_per_person,
                                                                     var_CV, n_year)
  
  # Calculate serve of food for protein before/after training
  
  total_training_provided_protein_gram_per_year_per_person <- if(two_trainings_per_day == 1){
    ((provided_egg_intake_serve_per_day * egg_whole_per_serve_protein*4) +
      (provided_dairy_intake_serve_per_day * dairy_per_serve_protein*4) +
      (provided_sandwich_intake_serve_per_day * sandwich_per_serve_protein*4)) * total_training_days_per_year
  } else{
    ((provided_egg_intake_serve_per_day * egg_whole_per_serve_protein*2) +
        (provided_dairy_intake_serve_per_day * dairy_per_serve_protein*2) +
        (provided_sandwich_intake_serve_per_day * sandwich_per_serve_protein*2))*total_training_days_per_year
  }
  
  
  # Total egg servings 
  total_egg_serving_per_year_per_person <- if(two_trainings_per_day == 1){
    total_training_days_per_year * (provided_egg_intake_serve_per_day *4)
  } else {
    total_training_days_per_year * (provided_egg_intake_serve_per_day * 2)
  }
  
  # Total dairy servings
  total_dairy_serving_per_year_per_person <- if(two_trainings_per_day == 1){
    total_training_days_per_year * provided_dairy_intake_serve_per_day *4
  } else {
    total_training_days_per_year * provided_dairy_intake_serve_per_day * 2
  }
  
  # Total sandwich servings
  total_sandwich_serving_per_year_per_person <- if(two_trainings_per_day == 1){
    total_training_days_per_year * provided_sandwich_intake_serve_per_day * 4
  } else {
    total_training_days_per_year * provided_sandwich_intake_serve_per_day * 2
  }
  
  # Calculate protein gram from fixed serves of vegetable, bean, fruit and sports drink
  
  total_fixed_serves_protein_gram_per_year_per_person <- vv(((provided_vegetable_mushroom_serve_per_day*
                                                               vegetable_mushroom_raw_per_serve_protein) +
                                                               (provided_bean_nut_intake_serve_per_day*
                                                                  bean_nut_raw_per_serve_protein)+
                                                               (provided_fruit_intake_serve_per_day*
                                                                  fruit_per_serve_protein) +
                                                               (sports_drink_bot_per_day*
                                                                  sports_drink_protein))*
                                                              total_training_days_per_year,
                                                            var_CV, n_year)
  
  # Total vegetable/mushroom serving
  total_vegetable_mushroom_serving_per_year_per_person <- vv(provided_vegetable_mushroom_serve_per_day*
                                                               total_training_days_per_year,
                                                             var_CV, n_year)
  
  # Total bean/nut serving
  total_bean_nut_serving_per_year_per_person <- vv(provided_bean_nut_intake_serve_per_day *
                                                     total_training_days_per_year,
                                                   var_CV, n_year)
  
  # Total fruit serving
  total_fruit_serving_per_year_per_person <- vv(provided_fruit_intake_serve_per_day*
                                                  total_training_days_per_year,
                                                var_CV, n_year)
  
  # Total sports drink bottles
  total_sports_drink_serving_per_year_per_person <- vv(sports_drink_bot_per_day*
                                                         total_training_days_per_year,
                                                       var_CV, n_year)
  
  # Calculate allocated protein (gram) for main meals
  
  protein_gram_for_main_meals <- vv((total_protein_need_gram/number_athlete)- 
                                      (total_training_provided_protein_gram_per_year_per_person +
                                         total_fixed_serves_protein_gram_per_year_per_person),
                                    var_CV, n_year)
  
  
  # Calculate the new allocation of protein foods for main meals
  
  number_provided_chicken_meals_per_year_per_person <- vv(total_main_meals_per_year_per_person*
                                                            chicken_frequency,
                                                          var_CV, n_year)
  
  number_provided_pork_meals_per_year_per_person <- vv(total_main_meals_per_year_per_person*
                                                         pork_frequency_new,
                                                       var_CV, n_year)
  
  number_provided_beef_meals_per_year_per_person <- vv(total_main_meals_per_year_per_person*
                                                         beef_frequency_new,
                                                       var_CV, n_year)
  
  number_provided_fish_meals_per_year_per_person <- vv(total_main_meals_per_year_per_person*
                                                         fish_frequency_new,
                                                       var_CV, n_year)
  
  number_provided_prawn_meals_per_year_per_person <- vv(total_main_meals_per_year_per_person*
                                                          prawn_frequency,
                                                        var_CV, n_year)
  
  # Calculate total protein (gram) of provided protein foods
  
  total_provided_protein_food_gram_per_year_per_person <- vv((number_provided_chicken_meals_per_year_per_person*
                                                                provided_chicken_serve_per_meal* 0.5*
                                                                chicken_raw_per_serve_protein) +
                                                               (number_provided_pork_meals_per_year_per_person*
                                                                  provided_pork_serve_per_meal* 0.5*
                                                                  pork_raw_per_serve_protein) +
                                                               (number_provided_beef_meals_per_year_per_person*
                                                                  provided_beef_serve_per_meal* 0.5*
                                                                  beef_raw_per_serve_protein)+
                                                               (number_provided_fish_meals_per_year_per_person*
                                                                  provided_fish_serve_per_meal* 0.5*
                                                                  fish_raw_per_serve_protein)+
                                                               (number_provided_prawn_meals_per_year_per_person*
                                                                  provided_prawn_serve_per_meal* 0.5*
                                                                  prawn_raw_per_serve_protein),
                                                             var_CV, n_year)
  
  # Total chicken serving
  total_chicken_serving_per_year_per_person <- vv(provided_chicken_serve_per_meal*0.5*
                                                    total_training_days_per_year,
                                                  var_CV, n_year)
  
  # Total pork serving
  total_pork_serving_per_year_per_person <- vv(provided_pork_serve_per_meal*0.5*
                                                 total_training_days_per_year,
                                               var_CV, n_year)
  
  # Total beef serving
  total_beef_serving_per_year_per_person <- vv(provided_beef_serve_per_meal*0.5*
                                                 total_training_days_per_year,
                                               var_CV, n_year)
  
  # Total fish serving
  total_fish_serving_per_year_per_person <- vv(provided_fish_serve_per_meal*0.5*
                                                 total_training_days_per_year,
                                               var_CV, n_year)
  
  # Total prawn serving
  total_prawn_serving_per_year_per_person <- vv(provided_prawn_serve_per_meal*0.5*
                                                  total_training_days_per_year,
                                                var_CV, n_year)
  
  # Total provided protein food in gram for all athletes
  total_protein_food_gram_per_year_all_athletes <- vv((total_training_provided_protein_gram_per_year_per_person +
                                                        total_fixed_serves_protein_gram_per_year_per_person +
                                                        total_provided_protein_food_gram_per_year_per_person)*
                                                        number_athlete,
                                                      var_CV, n_year)
  
  
  ## Calculate serving of rice/noodle group to meet carbohydrate needs
  
  # Calculate carbohydate (g) provided by protein and fix-served groups
  
  total_carb_from_protein_group_gram_per_year_per_person <-vv((total_egg_serving_per_year_per_person*
                                                                 egg_whole_per_serve_CHO) +
                                                                (total_dairy_serving_per_year_per_person*
                                                                   dairy_per_serve_CHO) +
                                                                (total_sandwich_serving_per_year_per_person*
                                                                   sandwich_per_serve_CHO) +
                                                                (total_vegetable_mushroom_serving_per_year_per_person*
                                                                   vegetable_mushroom_raw_per_serve_CHO) +
                                                                (total_bean_nut_serving_per_year_per_person*
                                                                   bean_nut_raw_per_serve_CHO)+
                                                                (total_fruit_serving_per_year_per_person*
                                                                   fruit_per_serve_CHO)+
                                                                (total_sports_drink_serving_per_year_per_person*
                                                                   sports_drink_CHO)+
                                                                (total_chicken_serving_per_year_per_person*
                                                                   chicken_raw_per_serve_CHO)+
                                                                (total_pork_serving_per_year_per_person*
                                                                   pork_raw_per_serve_CHO)+
                                                                (total_beef_serving_per_year_per_person*
                                                                   beef_raw_per_serve_CHO)+
                                                                (total_fish_serving_per_year_per_person*
                                                                   fish_raw_per_serve_CHO)+
                                                                (total_prawn_serving_per_year_per_person*
                                                                   prawn_raw_per_serve_CHO),
                                                              var_CV, n_year)
  
  
  carb_from_rice_noodle_per_year_per_person <- vv((total_carb_need_gram/number_athlete) -
                                                  total_carb_from_protein_group_gram_per_year_per_person,
                                                var_CV, n_year)
  
  total_rice_noodle_serving_per_year_per_person <- vv(carb_from_rice_noodle_per_year_per_person/
                                                        rice_noodle_gram_per_serve,
                                                      var_CV, n_year)
  
  
  total_carb_food_gram_per_year_all_athletes <- vv((total_carb_from_protein_group_gram_per_year_per_person +
                                                     carb_from_rice_noodle_per_year_per_person)*
                                                     number_athlete,
                                                   var_CV, n_year)
  
 
  
  ## Calculate serving of fat/oil group to meet fat needs
  
  # Calculate fat (g) already provided by previous food groups
  total_fat_from_food_gram_per_year_per_person <- vv((total_egg_serving_per_year_per_person*
                                                        egg_whole_per_serve_fat) +
                                                       (total_dairy_serving_per_year_per_person*
                                                          dairy_per_serve_fat) +
                                                       (total_sandwich_serving_per_year_per_person*
                                                          sandwich_per_serve_fat) +
                                                       (total_vegetable_mushroom_serving_per_year_per_person*
                                                          vegetable_mushroom_raw_per_serve_fat) +
                                                       (total_bean_nut_serving_per_year_per_person*
                                                          bean_nut_raw_per_serve_fat)+
                                                       (total_fruit_serving_per_year_per_person*
                                                          fruit_per_serve_fat)+
                                                       (total_chicken_serving_per_year_per_person*
                                                          chicken_raw_per_serve_fat)+
                                                       (total_pork_serving_per_year_per_person*
                                                          pork_raw_per_serve_fat)+
                                                       (total_beef_serving_per_year_per_person*
                                                          beef_raw_per_serve_fat)+
                                                       (total_fish_serving_per_year_per_person*
                                                          fish_raw_per_serve_fat)+
                                                       (total_prawn_serving_per_year_per_person*
                                                          prawn_raw_per_serve_fat)+
                                                       (total_rice_noodle_serving_per_year_per_person*
                                                          rice_noodle_cooked_per_serve_fat),
                                                     var_CV, n_year)
  
  # Calculate remaining fat (g) to be given by fat/oil group
  fat_from_fat_oil_group_gram_per_year_per_person <- vv((total_fat_need_gram/number_athlete)-
                                                          total_fat_from_food_gram_per_year_per_person,
                                                        var_CV, n_year)
  
  total_fat_oil_serving_per_year_per_person <- vv(fat_from_fat_oil_group_gram_per_year_per_person/
                                                    oil_fat_gram_per_serve,
                                                  var_CV, n_year)
  
  total_fat_oil_provided_per_year_all_athletes <- vv((fat_from_fat_oil_group_gram_per_year_per_person +
                                                       total_fat_from_food_gram_per_year_per_person)*
                                                       number_athlete,
                                                     var_CV, n_year)
  
  
  
  # Calculate total provided protein in gram (add back protein from rice and fat groups)
  
  protein_from_rice_group_per_year_all_athletes <- vv(total_rice_noodle_serving_per_year_per_person*
                                                      rice_noodle_cooked_per_serve_protein*
                                                        number_athlete,
                                                      var_CV, n_year)
  
  
  protein_from_fat_group_per_year_all_athletes <- vv(total_fat_oil_serving_per_year_per_person*
                                                       oil_fat_per_serve_protein,
                                                     var_CV, n_year)
  
  total_protein_provided_gram_per_year_all_athletes <- vv(total_protein_food_gram_per_year_all_athletes +
                                                            protein_from_rice_group_per_year_all_athletes +
                                                            protein_from_fat_group_per_year_all_athletes,
                                                          var_CV, n_year)
  
  # Calculate total provided carbohydrate in gram (add back carb from fat groups)
  
  carb_from_fat_group_gram_per_year_all_athletes <- vv((total_fat_oil_serving_per_year_per_person*
                                                         oil_fat_per_serve_CHO)*
                                                         number_athlete,
                                                       var_CV, n_year)
  
  
  total_carb_provided_gram_per_year_all_athletes <- vv(total_carb_food_gram_per_year_all_athletes +
                                                         carb_from_fat_group_gram_per_year_all_athletes,
                                                       var_CV, n_year)
  
  # Check calorie
  
  total_kcal_provided_per_year_all_athletes <- vv((total_fat_oil_provided_per_year_all_athletes*9) +
                                                    (total_carb_provided_gram_per_year_all_athletes*4) +
                                                    (total_protein_provided_gram_per_year_all_athletes*4),
                                                  var_CV, n_year)
  
  # Calculate provided calcium
  
  total_Ca_provided_per_year_per_person <- vv(((total_egg_serving_per_year_per_person*
                                                   egg_whole_per_serve_Ca) +
                                                  (total_dairy_serving_per_year_per_person*
                                                     dairy_per_serve_Ca) +
                                                  (total_sandwich_serving_per_year_per_person*
                                                     sandwich_per_serve_Ca) +
                                                  (total_vegetable_mushroom_serving_per_year_per_person*
                                                     vegetable_mushroom_raw_per_serve_Ca) +
                                                  (total_bean_nut_serving_per_year_per_person*
                                                     bean_nut_raw_per_serve_Ca)+
                                                  (total_fruit_serving_per_year_per_person*
                                                     fruit_per_serve_Ca)+
                                                  (total_chicken_serving_per_year_per_person*
                                                     chicken_raw_per_serve_Ca)+
                                                  (total_pork_serving_per_year_per_person*
                                                     pork_raw_per_serve_Ca)+
                                                  (total_beef_serving_per_year_per_person*
                                                     beef_raw_per_serve_Ca)+
                                                  (total_fish_serving_per_year_per_person*
                                                     fish_raw_per_serve_Ca)+
                                                  (total_prawn_serving_per_year_per_person*
                                                     prawn_raw_per_serve_Ca)+
                                                  (total_rice_noodle_serving_per_year_per_person*
                                                     rice_noodle_cooked_per_serve_Ca) +
                                                  (total_fat_oil_serving_per_year_per_person*
                                                     oil_fat_per_serve_Ca)),
                                                var_CV, n_year)
  
  # Calculate provided iron
  
  total_Fe_provided_per_year_per_person <- vv(((total_egg_serving_per_year_per_person*
                                                    egg_whole_per_serve_Fe) +
                                                   (total_dairy_serving_per_year_per_person*
                                                      dairy_per_serve_Fe) +
                                                   (total_sandwich_serving_per_year_per_person*
                                                      sandwich_per_serve_Fe) +
                                                   (total_vegetable_mushroom_serving_per_year_per_person*
                                                      vegetable_mushroom_raw_per_serve_Fe) +
                                                   (total_bean_nut_serving_per_year_per_person*
                                                      bean_nut_raw_per_serve_Fe)+
                                                   (total_fruit_serving_per_year_per_person*
                                                      fruit_per_serve_Fe)+
                                                   (total_chicken_serving_per_year_per_person*
                                                      chicken_raw_per_serve_Fe)+
                                                   (total_pork_serving_per_year_per_person*
                                                      pork_raw_per_serve_Fe)+
                                                   (total_beef_serving_per_year_per_person*
                                                      beef_raw_per_serve_Fe)+
                                                   (total_fish_serving_per_year_per_person*
                                                      fish_raw_per_serve_Fe)+
                                                   (total_prawn_serving_per_year_per_person*
                                                      prawn_raw_per_serve_Fe)+
                                                   (total_rice_noodle_serving_per_year_per_person*
                                                      rice_noodle_cooked_per_serve_Fe) +
                                                   (total_fat_oil_serving_per_year_per_person*
                                                      oil_fat_per_serve_Fe)),
                                                var_CV, n_year)
  
  
  # Total food servings for all athletes (to be used for next model)
  
  total_egg_serving_per_year_all_athletes <- vv(total_egg_serving_per_year_per_person *
                                                  number_athlete,
                                                var_CV, n_year)
  
  total_dairy_serving_per_year_all_athletes <- vv(total_dairy_serving_per_year_per_person*
                                                    number_athlete,
                                                  var_CV, n_year)
  
  total_sandwich_serving_per_year_all_athletes <- vv(total_sandwich_serving_per_year_per_person*
                                                       number_athlete,
                                                     var_CV, n_year)
  
  total_vegetable_mushroom_serving_per_year_all_athletes <- vv(total_vegetable_mushroom_serving_per_year_per_person*
                                                                 number_athlete,
                                                               var_CV, n_year)
  
  total_bean_nut_serving_per_year_all_athletes <- vv(total_bean_nut_serving_per_year_per_person*
                                                       number_athlete,
                                                     var_CV, n_year)
  
  total_fruit_serving_per_year_all_athletes <- vv(total_fruit_serving_per_year_per_person*
                                                    number_athlete,
                                                  var_CV, n_year)
  
  total_sports_drink_serving_per_year_all_athletes <- vv(total_sports_drink_serving_per_year_per_person*
                                                           number_athlete,
                                                         var_CV, n_year)
  
  total_chicken_serving_per_year_all_athletes <- vv(total_chicken_serving_per_year_per_person*
                                                      number_athlete,
                                                    var_CV, n_year)
  
  total_pork_serving_per_year_all_athletes <- vv(total_pork_serving_per_year_per_person*
                                                   number_athlete,
                                                 var_CV, n_year)
  
  total_beef_serving_per_year_all_athletes <- vv(total_beef_serving_per_year_per_person*
                                                   number_athlete,
                                                 var_CV, n_year)
  
  total_fish_serving_per_year_all_athletes <- vv(total_fish_serving_per_year_per_person*
                                                   number_athlete,
                                                 var_CV, n_year)
  
  total_prawn_serving_per_year_all_athletes <- vv(total_prawn_serving_per_year_per_person*
                                                    number_athlete,
                                                  var_CV, n_year)
  
  total_rice_noodle_serving_per_year_all_athletes <- vv(total_rice_noodle_serving_per_year_per_person*
                                                          number_athlete,
                                                        var_CV, n_year)
  
  total_fat_oil_serving_per_year_all_athletes <- vv(total_fat_oil_serving_per_year_per_person*
                                                      number_athlete,
                                                    var_CV, n_year)
  

  
################################################################################ 
  
  
  return(list(total_current_Ca_intake_per_year_per_person = sum(total_current_Ca_intake_per_year_per_person),
              total_current_Fe_intake_per_year_per_person = sum(total_current_Ca_intake_per_year_per_person),
              yearly_Ca_rdi = sum(yearly_Ca_rdi),
              yearly_Fe_rdi = sum(yearly_Fe_rdi),
              total_current_kcal_intake_per_year_all_athlete = sum(total_current_kcal_intake_per_year_all_athlete),
              total_kcal_need_yearly = sum(total_kcal_need_yearly),
              model_check_current_kcal = sum(model_check_current_kcal),
              total_current_protein_intake_per_year_all_athlete = sum(total_current_protein_intake_per_year_all_athlete),
              total_protein_need_gram = sum(total_protein_need_gram),
              total_current_CHO_intake_per_year_all_athlete = sum(total_current_CHO_intake_per_year_all_athlete),
              total_carb_need_gram = sum(total_carb_need_gram),
              total_current_fat_intake_per_year_all_athlete = sum(total_current_fat_intake_per_year_all_athlete),
              total_fat_need_gram = sum(total_fat_need_gram),
              total_before_after_training_protein_gram_per_year_per_person = sum(total_before_after_training_protein_gram_per_year_per_person),
              total_training_provided_protein_gram_per_year_per_person = sum(total_training_provided_protein_gram_per_year_per_person),
              total_protein_provided_gram_per_year_all_athletes = sum(total_protein_provided_gram_per_year_all_athletes),
              total_carb_provided_gram_per_year_all_athletes = sum(total_carb_provided_gram_per_year_all_athletes),
              total_fat_oil_provided_per_year_all_athletes = sum(total_fat_oil_provided_per_year_all_athletes),
              total_kcal_provided_per_year_all_athletes = sum(total_kcal_provided_per_year_all_athletes),
              total_Ca_provided_per_year_per_person = sum(total_Ca_provided_per_year_per_person),
              total_Fe_provided_per_year_per_person = sum(total_Fe_provided_per_year_per_person),
              total_egg_serving_per_year_all_athletes = sum(total_egg_serving_per_year_all_athletes),
              total_dairy_serving_per_year_all_athletes = sum(total_dairy_serving_per_year_all_athletes),
              total_sandwich_serving_per_year_all_athletes = sum(total_sandwich_serving_per_year_all_athletes),
              total_vegetable_mushroom_serving_per_year_all_athletes = sum(total_vegetable_mushroom_serving_per_year_all_athletes),
              total_bean_nut_serving_per_year_all_athletes = sum(total_bean_nut_serving_per_year_all_athletes),
              total_fruit_serving_per_year_all_athletes = sum(total_fruit_serving_per_year_all_athletes),
              total_sports_drink_serving_per_year_all_athletes = sum(total_sports_drink_serving_per_year_all_athletes),
              total_chicken_serving_per_year_all_athletes = sum(total_chicken_serving_per_year_all_athletes),
              total_pork_serving_per_year_all_athletes = sum(total_pork_serving_per_year_all_athletes),
              total_beef_serving_per_year_all_athletes = sum(total_beef_serving_per_year_all_athletes),
              total_fish_serving_per_year_all_athletes = sum(total_fish_serving_per_year_all_athletes),
              total_prawn_serving_per_year_all_athletes = sum(total_prawn_serving_per_year_all_athletes),
              total_rice_noodle_serving_per_year_all_athletes = sum(total_rice_noodle_serving_per_year_all_athletes),
              total_fat_oil_serving_per_year_all_athletes = sum(total_fat_oil_serving_per_year_all_athletes)
              ))
  
}

# Run the Monte Carlo Simulation

Food_mc_simulation <- mcSimulation(estimate = estimate_read_csv("2_Food_type.csv"),
                                      model_function = Food_function,
                                      numberOfModelRuns = 1000,
                                      functionSyntax = "plainNames")


# Plot distributions histogram

plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_current_kcal_intake_per_year_all_athlete", "model_check_current_kcal"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Calorie (kcal)',
                   base_size = 7) # model check successful

plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_current_kcal_intake_per_year_all_athlete", "total_kcal_need_yearly"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Calorie (kcal)',
                   base_size = 7) # same result as model 1; kcal seems okay despite potentially slightly lower kcal intake

plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_current_protein_intake_per_year_all_athlete", "total_protein_need_gram"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Protein (g)',
                   base_size = 7) # protein intake is low compared to protein need 
                                  # so kcal differences might come from lower protein intake

plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_current_CHO_intake_per_year_all_athlete", "total_carb_need_gram"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Carbohydrate (g)',
                   base_size = 7) # carb intake can be okay, but range is wider towards lower extreme

plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_current_fat_intake_per_year_all_athlete", "total_fat_need_gram"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Fat (g)',
                   base_size = 7) # fat intake is okay (although I don't separate good vs. bad fats)-prob this is bad fat

## So, the problem lies in protein and carb intake. Kcal and fat intake are okay. 

plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_current_Ca_intake_per_year_per_person", "yearly_Ca_rdi"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Calcium (mg)',
                   base_size = 7) # large range, need more info- need more detailing of food intake

plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_current_Fe_intake_per_year_per_person", "yearly_Fe_rdi"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Iron (mg)',
                   base_size = 7) # Model is to know macronutrient distribution
                                  # So, it doesn't work well with micronutrients- need more detailing of food intake

## Ca and Fe current intakes could be okay (need exact food type to be sure)

######################################################################################

plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_before_after_training_protein_gram_per_year_per_person",
                            "total_training_provided_protein_gram_per_year_per_person"), 
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Protein (g)',
                   base_size = 7) # protein need and provided protein (before/after training) match- model successful

plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_protein_need_gram", "total_protein_provided_gram_per_year_all_athletes"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Protein (g)',
                   base_size = 7) # Not exact match but still okay- model successful
                                  # it can be coz I separate training days using chance_event
                                  # Training days are not separated when estimating for total protein needs
                               

plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_carb_provided_gram_per_year_all_athletes", "total_carb_need_gram"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Carbohydrate (g)',
                   base_size = 7) # carb need and provided carb match- model successful


plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_fat_oil_provided_per_year_all_athletes", "total_fat_need_gram"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Fat (g)',
                   base_size = 7) # fat need and provided fat match- model successful


plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_kcal_provided_per_year_all_athletes", "total_kcal_need_yearly"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Calorie (kcal)',
                   base_size = 7) # kcal need and provided kcal match- model successful


plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_Ca_provided_per_year_per_person", "yearly_Ca_rdi"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Calcium (mg)',
                   base_size = 7) # same trend as current Ca intake- need exact type of food for micronutrient simulation i think


plot_distributions(mcSimulation_object = Food_mc_simulation,
                   vars = c("total_Fe_provided_per_year_per_person", "yearly_Fe_rdi"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Iron (mg)',
                   base_size = 7) # Fe provided is a lot higher than Fe needs (need exact type of food)



Food_mc_simulation <- decisionSupport("2_Food_type.csv",
                                         outputPath = 'results_Food',
                                         welfareFunction = Food_function,
                                         numberOfModelRuns = 1000,
                                         functionSyntax = "plainNames")

read.csv("results_Food/mcSimulationResults.csv")  


## Model 2 successful- move on to model 3




















