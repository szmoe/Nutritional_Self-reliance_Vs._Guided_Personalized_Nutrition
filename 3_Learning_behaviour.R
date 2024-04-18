
### Model 3: Impact of learning outcome and behavioural change on nutrition intake

# In the weather forecast example, predictions are more accurate for near future, and
# become less accurate further into the future. I followed this example to build this model-
# choose input variables that can be estimated confidently using present and past knowledge.
# For example, instead of asking the athletes "the chance of good learning outcome after the training"
# (which is in the future), I would ask their level of interests, their level of motivation and 
# what would they need to have good learning outcome (which are all easily answerable). And I use
# these bits of information to predict the chance of good learning outcome. This reduces uncertainty
# but that doesn't mean I would have much narrower range of estimates, esp if I'm dealing with a large group of people.
# Meaning if I directly ask for estimates of good learning outcome, there will be two layers of uncertainty-
# the uncertainty about learning outcome and the uncertainty of the variation among the whole group- which can
# result in a wider range of estimates. If I use the second method of breaking down into links, there will
# only be one layer of uncertainty (most of the time), i.e., the uncertainty about the variation (only if there are 
# more than one phenomenon, e.g., a population). So, with a lesser layer of uncertainty, the second method can result
# in narrower range, but won't necessarily narrow by a wide margin if the population is large and heterogeneous. 
# Even so, I think the second method will give a stronger model with more logical links??? It may also prevent wrong
# assumption of having effect/correlation when there is none (overestimation problem)????


library(decisionSupport)

# Make variables

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("3_Learning_behaviour.csv")))

Learning_behaviour_function <- function(x, varnames){

  
  ### Learning outcome
  
  ## Intervention one
  
  # Calculate percent contribution of having prior knowledge to good learning outcome
  
  prior_knowledge_inter_one <- vv(((prior_knowledge_ch_1*weight_contribution_ch_1) +
                                    (prior_knowledge_ch_2*weight_contribution_ch_2) +
                                    (prior_knowledge_ch_3*weight_contribution_ch_3) +
                                    (prior_knowledge_ch_4*weight_contribution_ch_4))/
                                  (weight_contribution_ch_1 +
                                   weight_contribution_ch_2 +
                                   weight_contribution_ch_3 +
                                   weight_contribution_ch_4),
                                 var_CV, n_year)
  
  # Calculate improved knowledge with good trainer conditional on having prior knowledge
  
  knowledge_to_learning_outcome_inter_one <- vv(prior_knowledge_inter_one *
                                                  (((chance_of_good_trainer *
                                                       weight_good_trainer_to_successful_ch1) +
                                                      (chance_of_good_trainer *
                                                         weight_good_trainer_to_successful_ch2) +
                                                      (chance_of_good_trainer *
                                                         weight_good_trainer_to_successful_ch3) +
                                                      (chance_of_good_trainer *
                                                         weight_good_trainer_to_successful_ch4))/
                                                     (weight_good_trainer_to_successful_ch1 +
                                                        weight_good_trainer_to_successful_ch2 +
                                                        weight_good_trainer_to_successful_ch3 +
                                                        weight_good_trainer_to_successful_ch4)),
                                                var_CV, n_year)
  
  # Calculate athletes' interest percent
  
  chance_athletes_interest_inter_one <- vv(((chance_of_good_learning_material*
                                               weight_learning_material_to_interest) +
                                              (chance_of_good_trainer*weight_good_trainer_to_interest) +
                                              (chance_athletes_understand_nutrition_importance*
                                                 weight_understand_nutrition_importance_to_interest) +
                                              (chance_of_good_learning_hour*
                                                 weight_learning_hour_to_interest_in_class))/
                                             (weight_learning_material_to_interest +
                                                weight_good_trainer_to_interest +
                                                weight_understand_nutrition_importance_to_interest +
                                                weight_learning_hour_to_interest_in_class),
                                           var_CV, n_year)
  
  
  # Calculate athlete's motivation percent
  
  chance_athletes_motivation_inter_one <- vv(((chance_of_good_learning_material*
                                                 weight_learning_material_to_motivation_inter_one) +
                                                (chance_of_good_trainer*
                                                   weight_good_trainer_to_motivation_inter_one) +
                                                (chance_of_good_facility*
                                                   weight_good_facility_to_motivation) +
                                                (chance_athletes_understand_nutrition_importance*
                                                   weight_understand_nutrition_importance_to_motivation))/
                                               (weight_learning_material_to_motivation_inter_one +
                                                  weight_good_trainer_to_motivation_inter_two +
                                                  weight_good_facility_to_motivation +
                                                  weight_understand_nutrition_importance_to_motivation),
                                             var_CV, n_year)
  
  
  # Calculate athletes' persistence percent
  
  chance_athletes_persistence_inter_one <- vv(chance_athletes_understand_nutrition_importance*
                                                weight_understand_nutrition_importance_to_persistence,
                                              var_CV, n_year)
  
  # Calculate chance of good learning outcome
  
  good_learning_outcome_inter_one <- vv(((knowledge_to_learning_outcome_inter_one*
                                           weight_knowledge_to_learning_success) +
                                           (chance_athletes_interest_inter_one*
                                              weight_interest_to_learning_success) +
                                           (chance_athletes_motivation_inter_one*
                                              weight_motivation_to_learning_success) +
                                           (chance_athletes_persistence_inter_one*
                                              weight_persistence_to_learning_success))/
                                          (weight_knowledge_to_learning_success +
                                             weight_interest_to_learning_success +
                                             weight_motivation_to_learning_success +
                                             weight_persistence_to_learning_success),
                                        var_CV, n_year)
  
  
  #################################
  
  ## Intervention two
  
  # Calculate percent contribution of having prior knowledge to good learning outcome
  
  prior_knowledge_inter_two <- vv(((prior_knowledge_ch_1*
                                     weight_contribution_ch_1) +
                                     (prior_knowledge_ch_2*
                                        weight_contribution_ch_2))/
                                    (weight_contribution_ch_1 +
                                       weight_contribution_ch_2),
                                  var_CV, n_year)
  
  # Calculate improved knowledge with good trainer conditional on having prior knowledge
  
  knowledge_to_learning_outcome_inter_two <- vv(prior_knowledge_inter_two *
                                                  (((chance_of_good_trainer*
                                                       weight_good_trainer_to_successful_ch1) +
                                                      (chance_of_good_trainer*
                                                         weight_good_trainer_to_successful_ch2))/
                                                     (weight_good_trainer_to_successful_ch1 +
                                                        weight_good_trainer_to_successful_ch2)),
                                                var_CV, n_year)
  
  # Calculate athletes' interest percent
  
  chance_athletes_interest_inter_two <- vv(((chance_of_good_learning_hour*
                                              weight_learning_hour_to_interest_in_class) +
                                              (chance_of_good_learning_material*
                                                 weight_learning_material_to_interest) +
                                              (chance_of_good_trainer*
                                                 weight_good_trainer_to_interest) +
                                              (chance_athletes_understand_nutrition_importance*
                                                 weight_understand_nutrition_importance_to_interest))/
                                             (weight_learning_hour_to_interest_in_class +
                                                weight_learning_material_to_interest +
                                                weight_good_trainer_to_interest +
                                                weight_understand_nutrition_importance_to_interest),
                                           var_CV, n_year)
  
  
  # Calculate athlete's motivation percent
  
  chance_athletes_motivation_inter_two <- vv(((chance_of_good_learning_material*
                                                 weight_learning_material_to_motivation_inter_two) +
                                                (chance_of_good_trainer*
                                                   weight_good_trainer_to_motivation_inter_two) +
                                                (chance_of_good_facility*
                                                   weight_good_facility_to_motivation) +
                                                (chance_athletes_understand_nutrition_importance*
                                                   weight_understand_nutrition_importance_to_motivation))/
                                               (weight_learning_material_to_motivation_inter_two +
                                                  weight_good_trainer_to_motivation_inter_two +
                                                  weight_good_facility_to_motivation +
                                                  weight_understand_nutrition_importance_to_motivation),
                                             var_CV, n_year)
  
  

  # Calculate chance of good learning outcome
  
  good_learning_outcome_inter_two <- vv(((knowledge_to_learning_outcome_inter_two*
                                            weight_knowledge_to_learning_success) +
                                           (chance_athletes_interest_inter_two*
                                              weight_interest_to_learning_success) +
                                           (chance_athletes_motivation_inter_two*
                                              weight_motivation_to_learning_success))/
                                          (weight_knowledge_to_learning_success +
                                             weight_interest_to_learning_success +
                                             weight_motivation_to_learning_success),
                                        var_CV, n_year)
  
  
  ################################################################################
  
  ### Maintain good nutrition
  
  # Chance of good food safety
  
  chance_good_food_safety <- vv(((chance_good_kitchen_staff*
                                    weight_good_kitchen_staff_to_good_food_safety) +
                                   (chance_regular_audit*
                                      weight_regular_audit_to_good_food_safety))/
                                  (weight_good_kitchen_staff_to_good_food_safety +
                                     weight_regular_audit_to_good_food_safety),
                                var_CV, n_year)
  
  # Chance of delicious meals
  
  good_kitchen_staff <- chance_event(chance_good_kitchen_staff,
                                     value_if = 1,
                                     value_if_not = 0)
  
  chance_tasty_meal <- if(good_kitchen_staff == 1){
    chance_tasty_food_conditional_on_good_kitchen_staff
  }else{
    1 - chance_tasty_food_conditional_on_good_kitchen_staff
  }
  
  ## Intervention one
  
  # Calculate percent probability that athletes will maintain good nutrition habits
  # 100% probability means their actual intake will match the amount of nutrients provided
  
  maintain_good_nutrition_inter_one <- vv(((good_learning_outcome_inter_one*
                                              weight_learning_outcome_to_maintain_good_nutrition_inter_one) +
                                             (chance_athletes_interest_inter_one*
                                                weight_interest_to_maintain_good_nutrition_inter_one) +
                                             (chance_athletes_understand_nutrition_importance*
                                                weight_understand_importance_to_maintain_good_nutrition_inter_one) +
                                             (chance_athletes_motivation_inter_one*
                                                weight_motivation_to_maintain_good_nutrition_inter_one) +
                                             (chance_athletes_persistence_inter_one*
                                                weight_persistence_to_maintain_good_nutrition_inter_one) +
                                             (chance_good_nutritionist_inter_one*
                                                weight_good_nutritionist_to_maintain_good_nutrition) +
                                             (chance_good_support*
                                                weight_good_support_to_maintain_good_nutrition_inter_one) +
                                             (chance_good_food_safety*
                                                weight_good_food_safety_to_maintain_good_nutrition) +
                                             (chance_tasty_meal*
                                                weight_tasty_food_to_maintain_good_nutrition) +
                                             (chance_good_kitchen_staff*
                                                weight_good_kitchen_staff_to_maintain_good_nutrition_inter_one) +
                                             (chance_athletes_good_health*
                                                weight_good_health_to_maintain_good_nutrition))/
                                            (weight_learning_outcome_to_maintain_good_nutrition_inter_one +
                                               weight_interest_to_maintain_good_nutrition_inter_one +
                                               weight_understand_importance_to_maintain_good_nutrition_inter_one +
                                               weight_motivation_to_maintain_good_nutrition_inter_one +
                                               weight_persistence_to_maintain_good_nutrition_inter_one +
                                               weight_good_nutritionist_to_maintain_good_nutrition +
                                               weight_good_support_to_maintain_good_nutrition_inter_one +
                                               weight_good_food_safety_to_maintain_good_nutrition +
                                               weight_tasty_food_to_maintain_good_nutrition +
                                               weight_good_kitchen_staff_to_maintain_good_nutrition_inter_one +
                                               weight_good_health_to_maintain_good_nutrition),
                                          var_CV, n_year)
  

  
  ## Intervention two
  
  # Calculate percent probability that athletes will maintain good nutrition habits
  
  maintain_good_nutrition_inter_two <- vv(((good_learning_outcome_inter_two*
                                              weight_learning_outcome_to_maintain_good_nutrition_inter_two) +
                                             (chance_athletes_interest_inter_two*
                                                weight_interest_to_maintain_good_nutrition_inter_two) +
                                             (chance_athletes_understand_nutrition_importance*
                                                weight_understand_importance_to_maintain_good_nutrition_inter_two) +
                                             (chance_athletes_motivation_inter_two*
                                                weight_motivation_to_maintain_good_nutrition_inter_two) +
                                             (chance_good_nutrition_team_inter_two*
                                                weight_good_nutrition_team_to_maintain_good_nutrition) +
                                             (chance_good_support*
                                                weight_good_support_to_maintain_good_nutrition_inter_two) +
                                             (chance_good_food_safety*
                                                weight_good_food_safety_to_maintain_good_nutrition) +
                                             (chance_tasty_meal*
                                                weight_tasty_food_to_maintain_good_nutrition) +
                                             (chance_good_kitchen_staff*
                                                weight_good_kitchen_staff_to_maintain_good_nutrition_inter_two) +
                                             (chance_athletes_good_health*
                                                weight_good_health_to_maintain_good_nutrition))/
                                            (weight_learning_outcome_to_maintain_good_nutrition_inter_two +
                                               weight_interest_to_maintain_good_nutrition_inter_two +
                                               weight_understand_importance_to_maintain_good_nutrition_inter_two +
                                               weight_motivation_to_maintain_good_nutrition_inter_two +
                                               weight_good_nutrition_team_to_maintain_good_nutrition +
                                               weight_good_support_to_maintain_good_nutrition_inter_two +
                                               weight_good_food_safety_to_maintain_good_nutrition +
                                               weight_tasty_food_to_maintain_good_nutrition +
                                               weight_good_kitchen_staff_to_maintain_good_nutrition_inter_two +
                                               weight_good_health_to_maintain_good_nutrition),
                                          var_CV, n_year)

  
  ################################################################################
  
  ### Calculate actual intake and potential food waste (from leftover/uneaten food only)
  
  ## Dietary restriction
  
  # This is the only risk added to the nutrition outcome model. I take risk as the opposite of chance.
  # For example, risk of having a bad trainer is (1 - chance of having a good trainer). So, no additional
  # risks are added, instead I just reduce the chance (I initially added risk of junk food and intake issue
  # due to personal problem but I think it might be redundant, since I already reduce the chance. Risk may
  # only required if it is a case of chance_event?)
  
  # Here the first assumption is a small fraction of athletes have dietary restriction, possibly to pork, beef, prawn,
  # egg and diary. The second assumption is each of those athlete only have one type of dietary restriction (they might
  # have more than one type but here it can be quite rare because allergy risk is low). So, a person who
  # restrict eating pork will eat beef, prawn, egg and diary. 
  
  # Calculate number of athlete with dietary restriction
  
  number_athlete_dietary_restriction <- percent_athlete_with_dietary_restriction * number_athlete
  
  # Calculate percent athlete restricting each type of food 
  
  percent_athlete_restrict_pork <- vv(pork_restriction * (1- beef_restriction) *
                                        (1- prawn_restriction) * (1- egg_restriction)*
                                        (1- dairy_restriction), var_CV, n_year)
  
  percent_athlete_restrict_beef <- vv(beef_restriction * (1- pork_restriction) *
                                        (1- prawn_restriction) * (1- egg_restriction)*
                                        (1- dairy_restriction), var_CV, n_year)
  
  percent_athlete_restrict_prawn <- vv(prawn_restriction * (1- pork_restriction)*
                                         (1- beef_restriction)* (1- egg_restriction)*
                                         (1- dairy_restriction), var_CV, n_year)
  
  percent_athlete_restrict_egg <- vv(egg_restriction* (1- pork_restriction)*
                                       (1- beef_restriction)* (1- prawn_restriction)*
                                       (1- dairy_restriction), var_CV, n_year)
  
  percent_athlete_restrict_dairy <- vv(dairy_restriction* (1- pork_restriction)*
                                         (1- beef_restriction)* (1- prawn_restriction)*
                                         (1- egg_restriction), var_CV, n_year)
  
  
  # Normalize the percent so the percent value add up to 1
  
  total_percent <- (percent_athlete_restrict_pork + percent_athlete_restrict_dairy +
                        percent_athlete_restrict_beef + percent_athlete_restrict_egg +
                        percent_athlete_restrict_prawn)
    
  
  percent_athlete_restrict_pork_normalized <- vv(percent_athlete_restrict_pork/total_percent,
                                                 var_CV, n_year)
  
  percent_athlete_restrict_beef_normalized <- vv(percent_athlete_restrict_beef/ total_percent,
                                                 var_CV, n_year)
  
  percent_athlete_restrict_prawn_normalized <- vv(percent_athlete_restrict_prawn/total_percent,
                                                  var_CV, n_year)
  
  percent_athlete_restrict_egg_normalized <- vv(percent_athlete_restrict_egg/total_percent,
                                                var_CV, n_year)
  
  percent_athlete_restrict_dairy_normalized <- vv(percent_athlete_restrict_dairy/total_percent,
                                                  var_CV, n_year)
  
  # Calculate number of athlete for each type of dietary restriction
  
  number_restrict_pork <- round(percent_athlete_restrict_pork_normalized*
                                  number_athlete_dietary_restriction, 0)
  
  number_restrict_beef <- round(percent_athlete_restrict_beef_normalized*
                                  number_athlete_dietary_restriction, 0)
  
  number_restrict_prawn <- round(percent_athlete_restrict_prawn_normalized*
                                   number_athlete_dietary_restriction, 0)
  
  number_restrict_egg <- round(percent_athlete_restrict_egg_normalized*
                                 number_athlete_dietary_restriction, 0)
  
  number_restrict_dairy <- round(percent_athlete_restrict_dairy_normalized*
                                   number_athlete_dietary_restriction, 0)
  
  # Calculate number of serving to minus out from total servings
  
  restrict_pork_serving <- vv((total_pork_serving_per_person/n_year)* # divide by n_year because they are total 4 years value
                                number_restrict_pork, var_CV, n_year)
  
  restrict_beef_serving <- vv((total_beef_serving_per_person/n_year)*
                                number_restrict_beef, var_CV, n_year)
  
  restrict_prawn_serving <- vv((total_prawn_serving_per_person/n_year)*
                                 number_restrict_prawn, var_CV, n_year)
  
  restrict_egg_serving <- vv((total_egg_serving_per_person/n_year)*
                               number_restrict_egg, var_CV, n_year)
  
  restrict_dairy_serving <- vv((total_dairy_serving_per_person/n_year)*
                                 number_restrict_dairy, var_CV, n_year)
  
  # Calculate adjusted total servings 
  
  total_pork_serving_per_year_adjusted_athletes <- vv((total_pork_serving_all_athletes/n_year) -
                                                        restrict_pork_serving, var_CV, n_year)
  
  total_beef_serving_per_year_adjusted_athletes <- vv((total_beef_serving_all_athletes/n_year) -
                                                        restrict_beef_serving, var_CV, n_year)
  
  total_prawn_serving_per_year_adjusted_athletes <- vv((total_prawn_serving_all_athletes/n_year) -
                                                         restrict_prawn_serving, var_CV, n_year)
  
  total_egg_serving_per_year_adjusted_athletes <- vv((total_egg_serving_all_athletes/n_year) -
                                                       restrict_egg_serving, var_CV, n_year)
  
  total_dairy_serving_per_year_adjusted_athletes <- vv((total_dairy_serving_all_athletes/n_year) -
                                                         restrict_dairy_serving, var_CV, n_year)
  
  ## Calculate actual intake (Intervention one)
  
  # Here I differentiate between not eating because of bad dietary habits (esp don't like food) and
  # not eating because not enough food was provided. So, I chose (need experts' input here) vegetable,
  # bean, fish as potential "don't-like" food (can be improved by intervention), and egg, dairy, beef, pork, prawn as
  # restricted due to health or religious reason (assuming no replacement is given- this is also a limitation, I need
  # to do more calculation to adjust those. Not worth it since these portions are quite small). 
  
  # Another assumption, especially for intervention 1 is that they may like the food and enough food is also 
  # provided, but they don't meet intake (or increase intake- need more links for this, I didn't include this
  # in these models) because their learning outcome and chance of maintaining good nutrition are not good 
  # (they don't know how to manage diet well or just not interested). While it is a possibility, I assume the chance
  # is quite low- for example, protein food will be provided in serving sizes and there is still a nutritionist around
  # for consultation. And the nutritionist and the kitchen will collaborate to provide adequate amount of food in 
  # appropriate serving size and also measuring tools will be provided. In intervention 1, 
  # athletes are expected to serve themselves, but all food will be provided more or less the amount they need- 
  # not much extra. That means they will all get around the same portion they need, unless their learning outcome
  # and chance of maintaining good nutrition is extremely low. Things will work out otherwise.  
  
  # This chance is also very slim for intervention 2 where the kitchen staff are expected to fill the food trays
  # for athletes. So, I won't consider the chance of happening for both interventions. 
  
  # But that chance will become important if I consider outside food. Here, the model is built as if outside food
  # is non-existence, and that means there's no chance of over-nutrition but only under-nutrition, no replacement food. 
  # Assumption here is they will eat the food provided or just starve. This is also a limitation of the model. If I
  # consider outside food, there will be a whole new model with lots of calculations. I rather not go into detail for
  # a practice. 
  
  actual_egg_serving_per_year_all_athletes_inter_one <- vv(total_egg_serving_per_year_adjusted_athletes,
                                                          var_CV, n_year)
  
  actual_dairy_serving_per_year_all_athletes_inter_one <- vv(total_dairy_serving_per_year_adjusted_athletes,
                                                            var_CV, n_year)
  
  actual_sandwich_serving_per_year_all_athletes_inter_one <- vv(total_sandwich_serving_all_athletes/n_year,
                                                                var_CV, n_year) #values will sum to 4 years again in return function, so divide by 4 now
  
  actual_vegetable_mushroom_serving_per_year_all_athletes_inter_one <- vv(maintain_good_nutrition_inter_one*
                                                                   (total_vegetable_mushroom_serving_all_athletes/n_year),
                                                                 var_CV, n_year)
  
  actual_bean_nut_serving_per_year_all_athletes_inter_one <- vv(maintain_good_nutrition_inter_one*
                                                                  (total_bean_nut_serving_all_athletes/n_year),
                                                                var_CV, n_year)
  
  actual_fruit_serving_per_year_all_athletes_inter_one <- vv(total_fruit_serving_all_athletes/n_year,
                                                             var_CV, n_year)
  
  actual_sports_drink_serving_per_year_all_athletes_inter_one <- vv(total_sports_drink_serving_all_athletes/n_year,
                                                                    var_CV, n_year)
  
  actual_chicken_serving_per_year_all_athletes_inter_one <- vv(total_chicken_serving_all_athletes/n_year,
                                                               var_CV, n_year)
  
  actual_pork_serving_per_year_all_athletes_inter_one <- vv(total_pork_serving_per_year_adjusted_athletes/n_year,
                                                            var_CV, n_year)
  
  actual_beef_serving_per_year_all_athletes_inter_one <- vv(total_beef_serving_per_year_adjusted_athletes/n_year,
                                                            var_CV, n_year)
  
  actual_fish_serving_per_year_all_athletes_inter_one <- vv(maintain_good_nutrition_inter_one*
                                                              (total_fish_serving_all_athletes/n_year),
                                                            var_CV, n_year)
  
  actual_prawn_serving_per_year_all_athletes_inter_one <- vv(total_prawn_serving_per_year_adjusted_athletes,
                                                             var_CV, n_year)
  
  actual_rice_noodle_serving_per_year_all_athletes_inter_one <- vv(total_rice_noodle_serving_all_athletes/n_year,
                                                                   var_CV, n_year)
  
  actual_fat_oil_serving_per_year_all_athletes_inter_one <- vv(total_fat_oil_serving_all_athletes/n_year,
                                                               var_CV, n_year)
  
  ## Calculate actual intake (intervention 2)
  
  actual_egg_serving_per_year_all_athletes_inter_two <- vv(total_egg_serving_per_year_adjusted_athletes,
                                                           var_CV, n_year)
  
  actual_dairy_serving_per_year_all_athletes_inter_two <- vv(total_dairy_serving_per_year_adjusted_athletes,
                                                             var_CV, n_year)
  
  actual_sandwich_serving_per_year_all_athletes_inter_two <- vv(total_sandwich_serving_all_athletes/n_year,
                                                                var_CV, n_year)
  
  actual_vegetable_mushroom_serving_per_year_all_athletes_inter_two <- vv(maintain_good_nutrition_inter_two*
                                                                   (total_vegetable_mushroom_serving_all_athletes/n_year),
                                                                 var_CV, n_year)
  
  actual_bean_nut_serving_per_year_all_athletes_inter_two <- vv(maintain_good_nutrition_inter_two*
                                                                  (total_bean_nut_serving_all_athletes/n_year),
                                                                var_CV, n_year)
  
  actual_fruit_serving_per_year_all_athletes_inter_two <- vv(total_fruit_serving_all_athletes/n_year,
                                                             var_CV, n_year)
  
  actual_sports_drink_serving_per_year_all_athletes_inter_two <- vv(total_sports_drink_serving_all_athletes/n_year,
                                                                    var_CV, n_year)
  
  actual_chicken_serving_per_year_all_athletes_inter_two <- vv(total_chicken_serving_all_athletes/n_year,
                                                               var_CV, n_year)
  
  actual_pork_serving_per_year_all_athletes_inter_two <- vv(total_pork_serving_per_year_adjusted_athletes,
                                                            var_CV, n_year)
  
  actual_beef_serving_per_year_all_athletes_inter_two <- vv(total_beef_serving_per_year_adjusted_athletes,
                                                            var_CV, n_year)
  
  actual_fish_serving_per_year_all_athletes_inter_two <- vv(maintain_good_nutrition_inter_two*
                                                              (total_fish_serving_all_athletes/n_year),
                                                            var_CV, n_year)
  
  actual_prawn_serving_per_year_all_athletes_inter_two <- vv(total_prawn_serving_per_year_adjusted_athletes,
                                                             var_CV, n_year)
  
  actual_rice_noodle_serving_per_year_all_athletes_inter_two <- vv(total_rice_noodle_serving_all_athletes/n_year,
                                                                   var_CV, n_year)
  
  actual_fat_oil_serving_per_year_all_athletes_inter_two <- vv(total_fat_oil_serving_all_athletes/n_year,
                                                               var_CV, n_year)
  

  # Add total serving for all food in the function because I want to make comparison in plots
  
  total_egg_serving_per_year_all_athletes <- vv(total_egg_serving_all_athletes/n_year,
                                                var_CV, n_year)
  
  total_dairy_serving_per_year_all_athletes <- vv(total_dairy_serving_all_athletes/n_year,
                                                  var_CV, n_year)
  
  total_sandwich_serving_per_year_all_athletes <- vv(total_sandwich_serving_all_athletes/n_year,
                                                     var_CV, n_year)
  
  total_vegetable_mushroom_serving_per_year_all_athletes <- vv(total_vegetable_mushroom_serving_all_athletes/n_year,
                                                               var_CV, n_year)
  
  total_bean_nut_serving_per_year_all_athletes <- vv(total_bean_nut_serving_all_athletes/n_year,
                                                     var_CV, n_year)
  
  total_fruit_serving_per_year_all_athletes <- vv(total_fruit_serving_all_athletes/n_year,
                                                  var_CV, n_year)
  
  total_sports_drink_serving_per_year_all_athletes <- vv(total_sports_drink_serving_all_athletes/n_year,
                                                         var_CV, n_year)
  
  total_chicken_serving_per_year_all_athletes <- vv(total_chicken_serving_all_athletes/n_year,
                                                    var_CV, n_year)
  
  total_pork_serving_per_year_all_athletes <- vv(total_pork_serving_all_athletes/n_year,
                                                 var_CV, n_year)
  
  total_fish_serving_per_year_all_athletes <- vv(total_fish_serving_all_athletes/n_year,
                                                 var_CV, n_year)
  
  total_beef_serving_per_year_all_athletes <- vv(total_beef_serving_all_athletes/n_year,
                                                 var_CV, n_year)
  
  total_prawn_serving_per_year_all_athletes <- vv(total_prawn_serving_all_athletes/n_year,
                                                  var_CV, n_year)
  
  total_rice_noodle_serving_per_year_all_athletes <- vv(total_rice_noodle_serving_all_athletes/n_year,
                                                        var_CV, n_year)
  
  total_fat_oil_serving_per_year_all_athletes <- vv(total_fat_oil_serving_all_athletes/n_year,
                                                    var_CV, n_year)
  
  
  ## Calculate food waste (Intervention one) (for model 5)
  
  total_yearly_egg_serving_wasted_inter_one <- vv(total_egg_serving_per_year_all_athletes -
                                                    actual_egg_serving_per_year_all_athletes_inter_one,
                                                var_CV, n_year)
  
  total_yearly_dairy_serving_wasted_inter_one <- vv(total_dairy_serving_per_year_all_athletes -
                                                      actual_dairy_serving_per_year_all_athletes_inter_one,
                                                    var_CV, n_year)
  
  
  total_yearly_vegetable_mushroom_serving_wasted_inter_one <- vv(total_vegetable_mushroom_serving_per_year_all_athletes -
                                                                    actual_vegetable_mushroom_serving_per_year_all_athletes_inter_one,
                                                                  var_CV, n_year)
  
  total_yearly_bean_nut_serving_wasted_inter_one <- vv(total_bean_nut_serving_per_year_all_athletes -
                                                        actual_bean_nut_serving_per_year_all_athletes_inter_one,
                                                      var_CV, n_year)
  
  
  total_yearly_pork_serving_wasted_inter_one <- vv(total_pork_serving_per_year_all_athletes -
                                                      actual_pork_serving_per_year_all_athletes_inter_one,
                                                  var_CV, n_year)
  
  total_yearly_beef_serving_wasted_inter_one <- vv(total_beef_serving_per_year_all_athletes -
                                                      actual_beef_serving_per_year_all_athletes_inter_one,
                                                  var_CV, n_year)
  
  
  total_yearly_fish_serving_wasted_inter_one <- vv(total_fish_serving_per_year_all_athletes -
                                                      actual_fish_serving_per_year_all_athletes_inter_one,
                                                  var_CV, n_year)
  
  total_yearly_prawn_serving_wasted_inter_one <- vv(total_prawn_serving_per_year_all_athletes -
                                                       actual_prawn_serving_per_year_all_athletes_inter_one,
                                                    var_CV, n_year)
  
  
  
  ## Calculate food waste (Intervention two) (for model 5)
  
  total_yearly_egg_serving_wasted_inter_two <- vv(total_egg_serving_per_year_all_athletes -
                                                    actual_egg_serving_per_year_all_athletes_inter_two,
                                                  var_CV, n_year)
  
  total_yearly_dairy_serving_wasted_inter_two <- vv(total_dairy_serving_per_year_all_athletes -
                                                      actual_dairy_serving_per_year_all_athletes_inter_two,
                                                    var_CV, n_year)
  
  
  total_yearly_vegetable_mushroom_serving_wasted_inter_two <- vv(total_vegetable_mushroom_serving_per_year_all_athletes -
                                                                   actual_vegetable_mushroom_serving_per_year_all_athletes_inter_two,
                                                                 var_CV, n_year)
  
  total_yearly_bean_nut_serving_wasted_inter_two <- vv(total_bean_nut_serving_per_year_all_athletes -
                                                         actual_bean_nut_serving_per_year_all_athletes_inter_two,
                                                       var_CV, n_year)
  
  
  total_yearly_pork_serving_wasted_inter_two <- vv(total_pork_serving_per_year_all_athletes -
                                                     actual_pork_serving_per_year_all_athletes_inter_two,
                                                   var_CV, n_year)
  
  total_yearly_beef_serving_wasted_inter_two <- vv(total_beef_serving_per_year_all_athletes -
                                                     actual_beef_serving_per_year_all_athletes_inter_two,
                                                   var_CV, n_year)
  
  
  total_yearly_fish_serving_wasted_inter_two <- vv(total_fish_serving_per_year_all_athletes -
                                                     actual_fish_serving_per_year_all_athletes_inter_two,
                                                   var_CV, n_year)
  
  total_yearly_prawn_serving_wasted_inter_two <- vv(total_prawn_serving_per_year_all_athletes -
                                                      actual_prawn_serving_per_year_all_athletes_inter_two,
                                                    var_CV, n_year)


  ################################################################################
  
  ### Meeting nutrition goal
  
  ## Calculate probability of achieving nutrition outcome
  
  # In the impact pathway model, achieving nutrition outcome is characterized as 
  # meeting calorie requirement, meeting protein requirement, meeting carbohydrate requirement
  # adequate water intake, right type of food at right time 
  # In calculating probability of achieving nutrition goal, I won't include meeting calorie requirement
  # since kcal req will be meet if food groups target are met (no need to include separately) and
  # having right type of food at right time (this is achieved if the kitchen provide intake needs, no need to 
  # include separately since already accounted for in the calculation of prob of maintaining good nutrition)
  
  
  ## Intervention one
  
  # Calculate percent probability of having adequate water intake
  
  maintain_good_water_intake_inter_one <- vv(((chance_good_water_intake_current*
                                                 weight_current_good_water_intake_to_maintain_good_water_intake) +
                                                (good_learning_outcome_inter_one*
                                                   weight_good_learning_outcome_to_maintain_good_water_intake) +
                                                (maintain_good_nutrition_inter_one*
                                                   weight_maintain_good_nutrition_to_maintain_good_water_intake))/
                                               (weight_current_good_water_intake_to_maintain_good_water_intake +
                                                  weight_good_learning_outcome_to_maintain_good_water_intake +
                                                  weight_maintain_good_nutrition_to_maintain_good_water_intake),
                                             var_CV, n_year)
  
  
  # Calculate percent probability of meeting target for each food group
  
  percent_meeting_target_egg_inter_one <- vv(actual_egg_serving_per_year_all_athletes_inter_one/
                                               total_egg_serving_per_year_all_athletes,
                                             var_CV, n_year)
  
  percent_meeting_target_dairy_inter_one <- vv(actual_dairy_serving_per_year_all_athletes_inter_one/
                                                 total_dairy_serving_per_year_all_athletes,
                                               var_CV, n_year)
  
  percent_meeting_target_sandwich_inter_one <- vv(actual_sandwich_serving_per_year_all_athletes_inter_one/
                                                    total_sandwich_serving_per_year_all_athletes,
                                                  var_CV, n_year)
  
  percent_meeting_target_vegetable_inter_one <- vv(actual_vegetable_mushroom_serving_per_year_all_athletes_inter_one/
                                                     total_vegetable_mushroom_serving_per_year_all_athletes,
                                                   var_CV, n_year)
  
  percent_meeting_target_bean_nut_inter_one <- vv(actual_bean_nut_serving_per_year_all_athletes_inter_one/
                                                    total_bean_nut_serving_per_year_all_athletes,
                                                  var_CV, n_year)
  
  percent_meeting_target_fruit_inter_one <- vv(actual_fruit_serving_per_year_all_athletes_inter_one/
                                                 total_fruit_serving_per_year_all_athletes,
                                               var_CV, n_year)
  
  percent_meeting_target_sports_drink_inter_one <- vv(actual_sports_drink_serving_per_year_all_athletes_inter_one/
                                                        total_sports_drink_serving_per_year_all_athletes,
                                                      var_CV, n_year)
  
  percent_meeting_target_chicken_inter_one <- vv(actual_chicken_serving_per_year_all_athletes_inter_one/
                                                   total_chicken_serving_per_year_all_athletes,
                                                 var_CV, n_year)
  
  percent_meeting_target_pork_inter_one <- vv(actual_pork_serving_per_year_all_athletes_inter_one/
                                                total_pork_serving_per_year_all_athletes,
                                              var_CV, n_year)
  
  percent_meeting_target_beef_inter_one <- vv(actual_beef_serving_per_year_all_athletes_inter_one/
                                                total_beef_serving_per_year_all_athletes,
                                              var_CV, n_year)
  
  percent_meeting_target_fish_inter_one <- vv(actual_fish_serving_per_year_all_athletes_inter_one/
                                                total_fish_serving_per_year_all_athletes,
                                              var_CV, n_year)
  
  percent_meeting_target_prawn_inter_one <- vv(actual_prawn_serving_per_year_all_athletes_inter_one/
                                                 total_prawn_serving_per_year_all_athletes,
                                               var_CV, n_year)
  
  percent_meeting_target_rice_inter_one <- vv(actual_rice_noodle_serving_per_year_all_athletes_inter_one/
                                                total_rice_noodle_serving_per_year_all_athletes,
                                              var_CV, n_year)
  
  percent_meeting_target_fat_inter_one <- vv(actual_fat_oil_serving_per_year_all_athletes_inter_one/
                                               total_fat_oil_serving_per_year_all_athletes,
                                             var_CV, n_year)
  
  
  # Calculate percent probability of meeting nutrition goals
  
  percent_achieving_nutrition_outcome_inter_one <- vv(((maintain_good_water_intake_inter_one*
                                                          weight_good_water_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_egg_inter_one*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_dairy_inter_one*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_sandwich_inter_one*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_vegetable_inter_one*
                                                            weight_vegetable_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_bean_nut_inter_one*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_fruit_inter_one*
                                                            weight_fruit_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_sports_drink_inter_one*
                                                            weight_good_water_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_chicken_inter_one*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_pork_inter_one*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_beef_inter_one*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_fish_inter_one*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_prawn_inter_one*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_rice_inter_one*
                                                            weight_good_carb_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_fat_inter_one*
                                                            weight_good_fat_intake_to_nutrition_outcome))/
                                                        (weight_good_water_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_vegetable_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_fruit_intake_to_nutrition_outcome +
                                                           weight_good_water_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_carb_intake_to_nutrition_outcome +
                                                           weight_good_fat_intake_to_nutrition_outcome),
                                                      var_CV, n_year)
  
 ##############
  
  ## Intervention two
  
  # Calculate percent probability of having adequate water intake
  
  maintain_good_water_intake_inter_two <- vv(((chance_good_water_intake_current*
                                                 weight_current_good_water_intake_to_maintain_good_water_intake) +
                                                (good_learning_outcome_inter_two*
                                                   weight_good_learning_outcome_to_maintain_good_water_intake) +
                                                (maintain_good_nutrition_inter_two*
                                                   weight_maintain_good_nutrition_to_maintain_good_water_intake))/
                                               (weight_current_good_water_intake_to_maintain_good_water_intake +
                                                  weight_good_learning_outcome_to_maintain_good_water_intake +
                                                  weight_maintain_good_nutrition_to_maintain_good_water_intake),
                                             var_CV, n_year)
  
  
  # Calculate percent probability of meeting target for each food group
  
  percent_meeting_target_egg_inter_two <- vv(actual_egg_serving_per_year_all_athletes_inter_two/
                                               total_egg_serving_per_year_all_athletes,
                                             var_CV, n_year)
  
  percent_meeting_target_dairy_inter_two <- vv(actual_dairy_serving_per_year_all_athletes_inter_two/
                                                 total_dairy_serving_per_year_all_athletes,
                                               var_CV, n_year)
  
  percent_meeting_target_sandwich_inter_two <- vv(actual_sandwich_serving_per_year_all_athletes_inter_two/
                                                    total_sandwich_serving_per_year_all_athletes,
                                                  var_CV, n_year)
  
  percent_meeting_target_vegetable_inter_two <- vv(actual_vegetable_mushroom_serving_per_year_all_athletes_inter_two/
                                                     total_vegetable_mushroom_serving_per_year_all_athletes,
                                                   var_CV, n_year)
  
  percent_meeting_target_bean_nut_inter_two <- vv(actual_bean_nut_serving_per_year_all_athletes_inter_two/
                                                    total_bean_nut_serving_per_year_all_athletes,
                                                  var_CV, n_year)
  
  percent_meeting_target_fruit_inter_two <- vv(actual_fruit_serving_per_year_all_athletes_inter_two/
                                                 total_fruit_serving_per_year_all_athletes,
                                               var_CV, n_year)
  
  percent_meeting_target_sports_drink_inter_two <- vv(actual_sports_drink_serving_per_year_all_athletes_inter_two/
                                                        total_sports_drink_serving_per_year_all_athletes,
                                                      var_CV, n_year)
  
  percent_meeting_target_chicken_inter_two <- vv(actual_chicken_serving_per_year_all_athletes_inter_two/
                                                   total_chicken_serving_per_year_all_athletes,
                                                 var_CV, n_year)
  
  percent_meeting_target_pork_inter_two <- vv(actual_pork_serving_per_year_all_athletes_inter_two/
                                                total_pork_serving_per_year_all_athletes,
                                              var_CV, n_year)
  
  percent_meeting_target_beef_inter_two <- vv(actual_beef_serving_per_year_all_athletes_inter_two/
                                                total_beef_serving_per_year_all_athletes,
                                              var_CV, n_year)
  
  percent_meeting_target_fish_inter_two <- vv(actual_fish_serving_per_year_all_athletes_inter_two/
                                                total_fish_serving_per_year_all_athletes,
                                              var_CV, n_year)
  
  percent_meeting_target_prawn_inter_two <- vv(actual_prawn_serving_per_year_all_athletes_inter_two/
                                                 total_prawn_serving_per_year_all_athletes,
                                               var_CV, n_year)
  
  percent_meeting_target_rice_inter_two <- vv(actual_rice_noodle_serving_per_year_all_athletes_inter_two/
                                                total_rice_noodle_serving_per_year_all_athletes,
                                              var_CV, n_year)
  
  percent_meeting_target_fat_inter_two <- vv(actual_fat_oil_serving_per_year_all_athletes_inter_two/
                                               total_fat_oil_serving_per_year_all_athletes,
                                             var_CV, n_year)
  
  
  # Calculate percent probability of meeting nutrition goals
  
  percent_achieving_nutrition_outcome_inter_two <- vv(((maintain_good_water_intake_inter_two*
                                                          weight_good_water_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_egg_inter_two*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_dairy_inter_two*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_sandwich_inter_two*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_vegetable_inter_two*
                                                            weight_vegetable_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_bean_nut_inter_two*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_fruit_inter_two*
                                                            weight_fruit_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_sports_drink_inter_two*
                                                            weight_good_water_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_chicken_inter_two*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_pork_inter_two*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_beef_inter_two*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_fish_inter_two*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_prawn_inter_two*
                                                            weight_good_protein_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_rice_inter_two*
                                                            weight_good_carb_intake_to_nutrition_outcome) +
                                                         (percent_meeting_target_fat_inter_two*
                                                            weight_good_fat_intake_to_nutrition_outcome))/
                                                        (weight_good_water_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_vegetable_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_fruit_intake_to_nutrition_outcome +
                                                           weight_good_water_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_protein_intake_to_nutrition_outcome +
                                                           weight_good_carb_intake_to_nutrition_outcome +
                                                           weight_good_fat_intake_to_nutrition_outcome),
                                                      var_CV, n_year)
  
  
  

  ################################################################################
  
  ### Emotional well-being (feeling energized for training, match)
  
  ## Intervention one
  
  good_mental_wellbeing_inter_one <- vv(((percent_achieving_nutrition_outcome_inter_one*
                                            weight_nutrition_outcome_to_mental_health) +
                                           (chance_good_rest*
                                              weight_good_rest_to_mental_health) +
                                           (chance_enough_recovery_time*
                                              weight_recovery_time_to_mental_health) +
                                           ((1- chance_personal_problem)*
                                              weight_personal_problem_to_mental_health) +
                                           (chance_good_team_relationship*
                                              weight_team_relationship_to_mental_health))/
                                          (weight_nutrition_outcome_to_mental_health +
                                             weight_good_rest_to_mental_health +
                                             weight_personal_problem_to_mental_health +
                                             weight_team_relationship_to_mental_health +
                                             weight_recovery_time_to_mental_health),
                                        var_CV, n_year)
  
  
  ## Intervention two
  
  good_mental_wellbeing_inter_two <- vv(((percent_achieving_nutrition_outcome_inter_two*
                                            weight_nutrition_outcome_to_mental_health) +
                                           (chance_good_rest*
                                              weight_good_rest_to_mental_health) +
                                           (chance_enough_recovery_time*
                                              weight_recovery_time_to_mental_health) +
                                           ((1- chance_personal_problem)*
                                              weight_personal_problem_to_mental_health) +
                                           (chance_good_team_relationship*
                                              weight_team_relationship_to_mental_health))/
                                          (weight_nutrition_outcome_to_mental_health +
                                             weight_good_rest_to_mental_health +
                                             weight_personal_problem_to_mental_health +
                                             weight_team_relationship_to_mental_health +
                                             weight_recovery_time_to_mental_health),
                                        var_CV, n_year)
  
  
  ###############################################################################
  
  ### Sports outcome (training performance)
  
  # If I do a proper simulation for training performance, I would need a whole new model just for this because
  # the variables and relevant threshold to measure the performance may vary with position in the field. I won't
  # consider such minute details for a practice, so this part of the model is just to show sports outcome is also
  # included in the model. 
  
  # So, I only included 3 variables to measure sports performance (there are other test measures like yo-yo, drill,
  # 1 RM, etc.) I only include 3 because the answer to which intervention is better can be answerable with just 3 or less,
  # since all other conditions are equal and only slight difference in nutrition target score, I can pretty much guess
  # that the two interventions will give more or less similar improvement in terms of sports performance. This part won't
  # be necessary if my goal is just to know which intervention is better. But it is necessary here because I need this model
  # output for next model (chance_of_winning)
  
  # Another thing is I only considered nutrition and sports performance during training, and not on the match days. 
  # It is possible that intake on match days can differ based on each intervention success- intervention one may
  # fare better on match days because athletes know which food is good or bad for them, while intervention two 
  # may not give that much knowledge. But, usually there's a medical person who would put stickers on the provided food,
  # and we don't control their intake too much during match period as long as they meet their carb and protein for 
  # loading and recovery. So, I don't think including match day nutrition will change the current scores for each
  # intervention. I consider it as redundant in decision making. 
  
  ## Intervention one
  
  optimal_training_condition_inter_one <- vv(((percent_achieving_nutrition_outcome_inter_one*
                                                 weight_nutrition_outcome_to_sports_performance) +
                                                (maintain_good_water_intake_inter_one*
                                                   weight_water_intake_to_sports_performance) +
                                                (good_mental_wellbeing_inter_one*
                                                   weight_mental_health_to_sports_performance))/
                                               (weight_nutrition_outcome_to_sports_performance +
                                                  weight_water_intake_to_sports_performance +
                                                  weight_mental_health_to_sports_performance),
                                             var_CV, n_year)
  
  
  
  # Calculate number of days to reach target for each training test
  # I don't use vv() function here because once reach the target, it's more likely maintain

  days_40_dash_inter_one <- ifelse(optimal_training_condition_inter_one >= 0.7,
    (log(forty_dash_current_sec/forty_dash_target_sec)/log(1 + percent_increment_40_dash_if_optimal_training))*30,
    (log(forty_dash_current_sec/forty_dash_target_sec)/log(1 + percent_increment_40_dash_current))*30)
  
  
  days_10_dash_inter_one <- ifelse(optimal_training_condition_inter_one >= 0.7,
    (log(ten_dash_current_sec/ten_dash_target_sec)/log(1 + percent_increment_10_dash_if_optimal_training))*30,
    (log(ten_dash_current_sec/ten_dash_target_sec)/log(1 + percent_increment_10_dash_current))*30)
  
  
  days_shuttle_run_inter_one <- ifelse(optimal_training_condition_inter_one >=0.7,
                                         (log(shuttle_run_current/shuttle_run_target)/
                                            log(1 + percent_increment_shuttle_run_if_optimal_training))*30,
                                         (log(shuttle_run_current/shuttle_run_target)/
                                            log(1 + percent_increment_shuttle_run_current))*30)
  

  ###################
  
  ## Intervention two
  
  optimal_training_condition_inter_two <- vv(((percent_achieving_nutrition_outcome_inter_two*
                                                 weight_nutrition_outcome_to_sports_performance) +
                                                (maintain_good_water_intake_inter_two*
                                                   weight_water_intake_to_sports_performance) +
                                                (good_mental_wellbeing_inter_two*
                                                   weight_mental_health_to_sports_performance))/
                                               (weight_nutrition_outcome_to_sports_performance +
                                                  weight_water_intake_to_sports_performance +
                                                  weight_mental_health_to_sports_performance),
                                             var_CV,n_year)
  
  
  
  # Calculate number of days to reach target for each training test
  
  days_40_dash_inter_two <- ifelse(optimal_training_condition_inter_two >= 0.7,
                                     (log(forty_dash_current_sec/forty_dash_target_sec)/
                                        log(1 + percent_increment_40_dash_if_optimal_training))*30,
                                     (log(forty_dash_current_sec/forty_dash_target_sec)/
                                        log(1 + percent_increment_40_dash_current))*30)
  
  
  days_10_dash_inter_two <- ifelse(optimal_training_condition_inter_two >= 0.7,
                                     (log(ten_dash_current_sec/ten_dash_target_sec)/
                                        log(1 + percent_increment_10_dash_if_optimal_training))*30,
                                     (log(ten_dash_current_sec/ten_dash_target_sec)/
                                        log(1 + percent_increment_10_dash_current))*30)
  
  
  days_shuttle_run_inter_two <- ifelse(optimal_training_condition_inter_two >=0.7,
                                         (log(shuttle_run_current/shuttle_run_target)/
                                            log(1 + percent_increment_shuttle_run_if_optimal_training))*30,
                                         (log(shuttle_run_current/shuttle_run_target)/
                                            log(1 + percent_increment_shuttle_run_current))*30)
  
  
  
  ################################################################################
  # total serving values have slight differences from model 2 results because I divided by 4 and vary the values. 
  # I will use the new ones from here onwards. I won't have this problem if models are connected by codes. Don't know how to do it. 
  # here I use both sum() and list(). Sum values (4 years value) for result plots and list value (yearly value) 
  # for feeding into next model. 
  return(list(total_egg_serving_all_athletes = sum(total_egg_serving_per_year_all_athletes), 
              total_egg_serving_per_year_all_athletes = list(total_egg_serving_per_year_all_athletes),
              total_actual_egg_serving_all_athletes_inter_one = sum(actual_egg_serving_per_year_all_athletes_inter_one),
              total_actual_egg_serving_all_athletes_inter_two = sum(actual_egg_serving_per_year_all_athletes_inter_two),
              total_dairy_serving_all_athletes = sum(total_dairy_serving_per_year_all_athletes),
              total_dairy_serving_per_year_all_athletes = list(total_dairy_serving_per_year_all_athletes),
              total_actual_dairy_serving_all_athletes_inter_one = sum(actual_dairy_serving_per_year_all_athletes_inter_one),
              total_actual_dairy_serving_all_athletes_inter_two = sum(actual_dairy_serving_per_year_all_athletes_inter_two),
              total_sandwich_serving_all_athletes = sum(total_sandwich_serving_per_year_all_athletes),
              total_sandwich_serving_per_year_all_athletes = list(total_sandwich_serving_per_year_all_athletes),
              total_actual_sandwich_serving_all_athletes_inter_one = sum(actual_sandwich_serving_per_year_all_athletes_inter_one),
              total_actual_sandwich_serving_all_athletes_inter_two = sum(actual_sandwich_serving_per_year_all_athletes_inter_two),
              total_vegetable_mushroom_serving_all_athletes = sum(total_vegetable_mushroom_serving_per_year_all_athletes),
              total_vegetable_mushroom_serving_per_year_all_athletes = list(total_vegetable_mushroom_serving_per_year_all_athletes),
              total_actual_vegetable_mushroom_serving_all_athletes_inter_one = sum(actual_vegetable_mushroom_serving_per_year_all_athletes_inter_one),
              total_actual_vegetable_mushroom_serving_all_athletes_inter_two = sum(actual_vegetable_mushroom_serving_per_year_all_athletes_inter_two),
              total_bean_nut_serving_all_athletes = sum(total_bean_nut_serving_per_year_all_athletes),
              total_bean_nut_serving_per_year_all_athletes = list(total_bean_nut_serving_per_year_all_athletes),
              total_actual_bean_nut_serving_all_athletes_inter_one = sum(actual_bean_nut_serving_per_year_all_athletes_inter_one),
              total_actual_bean_nut_serving_all_athletes_inter_two = sum(actual_bean_nut_serving_per_year_all_athletes_inter_two),
              total_fruit_serving_all_athletes = sum(total_fruit_serving_per_year_all_athletes),
              total_fruit_serving_per_year_all_athletes = list(total_fruit_serving_per_year_all_athletes),
              total_actual_fruit_serving_all_athletes_inter_one = sum(actual_fruit_serving_per_year_all_athletes_inter_one),
              total_actual_fruit_serving_all_athletes_inter_two = sum(actual_fruit_serving_per_year_all_athletes_inter_two),
              total_sports_drink_serving_all_athletes = sum(total_sports_drink_serving_per_year_all_athletes),
              total_sports_drink_serving_per_year_all_athletes = list(total_sports_drink_serving_per_year_all_athletes),
              total_actual_sports_drink_serving_all_athletes_inter_one = sum(actual_sports_drink_serving_per_year_all_athletes_inter_one),
              total_actual_sports_drink_serving_all_athletes_inter_two = sum(actual_sports_drink_serving_per_year_all_athletes_inter_two),
              total_chicken_serving_all_athletes = sum(total_chicken_serving_per_year_all_athletes),
              total_chicken_serving_per_year_all_athletes = list(total_chicken_serving_per_year_all_athletes),
              total_actual_chicken_serving_all_athletes_inter_one = sum(actual_chicken_serving_per_year_all_athletes_inter_one),
              total_actual_chicken_serving_all_athletes_inter_two = sum(actual_chicken_serving_per_year_all_athletes_inter_two),
              total_pork_serving_all_athletes = sum(total_pork_serving_per_year_all_athletes),
              total_pork_serving_per_year_all_athletes = list(total_pork_serving_per_year_all_athletes),
              total_actual_pork_serving_all_athletes_inter_one = sum(actual_pork_serving_per_year_all_athletes_inter_one),
              total_actual_pork_serving_all_athletes_inter_two = sum(actual_pork_serving_per_year_all_athletes_inter_two),
              total_beef_serving_all_athletes = sum(total_beef_serving_per_year_all_athletes),
              total_beef_serving_per_year_all_athletes = list(total_beef_serving_per_year_all_athletes),
              total_actual_beef_serving_all_athletes_inter_one = sum(actual_beef_serving_per_year_all_athletes_inter_one),
              total_actual_beef_serving_all_athletes_inter_two = sum(actual_beef_serving_per_year_all_athletes_inter_two),
              total_fish_serving_all_athletes = sum(total_fish_serving_per_year_all_athletes),
              total_fish_serving_per_year_all_athletes = list(total_fish_serving_per_year_all_athletes),
              total_actual_fish_serving_all_athletes_inter_one = sum(actual_fish_serving_per_year_all_athletes_inter_one),
              total_actual_fish_serving_all_athletes_inter_two = sum(actual_fish_serving_per_year_all_athletes_inter_two),
              total_prawn_serving_all_athletes = sum(total_prawn_serving_per_year_all_athletes),
              total_prawn_serving_per_year_all_athletes = list(total_prawn_serving_per_year_all_athletes),
              total_actual_prawn_serving_all_athletes_inter_one = sum(actual_prawn_serving_per_year_all_athletes_inter_one),
              total_actual_prawn_serving_all_athletes_inter_two = sum(actual_prawn_serving_per_year_all_athletes_inter_two),
              total_rice_noodle_serving_all_athletes = sum(total_rice_noodle_serving_per_year_all_athletes),
              total_rice_noodle_serving_per_year_all_athletes = list(total_rice_noodle_serving_per_year_all_athletes),
              total_actual_rice_noodle_serving_all_athletes_inter_one = sum(actual_rice_noodle_serving_per_year_all_athletes_inter_one),
              total_actual_rice_noodle_serving_all_athletes_inter_two = sum(actual_rice_noodle_serving_per_year_all_athletes_inter_two),
              total_fat_oil_serving_all_athletes = sum(total_fat_oil_serving_per_year_all_athletes),
              total_fat_oil_serving_per_year_all_athletes = list(total_fat_oil_serving_per_year_all_athletes),
              total_actual_fat_oil_serving_all_athletes_inter_one = sum(actual_fat_oil_serving_per_year_all_athletes_inter_one),
              total_actual_fat_oil_serving_all_athletes_inter_two = sum(actual_fat_oil_serving_per_year_all_athletes_inter_two),
              total_egg_serving_wasted_inter_one = sum(total_yearly_egg_serving_wasted_inter_one),
              total_yearly_egg_serving_wasted_inter_one = list(total_yearly_egg_serving_wasted_inter_one),
              total_egg_serving_wasted_inter_two = sum(total_yearly_egg_serving_wasted_inter_two),
              total_yearly_egg_serving_wasted_inter_two = list(total_yearly_egg_serving_wasted_inter_two),
              total_dairy_serving_wasted_inter_one = sum(total_yearly_dairy_serving_wasted_inter_one),
              total_yearly_dairy_serving_wasted_inter_one = list(total_yearly_dairy_serving_wasted_inter_one),
              total_dairy_serving_wasted_inter_two = sum(total_yearly_dairy_serving_wasted_inter_two),
              total_yearly_dairy_serving_wasted_inter_two = list(total_yearly_dairy_serving_wasted_inter_two),
              total_vegetable_mushroom_serving_wasted_inter_one = sum(total_yearly_vegetable_mushroom_serving_wasted_inter_one),
              total_yearly_vegetable_mushroom_serving_wasted_inter_one = list(total_yearly_vegetable_mushroom_serving_wasted_inter_one),
              total_vegetable_mushroom_serving_wasted_inter_two = sum(total_yearly_vegetable_mushroom_serving_wasted_inter_two),
              total_yearly_vegetable_mushroom_serving_wasted_inter_two = list(total_yearly_vegetable_mushroom_serving_wasted_inter_two),
              total_bean_nut_serving_wasted_inter_one = sum(total_yearly_bean_nut_serving_wasted_inter_one),
              total_yearly_bean_nut_serving_wasted_inter_one = list(total_yearly_bean_nut_serving_wasted_inter_one),
              total_bean_nut_serving_wasted_inter_two = sum(total_yearly_bean_nut_serving_wasted_inter_two),
              total_yearly_bean_nut_serving_wasted_inter_two = list(total_yearly_bean_nut_serving_wasted_inter_two),
              total_pork_serving_wasted_inter_one = sum(total_yearly_pork_serving_wasted_inter_one),
              total_yearly_pork_serving_wasted_inter_one = list(total_yearly_pork_serving_wasted_inter_one),
              total_pork_serving_wasted_inter_two = sum(total_yearly_pork_serving_wasted_inter_two),
              total_yearly_pork_serving_wasted_inter_two = list(total_yearly_pork_serving_wasted_inter_two),
              total_beef_serving_wasted_inter_one = sum(total_yearly_beef_serving_wasted_inter_one),
              total_yearly_beef_serving_wasted_inter_one = list(total_yearly_beef_serving_wasted_inter_one),
              total_beef_serving_wasted_inter_two = sum(total_yearly_beef_serving_wasted_inter_two),
              total_yearly_beef_serving_wasted_inter_two = list(total_yearly_beef_serving_wasted_inter_two),
              total_fish_serving_wasted_inter_one = sum(total_yearly_fish_serving_wasted_inter_one),
              total_yearly_fish_serving_wasted_inter_one = list(total_yearly_fish_serving_wasted_inter_one),
              total_fish_serving_wasted_inter_two = sum(total_yearly_fish_serving_wasted_inter_two),
              total_yearly_fish_serving_wasted_inter_two = list(total_yearly_fish_serving_wasted_inter_two),
              total_prawn_serving_wasted_inter_one = sum(total_yearly_prawn_serving_wasted_inter_one),
              total_yearly_prawn_serving_wasted_inter_one = list(total_yearly_prawn_serving_wasted_inter_one),
              total_prawn_serving_wasted_inter_two = sum(total_yearly_prawn_serving_wasted_inter_two),
              total_yearly_prawn_serving_wasted_inter_two = list(total_yearly_prawn_serving_wasted_inter_two),
              good_learning_outcome_inter_one = list(good_learning_outcome_inter_one),
              good_learning_outcome_inter_two = list(good_learning_outcome_inter_two),
              maintain_good_nutrition_inter_one = list(maintain_good_nutrition_inter_one),
              maintain_good_nutrition_inter_two = list(maintain_good_nutrition_inter_two),
              maintain_good_water_intake_inter_one = list(maintain_good_water_intake_inter_one),
              maintain_good_water_intake_inter_two = list(maintain_good_water_intake_inter_two),
              percent_achieving_nutrition_outcome_inter_one = list(percent_achieving_nutrition_outcome_inter_one),
              percent_achieving_nutrition_outcome_inter_two = list(percent_achieving_nutrition_outcome_inter_two),
              good_mental_wellbeing_inter_one = list(good_mental_wellbeing_inter_one),
              good_mental_wellbeing_inter_two = list(good_mental_wellbeing_inter_two),
              days_40_dash_inter_one = list(days_40_dash_inter_one),
              days_40_dash_inter_two = list(days_40_dash_inter_two),
              days_10_dash_inter_one = list(days_10_dash_inter_one),
              days_10_dash_inter_two = list(days_10_dash_inter_two),
              days_shuttle_run_inter_one = list(days_shuttle_run_inter_one),
              days_shuttle_run_inter_two = list(days_shuttle_run_inter_two)
  ))
  
  }


Learning_behaviour_mc_simulation <- mcSimulation(estimate = estimate_read_csv("3_Learning_behaviour.csv"),
                                      model_function = Learning_behaviour_function,
                                      numberOfModelRuns = 1000,
                                      functionSyntax = "plainNames")


plot_distributions(mcSimulation_object = Learning_behaviour_mc_simulation,
                   vars = c("total_egg_serving_all_athletes", "total_actual_egg_serving_all_athletes_inter_one"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Serving',
                   base_size = 7)


plot_distributions(mcSimulation_object = Learning_behaviour_mc_simulation,
                   vars = c("total_bean_nut_serving_all_athletes", 
                            "total_actual_bean_nut_serving_all_athletes_inter_one"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'Serving',
                   base_size = 7)



Learning_behaviour_mc_simulation <- decisionSupport("3_Learning_behaviour.csv",
                                      outputPath = 'results_Learning_behaviour',
                                      welfareFunction = Learning_behaviour_function,
                                      numberOfModelRuns = 1000,
                                      functionSyntax = "plainNames")

# Model 3 done, move on to model 4
