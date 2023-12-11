# Nutritional self-reliance Vs. guided personalized nutrition for 
# improved sports performance

# Learning outcome model (first model)

# Here we simulate baseline knowledge and learning outcome after training.
# We simulate outcome for before and after intervention (two time points).
# We model this based on scoring system of a formal exam. 
# But we are not simulating how well each player would score 
# in a test.We simulate their ability to understand (find and use) resources.
# The threshold for intervention 1 is a score of 90%- need at least 90% score to 
# achieve nutritional self-reliance. No threshold or target for intervention 2.


library(decisionSupport)

# Make variables

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("Learning_outcome.csv")))

Learning_outcome_function <- function(x, varnames) {
 
  # Calculate baseline knowledge ####
  
  # Here we use the correction factor (see explanation in excel) to prevent 
  # overestimation of score at baseline. Individuals may over- or under- 
  # estimate their knowledge level- a correction factor is used to fix that.
  # We focus on overestimation only in this case. 
  
  
  # Individual-baseline for intervention 1 ####
  
  know_food_groups <- vv(know_food_group,
                         var_CV, n)
  
  know_nutrients <- vv(know_nutrient,
                       var_CV, n)
  
  know_healthy_eatings <- vv(know_healthy_eating,
                             var_CV, n)
  
  know_nu_requirements <- vv(know_nu_requirement,
                             var_CV, n)
  
  know_nu_calculations <- vv(know_nu_calculation,
                             var_CV, n)
  
  plan_diets <- vv(plan_diet,
                   var_CV, n)
  
  know_food_exchanges <- vv(know_food_exchange,
                            var_CV, n)
  
  correction_factors <- vv(correction_factor,
                           var_CV, n)
  
  Individual_baseline_inter_one <- (((know_food_groups*10) + 
                                      (know_nutrients*15) +
                                      (know_healthy_eatings*15) + 
                                      (know_nu_requirements*20) + 
                                      (know_nu_calculations*20) + 
                                      ((plan_diets*know_food_exchanges)*20))- 
                                      (correction_factors* 100))
  
  # Individual-baseline for intervention 2 ####
  
  Individual_baseline_inter_two <- (((know_food_groups*25) + 
                                     (know_nutrients*25) +
                                     (know_healthy_eatings*25) + 
                                     (know_nu_requirements*25))- 
                                     (correction_factors* 100))
  
  # Before we calculate the outcomes after intervention, we string branch 
  # variables into core variables first, based on decision template.
  
  # We don't have priors for each links between the branch variables and
  # core variables. So, we assign degree of association based on past experience
  # and knowledge (DW, DT, IPM).
  
  # That means we need really good priors for this model to work properly. 
  # Not having priors for variable links will be a disadvantage. Otherwise, we
  # need to use ranges for each variable- a rather tedious task. 
  
  # And this is based on assumption that we don't need all branch variables to 
  # happen simultaneously to get the outcome (or higher variables). The existence
  # of each branch variable will add up to the outcome, and the extent of 
  # contribution is based on its degree of association. This is to ensure we
  # will still get benefit even if not all branch variables are present. 
  
  # Since we are simulating for different individuals, there may be different 
  # number of branch variables for a core variables. For example, a smart student
  # with prior knowledge would only need the guide book to achieve high score, 
  # whereas their counterparts may need more- good trainer, good motivation, good
  # learning material, good attitude, good facility, etc. We set ranges based on 
  # this assumption. 
  
  
  # Probability of having a good trainer ####
  
  good_trainer_in <- vv((if_trainer_good_interpersonal*percent_good_interpersonal) + 
                     (if_trainer_good_teaching*percent_good_teaching) +
                     (if_trainer_good_knowledge*percent_good_knowledge),
                      var_CV, n)

good_trainer <- min(good_trainer_in, 1)
  
  # Probability of having good value-related belief ####
  
  good_value_belief_in <- vv((know_importance_nu*percent_know_importance_nu) + 
                             (take_care_diet*percent_take_care_diet),
                             var_CV, n)
  
good_value_belief <- min(good_value_belief_in, 1)

  # Probability of having good learning and practice materials ####
  
  good_material_inter_one_in <- vv((like_learning_material*percent_like_learning_material) + 
                                   (like_initial_four*percent_like_initial_four) +
                                   (like_final_two*percent_like_final_two),
                                   var_CV, n)
  
  good_material_inter_one <- min(good_material_inter_one_in, 1)
  
  
  good_material_inter_two_in <- vv((like_learning_material*
                                      percent_like_learning_material) + 
                                   (like_initial_four*percent_like_initial_four),
                                   var_CV, n)
  
  good_material_inter_two <- min(good_material_inter_two_in, 1)
  
  
  # Probability of good attitude towards nutrition training ####
  
  good_attitude_training_inter_one_in <- vv((good_trainer*
                                               percent_good_trainer_to_attitude) + 
                                           (good_material_inter_one*
                                              percent_good_material_to_attitude) +
                                           (good_value_belief*
                                              percent_good_value_to_attitude),
                                           var_CV, n)
  
  good_attitude_training_inter_one <- min(good_attitude_training_inter_one_in, 1)
  
  
  good_attitude_training_inter_two_in <- vv((good_trainer*
                                               percent_good_trainer_to_attitude) + 
                                            (good_material_inter_two*
                                               percent_good_material_to_attitude) +
                                            (good_value_belief*
                                               percent_good_value_to_attitude),
                                            var_CV, n)
  
  good_attitude_training_inter_two <- min(good_attitude_training_inter_two_in, 1)
  
  # Probability of willingness to learn ####
  
  willingness_learn_inter_one_in <- vv((interest_advanced_training*
                                         percent_interest_training_to_willingness) +
                                      (extra_study_hour*
                                         percent_extra_hour_to_willingness) + 
                                      (motivation_apply_knowledge*
                                         percent_motivation_apply_to_willingness) +
                                      (easy_use_resource*
                                         percent_easy_resource_to_willingness),
                                      var_CV, n)
  
  willingness_learn_inter_one <- min(willingness_learn_inter_one_in, 1)
  
  
  # Probability of having persistance ####
  
  persistence_inter_one_in <- vv((good_trainer*
                                    percent_good_trainer_to_persistence) +
                                 (interest_advanced_training*
                                    percent_interest_training_to_persistence) +
                                 (motivation_apply_knowledge*
                                    percent_motivation_apply_to_persistence),
                                 var_CV, n)
  
  persistence_inter_one <- min(persistence_inter_one_in, 1)
  
  # Probability of good student attitude ####
  
  good_student_attitude_inter_one_in <- vv((willingness_learn_inter_one*
                                             percent_willingness_to_attitude) +
                                           (good_attitude_training_inter_one*
                                             percent_attitude_towards_training_to_attitude) +
                                           (persistence_inter_one*
                                              percent_persistence_to_attitude),
                                           var_CV, n)
  
  good_student_attitude_inter_one <- min(good_student_attitude_inter_one_in, 1)
  
  good_student_attitude_inter_two <- vv(good_attitude_training_inter_two,
                                        var_CV, n)
  
  
  # Probability of inconvenience ####
  
  inconvenience_in <- vv((if_improper_time*
                            percent_improper_time_to_inconvenience) + 
                         (if_improper_location*
                            percent_improper_location_to_inconvenience),
                         var_CV, n)
  
  inconvenience <- min(inconvenience_in, 1)
  
  
  # Probability of good participation ####
  
  good_participation_inter_one_in <- vv(((1-if_long_training)*
                                           percent_short_training_to_participation) + 
                                        ((1-if_weekend_training)*
                                           percent_weekday_training_to_participation)+
                                        ((1-inconvenience)*
                                           percent_convenience_to_participation) + 
                                        ((1-if_give_homework)*
                                           percent_no_homework_to_participation) +
                                        (good_student_attitude_inter_one*
                                           percent_overall_attitude_to_participation),
                                        var_CV, n) 
  
  good_participation_inter_one <- min(good_participation_inter_one_in, 1)
  
  
  good_participation_inter_two_in <- vv(((1-if_long_training)*
                                           percent_short_training_to_participation) +
                                        ((1-if_weekend_training)*
                                           percent_weekday_training_to_participation)+
                                        ((1-inconvenience)*
                                           percent_convenience_to_participation) + 
                                        ((1-if_give_homework)*
                                           percent_no_homework_to_participation) +
                                        (good_student_attitude_inter_two*
                                           percent_overall_attitude_to_participation),
                                        var_CV, n) 
  
  good_participation_inter_two <- min(good_participation_inter_two_in, 1)
  
  
  # Probability of good activities 
  
  good_activity_inter_one_in <- vv((good_participation_inter_one*
                                     percent_participation_to_activity) +
                                   (good_trainer*
                                     percent_good_trainer_to_activity),
                                   var_CV, n)
  
  good_activity_inter_one <- min(good_activity_inter_one_in, 1)
  
  
  good_activity_inter_two_in <- vv((good_participation_inter_two*
                                     percent_participation_to_activity) +
                                  (good_trainer*
                                     percent_good_trainer_to_activity),
                                   var_CV, n)
  
  good_activity_inter_two <- min(good_activity_inter_two_in, 1)
  
  
  # Probability of good prior knowledge/ experience ####
  
  good_prior_knowledge_inter_one_in <- vv((take_care_diet*
                                            percent_take_care_diet_to_prior_knowledge) + 
                                          (follow_diet_plan * 
                                            percent_follow_diet_plan_to_prior_knowledge),
                                          var_CV, n)
  
  good_prior_knowledge_inter_one <- min(good_prior_knowledge_inter_one_in, 1)
  
  good_prior_knowledge_inter_two <- vv(take_care_diet,
                                       var_CV, n)
  
  
  # Finding core variables
  
  # Probability of interest in learning ####
  
  interest_in_learning_inter_one_in <- vv((good_value_belief*
                                            percent_value_belief_to_learning_interest) + 
                                          (good_student_attitude_inter_one*
                                            percent_overall_attitude_to_learning_interest) +
                                          (good_participation_inter_one*
                                            percent_participation_to_learning_interest) +
                                          (good_material_inter_one*
                                            percent_good_material_to_learning_interest) +
                                          (good_activity_inter_one*
                                            percent_good_activity_to_learning_interest) +
                                          (good_prior_knowledge_inter_one*
                                            percent_prior_knowledge_to_learning_interest),
                                          var_CV, n)
  
  interest_in_learning_inter_one <- min(interest_in_learning_inter_one_in, 1)
  
  
  interest_in_learning_inter_two_in <- vv((good_value_belief*
                                            percent_value_belief_to_learning_interest) +
                                          (good_student_attitude_inter_two*
                                            percent_overall_attitude_to_learning_interest) +
                                          (good_participation_inter_two*
                                            percent_participation_to_learning_interest) +
                                         (good_material_inter_two*
                                            percent_good_material_to_learning_interest) +
                                         (good_prior_knowledge_inter_two*
                                            percent_prior_knowledge_to_learning_interest),
                                         var_CV, n)
  
  interest_in_learning_inter_two <- min(interest_in_learning_inter_two_in, 1)
  
  
  # Probability of good creativity ####
  
  good_creativity_inter_one_in <- vv((know_math*plan_diet*
                                       percent_math_diet_to_creativity) + 
                                     (interest_in_learning_inter_one*
                                       percent_learning_interest_to_creativity),
                                      var_CV, n)
  
  good_creativity_inter_one <- min(good_creativity_inter_one_in, 1)
  
  # Probability of motivation to learn ####
  
  motivation_to_learn_inter_one_in <- vv((interest_in_learning_inter_one*
                                           percent_learning_interest_to_motivation) +
                                         (if_trainer_good_interpersonal*
                                           percent_trainer_interpersonal_to_motivation),
                                         var_CV, n)
  
  motivation_to_learn_inter_one <- min(motivation_to_learn_inter_one_in, 1)
  
  
  motivation_to_learn_inter_two_in <- vv((interest_in_learning_inter_two*
                                           percent_learning_interest_to_motivation) +
                                         (if_trainer_good_interpersonal*
                                           percent_trainer_interpersonal_to_motivation),
                                         var_CV, n)
  
  motivation_to_learn_inter_two <- min(motivation_to_learn_inter_two_in, 1)
  
  
  # Probability of having good learning hour (longer hours and more focus) ####
  
  good_learning_hour_inter_one_in <- vv((good_prior_knowledge_inter_one*
                                          percent_prior_knowledge_to_learning_hour) +
                                        (good_student_attitude_inter_one*
                                          extra_study_hour*
                                           percent_time_attitude_to_learning_hour) +
                                       (good_trainer*
                                          percent_good_trainer_to_learning_hour),
                                       var_CV, n)
  
  good_learning_hour_inter_one <- min(good_learning_hour_inter_one_in, 1)
  
  
  good_learning_hour_inter_two_in <- vv((good_prior_knowledge_inter_two*
                                          percent_prior_knowledge_to_learning_hour) +
                                        (good_student_attitude_inter_two*
                                          extra_study_hour*
                                           percent_time_attitude_to_learning_hour) +
                                       (good_trainer*
                                          percent_good_trainer_to_learning_hour),
                                       var_CV, n)
   
  good_learning_hour_inter_two <- min(good_learning_hour_inter_two_in, 1)
  
  
  # Probability of good study habit ####
  
  good_study_habit_inter_one_in <- vv((interest_in_learning_inter_one*
                                        percent_learning_interest_to_study_habit) +
                                      (good_learning_hour_inter_one*
                                        percent_learning_hour_to_study_habit) +
                                     (willingness_learn_inter_one*
                                        percent_willingness_to_study_habit) +
                                     (motivation_to_learn_inter_one*
                                        percent_motivation_to_study_habit),
                                     var_CV, n)
  # Here we use both willingness to learn and motivation to learn. Willingness 
  # and motivation may sound the same but they have different branch variables. 
  # Willingness is more intrinsic and motivation is more extrinsic in this case.
  # We didn't factor willingness as a core variable in intervention two. 
  
  good_study_habit_inter_one <- min(good_study_habit_inter_one_in, 1)
  
 
  # Probability of good facility ####
  
  current_training_venue_like_dislike <- chance_event(current_training_venue,
                                                      value_if = 1,
                                                      value_if_not = 0)
  
  good_facility <- if (current_training_venue_like_dislike == 1) {
    vv(current_training_venue,
       var_CV, n)
  } else {
    vv((1- inconvenience),
       var_CV, n)
  }
  
  # Calculate learning outcome for intervention one
  
  # Here we assume the probability of each set as the probability of 
  # all independent events in the set happening together 
  
  # For chapter one and two 
  
  ch12_moti_interest_trainer_attitude_one_f <- vv(ch12_moti_interest_trainer_attitude_one,
                                                  var_CV, n)
  
  ch12_material_time_one_f <- vv(ch12_material_time_one,
                                 var_CV, n)
  
  ch12_prior_knowledge_one_f <- vv(ch12_prior_knowledge_one,
                                   var_CV, n)
  
  
  inter_one_chapter12_set_one <- (motivation_to_learn_inter_one*
                                  interest_in_learning_inter_one*
                                  good_trainer*
                                  good_student_attitude_inter_one*
                                  ch12_moti_interest_trainer_attitude_one_f)
  
  inter_one_chapter12_set_two <- ((good_material_inter_one- 
                                     (like_final_two*
                                        percent_like_final_two))*
                                   good_learning_hour_inter_one*
                                   ch12_material_time_one_f)
  
  inter_one_chapter12_set_three_one <- (good_prior_knowledge_inter_one*
                                        know_food_group*
                                        ch12_prior_knowledge_one_f)
  
  inter_one_chapter12_set_three_two <- (good_prior_knowledge_inter_one*
                                        know_nutrient*
                                        ch12_prior_knowledge_one_f)
  
  
  # Probability of good learning outcome for chapter one
  
  inter_one_chapter1_in <- (inter_one_chapter12_set_one + 
                            inter_one_chapter12_set_two +
                            inter_one_chapter12_set_three_one)
  
  inter_one_chapter1 <- min(inter_one_chapter1_in, 1)
  
  
  # Probability of good learning outcome for chapter two
  
  inter_one_chapter2_in <- (inter_one_chapter12_set_one + 
                            inter_one_chapter12_set_two +
                            inter_one_chapter12_set_three_two)
  
  inter_one_chapter2 <- min(inter_one_chapter2_in, 1)
  
  
  # For chapter three and four
  
  ch34_moti_interest_trainer_attitude_one_f <- vv(ch34_moti_interest_trainer_attitude_one,
                                                  var_CV, n)
  
  ch34_material_time_one_f <- vv(ch34_material_time_one,
                                 var_CV, n)
  
  ch34_prior_knowledge_one_f <- vv(ch34_prior_knowledge_one,
                                   var_CV, n)
  
  inter_one_chapter34_set_one <- (motivation_to_learn_inter_one*
                                  interest_in_learning_inter_one*
                                  good_trainer*
                                  good_student_attitude_inter_one*
                                  ch34_moti_interest_trainer_attitude_one_f)
  
  inter_one_chapter34_set_two <- ((good_material_inter_one- 
                                     (like_final_two*
                                        percent_like_final_two))*
                                  good_learning_hour_inter_one*
                                  ch34_material_time_one_f)
  
  inter_one_chapter34_set_three_three <- (good_prior_knowledge_inter_one*
                                          know_healthy_eating*
                                          ch34_prior_knowledge_one_f)
  
  inter_one_chapter34_set_three_four <- (good_prior_knowledge_inter_one*
                                         know_nu_requirement*
                                         ch34_prior_knowledge_one_f)
  
  # Probability of good learning outcome for chapter three
  
  inter_one_chapter3_in <- (inter_one_chapter34_set_one +
                            inter_one_chapter34_set_two +
                            inter_one_chapter34_set_three_three)
  
  inter_one_chapter3 <- min(inter_one_chapter3_in, 1)
  
  
  # Probability of good learning outcome for chapter four
  
  inter_one_chapter4_in <- (inter_one_chapter34_set_one +
                            inter_one_chapter34_set_two +
                            inter_one_chapter34_set_three_four)
  
  inter_one_chapter4 <- min(inter_one_chapter4_in, 1)
  
  
  # For chapter five and six
  
  ch56_material_trainer_one_f <- vv(ch56_material_trainer_one, 
                                    var_CV, n)
  
  ch56_moti_persist_one_f <- vv(ch56_moti_persist_one,
                                var_CV, n)
  
  ch56_attitude_commu_one_f <- vv(ch56_attitude_commu_one,
                                  var_CV, n)
  
  ch56_creati_interest_one_f <- vv(ch56_creati_interest_one,
                                   var_CV, n)
  
  ch56_time_prior_one_f <- vv(ch56_time_prior_one,
                              var_CV, n)
  
  ch56_facility_one_f <- vv(ch56_facility_one,
                            var_CV, n)
  
  inter_one_chapter56_set_one <- ((good_material_inter_one-
                                     (like_initial_four*
                                        percent_like_initial_four))*
                                   good_trainer*
                                   ch56_material_trainer_one_f)
  
  inter_one_chapter56_set_two <- (motivation_to_learn_inter_one*
                                  persistence_inter_one*
                                  ch56_moti_persist_one_f)
  
  inter_one_chapter56_set_three <- (good_student_attitude_inter_one*
                                    if_trainer_good_interpersonal*
                                    ch56_attitude_commu_one_f)
  
  inter_one_chapter56_set_four <- (good_creativity_inter_one*
                                   interest_in_learning_inter_one*
                                   ch56_creati_interest_one_f)
  
  inter_one_chapter56_set_five_five <- (good_learning_hour_inter_one*
                                        good_study_habit_inter_one*
                                        good_prior_knowledge_inter_one*
                                        know_nu_calculation*
                                        ch56_time_prior_one_f)
  
  inter_one_chapter56_set_five_six <- (good_learning_hour_inter_one*
                                       good_study_habit_inter_one*
                                       good_prior_knowledge_inter_one*
                                       plan_diet*
                                       know_food_exchange*
                                       ch56_time_prior_one_f)
  
  inter_one_chapter56_set_six <- (good_facility*
                                  ch56_facility_one_f)
  
  # Probability of good learning outcome for chapter five
  
  inter_one_chapter5_in <- (inter_one_chapter56_set_one +
                            inter_one_chapter56_set_two +
                            inter_one_chapter56_set_three +
                            inter_one_chapter56_set_four +
                            inter_one_chapter56_set_five_five +
                            inter_one_chapter56_set_six)
  
  inter_one_chapter5 <- min(inter_one_chapter5_in, 1)
  
  
  # Probability of good learning outcome for chapter six
  
  inter_one_chapter6_in <- (inter_one_chapter56_set_one +
                            inter_one_chapter56_set_two +
                            inter_one_chapter56_set_three +
                            inter_one_chapter56_set_four +
                            inter_one_chapter56_set_five_six +
                            inter_one_chapter56_set_six)
  
  inter_one_chapter6 <- min(inter_one_chapter6_in, 1)
  
  
  # Learning outcome for intervention one ####
  
  Learning_outcome_inter_one <- ((inter_one_chapter1* 10) +
                                 (inter_one_chapter2* 15) +
                                 (inter_one_chapter3* 15) +
                                 (inter_one_chapter4* 20) +
                                 (inter_one_chapter5* 20) +
                                 (inter_one_chapter6* 20))
  
  # Calculate learning outcome for intervention two
  
  # For chapter one and two
  
  ch12_moti_interest_trainer_attitude_two_f <- vv(ch12_moti_interest_trainer_attitude_two,
                                                  var_CV, n)
  
  ch12_commu_material_two_f <- vv(ch12_commu_material_two, 
                                  var_CV, n)
  
  ch12_convenience_facility_two_f <- vv(ch12_convenience_facility_two,
                                        var_CV, n)
  
  ch12_prior_knowledge_two_f <- vv(ch12_prior_knowledge_two,
                                   var_CV, n)
  
  inter_two_chapter12_set_one <- (motivation_to_learn_inter_two*
                                  interest_in_learning_inter_two*
                                  good_trainer*
                                  good_student_attitude_inter_two*
                                  ch12_moti_interest_trainer_attitude_two_f)
  
  inter_two_chapter12_set_two <- (if_trainer_good_interpersonal*
                                  good_material_inter_two*
                                  ch12_commu_material_two_f)
  
  inter_two_chapter12_set_three <- ((1- inconvenience)*
                                    good_facility*
                                    ch12_convenience_facility_two_f)
  
  inter_two_chapter12_set_four_one <- (good_prior_knowledge_inter_two*
                                       know_food_group*
                                       ch12_prior_knowledge_two_f)
  
  inter_two_chapter12_set_four_two <- (good_prior_knowledge_inter_two*
                                       know_nutrient*
                                       ch12_prior_knowledge_two_f)
  
  
  # Probability of good learning outcome for chapter one (intervention two)
  
  inter_two_chapter1_in <- (inter_two_chapter12_set_one +
                            inter_two_chapter12_set_two +
                            inter_two_chapter12_set_three +
                            inter_two_chapter12_set_four_one)
  
  inter_two_chapter1 <- min(inter_two_chapter1_in, 1)
  
  
  # Probability of good learning for chapter two (intervention two)
  
  inter_two_chapter2_in <- (inter_two_chapter12_set_one +
                            inter_two_chapter12_set_two +
                            inter_two_chapter12_set_three +
                            inter_two_chapter12_set_four_two)
  
  inter_two_chapter2 <- min(inter_two_chapter2_in, 1)
  
  
  # For chapter three and four
  
  ch34_moti_interest_trainer_attitude_two_f <- vv(ch34_moti_interest_trainer_attitude_two,
                                                  var_CV, n)
  
  ch34_commu_material_two_f <- vv(ch34_commu_material_two,
                                  var_CV, n)
  
  ch34_convenience_facility_two_f <- vv(ch34_convenience_facility_two,
                                        var_CV, n)
  
  ch34_prior_knowledge_two_f <- vv(ch34_prior_knowledge_two,
                                   var_CV, n)
  
  
  inter_two_chapter34_set_one <- (motivation_to_learn_inter_two*
                                  interest_in_learning_inter_two*
                                  good_trainer*
                                  good_student_attitude_inter_two*
                                  ch34_moti_interest_trainer_attitude_two_f)
  
  inter_two_chapter34_set_two <- (if_trainer_good_interpersonal*
                                  good_material_inter_two*
                                  ch34_commu_material_two_f)
  
  inter_two_chapter34_set_three <- ((1- inconvenience)*
                                    good_facility*
                                    ch34_convenience_facility_two_f)
  
  inter_two_chapter34_set_four_three <- (good_prior_knowledge_inter_two*
                                         know_healthy_eating*
                                         ch34_prior_knowledge_two_f)
  
  inter_two_chapter34_set_four_four <- (good_prior_knowledge_inter_two*
                                        know_nu_requirement*
                                        ch34_prior_knowledge_two_f)
  
  
  # Probability of good learning outcome for chapter three (intervention two)
  
  inter_two_chapter3_in <- (inter_two_chapter34_set_one +
                            inter_two_chapter34_set_two +
                            inter_two_chapter34_set_three +
                            inter_two_chapter34_set_four_three)
  
  
  inter_two_chapter3 <- min(inter_two_chapter3_in, 1)
  
  
  # Probability of good learning outcome for chapter four (intervention two)
  
  inter_two_chapter4_in <- (inter_two_chapter34_set_one +
                            inter_two_chapter34_set_two +
                            inter_two_chapter34_set_three +
                            inter_two_chapter34_set_four_four)
  
  inter_two_chapter4 <- min(inter_two_chapter4_in, 1)
  
  
  # Learning outcome for intervention two ####
  
  Learning_outcome_inter_two <- ((inter_two_chapter1* 25)+
                                 (inter_two_chapter2* 25)+
                                 (inter_two_chapter3* 25)+
                                 (inter_two_chapter4* 25))
  
  # Number of population that will achieve good learning outcome ####
  
  # We already strung the variables to calculate the distributions of learning
  # outcome for MFF players. For example, any MFF player has a baseline 
  # score of 30-50% and after training, the score can shift to 50-95%, etc. 
  # The distribution represents the 90% CI of percent score of MFF players.
  # In other words, we are 90% confident that the percent score of any MFF player
  # will fall within the simulated range of distribution. 
  
  # Now we will calculate the number of MFF players who will achieve at least
  # 90% score at baseline. 

  
  # Calculate number of MFF players who can have at least 90% score at baseline 
 
   # 90 percent baseline for intervention one ####
  
  # Here we assume a MFF player needs to have good prior knowledge of the content
  # in all chapters (1-6) to have at least 90% score. So, we calculate the 
  # probability of a MFF player having good knowledge of all chapters and then,
  # we multiply the resulted probability with total number of athletes. 
  
  Number_player_90score_baseline_inter_one <- (percent_know_food_group*
                                               percent_know_nutrient*
                                               percent_know_healthy_eating*
                                               percent_know_nu_requirement*
                                               percent_know_calculation*
                                               percent_plan_diet*
                                               percent_know_food_exchange
                                               )* number_athelete
  
  # 90 percent baseline for intervention two ####
  
  # We calculate this to see some comparison, but it is not necessary since we 
  # don't have a threshold or target score for intervention two. 
  
  # MFF players having good nutrition knowledge is not crucial for successful 
  # intervention two. The success of intervention mainly depends on nutritionist
  # and good nutritional care system. 
  
  Number_player_90score_baseline_inter_two <- (percent_know_food_group*
                                               percent_know_nutrient*
                                               percent_know_healthy_eating*
                                               percent_know_nu_requirement
                                               )* number_athelete
  
  # Return list ####
  
  return(list(Individual_baseline_inter_one = Individual_baseline_inter_one,
              Individual_baseline_inter_two =  Individual_baseline_inter_two,
              Learning_outcome_inter_one = Learning_outcome_inter_one,
              Learning_outcome_inter_two = Learning_outcome_inter_two,
              Number_player_90score_baseline_inter_one = 
                Number_player_90score_baseline_inter_one,
              Number_player_90score_baseline_inter_two =
               Number_player_90score_baseline_inter_two))
  
}

input_learning_outcome <- read.csv("Learning_outcome.csv")

  # Run the Monte Carlo Simulation

  Learning_outcome_mc_simulation <- mcSimulation(estimate = estimate_read_csv("Learning_outcome.csv"),
                                     model_function = Learning_outcome_function,
                                    numberOfModelRuns = 1000,
                                    functionSyntax = "plainNames")
  
  # Plot distributions histogram
  
  # For intervention one 
  
  plot_distributions(mcSimulation_object = Learning_outcome_mc_simulation,
                     vars = c("Individual_baseline_inter_one", "Learning_outcome_inter_one"),
                     method = 'hist_simple_overlay',
                     x_axis_name = 'Distribution of total score for all MFF players ',
                     base_size = 7)
  
  
  plot_distributions(mcSimulation_object = Learning_outcome_mc_simulation,
                     vars = c("Individual_baseline_inter_two", "Learning_outcome_inter_two"),
                     method = 'hist_simple_overlay',
                     x_axis_name = 'Distribution of total score for MFF players ',
                     base_size = 7)
  
  plot_distributions(mcSimulation_object = Learning_outcome_mc_simulation,
                     vars = "Number_player_90score_baseline_inter_one",
                     method = 'boxplot_density',
                     x_axis_name = 'Total number of MFF players with good learning outcome at baseline for intervention one',
                     base_size = 7)
  
  plot_distributions(mcSimulation_object = Learning_outcome_mc_simulation,
                     vars = "Number_player_90score_baseline_inter_two",
                     method = 'boxplot_density',
                     x_axis_name = 'Total number of MFF players with good learning outcome at baseline for intervention two',
                     base_size = 7)
  
  plot_distributions(mcSimulation_object = Learning_outcome_mc_simulation,
                     vars = "Learning_outcome_inter_one",
                     method = 'boxplot_density',
                     x_axis_name = 'Distribution of total scores for all MFF players after intervention one',
                     base_size = 7)
  
  plot_distributions(mcSimulation_object = Learning_outcome_mc_simulation,
                     vars = "Learning_outcome_inter_two",
                     method = 'boxplot_density',
                     x_axis_name = 'Distribution of total scores for all MFF players after intervention two',
                     base_size = 7)
  
  #Find EVPI 
  
  mcSimulation_table <- data.frame(Learning_outcome_mc_simulation$x, 
                                   Learning_outcome_mc_simulation$y[1:4])
  
  evpi_inter_one <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Learning_outcome_inter_one")
  evpi_inter_two <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Learning_outcome_inter_two")
  evpi_baseline_one <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Individual_baseline_inter_one")
  evpi_baseline_two <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Individual_baseline_inter_two")
 
  
  plot_evpi(evpi_inter_one, decision_vars = "Learning_outcome_inter_one")
  plot_evpi(evpi_inter_two, decision_vars = "Learning_outcome_inter_two")
  plot_evpi(evpi_baseline_one, decision_vars = "Individual_baseline_inter_one")
  plot_evpi(evpi_baseline_two, decision_vars = "Individual_baseline_inter_two")
  

  #Find PLS result
  
  names(Learning_outcome_mc_simulation$y)
  
  pls_result <- plsr.mcSimulation(object = Learning_outcome_mc_simulation,
                                  resultName = names
                                  (Learning_outcome_mc_simulation$y)[1], 
                                  ncomp = 1)

  plot_pls(pls_result, input_table = input_learning_outcome, threshold = 0.9)
  
  
  pls_result <- plsr.mcSimulation(object = Learning_outcome_mc_simulation,
                                  resultName = names
                                  (Learning_outcome_mc_simulation$y)[2], 
                                  ncomp = 1)
  plot_pls(pls_result, input_table = input_learning_outcome, threshold = 0.9)
  
  
  pls_result <- plsr.mcSimulation(object = Learning_outcome_mc_simulation,
                                  resultName = names
                                  (Learning_outcome_mc_simulation$y)[3], 
                                  ncomp = 1)
  plot_pls(pls_result, input_table = input_learning_outcome, threshold = 0.9)
  
  
  pls_result <- plsr.mcSimulation(object = Learning_outcome_mc_simulation,
                                  resultName = names
                                  (Learning_outcome_mc_simulation$y)[4], 
                                  ncomp = 1)
  plot_pls(pls_result, input_table = input_learning_outcome, threshold = 0.9)
  
  
  
  # Summary of distribution of total scores 
  
  summary(Learning_outcome_mc_simulation$y$Learning_outcome_inter_one)
  summary(Learning_outcome_mc_simulation$y$Learning_outcome_inter_two)
  summary(Learning_outcome_mc_simulation$y$Individual_baseline_inter_one)
  summary(Learning_outcome_mc_simulation$y$Individual_baseline_inter_two)
  
  
 