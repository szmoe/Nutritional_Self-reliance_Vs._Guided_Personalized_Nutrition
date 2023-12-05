# Nutritional self-reliance Vs. guided personalized nutrition for 
# improved sports performance

# Learning outcome model (first model)

# Here we simulate baseline knowledge and learning outcome after training.
# We simulate outcome for before and after intervention (two time points).
# So, time series function is not used. We model this based on scoring system of
# a formal exam. But we are not simulating how well each player would score 
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
 
  # Calculate baseline knowledge 
  
  # Here we use the correction factor (see explanation in excel) to prevent 
  # overestimation of score at baseline. Individuals may over- or under- 
  # estimate their knowledge level- a correction factor is used to fix that.
  
  
  # Individual-baseline for intervention 1 ####
  
  Individual_baseline_inter_one <- ((know_food_group*10) + (know_nutrient*15) +
    (know_healthy_eating*15) + (know_nu_requirement*20) + 
    (know_nu_calculation*20) + ((plan_diet*know_food_exchange)*20))- 
    (correction_factor* 100)
  
  # Individual-baseline for intervention 2 ####
  
  Individual_baseline_inter_two <- ((know_food_group*25) + (know_nutrient*25) +
    (know_healthy_eating*25) + (know_nu_requirement*25))- (correction_factor* 100)
  
  # Before we calculate the outcomes after intervention, we string branch 
  # variables into core variables first, based on decision template.
  
  # We don't have priors for each links between the branch variables and
  # core variables. So, we assign degree of association based on past experience
  # and knowledge (DW, DT, IPM).
  
  # That means we need really good priors for this model to work properly. 
  # Not having priors for variable links will be a disadvantage. Otherwise, we
  # need to use ranges for each variable- rather a tedious task. 
  
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
  
  good_trainer <- (if_trainer_good_interpersonal*percent_good_interpersonal) + 
                  (if_trainer_good_teaching*percent_good_teaching) +
                  (if_trainer_good_knowledge*percent_good_knowledge)
  
  if (good_trainer <= 1) {
    good_trainer <- good_trainer
  } else {
    good_trainer <- 1
  }
  
  # Probability of having good value-related belief ####
  
  good_value_belief <- (know_importance_nu*percent_know_importance_nu) + 
                       (take_care_diet*percent_take_care_diet)
  
  if (good_value_belief <= 1) {
    good_value_belief <- good_value_belief
  } else {
    good_value_belief <- 1
  }
  
  # Probability of having good learning and practice materials ####
  
  good_material_inter_one <- (like_learning_material*percent_like_learning_material) + 
                             (like_initial_four*percent_like_initial_four) +
                             (like_final_two*percent_like_final_two)
  
  if (good_material_inter_one <= 1) {
    good_material_inter_one <- good_material_inter_one
  } else {
    good_material_inter_one <- 1
  }
  
  good_material_inter_two <- (like_learning_material*percent_like_learning_material) + 
                             (like_initial_four*percent_like-initial_four)
  
  if (good_material_inter_two <= 1) {
    good_material_inter_two <- good_material_inter_two
  } else {
    good_material_inter_two <- 1
  }
  
  # Probability of good attitude towards nutrition training ####
  
  good_attitude_training_inter_one <- ((good_trainer*percent_good_trainer_to_attitude) + 
                                       (good_material_inter_one*percent_good_material_to_attitude) +
                                       (good_value_belief*percent_good_value_to_attitude))
  
  if (good_attitude_training_inter_one <= 1) {
    good_attitude_training_inter_one <- good_attitude_training_inter_one
  } else {
    good_attitude_training_inter_one <- 1
  }
  
  good_attitude_training_inter_two <- ((good_trainer*percent_good_trainer_to_attitude) + 
                                       (good_material_inter_two*percent_good_material_to_attitude) +
                                       (good_value_belief*percent_good_value_to_attitude))
  
  if (good_attitude_training_inter_two <= 1) {
    good_attitude_training_inter_two <- good_attitude_training_inter_two
  } else {
    good_attitude_training_inter_two <- 1
  }
  
  # Probability of willingness to learn ####
  
  willingness_learn_inter_one <- ((interest_advanced_training*
                                     percent_interest_training_to_willingness) +
                                 (extra_study_hour*
                                    percent_extra_hour_to_willingness) + 
                                 (motivation_apply_knowledge*
                                    percent_motivation_apply_to_willingness) +
                                 (easy_use_resource*
                                    percent_easy_resource_to_willingness))
  
  if (willingness_learn_inter_one <= 1) {
    willingness_learn_inter_one <- willingness_learn_inter_one
  } else {
    willingness_learn_inter_one <- 1
  }
  
  # Probability of having persistance ####
  
  persistence_inter_one <- ((good_trainer*percent_good_trainer_to_persistence) +
                            (interest_advanced_training*
                               percent_interest_training_to_persistence) +
                            (motivation_apply_knowledge*
                               percent_motivation_apply_to_persistence))
  
  if (persistence_inter_one <= 1) {
    persistence_inter_one <- persistence_inter_one
  } else {
    persistence_inter_one <- 1
  }
  
  # Probability of good student attitude ####
  
  good_student_attitude_inter_one <- ((willingness_learn_inter_one*
                                         percent_willingness_to_attitude) +
                                     (good_attitude_training_inter_one*
                                        percent_attitude_towards_training_to_attitude) +
                                     (persistence_inter_one*percent_persistence_to_attitude))
  
  if (good_student_attitude_inter_one <= 1) {
    good_student_attitude_inter_one <- good_student_attitude_inter_one
  } else {
    good_student_attitude_inter_one <- 1
  }
  
  good_student_attitude_inter_two <- good_attitude_training_inter_two
  
  # Probability of inconvenience ####
  
  inconvenience <- ((if_improper_time*percent_improper_time_to_inconvenience) + 
                    (if_improper_location*
                       percent_improper_location_to_inconvenience))
  
  if (inconvenience <= 1) {
    inconvenience <- inconvenience
  } else {
    inconvenience <- 1
  }
  
  # Probability of good participation ####
  
  good_participation_inter_one <- (((1-if_long_training)*
                                     percent_short_training_to_participation) + 
                                    ((1-if_weekend_training)*
                                       percent_weekday_training_to_participation)+
                                    ((1-inconvenience)*
                                       percent_convenience_to_participation) + 
                                    ((1-if_give_homework)*
                                       percent_no_homework_to_participation) +
                                     (good_student_attitude_inter_one*
                                        percent_overall_attitude_to_participation)) 
  
  if (good_participation_inter_one <= 1) {
    good_participation_inter_one <- good_participation_inter_one
  } else {
    good_participation_inter_one <- 1
  }
  
  good_participation_inter_two <- (((1-if_long_training)*
                                     percent_short_training_to_participation) +
                                    ((1-if_weekend_training)*
                                       percent_weekday_training_to_participation)+
                                     ((1-inconvenience)*
                                        percent_convenience_to_participation) + 
                                     ((1-if_give_homework)*
                                        percent_no_homework_to_participation) +
                                     (good_student_attitude_inter_two*
                                        percent_overall_attitude_to_participation)) 
  
  if (good_participation_inter_two <= 1) {
    good_participation_inter_two <- good_participation_inter_two
  } else {
    good_participation_inter_two <- 1
  }
  
  # Probability of good activities 
  
  good_activity_inter_one <- ((good_participation_inter_one*
                                 percent_participation_to_activity) +
                             (good_trainer*
                                percent_good_trainer_to_activity))
  
  if (good_activity_inter_one <= 1) {
    good_activity_inter_one <- good_activity_inter_one
  } else { 
    good_activity_inter_one <- 1
    }
  
  good_activity_inter_two <- ((good_participation_inter_two*
                                 percent_participation_to_activity) +
                             (good_trainer*
                                percent_good_trainer_to_activity))
  
  if (good_activity_inter_two <= 1) {
    good_activity_inter_two <- good_activity_inter_two
  } else {
    good_activity_inter_two <- 1
  }
  
  # Probability of good prior knowledge/ experience ####
  
  good_prior_knowledge_inter_one <- ((take_care_diet*
                                        percent_take_care_diet_to_prior_knowledge) + 
                                     (follow_diet_plan * 
                                       percent_follow_diet_plan_to_prior_knowledge))
  
  if (good_prior_knowledge_inter_one <= 1) {
    good_prior_knowledge_inter_one <- good_prior_knowledge_inter_one
  } else {
    good_prior_knowledge_inter_one <- 1
  }
  
  good_prior_knowledge_inter_two <- take_care_diet
  
  
  # Finding core variables
  
  # Probability of interest in learning ####
  
  interest_in_learning_inter_one <- ((good_value_belief*0.1) + 
                                    (good_student_attitude_inter_one*0.3) +
                                    (good_participation_inter_one*0.1) +
                                    (good_material_inter_one*0.2) +
                                    (good_activity_inter_one*0.1) +
                                    (good_prior_knowledge_inter_one*0.1))
  
  interest_in_learning_inter_two <- (good_value_belief*0.2) +
                                    (good_student_attitude_inter_two*0.3) +
                                    (good_participation_inter_two*0.1) +
                                    (good_material_inter_two*0.2) +
                                    (good_prior_knowledge_inter_two*0.2)
  
  # Probability of good creativity ####
  
  good_creativity_inter_one <- (know_math*plan_diet*0.5) + 
    (interest_in_learning_inter_one*0.5)
  
  # Probability of motivation to learn ####
  
  motivation_to_learn_inter_one <- (interest_in_learning_inter_one*0.5) +
                                   (if_trainer_good_interpersonal*0.5)
  
  motivation_to_learn_inter_two <- (interest_in_learning_inter_two*0.5) +
                                   (if_trainer_good_interpersonal*0.5)
  
  # Probability of having good learning hour (longer hours and more focus) ####
  
  good_learning_hour_inter_one <- (good_prior_knowledge_inter_one*0.1) +
    (good_student_attitude_inter_one*0.5) +
    (good_trainer*0.4)
  
  good_learning_hour_inter_two <- (good_prior_knowledge_inter_two*0.1) +
    (good_student_attitude_inter_two*0.5) +
    (good_trainer*0.4)
  
  # Probability of good study habit ####
  
  good_study_habit_inter_one <- (interest_in_learning_inter_one*0.3) +
                                (extra_study_hour*0.2) +
                                (willingness_learn_inter_one*0.2) +
                                (motivation_to_learn_inter_one*0.3)
  
  
  # Probability of good facility ####
  
  current_training_venue_like_dislike <- chance_event(current_training_venue,
                                                      value_if = 1,
                                                      value_if_not = 0)
  
  good_facility <- if (current_training_venue_like_dislike == 1) {
    current_training_venue
  } else {
    (1- inconvenience)
  }
  
  # Calculate learning outcome for intervention one
  
  # Here we assume the probability of each set as the probability of 
  # all independent events in the set happening together 
  
  # For chapter one and two 
  
  inter_one_chapter12_set_one <- (motivation_to_learn_inter_one*
                                  interest_in_learning_inter_one*
                                  good_trainer*
                                  good_student_attitude_inter_one*
                                  ch12_moti_interest_trainer_attitude_one)
  
  inter_one_chapter12_set_two <- ((good_material_inter_one- (like_final_two*0.25))*
                                   good_learning_hour_inter_one*
                                   ch12_material_time_one)
  
  inter_one_chapter12_set_three_one <- (good_prior_knowledge_inter_one*
                                        know_food_group*
                                        ch12_prior_knowledge_one)
  
  inter_one_chapter12_set_three_two <- (good_prior_knowledge_inter_one*
                                        know_nutrient*
                                        ch12_prior_knowledge_one)
  
  
  # Probability of good learning outcome for chapter one
  
  inter_one_chapter1 <- (inter_one_chapter12_set_one + 
                          inter_one_chapter12_set_two +
                          inter_one_chapter12_set_three_one)
  
  # Probability of good learning outcome for chapter two
  
  inter_one_chapter2 <- (inter_one_chapter12_set_one + 
                           inter_one_chapter12_set_two +
                           inter_one_chapter12_set_three_two)
  
  # For chatper three and four
  
  inter_one_chapter34_set_one <- (motivation_to_learn_inter_one*
                                  interest_in_learning_inter_one*
                                  good_trainer*
                                  good_student_attitude_inter_one*
                                  ch34_moti_interest_trainer_attitude_one)
  
  inter_one_chapter34_set_two <- ((good_material_inter_one- (like_final_two*0.25))*
                                  good_learning_hour_inter_one*
                                  ch34_material_time_one)
  
  inter_one_chapter34_set_three_three <- (good_prior_knowledge_inter_one*
                                          know_healthy_eating*
                                          ch34_prior_knowledge_one)
  
  inter_one_chapter34_set_three_four <- (good_prior_knowledge_inter_one*
                                         know_nu_requirement*
                                         ch34_prior_knowledge_one)
  
  # Probability of good learning outcome for chapter three
  
  inter_one_chapter3 <- (inter_one_chapter34_set_one +
                         inter_one_chapter34_set_two +
                         inter_one_chapter34_set_three_three)
  
  # Probability of good learning outcome for chapter four
  
  inter_one_chapter4 <- (inter_one_chapter34_set_one +
                         inter_one_chapter34_set_two +
                         inter_one_chapter34_set_three_four)
  
  # For chapter five and six
  
  inter_one_chapter56_set_one <- ((good_material_inter_one-(like_initial_four*0.25))*
                                   good_trainer*
                                   ch56_material_trainer_one)
  
  inter_one_chapter56_set_two <- (motivation_to_learn_inter_one*
                                  persistence_inter_one*
                                  ch56_moti_persist_one)
  
  inter_one_chapter56_set_three <- (good_student_attitude_inter_one*
                                    if_trainer_good_interpersonal*
                                    ch56_attitude_commu_one)
  
  inter_one_chapter56_set_four <- (good_creativity_inter_one*
                                   interest_in_learning_inter_one*
                                   ch56_creati_interest_one)
  
  inter_one_chapter56_set_five_five <- (good_learning_hour_inter_one*
                                        good_study_habit_inter_one*
                                        good_prior_knowledge_inter_one*
                                        know_nu_calculation*
                                        ch56_time_prior_one)
  
  inter_one_chapter56_set_five_six <- (good_learning_hour_inter_one*
                                       good_study_habit_inter_one*
                                       good_prior_knowledge_inter_one*
                                       plan_diet*
                                       know_food_exchange*
                                       ch56_time_prior_one)
  
  inter_one_chapter56_set_six <- (good_facility*
                                  ch56_facility_one)
  
  # Probability of good learning outcome for chapter five
  
  inter_one_chapter5 <- (inter_one_chapter56_set_one +
                         inter_one_chapter56_set_two +
                         inter_one_chapter56_set_three +
                         inter_one_chapter56_set_four +
                         inter_one_chapter56_set_five_five +
                         inter_one_chapter56_set_six)
  
  # Probability of good learning outcome for chapter six
  
  inter_one_chapter6 <- (inter_one_chapter56_set_one +
                         inter_one_chapter56_set_two +
                         inter_one_chapter56_set_three +
                         inter_one_chapter56_set_four +
                         inter_one_chapter56_set_five_six +
                         inter_one_chapter56_set_six)
  
  # Learning outcome for intervention one ####
  
  Learning_outcome_inter_one <- ((inter_one_chapter1* 10) +
                                 (inter_one_chapter2* 15) +
                                 (inter_one_chapter3* 15) +
                                 (inter_one_chapter4* 20) +
                                 (inter_one_chapter5* 20) +
                                 (inter_one_chapter6* 20))
  
  # Calculate learning outcome for intervention two
  
  # For chapter one and two
  
  inter_two_chapter12_set_one <- (motivation_to_learn_inter_two*
                                  interest_in_learning_inter_two*
                                  good_trainer*
                                  good_student_attitude_inter_two*
                                  ch12_moti_interest_trainer_attitude_two)
  
  inter_two_chapter12_set_two <- (if_trainer_good_interpersonal*
                                  good_material_inter_two*
                                  ch12_commu_material_two)
  
  inter_two_chapter12_set_three <- ((1- inconvenience)*
                                    good_facility*
                                    ch12_convenience_facility_two)
  
  inter_two_chapter12_set_four_one <- (good_prior_knowledge_inter_two*
                                       know_food_group*
                                       ch12_prior_knowledge_two)
  
  inter_two_chapter12_set_four_two <- (good_prior_knowledge_inter_two*
                                       know_nutrient*
                                       ch12_prior_knowledge_two)
  
  # Probability of good learning outcome for chapter one (intervention two)
  
  inter_two_chapter1 <- (inter_two_chapter12_set_one +
                         inter_two_chapter12_set_two +
                         inter_two_chapter12_set_three +
                         inter_two_chapter12_set_four_one)
  
  # Probability of good learning for chapter two (intervention two)
  
  inter_two_chapter2 <- (inter_two_chapter12_set_one +
                         inter_two_chapter12_set_two +
                         inter_two_chapter12_set_three +
                         inter_two_chapter12_set_four_two)
  
  # For chapter three and four
  
  inter_two_chapter34_set_one <- (motivation_to_learn_inter_two*
                                  interest_in_learning_inter_two*
                                  good_trainer*
                                  good_student_attitude_inter_two*
                                  ch34_moti_interest_trainer_attitude_two)
  
  inter_two_chapter34_set_two <- (if_trainer_good_interpersonal*
                                  good_material_inter_two*
                                  ch34_commu_material_two)
  
  inter_two_chapter34_set_three <- ((1- inconvenience)*
                                    good_facility*
                                    ch34_convenience_facility_two)
  
  inter_two_chapter34_set_four_three <- (good_prior_knowledge_inter_two*
                                         know_healthy_eating*
                                         ch34_prior_knowledge_two)
  
  inter_two_chapter34_set_four_four <- (good_prior_knowledge_inter_two*
                                        know_nu_requirement*
                                        ch34_prior_knowledge_two)
  
  # Probability of good learning outcome for chapter three (intervention two)
  
  inter_two_chapter3 <- (inter_two_chapter34_set_one +
                         inter_two_chapter34_set_two +
                         inter_two_chapter34_set_three +
                         inter_two_chapter34_set_four_three)
  
  # Probability of good learning outcome for chapter four (intervention two)
  
  inter_two_chapter4 <- (inter_two_chapter34_set_one +
                         inter_two_chapter34_set_two +
                         inter_two_chapter34_set_three +
                         inter_two_chapter34_set_four_four)
  
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
  # 90% score after interventions and at baseline. 

  
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
  
  # We calculate this to see some comparison, but it's not necessary since we 
  # don't have a threshold or target score for intervention two. 
  
  # MFF players having good nutrition knowledge is not crucial for successful 
  # intervention two. The success of intervention mainly depends on nutritionist
  # and good nutritional care system. 
  
  Number_player_90score_baseline_inter_two <- (percent_know_food_group*
                                               percent_know_nutrient*
                                               percent_know_healthy_eating*
                                               percent_know_nu_requirement
                                               )* number_athelete
  
  # Calculate number of player who have at least 90% score after intervention one
  
  # Here we find the probability of having good learning outcome for all chapters
  # and mulitply it with total number of athletes. 
  
  # 90 percent for intervention one using model result ####
  
  Number_player_90score_after_training_inter_one <- (inter_one_chapter1*
                                                     inter_one_chapter2*
                                                     inter_one_chapter3*
                                                     inter_one_chapter4*
                                                     inter_one_chapter5*
                                                     inter_one_chapter6
                                                     )* number_athelete
                  
  # 90 percent for intervention two using model result ####
  
  
  Number_player_90score_after_training_inter_two <- (inter_two_chapter1*
                                                     inter_two_chapter2*
                                                     inter_two_chapter3*
                                                     inter_two_chapter4
                                                     )* number_athelete
  
  # Return list ####
  
  return(list(Individual_baseline_inter_one = Individual_baseline_inter_one,
              Individual_baseline_inter_two =  Individual_baseline_inter_two,
              Learning_outcome_inter_one = Learning_outcome_inter_one,
              Learning_outcome_inter_two = Learning_outcome_inter_two,
              Number_player_90score_baseline_inter_one = 
                Number_player_90score_baseline_inter_one,
              Number_player_90score_baseline_inter_two =
                Number_player_90score_baseline_inter_two,
              Number_player_90score_after_training_inter_one =
                Number_player_90score_after_training_inter_one,
              Number_player_90score_after_training_inter_two =
                Number_player_90score_after_training_inter_two))
  
}

input_learing_outcome <- read.csv("Learning_outcome.csv")

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
                     x_axis_name = 'Possible distribution of total score for each MFF player ',
                     base_size = 7)
  
  
  plot_distributions(mcSimulation_object = Learning_outcome_mc_simulation,
                     vars = c("Individual_baseline_inter_two", "Learning_outcome_inter_two"),
                     method = 'hist_simple_overlay',
                     x_axis_name = 'Possible distribution of total score for each MFF player ',
                     base_size = 7)
  
  plot_distributions(mcSimulation_object = Learning_outcome_mc_simulation,
                     vars = c("Number_player_90score_baseline_inter_one", 
                              "Number_player_90score_after_training_inter_one"),
                     method = 'hist_simple_overlay',
                     x_axis_name = 'Total number of MFF players with good learning outcome ',
                     base_size = 7)
  
  plot_distributions(mcSimulation_object = Learning_outcome_mc_simulation,
                     vars = c("Number_player_90score_baseline_inter_two", 
                              "Number_player_90score_after_training_inter_two"),
                     method = 'hist_simple_overlay',
                     x_axis_name = 'Total number of MFF players with good learning outcome ',
                     base_size = 7)
  



