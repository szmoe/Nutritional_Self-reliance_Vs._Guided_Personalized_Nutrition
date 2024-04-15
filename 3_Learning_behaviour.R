
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
  
  
  # Calculate improved knowledge with good trainer conditional on having prior knowledge
  
  
  # Calculate athletes' interest percent
  
  
  # Calculate athlete's motivation percent
  
  
  # Calculate chance of good learning outcome
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################################################################
  
  ### Maintain good nutrition
  
  
  
  
  ################################################################################
  
  ### Calculate actual intake and potential food waste (from leftover/uneaten food only)
  
  
  
  
  
  
  
  
  ################################################################################
  
  ### Well being
  
  
  
  
  
  ################################################################################
  
  ### Sports outcome
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}







