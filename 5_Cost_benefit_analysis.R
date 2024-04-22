
### Model 5: Cost and Benefit analysis

library(decisionSupport)

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("5_Cost_benefit.csv")))

Cost_benefit_function <- function(x, varnames){
  
  # Calculate total food cost
  
  yearly_food_cost <- vv((yearly_egg_serving_all_athletes*egg_price_per_serving) +
                           (yearly_dairy_serving_all_athletes*dairy_price_per_serving) +
                           (yearly_sandwich_serving_all_athletes*sandwich_price_per_serving) +
                           (yearly_vegetable_mushroom_serving_all_athletes*vegetable_mushroom_price_per_serving)+
                           (yearly_bean_nut_serving_all_athletes*bean_nut_price_per_serving)+
                           (yearly_fruit_serving_all_athletes*fruit_price_per_serving) +
                           (yearly_sports_drink_serving_all_athletes*sports_drink_price_per_serving) +
                           (yearly_chicken_serving_all_athletes*chicken_price_per_serving) +
                           (yearly_pork_serving_all_athletes*pork_price_per_serving) +
                           (yearly_beef_serving_all_athletes*beef_price_per_serving) +
                           (yearly_fish_serving_all_athletes*fish_price_per_serving) +
                           (yearly_prawn_serving_all_athletes*prawn_price_per_serving) +
                           (yearly_rice_noodle_serving_all_athletes*rice_noodle_price_per_serving) +
                           (yearly_fat_oil_serving_all_athletes*fat_oil_price_per_serving),
                         var_CV, n_year, relative_trend = inflation_rate)
  
  
  # Calculate food waste cost for intervention one
  
  yearly_food_waste_cost_inter_one <- vv((yearly_egg_serving_wasted_inter_one*egg_price_per_serving) +
                                 (yearly_dairy_serving_wasted_inter_one*dairy_price_per_serving) +
                                 (yearly_vegetable_mushroom_serving_wasted_inter_one*
                                    vegetable_mushroom_price_per_serving) +
                                 (yearly_bean_nut_serving_wasted_inter_one*bean_nut_price_per_serving) +
                                 (yearly_pork_serving_wasted_inter_one*pork_price_per_serving) +
                                 (yearly_beef_serving_wasted_inter_one*beef_price_per_serving) +
                                 (yearly_fish_serving_wasted_inter_one*fish_price_per_serving) +
                                 (yearly_prawn_serving_wasted_inter_one*prawn_price_per_serving),
                               var_CV, n_year, relative_trend = inflation_rate)
  
  
  # Calculate food waste cost for intervention two
  
  yearly_food_waste_cost_inter_two <- vv((yearly_egg_serving_wasted_inter_two*egg_price_per_serving) +
                                           (yearly_dairy_serving_wasted_inter_two*dairy_price_per_serving) +
                                           (yearly_vegetable_mushroom_serving_wasted_inter_two*
                                              vegetable_mushroom_price_per_serving) +
                                           (yearly_bean_nut_serving_wasted_inter_two*bean_nut_price_per_serving) +
                                           (yearly_pork_serving_wasted_inter_two*pork_price_per_serving) +
                                           (yearly_beef_serving_wasted_inter_two*beef_price_per_serving) +
                                           (yearly_fish_serving_wasted_inter_two*fish_price_per_serving) +
                                           (yearly_prawn_serving_wasted_inter_two*prawn_price_per_serving),
                                         var_CV, n_year, relative_trend = inflation_rate)
  
  
  # Calculate usable food cost for intervention one 
  
  usable_food_cost_inter_one <- yearly_food_cost - yearly_food_waste_cost_inter_one
  
  
  # Calculate usable food cost for intervention two 
  
  usable_food_cost_inter_two <- yearly_food_cost - yearly_food_waste_cost_inter_two
  
  ## Calculate total cost for intervention one
  
  # First year extra cost
  
  first_year_cost_inter_one <- vv(training_cost_inter_one + support_material_cost_inter_one +
                                  yearly_food_cost + yearly_cost_condiments +
                                  yearly_cost_electricity_gas + yearly_cost_maintenance +
                                  yearly_nutrition_person_cost + yearly_audit_cost,
                                  var_CV, n = 1, relative_trend = inflation_rate)
    
  
  # Total cost
  
  total_cost_inter_one <- vv(yearly_food_cost + yearly_cost_condiments +
                               yearly_cost_electricity_gas + yearly_cost_maintenance +
                               yearly_nutrition_person_cost + yearly_audit_cost,
                               var_CV, n_year, relative_trend = inflation_rate)
  
  total_cost_inter_one[1] <- first_year_cost_inter_one
                                       
  
  # Calculate total cost for intervention two
  
  first_year_cost_inter_two <- vv(training_cost_inter_two + support_material_cost_inter_two +
                                    yearly_food_cost + yearly_cost_condiments +
                                    yearly_cost_electricity_gas + yearly_cost_maintenance +
                                    yearly_nutrition_team_cost + yearly_audit_cost +
                                    extra_kitchen_staff_cost_inter_two,
                                  var_CV, n = 1, relative_trend = inflation_rate)
  
  total_cost_inter_two <- vv(yearly_food_cost + yearly_cost_condiments +
                          yearly_cost_electricity_gas + yearly_cost_maintenance +
                          yearly_nutrition_team_cost + yearly_audit_cost+
                         extra_kitchen_staff_cost_inter_two,
                        var_CV, n_year, relative_trend = inflation_rate)
  
  total_cost_inter_two[1] <- first_year_cost_inter_two
  
  
  ####################################
  
  # Calculate nutrition benefit for intervention one
  
  nutrition_value_inter_one <- vv(nutrition_value*chance_good_nutrition_inter_one,
                                  var_CV, n_year, relative_trend = inflation_rate)
  
  # Calculate increased nutrition benefit if food waste is saved
  
  chance_save_food_waste <- chance_event(percent_waste_reduced_inter_one,
                                         value_if = 1,
                                         value_if_not = 0)
  
  benefit_nutrition_inter_one <- if(chance_save_food_waste == 1){
    
    yearly_food_cost * (nutrition_value_inter_one/usable_food_cost_inter_one)
  } else{
    nutrition_value_inter_one
  }
  
  # Calculate training value for intervention one
  # higher performance may bring in more sponsorship and merchandise sales
  
  benefit_training_inter_one <- vv(chance_training_performance_inter_one*
                                     merchandise_sponsorship_value,
                                   var_CV, n_year, relative_trend = inflation_rate)*weather_risk
  
  # Calculate mental health benefit for intervention one
  
  benefit_mental_health_inter_one <- vv(chance_wellbeing_inter_one*mental_health_value,
                                        var_CV, n_year, relative_trend = inflation_rate)
  
  # Total benefit for intervention one
  
  total_benefit_inter_one <- vv(benefit_nutrition_inter_one + benefit_training_inter_one + 
                                  benefit_mental_health_inter_one,
                                var_CV, n_year,
                                relative_trend = inflation_rate)
  
  
  #######################
  
  # Calculate nutrition benefit for intervention two
  
  nutrition_value_inter_two <- vv(nutrition_value*chance_good_nutrition_inter_two,
                                  var_CV, n_year, relative_trend = inflation_rate)
  
  # Calculate increased nutrition benefit if food waste is saved
  
  chance_save_food_waste_two <- chance_event(percent_waste_reduced_inter_two,
                                         value_if = 1,
                                         value_if_not = 0)
  
  benefit_nutrition_inter_two <- if(chance_save_food_waste_two == 1){
    
    yearly_food_cost * (nutrition_value_inter_two/usable_food_cost_inter_two)
  } else{
    nutrition_value_inter_two
  }
  
  # Calculate training value for intervention two
  
  benefit_training_inter_two <- vv(chance_training_performance_inter_two*
                                     merchandise_sponsorship_value,
                                   var_CV, n_year, relative_trend = inflation_rate)*weather_risk
  
  # Calculate mental health benefit for intervention two
  
  benefit_mental_health_inter_two <- vv(chance_wellbeing_inter_two*mental_health_value,
                                        var_CV, n_year, relative_trend = inflation_rate)
  
  # Total benefit for intervention two
  
  total_benefit_inter_two <- vv(benefit_nutrition_inter_two + benefit_training_inter_two + 
                                  benefit_mental_health_inter_two,
                                var_CV, n_year,
                                relative_trend = inflation_rate)
  
  ##################################
  
  # Calculate net profit 
  
  net_profit_inter_one <- total_benefit_inter_one - total_cost_inter_one
  
  net_profit_inter_two <- total_benefit_inter_two - total_cost_inter_two
                             
  # Calculate NPV with discount rate
  
  NPV_inter_one <- discount(x = net_profit_inter_one,
                            discount_rate = discount_rate,
                            calculate_NPV = TRUE)
  
  NPV_inter_two <- discount(x = net_profit_inter_two,
                            discount_rate = discount_rate,
                            calculate_NPV = TRUE)
  
  
  return(list(NPV_intervention_one = NPV_inter_one,
              NPV_intervention_two = NPV_inter_two,
              Cashflow_intervention_one = net_profit_inter_one,
              Cashflow_intervention_two = net_profit_inter_two,
              total_cost_intervention_one = sum(total_cost_inter_one),
              total_cost_intervention_two = sum(total_cost_inter_two)
              ))

  
}

# Run the Monte Carlo Simulation

Cost_benefit_mc_simulation <- mcSimulation(estimate = estimate_read_csv("5_Cost_benefit.csv"),
                                   model_function = Cost_benefit_function,
                                   numberOfModelRuns = 1000,
                                   functionSyntax = "plainNames")

# Plot distributions histogram

plot_distributions(mcSimulation_object = Cost_benefit_mc_simulation,
                   vars = c("NPV_intervention_one", "NPV_intervention_two"),
                   method = 'hist_simple_overlay',
                   x_axis_name = 'Myanmar kyat',
                   base_size = 7)

# Plot distributions boxplot

plot_distributions(mcSimulation_object = Cost_benefit_mc_simulation,
                   vars = c("NPV_intervention_one", "NPV_intervention_two"),
                   x_axis_name = 'Myanmar kyat',
                   method = 'boxplot')

# Plot distributions smooth overlay

plot_distributions(mcSimulation_object = Cost_benefit_mc_simulation,
                   vars = c("total_cost_intervention_one", "total_cost_intervention_two"),
                   method = 'smooth_simple_overlay')

plot_cashflow(mcSimulation_object = Cost_benefit_mc_simulation,
              cashflow_var_name = c("Cashflow_intervention_one", "Cashflow_intervention_two"),
              x_axis_name = "Year",
              y_axis_name = "Cashflow in Myanmar Kyat") 

#Find EVPI 

mcSimulation_table <- data.frame(Cost_benefit_mc_simulation$x, 
                                 Cost_benefit_mc_simulation$y[1:2])

evpi_one <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_intervention_one")
evpi_two <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_intervention_two")
plot_evpi(evpi_one, decision_vars = "NPV_intervention_one")
plot_evpi(evpi_two, decision_vars = "NPV_intervention_two")

# PLS result

pls_result <- plsr.mcSimulation(object = Cost_benefit_mc_simulation,
                                resultName = names
                                (Cost_benefit_mc_simulation$y)[1], 
                                ncomp = 1)

plot_pls(pls_result, input_table = read.csv("5_Cost_benefit.csv"), threshold = 1)

pls_result <- plsr.mcSimulation(object = Cost_benefit_mc_simulation,
                                resultName = names
                                (Cost_benefit_mc_simulation$y)[2], 
                                ncomp = 1)

plot_pls(pls_result, input_table = read.csv("5_Cost_benefit.csv"), threshold = 1)
