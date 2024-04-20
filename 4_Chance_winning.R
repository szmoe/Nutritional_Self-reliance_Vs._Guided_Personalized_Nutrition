
### Model 4: Chance of Winning


library(decisionSupport)

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("4_Chance_winning.csv")))


Chance_function <- function(x, varnames){
  
# Chance of winning current
  
  chance_winning_current <- vv((((good_nutrition_current*optimal_training_condition_current)*0.5) + 
                                  0.7*(past_winning_percent*team_morale) + (home_field*0.3) +
                                0.8*(good_player*backup_player) + 0.8*(strategy_advantage))/ (3.1),
                               var_CV, n_year)
  
  
  # Chance of winning after intervention
  
  chance_winning_intervention <- vv((((good_nutrition_intervention*optimal_training_condition_intervention)*0.5) + 
                                       0.7*(past_winning_percent*team_morale) + (home_field*0.3) +
                                       0.8*(good_player*backup_player) + 0.8*(strategy_advantage))/ (3.1),
                                    var_CV, n_year)
  
  # Chance against opponent
  
  chance_winning_opponent <- vv((((chance_better_nutrition*chance_better_condition)*0.5) + 
                                       0.7*(past_winning_percent*team_morale) + (home_field*0.3) +
                                       0.8*(good_player*backup_player) + 0.8*(strategy_advantage))/ (3.1),
                                    var_CV, n_year)
  
  
  # Net chance of winning
  
  chance_winning_net <- chance_winning_intervention - chance_winning_current
  
  chance_winning_net_opponent <- chance_winning_opponent - chance_winning_current
  
  return(list(
    chance_winning_current = sum(chance_winning_current),
    chance_winning_intervention = sum(chance_winning_intervention),
    chance_winning_net = sum(chance_winning_net),
    chance_winning_opponent = sum(chance_winning_opponent),
    chance_winning_net_opponent = sum(chance_winning_net_opponent)
  ))
}

# Run the Monte Carlo Simulation

Chance_mc_simulation <- mcSimulation(estimate = estimate_read_csv("4_Chance_winning.csv"),
                                   model_function = Chance_function,
                                   numberOfModelRuns = 1000,
                                   functionSyntax = "plainNames")


# Plot distributions histogram

plot_distributions(mcSimulation_object = Chance_mc_simulation,
                   vars = c("chance_winning_intervention", "chance_winning_current"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'probability',
                   base_size = 7)


plot_distributions(mcSimulation_object = Chance_mc_simulation,
                   vars = c("chance_winning_opponent","chance_winning_current"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'probability',
                   base_size = 7) 

# chance of winning may look increase if we compare between baseline and after intervention of the same team.
# But chance of winning may not show significant increase if we consider against opponent, i.e.,
# when everything else is left to random chance, improving nutrition may not improve chance of winning
# significantly. So, nutrition alone is not the answer. Improvement should be achieved in all areas
# including nutrition. 

plot_distributions(mcSimulation_object = Chance_mc_simulation,
                   vars = c("chance_winning_net", "chance_winning_net_opponent"),
                   method = 'smooth_simple_overlay',
                   x_axis_name = 'probability',
                   base_size = 7) # here is the comparison between net chance of winning-
# net chance of winning of MFF team between baseline and intervention (compare itself at two time intervals)
# is significantly higher than the net chance of winning between MFF and opposition team between baseline 
# and after intervention. So, this means if we compare between team status at baseline and intervention,
# we can see improvement. But if we compare our team status against another team at baseline and intervention, 
# there may not be any improvement. 


Chance_mcSimulation_table <- data.frame(Chance_mc_simulation$x,
                                        Chance_mc_simulation$y[1:5])

evpi <- multi_EVPI(mc = Chance_mcSimulation_table, first_out_var = "chance_winning_current")
evpi_1 <- multi_EVPI(mc = Chance_mcSimulation_table, first_out_var = "chance_winning_intervention")
evpi_2 <- multi_EVPI(mc = Chance_mcSimulation_table, first_out_var = "chance_winning_opponent")
plot_evpi(evpi, decision_vars = "chance_winning_current")
plot_evpi(evpi_1, decision_vars = "chance_winning_intervention")
plot_evpi(evpi_2, decision_vars = "chance_winning_opponent")


#Find PLS result
pls_result <- plsr.mcSimulation(object = Chance_mc_simulation,
                                resultName = names
                                (Chance_mc_simulation$y)[1], 
                                ncomp = 1)

plot_pls(pls_result, input_table = read.csv("4_Chance_winning.csv"), threshold = 0)

Calorie_mc_simulation <- decisionSupport("4_Chance_winning.csv",
                                         outputPath = 'results_Chance',
                                         welfareFunction = Chance_function,
                                         numberOfModelRuns = 1000,
                                         functionSyntax = "plainNames")

# model 4 done, move on to model 5
