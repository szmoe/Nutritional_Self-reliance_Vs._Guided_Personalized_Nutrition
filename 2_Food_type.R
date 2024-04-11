
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
# This is also a limitation. Pooling food items is not a good idea- coz blank range can exist.
# Should use constant throughout (if use range, it should be narrow) and separate the food items- 
# dataset will be REALLY big though.

Food_function <- function(x, varnames){
  
  ## Change 100g value to value per serve 
  
  # Chicken
  
  chicken_raw_per_serve_kcal <- chicken_raw_100g_kcal * chicken_gram_per_serve/100
  
  chicken_raw_per_serve_protein <- chicken_raw_100g_protein * chicken_gram_per_serve/100
  
  chicken_raw_per_serve_CHO <- chicken_raw_100g_CHO * chicken_gram_per_serve/100
  
  chicken_raw_per_serve_fat <- chicken_raw_100g_fat * chicken_gram_per_serve/100
  
  chicken_raw_per_serve_Ca <- chicken_raw_100g_Ca * chicken_gram_per_serve/100
  
  chicken_raw_per_serve_Fe <- chicken_raw_per_serve_Fe * chicken_gram_per_serve/100
  
  
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
  
  vegetable_mushroom_raw_per_serve_kcal <- 
  
  
  
  # Bean/Nut 
  # (I use same serving size for bean as nut because beans was rarely on the menu- upset stomach risk?)
  
  
  
  # Egg
  
  
  
  
  # Dairy
  
  
  
  
  # Fruit
  
  
  
  
  # Rice/Noodle
  
  
  
  
  # Sandwich
  
  
  
  
  
  # Oil/fat
  
  
  
  
  
  
  
  
  
  
  
  
  
}






































## Main meals on normal training day

# protein food type


# carb food type


# fat


# Total calorie per main meal on normal training day


##############################

## Main meals on intensive training day

# protein food type


# carb food type


# fat


# Total calorie per main meal on intensive training day


################################################################################

## Light snacks on normal and intensive training days

# Before training 


# After training


# Before bed

################################################################################

## Total calorie, protein, carb and fat provided


################################################################################

## Current calorie, protein, carb and fat intake based on food types



################################################################################

## Extra calorie provided


################################################################################

## Micro-nutrients


# Calculate micro-nutrients intake from nutrients provided


# Current micro-nutrient intake


################################################################################















