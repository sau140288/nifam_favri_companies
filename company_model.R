# FAVRI and the companies
# selling point of safe vegetable

# test model with a function to do one random draw of the model
# make sure to clear in Enviroment
source("functions/make_variables.R")
source("functions/estimate_read_csv.R")
make_variables(estimate_read_csv(paste("data/company_input.csv", sep="")))

data <- read.csv("data/company_input.csv")  # replace with your actual CSV file name
var_list <- unique(data$variable)  # extract the variable names you're checking
r_script_lines <- readLines("company_model.R")  # replace with your actual R file
r_script_text <- paste(r_script_lines, collapse = " ")
# Check which variables from CSV are NOT found in the R script text
var_list[!sapply(var_list, function(v) grepl(v, r_script_text))]

# value varier function to add variability to values
source("functions/vv.R")

# chance event function to assess the chances 
# mostly for risks
source("functions/chance_event.R")

# discount values for NPV (time value for money)
source("functions/discount.R")

company_model <- function(x, varnames){
  
  # Costs of the farmstand ####
  
 # establishment costs ####

   one_time_establishment_costs_farmstand <- cost_of_establishment_capital +
                       cost_establishment_cleaning_establishment +
                       cost_establishment_human_resource_staff +
                       cost_establishment_allowance_extended_participants+
                       cost_establishment_promotion +
                       cost_establishment_stakeholder_engagement +
                       cost_establishment_communication +
                       cost_establishment_legal_permission +
                       cost_establishment_overhead_cost +
                       cost_establishment_equipment
                       
                       
 # maintaining costs####
   
    annual_running_costs_farmstand <- cost_of_maintaining_monitoring_and_evaluation+
               cost_of_maintaining_depreciation_of_equipment+
               cost_of_maintaining_capital+
               cost_of_maintaining_cleaning+
               cost_of_maintaining_human_resource_staff+
               cost_of_maintaining_allowance_extended_partipants+
               cost_of_maintaining_promotion
    
  
  # annual costs ####
  total_costs_farmstand <-  vv(annual_running_costs_farmstand, var_CV = CV_value, 
                     relative_trend = inflation_rate,
                     n = number_of_years)
  
  
  #assumning that in the first year we establishment costs and running costs
  total_costs_farmstand[1] <- total_costs_farmstand[1] + one_time_establishment_costs_farmstand
  
  # Benefits ####
  
  # vulnerable urban communities have underlying health effects
  
  # By the beginning of 2020, according to central standards,
  # Hanoi city has 4,112 poor households, accounting for 0.2% and 3,939
  # near-poor households, accounting for 0.19% of the total number of
  # households. The poverty rate according to central standards decreased from
  # 2.97% at the beginning of the period to 0.2%.
  
  # According to the city's standards, Hanoi city has 8,692 poor households,
  # accounting for 0.42% and 41,937 near-poor households, accounting for 2.01%
  # of the total number of households. The poverty rate according to the city
  # standard decreased from 3.64% to 0.42% (down 52,212 households).
  
  # This is the official statistics of Vietnam (no poverty
  # rate of Hanoi) https://www.gso.gov.vn/en/?s=poverty+rate+hanoi&lang=en
  # Newspaper (quite reliable): Toàn thành phố Hà Nội chỉ còn 0,2% tỷ lệ hộ nghèo
  # (laodongthudo.vn) 
  
  # Risks to access ####
  
  ## if_accessible  
  
  # Keep the probabilities in a separate list
  farmstand_access_risk_probs <- list(
    disease_outbreak = disease_outbreak,
    change_of_source_undesirable = change_of_source_undesirable,
    chance_low_affordability = chance_low_affordability,
    chance_poorly_organized_inconvenient = chance_poorly_organized_inconvenient,
    chance_not_meet_preference = chance_not_meet_preference
  )
  
  farmstand_access_risk_reductions <- list(
    disease_outbreak = access_reduction_disease,
    change_of_source_undesirable = access_reduction_change_of_source,
    chance_low_affordability = access_reduction_affordability,
    chance_poorly_organized = access_reduction_organization_inconvenient,
    chance_farmstand_inconvenient = access_reduction_organization_inconvenient,
    chance_not_meet_preference = access_reduction_preference
  )
  
  # Now apply the loop
  if_accessible <- 1  # Start at full accessibility
  
  for (event in names(farmstand_access_risk_probs)) {
    happened <- chance_event(farmstand_access_risk_probs[[event]], value_if = 1, value_if_not = 0)
    if (happened == 1) {
      if_accessible <- if_accessible - farmstand_access_risk_reductions[[event]]
    }
  }
  
  # Bound it to 0
  if_accessible <- max(if_accessible, 0)
  
  # worker's health benefit from nutrition
  # calculated as a cost savings to the company 
  # annual benefits to work productivity
  # due to better nutrition for the workers (less health care costs, more efficient)
  
  # benefit safety_of_food
  
  # CW NOTES We are mixing together:
  # Intrinsic food environment attributes (access, prep, safety, nutrition)
  # Modifiers like quality loss due to monopoly and quality gain from farming
  # Then multiplying all of that by if_accessible again at the end
  # So if_accessible is kind of double counted, both in
  # work_benefits_from_accessibility and again in benefit_worker_health.
  
  # Food environment ####
  
  # Food Environment #### 
  # Problem: Food environment effects on workers are
  # implied (accessibility, nutrition, etc.) but not clearly modeled or linked
  # to actual outcomes like diet quality, health status, productivity, or
  # satisfaction.
  # Effect: The core intended benefit—improved food environment—is not
  # transparently evaluated.
  
  # Clarify and Expand Food Environment Outcomes Use the five food environment
  # domains as dimensions (Turner has 8, we have information for fewer but still comprehensive):
  # Availability (e.g., number of fresh veg stalls)
  # Affordability (prices vs worker income)
  # Accessibility (distance/time)
  # Desirability (preference match)
  # Quality/Safety (hygiene, freshness)
  
  # Simulate diversity and quality risks
  diversity_risk <- chance_event(chance_vegetable_diversity, value_if = 1, value_if_not = 0)
  vegetable_quality_risk <- chance_event(chance_vegetable_quality, value_if = 1, value_if_not = 0)
  
  # Base food environment components
  food_env_access <- base_accessibility_of_food 
  food_env_prep <- base_prep_of_food
  food_env_safety <- base_safety_of_food
  food_env_nutrition <- base_nutrition_of_food
  
  # Adjust for supply monopoly and farming practice effects
  quality_multiplier <- (1 - reduction_in_veg_quality_if_monopoly) * (1 + farming_practice_increase_quality)
  
  # Adjust for quality/diversity risks
  if (diversity_risk == 1 | vegetable_quality_risk == 1) {
    nutrition_multiplier <- quality_multiplier * (1 - reduction_nutrition_low_diverse_and_quality)
  } else {
    nutrition_multiplier <- quality_multiplier
  }
  
  # Final adjusted components
    farmstand_food_env_accessibility = food_env_access * quality_multiplier * if_accessible
    farmstand_food_env_prep = food_env_prep * quality_multiplier
    farmstand_food_env_safety = food_env_safety * quality_multiplier
    farmstand_food_env_nutrition = food_env_nutrition * nutrition_multiplier
  # ####
  # Composite food environment score
    farmstand_food_env_score <- farmstand_food_env_accessibility + farmstand_food_env_prep + 
    farmstand_food_env_safety + farmstand_food_env_nutrition
  
  # Impact on worker health and factory productivity
  benefit_worker_health <- farmstand_food_env_score * health_response_to_food_env
  
# Risk of poor relationship ####
  # Modeling trust/reputation risks based on conflict and rumors
  # Adjusting reputation-dependent effects on productivity
  # Applying nutrition risk due to diversity/quality issues
  
  # Define trust-breaking events
  reputation_risks <- c(
    chance_conflict_farmer_buyer,
    chance_conflict_farmer_company,
    chance_rumor
  )
  
  # Simulate whether any trust-breaking event occurred
  reputation_events <- sapply(reputation_risks, function(prob) chance_event(prob, 1, 0))
  
  # If any happened, reduce factory reputation
  if (any(reputation_events == 1)) {
    factory_reputation <- benefit_factory_reputation - relationship_reduction
  } else {
    factory_reputation <- benefit_factory_reputation
  }
  
  benefit_work_efficiency <- (benefit_worker_health + benefit_worker_time_savings * if_accessible) *
    factory_reputation  
  
   # calculate company revenue ####
  added_company_revenue_annual <- 
    benefit_work_efficiency +
    #shared with company, increased due to better farm reputation
    benefit_farmer_revenue* 1-(prob_increased_farmer_reputation) 
  
  company_revenue_through_intervention <- vv(added_company_revenue_annual, 
                                             var_CV = CV_value, 
                                             relative_trend = inflation_rate,
                                             n = number_of_years)
  
  conflicting_interest <- chance_event(conflicting_business_interests) 
  
  if(conflicting_interest == 1) {
    company_revenue_through_intervention <- company_revenue_through_intervention - 
      revenue_reduction_if_conflicting_interest
  }else{
    company_revenue_through_intervention <- company_revenue_through_intervention 
  }
  
  farmstand_company_result <- (company_revenue_through_intervention - total_costs_farmstand) + 
    benefit_enviroment_on_campus # quality of factory campus improved
  
  # ConnectFarm Intervention ####
  
  # Costs: Simplified recurring human resources and outreach efforts
  connectFarm_costs_annual <- cost_establishment_human_resource_staff * connectFarm_meeting_fraction +
    cost_of_maintaining_promotion * connectFarm_promo_fraction
  
  cost_of_connectFarm <- vv(connectFarm_costs_annual,
                                     var_CV = CV_value,
                                     n = number_of_years)
  
  # Accessibility effect: less structured, less reliable
  connect_if_accessible <- if_accessible * connectFarm_accessibility_multiplier  
  
  # Food environment benefits: scaled-down effects
  connect_quality_multiplier <- (1 - reduction_in_veg_quality_if_monopoly * 1.5) * 
    (1 + farming_practice_increase_quality * 0.5)
  
  # Quality and diversity risk adjustment
  diversity_risk_connect <- chance_event(chance_vegetable_diversity, 1, 0)
  veg_quality_risk_connect <- chance_event(chance_vegetable_quality, 1, 0)
  
  if (diversity_risk_connect == 1 | veg_quality_risk_connect == 1) {
    connect_nutrition_multiplier <- connect_quality_multiplier * 
      (1 - reduction_nutrition_low_diverse_and_quality * 1.5)
  } else {
    connect_nutrition_multiplier <- connect_quality_multiplier
  }
  
  # Components of the food environment connect ####
    connect_food_accessibility = food_env_access * connect_if_accessible * connect_quality_multiplier 
    connect_food_prep = food_env_prep * connect_quality_multiplier
    connect_food_safety = food_env_safety * connect_quality_multiplier 
    connect_food_nutrition = food_env_nutrition * connect_nutrition_multiplier

  connect_food_env_score <- connect_food_accessibility + connect_food_prep + 
    connect_food_safety + connect_food_nutrition
  
  # Worker health and productivity
  connect_worker_health <- connect_food_env_score * health_response_to_food_env
  connect_work_efficiency <- (connect_worker_health +
                                benefit_worker_time_savings * connect_if_accessible) 
  
  # Revenue pathway
  connect_company_revenue_annual <- connect_work_efficiency 
  
  expected_benefit_of_connectFarm <- vv(connect_company_revenue_annual,
                                        var_CV = CV_value,
                                        relative_trend = inflation_rate,
                                        n = number_of_years)
  
  # Final result connect farm
  connectFarm_result <- expected_benefit_of_connectFarm - cost_of_connectFarm
  
  # Do Nothing Scenario ####
  
  # Quality and diversity risks still apply (unmitigated)
  diversity_risk_do_nothing <- chance_event(chance_vegetable_diversity, 1, 0)
  veg_quality_risk_do_nothing <- chance_event(chance_vegetable_quality, 1, 0)
  
  # Food environment is worse overall: low-quality, uncoordinated
  do_nothing_quality_multiplier <- (1 - reduction_in_veg_quality_if_monopoly * 1.5)
  
  if (diversity_risk_do_nothing == 1 | veg_quality_risk_do_nothing == 1) {
    do_nothing_nutrition_multiplier <- do_nothing_quality_multiplier *
      (1 - reduction_nutrition_low_diverse_and_quality * 1.5)
  } else {
    do_nothing_nutrition_multiplier <- do_nothing_quality_multiplier
  }
  
  # Components of the food environment do nothing
  do_nothing_food_accessibility = food_env_access * do_nothing_quality_multiplier * if_accessible
  do_nothing_food_prep = food_env_prep * do_nothing_quality_multiplier 
  do_nothing_food_safety = food_env_safety * do_nothing_quality_multiplier
  do_nothing_food_nutrition = food_env_nutrition * do_nothing_quality_multiplier * do_nothing_nutrition_multiplier
  
  # Base food environment components
   do_nothing_food_env_score <- do_nothing_food_accessibility + do_nothing_food_prep + 
     do_nothing_food_safety + do_nothing_food_nutrition
  
  # Worker health and work efficiency (no reputation gains)
  do_nothing_worker_health <- do_nothing_food_env_score * health_response_to_food_env
  
  do_nothing_work_efficiency <- (do_nothing_worker_health + 
                                   benefit_worker_time_savings * do_nothing_food_accessibility) 
  # benefits slightly from existing health and time-savings
  do_nothing_company_revenue_annual <- do_nothing_work_efficiency + 
    benefit_farmer_revenue * 0  # no revenue share
  
  
  do_nothing_company_result <- vv(do_nothing_company_revenue_annual,
                                  var_CV = CV_value,
                                  relative_trend = inflation_rate,
                                  n = number_of_years)
  
  # NPV ####
  
NPV_farmstand <-
    discount(farmstand_company_result, 
             discount_rate, calculate_NPV = TRUE) 
  
NPV_connectFarm <-
    discount(connectFarm_result, 
             discount_rate, calculate_NPV = TRUE)
 
NPV_do_nothing <- discount(do_nothing_company_result,
                            discount_rate, calculate_NPV = TRUE)
 

  # name outputs (left-hand side of the equal sign) in the return section, 
  # or the variables will be called output_1, _2, etc.
#   
  return(list(decision_farmstand = NPV_farmstand - NPV_do_nothing,
              decision_connectFarm = NPV_connectFarm - NPV_do_nothing,
              cashflow_farmstand = farmstand_company_result,
            cashflow_connectFarm = connectFarm_result, 
  farmstand_food_env_score = farmstand_food_env_score -do_nothing_food_env_score,
  connect_food_env_score = connect_food_env_score - do_nothing_food_env_score 
  ))
}


