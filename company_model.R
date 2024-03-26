# FAVRI and the companies
# selling point of safe vegetable

# test model with a function to do one random draw of the model
# make sure to clear in Enviroment
source("functions/make_variables.R")
source("functions/estimate_read_csv.R")
make_variables(estimate_read_csv(paste("company_input.csv", sep="")))

# value varier function to add variability to values
source("functions/vv.R")

# chance event function to assess the chances 
# mostly for risks
source("functions/chance_event.R")

# discount values for NPV (time value for money)
source("functions/discount.R")

# Model ####
# set.seed(1234) # to keep the same collection of results each time
company_function <- function(x, varnames){
  
  # Cost####
  
 # establishment costs
  
  
   one_time_establishment_costs <- cost_of_establishment_capital+
                       cost_establishment_cleaning_establishment+
                       cost_establishment_human_resource+
                       cost_establishment_time+
                       cost_establishment_promotion+
                       cost_establishment_stakeholder_engagement+
                       cost_establishment_communication+
                       cost_establishment_legal_permission+
                       cost_establishment_overhead_cost+
                       cost_establishment_equipments
                       
                       
 # maintaining costs
   
    annual_running_costs <- cost_of_maintaining_monitoring_and_evaluation+
               cost_of_maintaining_depreciation_of_equipments+
               cost_of_maintaining_capital+
               cost_of_maintaining_cleaning+
               cost_of_maintaining_human_resource+
               cost_of_maintaining_time+
               cost_of_maintaining_promotion
    
  
  # costs over five years
  total_costs <-  vv(annual_running_costs, var_CV = CV_value, 
                     relative_trend = inflation_rate,
                     n = number_of_years)
  
  #assumning that in the first year we establishment costs and running costs
  total_costs[1] <- total_costs[1] + one_time_establishment_costs
  
  
  
  
  # Benefits ####
  
 
 benefits<- benefit_company_revenue+
                  benefit_heath_care_cost+
                  benefit_worker_time+
                  benefit_nutrition+
                  benefit_farmer_reputation+
                  benefit_factory_reputation+
                  benefit_food_prep_cooking_and_food_waste+
                  benefit_enviroment_on_campus+
                  benefit_work_efficiency+
                  benefit_safety_of_food
  
  
  
  
  
  
  # Risks ###
 
  ## if_accessible  

 
  
   ### if(  disease_outbreak >= access_threshold | 
     #   taste_reduced >= access_threshold |  
      #  family_does_not_like >= access_threshold | 
       # change_of_source_undesirable >= access_threshold |
       # affordability>= access_threshold |
       # organization>= access_threshold |
       # inconvenient>= access_threshold |
       # unsuitable_day_of_sale>= access_threshold |
       #taste_and_taboo>= access_threshold |
       # cultural>= access_threshold |
       # reliance_supply>= access_threshold |
       # monopoly>= access_threshold |  )  {
         
       #if_accessible <- if_accessible - access_reduction
           } else {
          if_accessible
           }
  
 

  disease_outbreak <-chance_event(disease_outbreak,value_if=1,
                                  value_if_not=0)
  
  taste_reduced <-chance_event(taste_reduced,value_if=1,
                                  value_if_not=0)
  
  family_does_not_like <-chance_event( family_does_not_like,value_if=1,
                                  value_if_not=0)
  
  change_of_source_undesirable <-chance_event(change_of_source_undesirable,
                                              value_if=1,value_if_not=0)
                                    
  
  affordability <-chance_event(affordability,value_if=1,
                                       value_if_not=0)
  
  organization <-chance_event(organization,value_if=1,
                                       value_if_not=0)
  
  inconvenient <-chance_event(inconvenient,value_if=1,
                                       value_if_not=0)
 
  unsuitable_day_of_sale <-chance_event(unsuitable_day_of_sale,value_if=1,
                              value_if_not=0)
  
  inconvenient <-chance_event(inconvenient,value_if=1,
                              value_if_not=0)
  
  taste_and_taboo <-chance_event( taste_and_taboo,value_if=1,
                              value_if_not=0)
  
  cultural_accessible <-chance_event(cultural,value_if=1,
                              value_if_not=0)
  
 
  monopoly_reliance_supply_accessible<-chance_event(monopoly_reliance_supply,
                                         value_if=1,
                                       value_if_not=0)
 

  if( disease_outbreak == 1|
      taste_reduced == 1|
      family_does_not_like == 1|
      change_of_source_undesirable== 1|
      affordability== 1|
      organization== 1|
      inconvenient== 1|
      unsuitable_day_of_sale== 1|
      taste_and_taboo== 1|
      cultural== 1|
      monopoly_reliance_supply== 1|)
  
  {if_accessible <- if_accessible - access_reduction} else {if_accessible}
 
  
  
  
    ## if_nutrition

 
  monopoly_reliance_supply_nutrition <-chance_event(monopoly_reliance_supply,value_if=1,
                          value_if_not=0)
  
  diversity <-chance_event( diversity,value_if=1,
                          value_if_not=0)
  
  vegetable_quality <-chance_event(vegetable_quality,value_if=1,
                          value_if_not=0)
  
  
  if(monopoly == 1|
     diversity == 1|
    vegetable_quality == 1|)
 
  {if_nutrition<- if_nutrition - nutrition_reduction} else {if_nutrition}



  
  
  
  ## if_relationship

 
  conflict_farmer_buyer <-chance_event(conflict_farmer_buyer,value_if=1,
                          value_if_not=0)
  
  conflict_farmer_company <-chance_event(conflict_farmer_company,
                            value_if=1,
                            value_if_not=0)
  
  rumor <-chance_event(rumor,value_if=1,
                             value_if_not=0)
  
  
  if(conflict_farmer_buyer == 1|
     conflict_farmer_company == 1|
     rumor == 1|)
    
  {if_relationship<- if_relationship - relationship_reduction} else {if_relationship}
  
  
  ## if_safety_of_food

  
  vegetable_quality <-chance event(vegetable_quality,value_if=1,
                                       value_if_not=0)
  
  monopoly_reliance_supply_safety <-chance_event(monopoly_reliance_supply,
                                         value_if=1,
                                         value_if_not=0)
  

  if(vegetable_quality == 1|
     monopoly == 1|
   
    
     {if_safety_of_food<- if_safety_of_food - safety_of_food_reduction
} else {if_safety_of_food}
  
  
## if_benefit_farmer_revenue


competition_other_farmer<-chance event(competition_other_farmer
,value_if=1, value_if_not=0)


if(vegetable_quality == 1|
     {if_benefit_farmer_revenue<- if_benefit_farmer_revenue - if_benefit_farmer_revenue_reduction
   } else {if_benefit_farmer_revenue}
   
### total_risks
total_risks<-if_accessible+
             if_nutrition+
             if_relationship+
             if_safety_of_food+
             if_benefit_farmer_revenue
   
  
  
  
  
  
  
  
  
  ììhealthy_veggies_benefit <- diverse_veg_benefit +  # savings on programs for fresh veggies
    knowledge_exchange_benefit + # savings on community outreach program
  
    reputation_benefit + # added economic benefit of reputation gains
    environmental_benefit # savings on environmental programs
  
  total_benefits <-  vv(healthy_veggies_benefit + # savings on health related expenses
                          percentage_of_sales, # revenue from farmer's sales
                        var_CV = CV_value, 
                        n = number_of_years, 
                        relative_trend = inflation_rate) * if_accessible #conditional on access
  
  # disease outbreak risk (cf. COVID)
  shut_down_for_disease_control <- chance_event(chance_of_disease_outbreak)
  
  # no benefits if disease outbreak
  if(shut_down_for_disease_control == 1) {
    total_benefits <- 0
  } else {
    total_benefits }
  
  # still have establishment costs
  if(shut_down_for_disease_control == 1) {
    total_costs <- total_costs[2:number_of_years]<-0
  } else {
    total_costs <- total_costs }
  
  sales_intervention_result <- total_benefits - total_costs
  
  # Alternative ####
  # what might the company do instead 
  # with the same kinds of outcomes in mind
  # what action would they take to address health, gender etc.?
  # i.e. just connect farmers and consumers and allow them to handle logistics
  
  expected_cost_of_altervative <- vv(cost_of_establishment_meetings * percent_meetings_no_intervention, # ten percent of the meetings, but each year
                                     var_CV = CV_value, n = number_of_years, 
                                     relative_trend = inflation_rate)  
  
  expected_benefit_of_altervative <- vv(diverse_veg_benefit * percent_veg_benefit_no_intervention,
                                        var_CV = CV_value, n = number_of_years, 
                                        relative_trend = inflation_rate)  
  
  no_intervention_result <- expected_benefit_of_altervative - expected_cost_of_altervative
  
  # The difference is inherent in our calculation so we do not need the 
  # comparative NPV here, just discount the intervention result
  # # = (1 + Real Discount Rate)(1 + Inflation Rate) – 1 ≈ Real Discount Rate + Inflation Rate
  # Real Discount Rate ≈ Nominal Discount Rate - Inflation Rate
  # Nominal discount rate is often considered as nominal bank interest rate. If we have already considered inflation in the cost and benefit calculation, then for calculating NPV, we should consider
  # 
  # Nominal interest rate
  # Company's choice of nominal interest rate (or nominal discount rate).
  # We suggest the discount rate from 3-15%  

  
  NPV_interv <-
    discount(sales_intervention_result, 
             discount_rate, calculate_NPV = TRUE) #consider inflation rate???
  
  NPV_no_interv <-
    discount(no_intervention_result, 
             discount_rate, calculate_NPV = TRUE)
  
  # Beware, if you do not name your outputs (left-hand side of the equal sign) in the return section, 
  # the variables will be called output_1, _2, etc.
  
  return(list(NPV_company_sales = NPV_interv,
              NPV_no_interv = NPV_no_interv,
              decision = NPV_interv - NPV_no_interv, #the decision is in the trade off
              Cashflow_company_sales = sales_intervention_result))
}

# Monte Carlo simulation 
source("functions/mcSimulation.R")

company_results <- mcSimulation(
  estimate = estimate_read_csv("company_input.csv"),
  model_function = company_function,
  numberOfModelRuns = 1e3, #run 1,000 times
  functionSyntax = "plainNames"
)

# plot distributions for the two options
source("functions/plot_distributions.R")
plot_distributions(mcSimulation_object = company_results, 
                   vars = c("NPV_company_sales","NPV_no_interv"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

plot_distributions(mcSimulation_object = company_results, 
                   vars = c("NPV_company_sales","NPV_no_interv"),
                   method = 'boxplot')

plot_distributions(mcSimulation_object = company_results, 
                   vars = "decision",
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

# Cashflow of the company option
source("functions/plot_cashflow.R")
plot_cashflow(mcSimulation_object = company_results, 
              cashflow_var_name = "Cashflow_company_sales")

# PLS
source("functions/pls_model.R")
pls_result <- pls_model(object = company_results,
                                resultName = names(company_results$y)[3], 
                                ncomp = 1)

input_table <- read.csv("company_input.csv")
source("functions/plot_pls.R")
plot_pls(pls_result, input_table = input_table, threshold = 0)

# EVPI 
source("functions/multi_EVPI.R")
#here we subset the outputs from the mcSimulation function (y) 
# by selecting the correct variables
# be sure to run the multi_EVPI only on the variables that the we want
mcSimulation_table <- data.frame(company_results$x, 
                                 company_results$y[1:3])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_company_sales")

source("functions/plot_evpi.R")
plot_evpi(evpi, decision_vars = "NPV_company_sales")
plot_evpi(evpi, decision_vars = "decision")

# Get summary statistics
summary(company_results$y$decision)

