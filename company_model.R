# FAVRI and the companies
# selling pointpoint of safe vegetable

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
  
  one_time_establishment_costs <- cost_of_establishment_meetings + 
    cost_of_promotion
  
  annual_running_costs <- cost_of_space_opportunity_cost + 
    cost_of_regular_meetings +
    cost_of_cleaning_event + 
    cost_of_communication
  
  # costs over five years
  total_costs <-  vv(annual_running_costs, var_CV = CV_value, 
                     relative_trend = inflation_rate,
                     n = number_of_years)
  
  #assumning that in the first year we establishment costs and running costs
  total_costs[1]<-total_costs[1]+one_time_establishment_costs
  
  
  # Benefits ####
  
  # Add more information about accessibility
  # Is the organization of the intervention aligned with 
  # what people need? 

  
  # Risks ###
  if(poor_organization >= access_threshold | 
     poor_management >= access_threshold |  
     inconvenient >= access_threshold | 
     family_cannot_wait >= access_threshold |
     unsuitable_day_of_sale >= access_threshold |
     conflict_farmer_buyer>= access_threshold |
     conflict_farmer_company>= access_threshold |
     
     
     # Do people go and get and also eat the veggies?
     
     taste_reduced >= access_threshold | 
     family_does_not_like >= access_threshold |  
     family_cannot_afford >= access_threshold | 
     undesirable_change_of_veg >= access_threshold |
     family_cannot_wait >= access_threshold |
     dont_care >= access_threshold |
     dont_know >= access_threshold | 
     change_of_veg_source_undesirable >= access_threshold | 
     already_have_other_source >= access_threshold | 
     negative_rumors >= access_threshold){
    
    if_accessible <- if_accessible - access_reduction
    
  } else {
    
    if_accessible
    
  }
  
  # here we need to think slightly differently about benefits
  # These are related to the savings in programs ... perhaps, 
  # although that is slightly low in the urban areas
  # because the people of Hanoi are relatively richer than other parts of Vietnam
  # the savings will not be much, the government is busy taking care of very poor people
  # in other areas of Vietnam
  
  healthy_veggies_benefit <- diverse_veg_benefit +  # savings on programs for fresh veggies
    knowledge_exchange_benefit + # savings on community outreach program
  
    reputation_benefit + # added economic benefit of reputation gains
    environmental_benefit # savings on environmental programs
  
  total_benefits <-  vv(healthy_veggies_benefit + # savings on health related expenses
                          percentage_of_sales, # revenue from farmer's sales
                        var_CV = CV_value, 
                        n = number_of_years, 
                        relative_trend = inflation_rate) * if_accessible #conditional on access
  
  
  sales_intervention_result <- total_benefits - total_costs
  
  # Alternative ####
  # what might the companycompany do instead 
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
  NPV_interv <-
    discount(sales_intervention_result, 
             discount_rate, calculate_NPV = TRUE)
  
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

# Cashflow of the mobile sales option
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

