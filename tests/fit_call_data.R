## fit_call_data ##
# fit a polynomial function to Franklin's call data

# setup -------------------------------------------------------------------

call_file = 'data/processed/call_production_rate.rds'

#  plot -------------------------------------------------------------------

call_rates = readRDS(call_file)

call_rates$deployment = seq(from=1, to=37)

call_rates = as.data.frame(table(call_rates))

plot(x=call_rates$deployment,y=call_rates$upcall_production_rate)

