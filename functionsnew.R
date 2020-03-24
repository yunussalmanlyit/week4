#function needed to allow us to input number or set of numbers
# the function shpuld calculate the percentage values of each number
# reduce output to 1 decimal place

add_percent <- function(my_number)
{
  #multiply by 
  percent <- round(my_number * 100, digits = 1)
  result <- paste(percent, "%", sep = "" )
  
  # return the calculation 
  return(result)
}

sample_vector <- c(0.786, 1.786, 0.345)
add_percent(sample_vector)

#multiplier

add_percent <- function(my_number, multiplier, no_digits)
{
  #multiply by 
  percent <- round(my_number * multiplier, digits = no_digits)
  result <- paste(percent, "%", sep = "" )
  
  # return the calculation 
  return(result)
}

# test out function

my_result <- add_percent(sample_vector, 3, 4)
my_result
