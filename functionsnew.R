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

# stats
 
my_stats <- function(values, parametric = TRUE, allow_print = FALSE)
{
  if  (parametric) 
    {
    central_tendency <- mean(values)
    spread <- sd(values)
  }
  else
  {
    central_tendency <- median(values)
    spread  <- mad(values)
  }
  if(allow_print & parametric)
  {
    # CONSTRUCT relevant output
    cat("mean = ", central_tendency, "\n", "SD = ", spread, "\n")
  }
  else if (allow_print & !parametric)
  {
    cat("median =", central_tendency, "\n", "MAD = ", spread, "\n")
  }
}
sample_vector <- c(0.432, 1.234, 0.567)
result <- my_stats(sample_vector, FALSE, TRUE)