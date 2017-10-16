# Exercise 1a
# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL
sum_column <- function(d, var) {
  # Set a default value to return
  result <- NULL
  x <- d[[var]] # Remember, as we showed in class: there are two ways
  # to access a column, and this is one; you should try
  # and figure out why the other way won't work here,
  # but that's not part of the homework
  if (!is.null(x)) { # This tests to see whether the column exists in
    # d; again, you should try and find a way to test
    # this out for yourself to verify that it's true,
    # but that's not part of the homework
    # YOUR CODE HERE: if x contains numbers, set the variable
    # result to be the sum of the values in x
    
    if (!is.numeric(x)) { # For testing the type of vector/column x to see if it contains numbers
      result <- NULL
    }
    else{
      result <- sum(x) # sum allows to sum all the values of the column x
    }
  }
  # YOUR CODE HERE: return the result

  return(result)
}


# Exercise 1b
# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
my_sum <- function(x) { # x is the only argument who is a vector
  result <- 0 # initialize the result
  if (!is.numeric(x)) { # if vector x doesn't contain numbers
    result <- NULL # there's no sum for non-numeric values
  } # end if
  else { # x is a vector type numeric
    for (i in x) { # for every item in vector x. PS: don't forget the ()
      result <- result+i # do result+item
    } # end for
  } # end else
  return(result)
}


# Exercise 1c
# two arguments: a vector x and a number k
# It returns the sum of the elements of x divided by the number k
# If either x or k are non-numeric, it should return NULL
# use function my_sum
sum_divided_by <- function(x,k) { # x is a vector, k is a scalar
  if (is.numeric(k) & (!(k==0))) { # check if k is a number and k is not 0. PS: we write "&" rather than "and" here
    sum_x <- my_sum(x) # use my_sum to get sum
    if (!is.numeric(sum_x)) { # if my_sum returns NULL:
      result <- NULL # than we have also NULL
    } # end if
    else { # sum_x is normal
      result <- sum_x/k # divide sum_x by k     
    } # end else
  } # end if
  else { # other cases like "k" is not number, or "k==0" etc...
    # print("k not numeric")
    result <- NULL # than we have NULL
  }
  return(result) # return result
}


# Exercise 1d
# calculate the mean of a vector using sum_divided_by
# but this time k is replaced by the length of vector x
my_mean <- function(x) { # we only have to know the vector
  k <- length(x) # get k the length of x. PS: this is different from python who uses len(x)
  # we can do it ourselves by:
  # k <- 0
  # for (i in x) {
  #   k <- k+1
  # }
  if (!is.numeric(x)) { # check if x is numeric, if not:
    result <- NULL # than have NULL
  }
  else { # if it is:
    result <- sum_divided_by(x,k) # use sum_divided_by
  }
  return(result) # return result
}

# Exercise 2a
# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.
#
grouped_violin_plot <- function(d, var, grouping_var) { # a function that create a violin plot
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var, # for the source d, take "Sepal.Length" like y values 
                                              x=grouping_var, # and grouped by "Species"
                                              fill=grouping_var)) # fill violins by "Species"
  
  # make the object under the form of a violin plot
  p <- p + ggplot2::geom_violin()
  p + ggplot2::labs(# title="Iris data", # option: title of the plot
         # subtitle=" ",                  # subtitle named ?
         # caption="Source: d",           # where comes from the data
         x=grouping_var,                  # aes x named as grouping_var == "Species"
         y=var)                           # aes y named as var == "Sepal.Length"
  return(p) # show p
}


# Exercise 2b
# see in Rmd

# Exercise 3a
# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1) # get the partial data of group1
  d_2 <- dplyr::filter(d, get(grouping_var) == group2) # get the partial data of group2
  # YOUR CODE HERE: assign the difference in the medians to to the variable 'result'
  result <- median(d_1[[var]]) - median(d_2[[var]]) # median of the column1 minus median of the column2.
  # Attention, here use d[[var]] rather than d$var for the reason that the last one will get you an interger
  
  return(result)
}


# Exercise 3b
# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
# provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]], nrow(d)) # generate a shuffled version of d[[var]]
  
    return(d)
}



# Exercise 3c
# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
# provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
#
# statistic: a function yielding a test statistic, which takes as input
# a data frame, the name of a variable on which to calculate the
# test statistic, the name of a grouping variable, the value of
# the grouping variable corresponding to the first group, and
# the value of the grouping variable corresponding to the second
# group
#
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
# permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2) # the real statistic data that we observed
  # In this case of our assignment, it's the real median difference observed
  permutation_statistics <- rep(0, n_samples) # replicate 0 as many as n_samples
  # In fact, this step could be seen as "initialization of variable with indicating its type: interger"
  for (i in 1:n_samples) { 
    # Here's a specific usage of "i" in R, in Python list[0] is actually [1] in R
    # YOUR CODE HERE: use randomize(...) to create a permutation and then
    # fill in the vector permutation_statistics with the
    # value of statistic(...) for this new permutation
    
    d_fake <- randomize(d,var) # randomize the column var to generate fake data
    #d_fake <- randomize(d,grouping_var)
    #d_fake <- d
    #print(d_fake)
    
    permutation_statistics[i] <- statistic(d_fake, var, grouping_var, group1, group2) # get the i th fake statistic data of permuted data
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result) # result contains "observed" and "permuted" which will be useful after
}

# Exercise_3f
# I made a simple one to see the difference of their max sepal width
new_test_statistic <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1) # get the partial data of group1
  d_2 <- dplyr::filter(d, get(grouping_var) == group2) # get the partial data of group2
  # YOUR CODE HERE: assign the difference in the medians to to the variable 'result'
  result <- max(d_1[[var]])-max(d_2[[var]]) # median of the column1 minus median of the column2.
  # Attention, here use d[[var]] rather than d$var for the reason that the last one will get you an interger
  
  return(result)
}

# Exercise_3g
permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}
permutation_pvalue_left <- function(p) {
  n_below <- sum(p$permuted <= p$observed)
  n_samples <- length(p$permuted)
  return((n_below + 1)/(n_samples + 1))
}

