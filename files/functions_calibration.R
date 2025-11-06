library(foreach)
library(doParallel)
library(tidyverse)

# Example measure of fit between two reports
measure_fit <- function(historic_report, sim_report) {
  # Join by step (inner join: only compare matching steps)
  df <- dplyr::inner_join(historic_report, sim_report,
                          by = "step",
                          suffix = c("_hist", "_sim"))
  
  # Identify numeric columns to compare (excluding "step")
  compare_cols <- setdiff(names(df), "step")
  compare_cols_hist <- grep("_hist$", compare_cols, value = TRUE)
  compare_cols_sim  <- grep("_sim$",  compare_cols, value = TRUE)
  
  # We'll assume the columns match in order, i.e. 
  # for every X_hist there is a corresponding X_sim
  # For safety, you could match them by sub-string if needed.
  
  # For each pair, compute squared differences
  diff <- c()
  for (col_hist in compare_cols_hist) {
    col_sim <- sub("_hist$", "_sim", col_hist)  # get the matching sim column
    if (col_sim %in% names(df)) {
      # numeric difference
      v_hist <- df[[col_hist]]
      v_sim  <- df[[col_sim]]
      # you can use a different method to calculate the difference
      # but this function has the following property:
      # if the min.value is 0, regardless of the max.value the diff is in the interval [0,4], this makes it very comparable
      diff <- c(diff, ((v_hist - v_sim)/((v_hist +v_sim)/2))^2) 
      diff[is.nan(diff)] <- 0
    }
  }
  
  # Return mean difference (or sum, or RMSE, etc.)
  mean(diff, na.rm = TRUE)
}


# a more complex function that allows for different weights with respect to certain cols or steps
measure_fit_weighted <- function(historic_report, 
                                 sim_report, 
                                 weight_steps = NULL,    # e.g. c(1,2,3,4,5)
                                 weight_cols  = NULL    # e.g. c("share_global"=2, "s_1"=1, "s_2"=1, ...)
) {
  # Join by step (inner join: only compare matching steps)
  df <- dplyr::inner_join(
    historic_report, sim_report,
    by = "step", suffix = c("_hist", "_sim")
  )
  
  # Identify numeric columns to compare (excluding "step")
  compare_cols <- setdiff(names(df), "step")
  compare_cols_hist <- grep("_hist$", compare_cols, value = TRUE)
  compare_cols_sim  <- grep("_sim$",  compare_cols, value = TRUE)
  
  # We will store our weighted differences in a vector:
  all_diff <- numeric(0)  # empty numeric vector
  
  for (col_hist in compare_cols_hist) {
    # Find the matching simulated column
    col_sim <- sub("_hist$", "_sim", col_hist)
    if (!(col_sim %in% names(df))) next  # skip if missing
    
    # Extract the underlying variable name (without _hist / _sim)
    var_name <- sub("_hist$", "", col_hist)
    
    # Grab vectors from the data frame
    v_hist <- df[[col_hist]]
    v_sim  <- df[[col_sim]]
    
    # We'll compute the unweighted difference
    raw_diff <- ((v_hist - v_sim)/((v_hist + v_sim)/2))^2
    
    # Replace NaN with 0
    raw_diff[is.nan(raw_diff)] <- 0
    
    # ------------------------------------------------------------------
    # Column-based weighting
    # ------------------------------------------------------------------
    # If `weight_cols` is provided, it should be something like:
    #   weight_cols = c("share_global"=2, "s_1"=1, "s_2"=3, ...)
    # If var_name isn't in weight_cols, we'll assume weight = 1
    if (!is.null(weight_cols) && var_name %in% names(weight_cols)) {
      w_col <- weight_cols[[var_name]]
    } else {
      w_col <- 1
    }
    
    # ------------------------------------------------------------------
    # Step-based weighting
    # ------------------------------------------------------------------
    # If `weight_steps` is provided, e.g. weight_steps = c(1,2,3,4,5)
    # We assume the step values go from 1..N, so we can do:
    #   w_step[i] = weight_steps[df$step[i]]
    # If no match, default to 1.
    
    # Create a vector of step weights, same length as raw_diff
    if (!is.null(weight_steps)) {
      # Some caution: step might not start at 1. 
      # If your steps are strictly 1..max, we can do:
      w_steps <- weight_steps[df$step]
      # If step is not guaranteed to be an integer index, you can do 
      # a named lookup, e.g. weight_steps = c("1"=1, "2"=2, ...)
      # w_steps <- weight_steps[as.character(df$step)]
      # fallback for missing names
      w_steps[is.na(w_steps)] <- 1
    } else {
      w_steps <- rep(1, length(raw_diff))
    }
    
    # ------------------------------------------------------------------
    # Combine the weights: multiply raw_diff by (w_col * w_step)
    # ------------------------------------------------------------------
    weighted_diff <- raw_diff * w_col * w_steps
    
    # Accumulate in our all_diff vector
    all_diff <- c(all_diff, weighted_diff)
  }
  
  # Finally, compute your desired summary (mean, sum, RMSE, etc.)
  mean(all_diff, na.rm = TRUE)
}

calibrate_model <- function(
    historic_report,
    model,
    # if model variable is FALSE, use default provided by `model`, otherwise use them to generate a grid
    world_size               = NA,
    world_agents_per_cell    = NA,
    agent_distance_function  = NA,
    agent_distance_parameter = NA,    
    agent_adoption_p  = NA,
    # number of simulations runs for each combination
    n_sims = 3,
    # number of steps in each run
    max_steps = 25
) {
  # 1) Build a data frame of all combinations
  param_grid <- expand.grid( 
    world_size               = if_else(is.na(world_size)              , model$world_size              , world_size),
    world_agents_per_cell    = if_else(is.na(world_agents_per_cell)   , model$world_agents_per_cell   , world_agents_per_cell),
    agent_distance_function  = if_else(is.na(agent_distance_function) , model$agent_distance_function , agent_distance_function),
    agent_distance_parameter = if_else(is.na(agent_distance_parameter), model$agent_distance_parameter, agent_distance_parameter),
    agent_adoption_p         = if_else(is.na(agent_adoption_p)        , model$agent_adoption_p        , agent_adoption_p),
    stringsAsFactors = FALSE
  )
  
  # We'll store results in a data frame
  results <- tibble()
  
  # 2) Loop over all combinations in the grid
  for (i in seq_len(nrow(param_grid))) {
    tmp_model <- modifyList(model, as.list(param_grid[i, ]))
    
    print(param_grid[i, ])

    fit_values <- numeric(n_sims)  # holds error score for each replicate
    
    # 3) Repeat the simulation multiple times to average out randomness
    for (rep_i in seq_len(n_sims)) {
      # Increase seed to vary each run
      tmp_model$seed <- model$seed - 1 + rep_i
      
      # Run the simulation
      sim_worlds <- run_simulation(tmp_model, max_steps)
      
      # Generate the simulated report
      sim_report <- report_generate(sim_worlds)
      
      # Compute error/fit relative to historic data
      fit_values[rep_i] <- measure_fit(historic_report, sim_report)
    }
    
    # Average fit score across replicates
    avg_fit <- mean(fit_values)
    
    # 4) Store the results
    results <- results %>%
      bind_rows(tibble(
        world_size               = tmp_model$world_size,
        world_agents_per_cell    = tmp_model$world_agents_per_cell,
        agent_distance_function  = tmp_model$agent_distance_function,
        agent_distance_parameter = tmp_model$agent_distance_parameter,
        agent_adoption_p         = tmp_model$agent_adoption_p,
        start_seed               = model$seed,
        simulations              = n_sims,
        fit = avg_fit
      ))
  }
  
  return(results)
}

calibrate_model_mc <- function(
    historic_report, 
    model,
    # if model variable is FALSE, use default provided by `model`, otherwise use them to generate a grid
    world_size               = NA,
    world_agents_per_cell    = NA,
    agent_distance_function  = NA,
    agent_distance_parameter = NA,    
    agent_adoption_p         = NA,
    # number of simulations runs for each combination
    n_sims = 3,
    # number of steps in each run
    max_steps = 25,
    # maximum cores to use 
    max_cores = 24
) {
  # 1) Build a data frame of all combinations
  param_grid <- expand.grid( 
    world_size               = if_else(is.na(world_size)              , model$world_size              , world_size),
    world_agents_per_cell    = if_else(is.na(world_agents_per_cell)   , model$world_agents_per_cell   , world_agents_per_cell),
    agent_distance_function  = if_else(is.na(agent_distance_function) , model$agent_distance_function , agent_distance_function),
    agent_distance_parameter = if_else(is.na(agent_distance_parameter), model$agent_distance_parameter, agent_distance_parameter),
    agent_adoption_p         = if_else(is.na(agent_adoption_p)        , model$agent_adoption_p        , agent_adoption_p),
    stringsAsFactors = FALSE
  )
  
  # 2) Set up parallel back-end
  n_cores <- min(parallel::detectCores(), max_cores, nrow(param_grid))
  cat("Using", n_cores, "cores\n")
  
  # Initialize a cluster and register it
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  # 3) Use foreach to iterate in parallel over each row of param_grid
  #    .combine = rbind => combine results row-by-row
  #    .packages = "dplyr" => loads dplyr on each worker
  #    .export => exports functions from global environment
  #               that the workers need (run_simulation, report_generate, measure_fit)
  
  results <- foreach(
     i = seq_len(nrow(param_grid)),
    .combine  = rbind,
    .packages = c("dplyr"),
    .export   = c("run_simulation", "report_generate", "measure_fit")
  ) %dopar% {
    source("files/functions_simulation.R")
    
    # Copy the base model and override parameters from row i
    tmp_model <- modifyList(model, as.list(param_grid[i, ]))
    
    # Run multiple simulations for this combination
    fit_values <- numeric(n_sims)
    for (rep_i in seq_len(n_sims)) {
      # Vary the seed slightly for each replicate
      tmp_model$seed <- model$seed - 1 + rep_i
      
      sim_worlds  <- run_simulation(tmp_model, max_steps)
      sim_report  <- report_generate(sim_worlds)
      fit_values[rep_i] <- measure_fit(historic_report, sim_report)
    }
    
    # Average fit score across replicates
    avg_fit <- mean(fit_values)
    
    # Return a one-row data.frame for foreach to combine
    data.frame(
      world_size               = tmp_model$world_size,
      world_agents_per_cell    = tmp_model$world_agents_per_cell,
      agent_distance_function  = tmp_model$agent_distance_function,
      agent_distance_parameter = tmp_model$agent_distance_parameter,
      agent_adoption_p         = tmp_model$agent_adoption_p,
      start_seed               = model$seed,
      simulations              = n_sims,
      fit                      = avg_fit,
      stringsAsFactors         = FALSE
    )
  }
  
  # 4) Stop the cluster
  parallel::stopCluster(cl)
  
  # Convert results to a tibble and return
  results <- as_tibble(results)
  return(results)
}

calibration_heatmap_pdf <- function(
    data,
    x_var       = "agent_distance_parameter",   # Name of the column to use for x-axis
    y_var       = "agent_adoption_p",   # Name of the column to use for y-axis
    fill_var    = "fit",                       # Name of the column to use for the fill (e.g. fit/error)
    palette     = "RdYlBu",                    # Brewer palette name (see ?RColorBrewer)
    direction   = -1,                          # 1 or -1, flips the palette direction
    plot_title  = "Calibration Heatmap"
) {
  pdf("heatmap.pdf", width = 8, height = 6)  # open PDF device
  p <- calibration_heatmap(data,
                           x_var,       
                           y_var,       
                           fill_var,    
                           palette,     
                           direction,   
                           plot_title)
  print(p)
  dev.off()
}

calibration_heatmap <- function(
    data,
    x_var       = "agent_distance_parameter",   # Name of the column to use for x-axis
    y_var       = "agent_adoption_p",   # Name of the column to use for y-axis
    fill_var    = "fit",                       # Name of the column to use for the fill (e.g. fit/error)
    palette     = "RdYlBu",                    # Brewer palette name (see ?RColorBrewer)
    direction   = -1,                          # 1 or -1, flips the palette direction
    plot_title  = "Calibration Heatmap"
) {
  ggplot(data = data, aes_string(x = x_var, y = y_var, fill = fill_var)) +
    geom_tile() +
    scale_fill_distiller(palette = palette, direction = direction) +
    labs(
      title = plot_title,
      x     = x_var,
      y     = y_var,
      fill  = fill_var
    ) +
    theme_minimal()
}
