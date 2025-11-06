library(tidyverse)

# Create the world
world_init <- function(world_size = 9,
                       world_agents_per_cell = 3) {
  # Create the grid
  cells <- expand.grid(x = 1:world_size, y = 1:world_size)
  
  # Assign agents to grid cells
  world <- cells %>%
    slice(rep(1:n(), each = world_agents_per_cell)) %>%
    mutate(id   = row_number(),
           cell_id = x + (y-1) * world_size,
           # Initialize the innovation to 1 in the center for just one agent
           innovation = ifelse(x == ceiling(world_size/2) & 
                               y == ceiling(world_size/2) & 
                               row_number() == ceiling((world_size*world_size*world_agents_per_cell)/2),
                               1,
                               0)
    )
  return(world)
}

# This function attaches a `draw_distance()` function to the `model` for later use in our ABM.
# By defining the function once here, we avoid repeatedly running a `switch` at each simulation step, which can improve performance. 
# Currently, two probability distributions are supported: "weibull" and "geometric".
model_implement_distance_function <- function(model){
  model$draw_distance <- switch(
    model$agent_distance_function,
    "weibull" = function(n = 1) {
      round(rweibull(n,
                     shape = model$agent_distance_parameter[1],
                     scale = model$agent_distance_parameter[2]))
    },
    "geometric" = function(n = 1) {
      round(rgeom(n, prob = model$agent_distance_parameter[1]))
    },
    stop("Unsupported agent distance function")
  )
  return(model)
}

# This function creates a lookup table to quickly find all agents in a specific cell.
# It rearranges the world data by cell_id and associates each cell with a list of agent IDs.
# This significantly improves performance by providing direct access to the agents in each cell.
create_position_lookup <- function(world) {
  # Group by cell_id, store a list of all IDs in that cell
  # 'deframe()' makes a named list with 'cell_id' as the name
  world %>%
    group_by(cell_id) %>%
    summarize(agent_ids = list(id), .groups = "drop") %>%
    deframe()
}

# Generates a picture of this square world. Cells without agents are filled black, cells with agents are filled according to the share of agents that have innovation = 1.
# The colors are defined by the variable color. The first color is used when the share is 0, the second color is used when the share is 0.5 and the third color is used if the share is 1. 
# If the share is anything in between, the colors reflect this accordingly
world_print <- function(world, 
                        i = NA,
                        palette = "RdYlBu") {
  # Summarize innovation per cell
  world_summary <- world %>%
    group_by(x, y) %>%
    summarise(share_innovation = mean(innovation), .groups = 'drop')
  
  # Create the plot
  plot <- ggplot(world_summary, aes(x = x, y = y, fill = share_innovation)) +
    # Draw a black background for the entire grid
    geom_tile(data = expand.grid(x = seq(min(world$x), max(world$x)), y = seq(min(world$y), max(world$y))), aes(x = x, y = y), fill = "black") +
    # Overlay the tiles for non-empty cells
    geom_tile() +
    scale_x_continuous(breaks = 1:max(world_summary$x)) +
    scale_y_continuous(breaks = 1:max(world_summary$y)) +
    coord_fixed() +
    theme_minimal() +
    scale_fill_distiller(palette = palette, limits = c(0, 1)) +  # Fill color for tiles
    labs(title = paste0("World Innovation Map: Round ",i),
         x = "X Coordinate",
         y = "Y Coordinate")
  
  print(plot)
}

# This functions returns a random cell id that is exactly d away from the coordinates (x,y) (Manhattan metric)
# If for a certain d there are no valid cells, -1 (meaning no valid cell) is returned
# Note: This function is somewhat optimized for speed, but I bet there is improvement possible! 
get_a_cell_at_manhattan_distance <- function(x,
                                             y,
                                             d, 
                                             world_size
) {
  # Return the same cell right away if d == 0
  if (d == 0) return(x+(y-1)*world_size)
  
  # For these values, we know for sure, there are no valid cells
  if (d > (world_size-1)*2) return(-1)
  
  cells_at_distance <- do.call(
    rbind,
    lapply(0:d, function(k) {
      dist_rem <- d - k
      # Four possible points for each k
      rbind(
        c(x + k, y + dist_rem),
        c(x + k, y - dist_rem),
        c(x - k, y + dist_rem),
        c(x - k, y - dist_rem)
      )
    })
  )
  
  # Remove duplicates (especially when k=0 or dist_rem=0)
  cells_at_distance <- unique(cells_at_distance)
  
  # Filter out-of-bounds points
  in_bounds <- cells_at_distance[,1] >= 1 & cells_at_distance[,1] <= world_size &
    cells_at_distance[,2] >= 1 & cells_at_distance[,2] <= world_size
  cells_at_distance <- cells_at_distance[in_bounds, , drop = FALSE]
  
  # If no valid cells remain return -1
  if (nrow(cells_at_distance) == 0) return(-1) 
  
  # Pick one cell at random
  chosen_cell <- cells_at_distance[sample(nrow(cells_at_distance), 1), ]
  
  return(chosen_cell[1]+(chosen_cell[2]-1)*world_size)
}

# simulates one time step for a given world and model
world_step <- function(world, 
                       model, 
                       position_lookup = create_position_lookup(world) # if no position_lookup is provided, it is calculated  
){
  
  # 1) Select all agents who are innovators
  innovators <- world %>% filter(innovation == 1)
  
  # If everyone is already converted, don't run the simulation step
  if (nrow(innovators) == nrow(world)) return(world)
  
  # 2) Filter out innovators whose random_value is above the threshold. 
  #    That is, only keep those who will successfully convince a target agent.
  #    Filtering here saves performance because we skip the following steps 
  #    for innovators who wouldn't convert anyone anyway.
  innovators <- innovators %>% 
    mutate(random_value = runif(n())) %>%
    filter(random_value > model$agent_adoption_p)
    
  # If no innovators remain after filtering, skip the step
  if (nrow(innovators) == 0) return(world)

  # 3) Draw a random distance for each innovator
  innovators <- innovators %>% 
    mutate(
      distances = model$draw_distance(nrow(innovators))
    ) 
  
  # 4) For each innovator, determine a random target cell using Manhattan distance
  innovators <- innovators %>%
    mutate(
      target_cell = mapply(
        function(x, y, d) get_a_cell_at_manhattan_distance(x, y, d, model$world_size),
        x, y, distances
      )
    )
  
  # 5) For each innovator, pick a random target agent within the target cell
  innovators <- innovators %>%
    mutate(
      target_id = mapply(
        function(tc, my_id) {
          # If invalid cell, return -1
          if (tc == -1) return(-1)
          
          # Possible targets in that cell
          targets_id <- position_lookup[[tc]]
          
          # Exclude myself
          targets_id <- targets_id[targets_id != my_id]
          
          # If no one left in that cell, return -1
          if (length(targets_id) == 0) return(-1)
          
          # Otherwise pick one at random
          sample(targets_id, 1)
        },
        target_cell,  # from innovators
        id            # from innovators
      )
    )
  
  # Collect all valid targets (target_id != -1)
  targets = innovators %>% filter(target_id != -1) %>% pull(target_id)
  
  # 6) Update those target agents to become innovators
  if (length(targets) > 0) world$innovation[world$id %in% targets] <- 1

  return(world)
}

run_simulation <- function(model, 
                           max_steps = 25
){
  set.seed(model$seed)
  worlds <- list()
  
  for (i in 1:max_steps){
    if (i==1) {
      # 1) Initialize the world in the first iteration
      worlds[[i]] <- world_init(model$world_size, model$world_agents_per_cell)
      
      # 2) Create a lookup table for quick agent lookups
      #    Agents do not move, so building this once is sufficient.
      position_lookup <- create_position_lookup(worlds[[i]]) 
      
      # 3) Attach the chosen distance-drawing function to the model
      #    (e.g., "weibull" or "geometric")
      model <- model_implement_distance_function(model)
    }
    else {
      # 4) Update the world one step at a time
      worlds[[i]] <- world_step(worlds[[i-1]], model, position_lookup)
    }
  }
  return(worlds)
}

# generates a report (a data frame) from an entire simulation history
report_generate <- function(world_history){
  report <- tibble()
  
  for (i in seq_along(world_history)) {
    
    world <- world_history[[i]]
    
    share_global <- sum(world$innovation, na.rm = TRUE) / nrow(world)
    
    world_summary <- world %>%
      group_by(x, y) %>%
      summarise(share_innovation = mean(innovation), .groups = 'drop') %>%
      mutate(share_innovation = round(share_innovation, 3) |> as.character()) |> 
      group_by(share_innovation) %>%
      summarise(cells = n(), .groups = 'drop') %>%
      mutate(step = i) %>% 
      pivot_wider(
        names_from = share_innovation, 
        values_from = cells,
        values_fill = 0  # Fill missing values with 0
      ) %>% 
      ungroup() %>% 
      mutate(share_global = share_global)
    
    report <- bind_rows(report, world_summary)
    
  }
  
  report <- report %>%
    mutate(across(everything(), ~ replace_na(., 0))) %>% 
    relocate(step, share_global)
  
  share_cols <- names(report)[!names(report) %in% c("share_global", "step")]  # Exclude original columns
  new_names <- paste0("s_", seq_along(share_cols))              # Generate s_1, s_2, ...
  names(report)[!names(report) %in% c("share_global", "step")] <- new_names
  
  return(report)
}
