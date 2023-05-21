# functions for who disability visualisation


# coordinates for points to create bar 
get_bar_data <- function(
    n_point, # number of points
    bar_n_row = 10, # number of rows of points in bar
    bar_n_col = 50, # only used if bar_n_row is null (default)
    bar_height = 10, # height of bar (length along y axis)
    bar_length = 100, # length of bar (length along x axis)
    jitter_seed = 1, # seed for adding jitter to points
    jitter_sd = 0.05 # standard deviation of jitter (for normal distribution)
) {
  
  if (is.null(bar_n_row)) {
    bar_n_row <- ceiling(n_point/bar_n_col)
  } else {
    bar_n_col <- ceiling(n_point/bar_n_row)
  }
  
    # make y coordinates
    y_step <- bar_height/bar_n_row # amount of space between points
    y <- rep(
      seq(
        bar_height - (y_step/2), 
        y_step/2, 
        length.out = bar_n_row
      ), 
      each = bar_n_col)
    y <- y[1:n_point] # remove extra points
    
    # make x coordinates
    x_step <- bar_length/bar_n_col
    x <- rep(seq(x_step/2, bar_length - (x_step/2), length.out = bar_n_col), bar_n_row)
    x <- x[1:n_point] # remove extra points
    
    # add jitter to points
    set.seed(jitter_seed)
    bar_data <- tibble(
      x = x + rnorm(n_point) * jitter_sd,
      y = y + rnorm(n_point) * jitter_sd
    )

  # below code keeps vertical removes excess points from last column instead of last row
  # can add back as optional argument
    
  # # make x coordinates
  # x_step <- bar_length/bar_n_col # amount of space between points
  # x <- rep(
  #   seq(
  #     x_step/2, 
  #     bar_length - (x_step/2), 
  #     length.out = bar_n_col
  #     ), 
  #   each = bar_n_row)
  # x <- x[1:n_point] # remove extra points
  # 
  # # make y coordinates
  # y <- rep(seq(bar_height, 1, length.out = bar_n_row), bar_n_col)
  # y <- y[1:n_point] # remove extra points
  # 
  # # add jitter to points
  # set.seed(jitter_seed)
  # bar_data <- tibble(
  #   x = x + rnorm(n_point) * jitter_sd,
  #   y = y + rnorm(n_point) * jitter_sd
  # )
  # }
  
  return(bar_data)
}

# add group labels to bar points
bar_data_with_groups <- function(
    bar_data, # output from get_bar_data
    group_counts # number of points in each group
) {
  # number of groups
  n_group <- length(group_counts)
  
  # number of points
  n_point <- nrow(bar_data)
  
  # make group vector
  group_vec <- c()
  for (i in 1:n_group) {
    group_vec <- c(group_vec, rep(i, group_counts[i]))
  }
  
  # add to tibble
  bar_data <- bar_data |>
    mutate(group = group_vec)
  
  return(bar_data)
}

# create bar out of points for multiple groups
# length will be 100
# not used in this analysis
get_groups_bar_data <- function(
    groups_n_point, # vector of number of points in each group
    bar_n_row = 10, # number of rows of points in bar
    bar_height = 10, # height of bar (length along y axis)
    jitter_seed = 1, # seed for adding jitter to points
    jitter_sd = 0.05 # standard deviation of jitter (for normal distribution)
) {
  
  # number of groups
  n_groups = length(groups_n_point)
  
  # percentage of points in each group
  groups_perc <- groups_n_point/sum(groups_n_point) * 100
  
  # start tibble for storing coordinates
  groups_bar_data <- tibble()
  
  # create coordinates for each group
  for (i in 1:n_groups){
    
    # get coordinates for group
    data_i <- get_bar_data(
      groups_n_point[i], 
      bar_length = groups_perc[i],
      bar_n_row = bar_n_row,
      bar_height = bar_height,
      jitter_seed = jitter_seed,
      jitter_sd = jitter_sd,
      ) |>
      # record group that the points belong to (for colour)
      mutate(group = rep(i, groups_n_point[i])) 
    
    # if not the first group, shift the x coordinates
    if (i > 1) {
      data_i <- data_i |>
        mutate(x = x + sum(groups_perc[1:(i-1)]))
    }
    # add to all group data
    groups_bar_data = rbind(groups_bar_data, data_i)
  }
  
  return(groups_bar_data)
}

# textbox with vis font and default settings (e.g., colour)
who_textbox <- function(
    x,
    y,
    label,
    width = 2,
    vjust = 0,
    hjust = 0,
    valign = 0,
    halign = 0,
    lineheight = 1,
    colour = '#262626'
  ) {
  
  # put data into tibble
  data = tibble(
    x = x, 
    y = y,
    label = label,
  )
  
  # plot
  geom_textbox(
    data = data, 
    mapping = aes(x = x, y = y, label = label),
    family = 'Atkinson Hyperlegible', 
    colour = colour,
    size = 3.25,
    inherit.aes = FALSE,
    box.colour = NA, fill = NA,     
    width = unit(width, 'inch'),
    box.padding = unit(rep(0, 4), 'pt'),
    hjust = hjust, vjust = vjust,
    halign = halign, valign = valign,
    lineheight = lineheight)
}

# get bar label coordinates
get_label_coord <- function(
    group_data,
    label_height
) {
  
  # space between x coordinates
  x_step <- group_data$x[2] - group_data$x[1]
  
  # get last x coordinates of each group
  x <- group_data |>
    group_by(group) |>
    select(x) |>
    summarise(max_x = max(x))
  x <- x$max_x
  
  n_groups <- length(x)
  
  # repeat for plotting
  x <- rep(x, each = 2)
  
  # y coordinates
  y <- rep(
    c(0, 
      max(group_data$y) + label_height), 
    n_groups)
  
  # group labels
  group <- rep(1:n_groups, each = 2)
  
  label_coord <- tibble(
    x = x,
    y = y,
    group = group
  )
}

# coordinates for packed circles (equal sizes)
get_circ_data <- function(
    n_circ, # number of circles
    size_seed = 1, # seed for varying size
    size_std = 0.1, # standard deviations of sizes
    jitter_seed = 1, # seed for jittering x/y locations
    jitter_std = 0.2, # standard deviation of jitter
    r = 10 # radius of circle; scales x and y coordinates to be between -1*r and r
) {
  
  # get circle locations
  set.seed(size_seed)
  circ_data <- tibble(
    values = abs(rep(1, n_circ) + rnorm(n_circ) * size_std) # size of each circle (slight variation so packing is less uniform)
  )
  layout_data <- circleProgressiveLayout(circ_data$values, sizetype = 'area')
  circ_data <- cbind(circ_data, layout_data)
  
  # add some jitter to circle locations
  set.seed(jitter_seed)
  circ_data <- circ_data |>
    mutate(x = x + rnorm(n_circ)*jitter_std,
           y = y + rnorm(n_circ)*jitter_std
    )
  
  # scale 
  circ_data <- circ_data |>
    mutate(x = (x/max(abs(x))) * r,
           y = (y/max(abs(y))) * r)
  
}
