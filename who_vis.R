# ----
rm(list = ls())
library(tidyverse)
library(readxl)
library(packcircles)
library(ggtext)
library(systemfonts)
library(png)
library(geomtextpath)

# load data ----

# path to excel file
who_xlsx <- file.path('WHO_data', 'data', 'DisabilityPrevalence.xlsx')

col_types <- c('text', 'text', 'text', 'text', 
               'text', 'text', 'text', 'text', 
               'text', 'numeric', 'numeric', 'numeric')

# metric variables 
metric_perc <- 3
metric_count <- 1

# breakdown by sex (from age data)
who_sex <- read_xlsx(who_xlsx, sheet = 2, col_types = col_types) |>
  filter(level == 1, sex_id < 3, age_group_id == 22, severity == 'moderate and severe') |>
  select(!cause_name)

# breakdown by age
who_age <- read_xlsx(who_xlsx, sheet = 2, col_types = col_types) |>
  filter(level == 1, sex_id == 3, severity == 'moderate and severe') |>
  select(!cause_name)

# breakdown by region (not used)
who_region <- read_xlsx(who_xlsx, sheet = 4, col_types = col_types) |>
  filter(level == 1, sex_id == 3, severity == 'moderate and severe')

# breakdown by country income
col_types_income <- c('text', 'text', 'text', 
               'text', 'text', 'text', 'text', 
               'text', 'numeric', 'numeric', 'numeric')
who_income <- read_xlsx(who_xlsx, sheet = 3, col_types = col_types_income) |>
  filter(level == 1, sex_id == 3, severity == 'moderate and severe', metric_id == metric_count)


# breakdown by cause (level 2) (not used)
who_cause <- read_xlsx(who_xlsx, sheet = 2, col_types = col_types) |>
  mutate(level = ifelse(cause_name == 'injuries', 2, level)) |> # injury is a level 2 and 3 cause
  filter(
    level == 2, 
    metric_id == metric_count, 
    severity == 'moderate and severe',
    sex_id == 3,
    age_group_id == 22
    ) |>
  arrange(desc(prev))


# get number and % of people with disability ----

source('who_lib.R')

who_all <- who_age |>
  filter(age_group == 'All age')

show(who_all)

who_total_count <- who_all$prev[who_all$metric_id == metric_count]
who_total_perc <- who_all$prev[who_all$metric_id == metric_perc]

# coordinates for global number of people with disability ---

# global population
global_count <- who_total_count/who_total_perc

# how many people should be represented by each point
n_people_per_point <- 1*10^6

# number of points to represent global population and people with disability
n_point <- round(global_count/n_people_per_point)
n_point_pwd <- round(who_total_count/n_people_per_point) # pwd = person with disability

# get circle locations for global data 
r_global <- 50 # circle radius
jitter_std <- 0.1
global_circ <- get_circ_data(
  n_point,
  size_seed = 1,
  size_std = 0.1,
  jitter_seed = 1,
  jitter_std = jitter_std,
  r = r_global
) |>
  mutate(x = x + r_global)

# add group variable for color
pwd_idx <- sample(1:n_point, n_point_pwd)
pwd <- rep(0, n_point)
pwd[pwd_idx] <- 1
global_circ <- global_circ |>
  mutate(group = pwd)
rm(list = c(pwd_idx, pwd))

# coordinates for PWD (rectangle) ---
point_jitter <- 0.05
pwd_height <- 10
pwd_n_row <- 12
pwd_data <- get_bar_data(
  n_point_pwd, 
  bar_n_row = pwd_n_row,
  bar_height = pwd_height,
  jitter_sd = point_jitter
) |>
  # add groups
  bar_data_with_groups(n_point_pwd)

# coordinates for PWD (circle) ---
# determine radius using area of global circle
A_global <- pi * (r_global ^ 2)
A_pwd <- A_global * who_total_perc
r_pwd <- sqrt(A_pwd / pi)
pwd_circ <- get_circ_data(
  n_point_pwd,
  size_seed = 2,
  size_std = 0.1,
  jitter_seed = 5,
  jitter_std = jitter_std,
  r = r_pwd
) |>
  mutate(group = rep(1, n_point_pwd)) # for colour (same for all points)

# sex data ---
label_length <- pwd_height * 0.9

# 1 = males, 2 = females
who_sex_vis <- who_sex |>
  filter(metric_id == metric_count) # only keep counts
show(who_sex_vis)

sex_groups_n_point <- round(who_sex_vis$prev/n_people_per_point)

sex_data <- get_groups_bar_data(
  (sex_groups_n_point),
  bar_n_row = pwd_n_row, 
  bar_height = pwd_height,
  jitter_sd = 0,
  jitter_seed = 3
) |>
  # shift group numbers
  mutate(group = group + 1)

# label coordinates
label_coord_sex <- get_label_coord(sex_data, label_length)

# labels
perc_by_sex <- round(who_sex_vis$prev/sum(who_sex_vis$prev)*100)
label_sex <- c(
  paste0('<b>',perc_by_sex[1], '%</b><br> male'),
  paste0('<b>',perc_by_sex[2], '%</b><br> female')
)

# add jitter
set.seed(0)
sex_data <- sex_data |>
  mutate(
    x = x + rnorm(length(x)) * point_jitter,
    y = y + rnorm(length(y)) * point_jitter
  )

# age data ---

who_age_vis <- who_age |>
  mutate(age_group_id = as.numeric(age_group_id)) |> 
  filter(metric_id == metric_count, is.na(age_group_id)) |>
  filter(age_group != '15 plus') |>
  mutate(pwd_perc = prev/who_total_count * 100)
show(who_age_vis)

age_groups_n_point <- round(who_age_vis$prev/n_people_per_point)

age_data <- get_groups_bar_data(
  age_groups_n_point,
  bar_n_row = pwd_n_row, 
  bar_height = pwd_height,
  jitter_sd = 0,
) |>
  # shift group numbers using counts of other groups
  mutate(group = group + 
           1 + 
           length(sex_groups_n_point))

# label coordinates
label_coord_age <- get_label_coord(age_data, label_length)

# add jitter
set.seed(6)
age_data <- age_data |>
  mutate(
    x = x + rnorm(length(x)) * point_jitter,
    y = y + rnorm(length(y)) * point_jitter
  )

# labels
perc_by_age <- round(who_age_vis$prev/sum(who_age_vis$prev)*100)
label_age <- c(
  paste0('<b>',perc_by_age[1], '%</b><br> 0 to 14 y.o.'),
  paste0('<b>',perc_by_age[2], '%</b><br> 15 to 59 y.o.'),
  paste0('<b>',perc_by_age[3], '%</b><br> 60+ y.o.')
)


# income data ---

# manually re-order from low to high income
who_income_vis <- who_income[c(2, 3, 4, 1), ]
show(who_income_vis)

income_groups_n_point <- round(who_income_vis$mean/n_people_per_point)

income_data <- get_groups_bar_data(
  income_groups_n_point,
  bar_n_row = pwd_n_row, 
  bar_height = pwd_height,
  jitter_sd = 0,
) |>
  # shift group numbers using counts of other groups
  mutate(group = group + 
           1 + 
           length(sex_groups_n_point) +
           length(age_groups_n_point))

# label coordinates
label_coord_income <- get_label_coord(income_data, label_length)

# add jitter
set.seed(10)
income_data <- income_data |>
  mutate(
    x = x + rnorm(length(x)) * point_jitter,
    y = y + rnorm(length(y)) * point_jitter
  )

# labels
who_income_vis$IncomeGroupCountry <- sub(" ", "-", who_income_vis$IncomeGroupCountry)
perc_by_income <- round(who_income_vis$mean/sum(who_income_vis$mean)*100)
label_income <- c()
for (i in 1:length(perc_by_income)) {
  label_income <- c(label_income,
                 paste0('<b>',perc_by_income[i], '%</b><br>',
                        tolower(who_income_vis$IncomeGroupCountry[i]))
  )
}

# VFSG logo ---

vfsg_png <- readPNG(file.path('WHO_data','vfsg_logo2.png'))

vfsg_alpha <- vfsg_png[,,4]
vfsg_alpha[vfsg_alpha > 0] <- vfsg_alpha[vfsg_alpha > 0] * 0.6
vfsg_png[,,4] <- vfsg_alpha

vfsg_dim <- dim(vfsg_png)
vfsg_size_ratio <- vfsg_dim[1]/vfsg_dim[2]
# colours ---

clr_pwd <- c('#852491') #c('#8C2699') #c('#9429A3')
clr_not_pwd <- c('#C4C4C4')
clr_global <- c(clr_not_pwd, clr_pwd)
clr_sex <- c('#D15F2B','#138A7E') #'c('#C26624','#188C99')
clr_age <- c('#7bb3b9','#0072a3','#052879')
#clr_income <- c('#FFAB0F','#ea5460','#843b7c','#002b54')
clr_income <- c('#ffab0f', '#e85a3b','#aa1750','#56004D')
clr_background <- c('white') # c('grey90') # grey for testing (so fig background is visible)

clr_title <- c('#0D0D0D')
clr_text <- c('#262626')
clr_text_light <- c('#4D4D4D')
clr_text_extralight <- c('#595959')

# plot ---
plot_width <- 8
plot_height <- 12
if (length(dev.list() > 0)) {dev.off()}

# plot window, used for development

#dev.set(4)
#dev.new(width = plot_width, height = plot_height, unit = 'in', noRStudioGD = TRUE)

point_size <- 0.1
y_step <- pwd_height * 2.5 # space between bars
x_start <- -30 # plot lower x limit
x_stop <- 110 # plot upper x limit
x_start_text <- x_start # leftmost location of text
y_start <- r_global*1.2 # plot upper y limit
y_pwd <- (r_global * -1.2) # y shift for PWD circle
x_circ_shift <- -1 * r_global/1.75 # how much to shift both circles laterally


y_lim <- c(r_global * -4.2, y_start + 10)
x_lim <- c(x_start, x_stop)
y_buff <- 7
x_buff <- 10
# y coordinates for demographic plots
y_shift_sex <- y_pwd - y_step * 2.25
y_shift_age <- y_pwd - y_step * 3.25
y_shift_income <- y_pwd - y_step * 4.25
y_shift_inequity <- y_pwd - y_step * 4.75
y_shift_source <- y_pwd - y_step * 6

# settings for label text and lines
label_lw <- 1
label_clr <- clr_text_light
label_x_shift <- -1.5 # shift for text relative to label line
label_y_shift <- -1
label_alpha <- 0.5 # line transparency

ggplot() + 
  # circle - global data
  geom_point(
    data = global_circ, 
    mapping = aes(x = x + x_circ_shift, y = y, colour = factor(group)), 
    size = point_size,
  ) +
  # circle - people with disability
  geom_point(
    data = pwd_circ |>
      mutate(
        x = x + (-1 * x_circ_shift) + r_global + 5,
        y = y + y_pwd
      ),
    mapping = aes(x = x, y = y, colour = factor(group)),
    size = point_size
  ) +
  # bar - people with disability, coloured by sex
  geom_point(
    data = sex_data |>
      mutate(
        y = y + y_shift_sex
      ),
    mapping = aes(x = x, y = y, colour = factor(group)),
    size = point_size
  ) +
  geom_line( # label lines
    data = label_coord_sex |>
      mutate(
        y = y + y_shift_sex
      ),
    mapping = aes(x = x, y = y, group = group, colour = factor(group + 1)),
    linewidth = label_lw,
    alpha = label_alpha
  ) +
  who_textbox(
    x = label_coord_sex$x[seq(2, length(label_sex)*2, by = 2)] + label_x_shift,
    y = label_coord_sex$y[seq(2, length(label_sex)*2, by = 2)] + y_shift_sex + label_y_shift,
    label = label_sex,
    vjust = 1, hjust = 1, valign = 1, halign = 1
  ) +

  # bar - people with disability, coloured by age
  geom_point(
    data = age_data |>
      mutate(
        y = y + y_shift_age
      ),
    mapping = aes(x = x, y = y, colour = factor(group)),
    size = point_size
  ) +
  geom_line( # label lines
    data = label_coord_age |>
      mutate(
        y = y + y_shift_age
      ),
    mapping = aes(
      x = x,
      y = y,
      group = group,
      colour = factor(group + length(label_sex) + 1)),
    linewidth = label_lw,
    alpha = label_alpha
  ) +
  who_textbox(
    x = label_coord_age$x[seq(2, length(label_age)*2, by = 2)] + label_x_shift,
    y = label_coord_age$y[seq(2, length(label_age)*2, by = 2)] + y_shift_age + label_y_shift,
    label = label_age,
    vjust = 1, hjust = 1, valign = 1, halign = 1
  ) +

  # bar - people with disability, coloured by country income
  geom_point(
    data = income_data |>
      mutate(
        y = y + y_shift_income
      ),
    mapping = aes(x = x, y = y, colour = factor(group)),
    size = point_size
  ) +
  geom_line( # label lines
    data = label_coord_income |>
      mutate(
        y = y + y_shift_income
      ),
    mapping = aes(
      x = x,
      y = y,
      group = group,
      colour = factor(group + length(label_sex) + length(label_age) + 1)),
    linewidth = label_lw,
    alpha = label_alpha
  ) +
  who_textbox(
    x = label_coord_income$x[seq(2, length(label_income)*2, by = 2)] + label_x_shift,
    y = label_coord_income$y[seq(2, length(label_income)*2, by = 2)] + y_shift_income + label_y_shift,
    label = label_income,
    vjust = 1, hjust = 1, valign = 1, halign = 1
  ) +
  
  # settings for x and y axes
  scale_x_continuous(
    limits = x_lim,
    breaks = c(),
    labels = '',
    expand = c(0, x_buff)
  ) + 
  scale_y_continuous(
    limits = y_lim,
    breaks = c(),
    labels = '',
    expand = c(0, y_buff)
  ) + 
  
  # remove unnecessary plot elements
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = clr_background, colour = 'white')
  ) +
  
  # colours
  scale_colour_manual(
    breaks = 0:max(income_data$group),
    values = c(clr_global, clr_sex, clr_age, clr_income)
    ) +
  
  # fix aspect ratio
  coord_fixed() +
  
  # text
  # title
  who_textbox(
      x = x_start_text,
      y = y_start - 3,
      label = paste0('<b style = "color:', clr_title, '; font-size:29pt;">',
                     'Anyone can have a disability',
                     '</b>'),
      width = plot_width
    ) +

 # global label
  who_textbox(
    x = x_start_text + r_global*1.8,
    y = y_start - 25,
    label = paste0('<span style = "color:', clr_title, '; font-size:16pt;">',
                   '<b style = "color:', clr_pwd, ';">1</b>',
                   ' in <b>6</b> people have a significant disability.', '</span>'),
    lineheight = 2,
    width = 2.2
  ) +

  # point labels
  who_textbox(
    x = rep(x_start_text + r_global*2.1, 2),
    y = c(y_start - 55, y_start - 75) + 2,
    label = c(paste0('<span style = "color:', clr_text, '; font-size:12pt;">',
                   'Each dot represents <b>1 million</b> people</span>'),
              paste0('<span style = "color:', clr_text, '; font-size:12pt;">',
                     '<b style = "color:', clr_pwd,';">',
                     'Purple dots</b> represent people with disabilities</span>')),
    width = plot_width/5.2,
    lineheight = 1.3
  ) +

  # pwd circle label
  who_textbox(
    x = x_start_text + 22,
    y = y_pwd + 5,
    label = paste0('<span style = "color:', clr_title, '; font-size:16pt;">',
                   'Worldwide, there are',
                   '<b style = "color:', clr_pwd, '; font-size:30pt;"> <br>',
                   round(who_total_count/10^9, digits = 1),
                   ' billion</b><br>people living with disabilities.', '</span>'),
    width = plot_width/2.5,
    lineheight = 2,
    vjust = 1
  ) +

  # diversity text
  who_textbox(
    x = x_start_text,
    y = y_pwd - 35,
    label = paste0('<span style = "color:', clr_title, '; font-size:14pt;">',
                   'People with disabilities are diverse:', '</span>'),
    width = 5
  ) +

  # sex
  who_textbox(
    x = x_start_text,
    y = y_shift_sex + pwd_height - 1,
    label = paste0('<span style = "color:', clr_title, '; font-size:11pt;">',
                   'They can be<br>any <b>sex</b>', '</span>'),
    width = 5,
    vjust = 1,
    lineheight = 1.3
  ) +

  # age
  who_textbox(
    x = x_start_text,
    y = y_shift_age + pwd_height - 1,
    label = paste0('<span style = "color:', clr_title, '; font-size:11pt;">',
                   'They can be<br>any <b>age</b>', '</span>'),
    width = 5,
    vjust = 1,
    lineheight = 1.3
  ) +

  # income
  who_textbox(
    x = x_start_text,
    y = y_shift_income + pwd_height + 3,
    label = paste0('<span style = "color:', clr_title, '; font-size:11pt;">',
                   'And they live <br>in <b>countries</b> <br>with different <br><b>income</b> levels', '</span>'),
    width = 5,
    vjust = 1,
    lineheight = 1.3
  ) +

  # inequities
  who_textbox(
    x = x_start_text,
    y = y_shift_inequity,
    label = paste0('<span style = "color:', clr_title, '; font-size:14pt;">',
                   'These factors all impact the health inequities experienced by people with disabilities.</span>'),
    width = plot_width * 0.7,
    vjust = 1,
    lineheight = 1.5
  ) +

  # vis and data info
  who_textbox(
    x = x_start_text,
    y = y_shift_source,
    label = paste(
      '<span style = "color:', clr_text_light, ' font-size:10pt;">',
      'Visualisation by <b>Gabrielle M. Schroeder</b><br>',
      'Data source: World Health Organisation (2021 global disability data)<br>',
      'Viz for Social Good volunteer project</span>'
    ),
    width = plot_width,
    lineheight = 1.3
  ) +
  
  # vfsg_logo
  annotation_raster(
    vfsg_png, 
    ymin = y_shift_source - 1,
    ymax = y_shift_source + 14,
    xmin = 109 - 15/vfsg_size_ratio,
    xmax = 109) +
  
  # label global population circle
  geom_textcurve(data = tibble(
    x = x_start_text + 10, xend = x_start_text + r_global - 5, y = 34, yend = 53
    ),
    aes(x, y, xend = xend, yend = yend), 
    curvature = -0.2, 
    label = 'global population',
    family = 'Atkinson Hyperlegible', 
    colour = clr_text_extralight,
    size = 5
  ) +
  
  # label pwd population circle
  geom_textcurve(data = tibble(
    x =  62+2, xend = 106.5, y = y_pwd + 12.75, yend = y_pwd + 1
  ),
  aes(x, y, xend = xend, yend = yend), 
  curvature = -0.73, 
  ncp = 20,
  label = 'all people with disabilities',
  family = 'Atkinson Hyperlegible', 
  colour = clr_pwd,
  size = 5
  )

# save ---  
ggsave(file.path('vis','vfsg_who_gms.png'), 
       width = 6.5, 
       height = 6.5*(y_lim[2] - y_lim[1] + y_buff*2)/(x_lim[2] - x_lim[1] + x_buff*2), 
       units = "in", dpi = 750)



