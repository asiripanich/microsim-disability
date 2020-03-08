
<!-- README.md is generated from README.Rmd. Please edit that file -->

# microsim-disability

<!-- badges: start -->

<!-- badges: end -->

# Import data and model

``` r
library(dymiumCore)
#> ── * dymium's options * ────────────────────────────────────────────────────────
#> ● dymium.input_dir:
#>   /var/folders/0d/9srpj_750lxbkfs2_8nwkcpw0000gn/T//RtmpTMrdTm/scenario/inputs
#> ● dymium.output_dir:
#>   /var/folders/0d/9srpj_750lxbkfs2_8nwkcpw0000gn/T//RtmpTMrdTm/scenario/outputs
#> ● dymium.scenario_dir:
#>   /var/folders/0d/9srpj_750lxbkfs2_8nwkcpw0000gn/T//RtmpTMrdTm/scenario
#> ● dymium.simulation_scale: 1
library(data.table)

n_agents <- 1000L

# generate a population
population <- 
  data.table(
    pid = 1:n_agents,
    age = sample(0:100, n_agents, replace = T),
    sex = sample(c("female", "male"), n_agents, replace = T),
    state = sample(c("healthy", "mild", "severe"), n_agents, replace = T)
  ) 

# create World
w <- World$new()
a <- Agent$new(.data = population, id_col = "pid")

# add Agent to World
w$add(a, name = "Agent")
#> [11:45:24] WARN  dymiumCore w$add: The given `name` will be ignored since the object in x is of a Dymium class object. The classname of the object will be used as its name.

# convert the transition matrix to a format that dymiumCore can understand
# see https://core.dymium.org/articles/dymium-intro.html#transition
trans_model <-
  fread("data/tprob.csv") %>%
  .[, `:=`(probs = .(c(healthy, mild, severe, death)),
           choices = .(c("healthy", "mild", "severe", "death"))), 
    by = 1:nrow(.)] %>%
  setnames(old = "from", new = "state") %>%
  .[, sex := ifelse(female == 1, "female", "male")] %>%
  .[, c("healthy", "mild", "severe", "death", "female") := NULL]
```

# Create microsimulation events

``` r
event_disability <- function(w, model) {
  
  Agt <- w$get("Agent")
  
  eligible_agent_ids <- Agt$get_data()[age > 50, get(Agt$get_id_col())]
  
  TransitionDisability <- TransitionClassification$new(x = Agt, 
                                                       model = model, 
                                                       targeted_agents = eligible_agent_ids)
  
  TransitionDisability$update_agents(attr = "state")
  
  dead_agent_ids <- Agt$get_data()[state == "death", get(Agt$get_id_col())]
  
  if (length(dead_agent_ids) != 0) {
    message("Removing ", length(dead_agent_ids), " agents in 'death' state.")
    Agt$remove(ids = dead_agent_ids)
  }
  
  Agt$log(desc = "disability:state", value = xtabs(~ state, data = Agt$get_data()))
  
  return(w)
}


event_age <- function(w) {
  Agt <- w$get("Agent")
  
  # update age of agents, with max of 114.
  Agt$get_data(copy = FALSE)[, age := ifelse(age + 1L > 114L, 114L, age + 1L)]
  
  return(w)
}
```

# Microsimulation pipeline

``` r
for (i in 1:10) {
  w$start_iter(time_step = i, unit = "year") %>%
    event_age(.) %>%
    event_disability(., trans_model)
}
#> Removing 90 agents in 'death' state.
#> Removing 57 agents in 'death' state.
#> Removing 52 agents in 'death' state.
#> Removing 46 agents in 'death' state.
#> Removing 30 agents in 'death' state.
#> Removing 36 agents in 'death' state.
#> Removing 33 agents in 'death' state.
#> Removing 21 agents in 'death' state.
#> Removing 27 agents in 'death' state.
#> Removing 13 agents in 'death' state.
```

# Visualisation

``` r
library(purrr) # for working with lists
#> 
#> Attaching package: 'purrr'
#> The following object is masked from 'package:data.table':
#> 
#>     transpose
library(ggplot2)

simlog <- get_log(w)

statelog <- 
  purrr::map2_dfr(simlog$time, simlog$value, ~ {
    .y %>% 
      as.data.table() %>%
      .[, time := .x]
  })

ggplot(data = statelog, aes(x = time, y = N, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(title = "Disability status")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
