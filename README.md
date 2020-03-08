
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

population <- 
  data.table(
    pid = 1:n_agents,
    age = sample(0:100, n_agents, replace = T),
    sex = sample(c("female", "male"), n_agents, replace = T),
    state = sample(c("healthy", "mild", "severe"), n_agents, replace = T)
  ) 

w <- World$new()
a <- Agent$new(.data = population, id_col = "pid")

w$add(a, name = "Agent")
#> [11:42:12] WARN  dymiumCore w$add: The given `name` will be ignored since the object in x is of a Dymium class object. The classname of the object will be used as its name.

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
  
  print(TransitionDisability)
  
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
#> There are 473 Agent agents with 4 unique responses of type character {death: 68 | healthy: 112 | mild: 136 | severe: 157}
#> Removing 68 agents in 'death' state.
#> There are 422 Agent agents with 4 unique responses of type character {death: 60 | healthy: 106 | mild: 91 | severe: 165}
#> Removing 60 agents in 'death' state.
#> There are 365 Agent agents with 4 unique responses of type character {death: 25 | healthy: 87 | mild: 99 | severe: 154}
#> Removing 25 agents in 'death' state.
#> There are 354 Agent agents with 4 unique responses of type character {death: 37 | healthy: 89 | mild: 88 | severe: 140}
#> Removing 37 agents in 'death' state.
#> There are 326 Agent agents with 4 unique responses of type character {death: 46 | healthy: 86 | mild: 65 | severe: 129}
#> Removing 46 agents in 'death' state.
#> There are 288 Agent agents with 4 unique responses of type character {death: 26 | healthy: 77 | mild: 66 | severe: 119}
#> Removing 26 agents in 'death' state.
#> There are 271 Agent agents with 4 unique responses of type character {death: 30 | healthy: 81 | mild: 55 | severe: 105}
#> Removing 30 agents in 'death' state.
#> There are 253 Agent agents with 4 unique responses of type character {death: 28 | healthy: 76 | mild: 64 | severe: 85}
#> Removing 28 agents in 'death' state.
#> There are 232 Agent agents with 4 unique responses of type character {death: 14 | healthy: 79 | mild: 51 | severe: 88}
#> Removing 14 agents in 'death' state.
#> There are 228 Agent agents with 4 unique responses of type character {death: 22 | healthy: 83 | mild: 42 | severe: 81}
#> Removing 22 agents in 'death' state.
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
