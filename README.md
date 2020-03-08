
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
#> [11:33:11] WARN  dymiumCore w$add: The given `name` will be ignored since the object in x is of a Dymium class object. The classname of the object will be used as its name.

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
  w %>%
    event_age(.) %>%
    event_disability(., trans_model)
}
#> There are 505 Agent agents with 4 unique responses of type character {death: 79 | healthy: 115 | mild: 155 | severe: 156}
#> Removing 79 agents in 'death' state.
#> There are 434 Agent agents with 4 unique responses of type character {death: 66 | healthy: 111 | mild: 115 | severe: 142}
#> Removing 66 agents in 'death' state.
#> There are 378 Agent agents with 4 unique responses of type character {death: 44 | healthy: 106 | mild: 100 | severe: 128}
#> Removing 44 agents in 'death' state.
#> There are 344 Agent agents with 4 unique responses of type character {death: 47 | healthy: 97 | mild: 78 | severe: 122}
#> Removing 47 agents in 'death' state.
#> There are 309 Agent agents with 4 unique responses of type character {death: 34 | healthy: 93 | mild: 67 | severe: 115}
#> Removing 34 agents in 'death' state.
#> There are 285 Agent agents with 4 unique responses of type character {death: 44 | healthy: 82 | mild: 67 | severe: 92}
#> Removing 44 agents in 'death' state.
#> There are 250 Agent agents with 4 unique responses of type character {death: 25 | healthy: 80 | mild: 57 | severe: 88}
#> Removing 25 agents in 'death' state.
#> There are 234 Agent agents with 4 unique responses of type character {death: 20 | healthy: 82 | mild: 53 | severe: 79}
#> Removing 20 agents in 'death' state.
#> There are 224 Agent agents with 4 unique responses of type character {death: 20 | healthy: 84 | mild: 52 | severe: 68}
#> Removing 20 agents in 'death' state.
#> There are 213 Agent agents with 4 unique responses of type character {death: 15 | healthy: 79 | mild: 48 | severe: 71}
#> Removing 15 agents in 'death' state.
```

# Visualisation

``` r
get_log(w)
```
