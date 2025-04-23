library(tidyverse)
library(causaldata)
library(ggdag)
library(readxl)


castle$treat_year <- ifelse(castle$post == 1, castle$year, 0)

lower <- 1
upper <- 11
i <- 1
while(i < 51){
  treat_year_1 <- min(castle$treat_year[lower:upper][castle$treat_year[lower:upper] != 0])
  castle$treat_year[lower:upper] <- rep(treat_year_1, 11)
  lower <- upper + 1
  upper <- lower + 10
  i <- i + 1
}

castle <- castle %>%
  mutate(years_after_treat = year - treat_year)

castle$years_after_treat <- ifelse(castle$years_after_treat == -Inf, NA, castle$years_after_treat)


castle <- castle %>%
  group_by(sid) %>%
  mutate_at(c("assault", "burglary", "larceny", "motor", "l_larceny", "l_motor", 
              "l_police", "l_income", "l_exp_subsidy", "l_exp_pubwelfare"), lag) %>%
  mutate(murder_lag = lag(murder)) %>% 
  ungroup()


castle$sid <- ifelse(as.numeric(castle$sid) > 8, castle$sid - 1, castle$sid)

state_id_ranks <- read_excel("state_id_ranks.xlsx", col_names = FALSE)
colnames(state_id_ranks) <- c("state", "pop", "sid")
state_id_ranks <- state_id_ranks %>%
  select(state, sid)
castle_dat <- full_join(castle, state_id_ranks, by = "sid")

castle_dat <- castle_dat[castle_dat$year != 2000,]


castle_dat <- castle_dat |> 
  select(!(starts_with("r20") |
             starts_with("trend") |
             starts_with("lead") |
             starts_with("lag")))
