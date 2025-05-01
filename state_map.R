library(tidyverse)
library(maps)
library(ggspatial)
library(causaldata)
library(readxl)
library(fiftystater)
library(mapproj)

# Data transforms
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

castle <- castle |>
  mutate(years_after_treat = year - treat_year)

castle$years_after_treat <- ifelse(castle$years_after_treat == -Inf, NA, castle$years_after_treat)


castle <- castle |>
  group_by(sid) |>
  mutate_at(c("assault", "burglary", "homicide", "larceny", "motor", "l_larceny", "l_motor", 
              "l_police", "l_income", "l_exp_subsidy", "l_exp_pubwelfare"), lag) |>
  mutate(murder_lag = lag(murder)) |> 
  ungroup()


castle$sid <- ifelse(as.numeric(castle$sid) > 8, castle$sid - 1, castle$sid)

state_id_list <- read_excel("state_id_list_fixed.xlsx", col_names = FALSE)
colnames(state_id_list) <- c("state", "pop", "sid")
state_id_ranks <- state_id_list |>
  select(state, sid)
castle_dat <- full_join(castle, state_id_list, by = "sid")

castle_dat <- castle_dat[castle_dat$year != 2000,]

#CCA + Dropping Washington
castle_dat <- castle_dat |> 
  select(!(starts_with("r20") |
             starts_with("trend") |
             starts_with("lead") |
             starts_with("lag"))) |>
  drop_na(robbery_gun_r)

########

# Map

castle_plot <- castle_dat %>%
  distinct(sid, .keep_all = TRUE) %>%
  mutate(state = tolower(state))

castle_plot <- castle_plot %>%
  mutate(`Has Castle Doctrine` = ifelse(is.na(years_after_treat), "No", "Yes"))


ggplot(castle_plot, aes(map_id = state)) + 
  geom_map(aes(fill = `Has Castle Doctrine`), map = fifty_states, color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  scale_fill_manual(values = c("#42af76", "#2064ad")) +
  labs(x = "", y = "") +
  theme(panel.background = element_blank()) +
  fifty_states_inset_boxes()


