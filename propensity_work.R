
prop_mod <- glm(post ~ poverty + robbery + robbery_gun_r + whitem_15_24 + blackm_15_24 + whitem_25_44 + blackm_25_44, data = castle_dat, family = "binomial")

# prop_mod <- glm(post ~ assault + burglary + homicide + larceny + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")

castle_dat <- prop_mod |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

ggplot(castle_dat, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post))) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Quit Smoking") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  ggtitle("Unweighted") +
  theme_minimal()


p1 <- ggplot(castle_dat, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_atu)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p2 <- ggplot(castle_dat, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p3 <- ggplot(castle_dat, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ate)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p4 <- ggplot(castle_dat, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ato)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

(p1 + p2)/(p3 + p4) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


## Trying other adjustment set
prop_mod_test <- glm(post ~ assault + burglary + homicide + larceny + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")

castle_test <- castle_dat

castle_test <- prop_mod_test |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

p5 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_atu)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p6 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p7 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ate)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p8 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ato)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

(p5 + p6)/(p7 + p8) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# Still a problem

# Love plots
weighted_for_love <- tidy_smd(
  castle_dat,
  .vars = c(post, poverty, robbery, robbery_gun_r, whitem_15_24, blackm_15_24, whitem_25_44, blackm_25_44),
  .group = post,
  .wts = c(w_ate)
)

p1 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_dat,
  .vars = c(post, poverty, robbery, robbery_gun_r, whitem_15_24, blackm_15_24, whitem_25_44, blackm_25_44),
  .group = post,
  .wts = c(w_atm)
)

p2 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_dat,
  .vars = c(post, poverty, robbery, robbery_gun_r, whitem_15_24, blackm_15_24, whitem_25_44, blackm_25_44),
  .group = post,
  .wts = c(w_atu)
)

p3 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_dat,
  .vars = c(post, poverty, robbery, robbery_gun_r, whitem_15_24, blackm_15_24, whitem_25_44, blackm_25_44),
  .group = post,
  .wts = c(w_att)
)

p4 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

(p1 + p2)/(p3 + p4) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Love plots, adj set 2
weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, larceny, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_ate)
)

p1 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, larceny, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_ato)
)

p2 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, larceny, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_atu)
)

p3 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, larceny, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_att)
)

p4 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

(p1 + p2)/(p3 + p4) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


#################
#######SPLINES TEST

prop_mod <- glm(post ~ splines::ns(poverty, 3) + robbery + robbery_gun_r + splines::ns(whitem_15_24, 1) + splines::ns(blackm_15_24, 1) + splines::ns(whitem_25_44, 1) + splines::ns(blackm_25_44, 1), data = castle_dat, family = "binomial")

# prop_mod <- glm(post ~ assault + burglary + homicide + larceny + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")

castle_dat <- prop_mod |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

ggplot(castle_dat, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post))) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Quit Smoking") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  ggtitle("Unweighted") +
  theme_minimal()


p1 <- ggplot(castle_dat, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_atu)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p2 <- ggplot(castle_dat, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p3 <- ggplot(castle_dat, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ate)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p4 <- ggplot(castle_dat, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_atm)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

(p1 + p2)/(p3 + p4) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Love plots
weighted_for_love <- tidy_smd(
  castle_dat,
  .vars = c(post, poverty, robbery, robbery_gun_r, whitem_15_24, blackm_15_24, whitem_25_44, blackm_25_44),
  .group = post,
  .wts = c(w_ate)
)

p1 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_dat,
  .vars = c(post, poverty, robbery, robbery_gun_r, whitem_15_24, blackm_15_24, whitem_25_44, blackm_25_44),
  .group = post,
  .wts = c(w_ate)
)

p2 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_dat,
  .vars = c(post, poverty, robbery, robbery_gun_r, whitem_15_24, blackm_15_24, whitem_25_44, blackm_25_44),
  .group = post,
  .wts = c(w_ate)
)

p3 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_dat,
  .vars = c(post, poverty, robbery, robbery_gun_r, whitem_15_24, blackm_15_24, whitem_25_44, blackm_25_44),
  .group = post,
  .wts = c(w_ate)
)

p4 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

(p1 + p2)/(p3 + p4) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


## Trying other adjustment set
prop_mod_test <- glm(post ~ ns::splines(assault, 3) + ns::splines(burglary, 3) + ns::splines(homicide, 3) + larceny + motor + robbery + ns::splines(robbery_gun_r), data = castle_dat, family = "binomial")

castle_test <- castle_dat

castle_test <- prop_mod_test |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

p5 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_atu)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p6 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p7 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ate)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p8 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_atm)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

(p5 + p6)/(p7 + p8) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# Still a problem


# Love plots, adj set 2
weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, larceny, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_ate)
)

p1 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, larceny, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_atm)
)

p2 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, larceny, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_atu)
)

p3 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, larceny, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_att)
)

p4 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

(p1 + p2)/(p3 + p4) + plot_layout(guides = "collect") & theme(legend.position = "bottom")




##########


p1 <- ggplot(castle_dat) +
  geom_density(aes(x = blackm_25_44), fill = "cyan1", alpha = .7) + 
  theme_minimal()

p2 <- ggplot(nhefs_dat) +
  geom_density(aes(x = wt82), fill = "cyan1", alpha = .7) + 
  ggtitle("1982 Weight(kg)") +
  theme_minimal()

p3 <- ggplot(nhefs_dat) +
  geom_density(aes(x = ht), fill = "cyan1", alpha = .7) + 
  ggtitle("Height(cm)") +
  theme_minimal()

p4 <- ggplot(nhefs_dat) +
  geom_density(aes(x = age), fill = "cyan1", alpha = .7) + 
  ggtitle("Age in 1971") +
  theme_minimal()

p5 <- ggplot(nhefs_dat) +
  geom_density(aes(x = smokeyrs), fill = "cyan1", alpha = .7) + 
  ggtitle("Years of Smoking") +
  theme_minimal()

p6 <- ggplot(nhefs_dat) +
  geom_density(aes(x = smokeintensity), fill = "cyan1", alpha = .7) + 
  ggtitle("Cigarettes per Day in 1971") +
  theme_minimal()

(p1 + p2) / (p3 + p4) / (p5 + p6)



############# Maybe the best choice?

## Trying other adjustment set
prop_mod_test <- glm(post ~ assault + burglary + homicide + larceny + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")

castle_test <- castle_dat

castle_test <- prop_mod_test |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, larceny, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_att)
)

ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

p10 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf() +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  ggtitle("Unweighted") +
  theme_minimal()

p11 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  ggtitle("ATM") +
  theme_minimal()

p10 + p11 + plot_layout(guides = "collect") & theme(legend.position = "bottom")


###########
# Splines Take 2

## Trying other adjustment set
prop_mod_test <- glm(post ~ assault + ns::splines(burglary, 6) + homicide + larceny + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")

castle_test <- castle_dat

castle_test <- prop_mod_test |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, larceny, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_att)
)

ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

p10 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf() +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  ggtitle("Unweighted") +
  theme_minimal()

p11 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

p10 + p11 + plot_layout(guides = "collect") & theme(legend.position = "bottom")


for(i in 1:10){
prop_mod_test <- glm(post ~ homicide + splines::ns(burglary, i) + splines::ns(assault, i-1) + larceny + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")

castle_test <- castle_dat

castle_test <- prop_mod_test |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, larceny, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_att)
)

pi1 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

pi2 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

print(pi1)
print(pi2)
}



###########
# Removing larceny
prop_mod_test <- glm(post ~ assault + burglary + homicide + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")

castle_test <- castle_dat

castle_test <- prop_mod_test |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_att)
)

ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

p10 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf() +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  ggtitle("Unweighted") +
  theme_minimal()

p11 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

p10 + p11 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

for(i in 5:6){
  for(j in 5:6){
  prop_mod_test <- glm(post ~ splines::ns(homicide, j) + splines::ns(burglary, i) + assault + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")
  
  castle_test <- castle_dat
  
  castle_test <- prop_mod_test |>
    augment(type.predict = "response", data = castle_dat) %>%
    mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
           w_att = wt_att(.fitted, post, exposure_type = "binary"),
           w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
           w_ato = wt_ato(.fitted, post, exposure_type = "binary"))
  
  weighted_for_love <- tidy_smd(
    castle_test,
    .vars = c(assault, burglary, homicide, motor, robbery, robbery_gun_r),
    .group = post,
    .wts = c(w_att)
  )
  
  pi1 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
    geom_love() +
    theme_minimal()
  
  pi2 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
    geom_ecdf(aes(weights = w_att)) +
    theme(legend.position = "bottom") +
    labs(x = "Burglary", color = "Castle Implemented") +
    ggtitle("ATT") +
    theme_minimal()
  
  print(pi1)
  print(pi2)
  }
}


prop_mod_test <- glm(post ~ splines::ns(homicide, 5) + splines::ns(burglary, 6) + assault + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")

castle_test <- castle_dat

castle_test <- prop_mod_test |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_att)
)

pi1 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

pi2 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

print(pi1)
print(pi2)


p5 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_atu)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p6 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p7 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ate)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p8 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ato)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

(p5 + p6)/(p7 + p8) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


##### Putting larceny back in makes a mess


prop_mod_test <- glm(post ~ splines::ns(homicide, 5) + splines::ns(burglary, 6) + splines::ns(larceny, 5) + assault + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")

castle_test <- castle_dat

castle_test <- prop_mod_test |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, larceny, homicide, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_att)
)

pi1 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

pi2 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

print(pi1)
print(pi2)


p5 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_atu)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p6 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p7 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ate)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p8 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ato)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

(p5 + p6)/(p7 + p8) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


########### My current pick

prop_mod_test <- glm(post ~ splines::ns(homicide, 5) + splines::ns(burglary, 6) + assault + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")

castle_test <- castle_dat

castle_test <- prop_mod_test |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_att)
)

pi1 <- ggplot(data = weighted_for_love, aes(x = abs(smd), y = variable, group = method, color = method)) +
  geom_love() +
  theme_minimal()

pi2 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

print(pi1)
print(pi2)


p5 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_atu)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p6 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p7 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ate)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

p8 <- ggplot(castle_test, aes(x = .fitted, group = post, fill = post)) +
  geom_mirror_histogram(bins = 30, alpha = .6, aes(fill = factor(post), weight = w_ato)) +
  theme(legend.position = "bottom") +
  labs(x = "Propensity Score", fill = "Passed Castle Doctrine") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("cyan1", "indianred")) +
  theme_minimal()

(p5 + p6)/(p7 + p8) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


#############
## Checking eCDF plots for my pick

prop_mod_test <- glm(post ~ splines::ns(homicide, 5) + splines::ns(burglary, 6) + assault + motor + robbery + robbery_gun_r , data = castle_dat, family = "binomial")

castle_test <- castle_dat

castle_test <- prop_mod_test |>
  augment(type.predict = "response", data = castle_dat) %>%
  mutate(w_atu = wt_atu(.fitted, post, exposure_type = "binary"),
         w_att = wt_att(.fitted, post, exposure_type = "binary"),
         w_ate = wt_ate(.fitted, post, exposure_type = "binary"),
         w_ato = wt_ato(.fitted, post, exposure_type = "binary"))

weighted_for_love <- tidy_smd(
  castle_test,
  .vars = c(assault, burglary, homicide, motor, robbery, robbery_gun_r),
  .group = post,
  .wts = c(w_att)
)

p1 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf() +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  theme_minimal()

p2 <- ggplot(castle_test, aes(x = burglary, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Burglary", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

p3 <- ggplot(castle_test, aes(x = assault, color = factor(post))) + 
  geom_ecdf() +
  theme(legend.position = "bottom") +
  labs(x = "Assault", color = "Castle Implemented") +
  theme_minimal()

p4 <- ggplot(castle_test, aes(x = assault, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Assault", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

p5 <- ggplot(castle_test, aes(x = robbery, color = factor(post))) + 
  geom_ecdf() +
  theme(legend.position = "bottom") +
  labs(x = "Robbery", color = "Castle Implemented") +
  theme_minimal()

p6 <- ggplot(castle_test, aes(x = robbery, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Robbery", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

p7 <- ggplot(castle_test, aes(x = homicide, color = factor(post))) + 
  geom_ecdf() +
  theme(legend.position = "bottom") +
  labs(x = "Homicide", color = "Castle Implemented") +
  theme_minimal()

p8 <- ggplot(castle_test, aes(x = homicide, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Homicide", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

p9 <- ggplot(castle_test, aes(x = robbery_gun_r, color = factor(post))) + 
  geom_ecdf() +
  theme(legend.position = "bottom") +
  labs(x = "Robbery with Gun", color = "Castle Implemented") +
  theme_minimal()

p10 <- ggplot(castle_test, aes(x = robbery_gun_r, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Robbery with Gun", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

p11 <- ggplot(castle_test, aes(x = motor, color = factor(post))) + 
  geom_ecdf() +
  theme(legend.position = "bottom") +
  labs(x = "Motor", color = "Castle Implemented") +
  theme_minimal()

p12 <- ggplot(castle_test, aes(x = motor, color = factor(post))) + 
  geom_ecdf(aes(weights = w_att)) +
  theme(legend.position = "bottom") +
  labs(x = "Motor", color = "Castle Implemented") +
  ggtitle("ATT") +
  theme_minimal()

(p1+p2)/(p3+p4)+ plot_layout(guides = "collect") & theme(legend.position = "bottom")
(p5+p6)/(p7+p8)+ plot_layout(guides = "collect") & theme(legend.position = "bottom")
(p9+p10)/(p11+p12)+ plot_layout(guides = "collect") & theme(legend.position = "bottom")
