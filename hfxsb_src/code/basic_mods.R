# libraries
library(tidyverse)
library(cowplot)
library(ggrepel)
library(ggpmisc)
library(patchwork)

# data
dat <- read.csv("data/group_dat_2016.csv")

# standardizing so scales are comparable
dat$Ideology <- dat$Ideology/100

# turn long

dat_long <- dat %>% pivot_longer(cols = c(Percent.Vote.Rep, Ideology))

# change a label

dat_long <- dat_long %>% mutate(name = case_when(
  str_detect(name, "Percent") ~ "Proportion Voting Republican",
  str_detect(name, "Ideo") ~ "Ideology"
))

# plot

regress_plot <- ggplot(dat_long, aes(x = value, y = Beta.Hat)) + 
  geom_vline(xintercept = .50, color = "gray50", linetype = 2) +
  geom_hline(yintercept = 0, color = "gray50", linetype = 2) +
  stat_poly_line() +
  stat_correlation(label.x = "right", small.r = TRUE) +
  geom_point() + 
  geom_text_repel(size = 3, aes(label = Group)) + 
  labs(y = "Partisanship-Animosity Beta Coefficients",
       x = "Group Characteristic") +
  coord_cartesian(xlim = c(0, 1)) +
  facet_wrap(~name) +
  theme_cowplot()

ggsave("figures/regress_plot.pdf", regress_plot, width = 10, height = 5)
 
# residual plot

id_reg <- lm(Beta.Hat ~ Ideology, data = dat)

dat$id_resid <- abs(residuals(id_reg))

vote_reg <- lm(Beta.Hat ~ Percent.Vote.Rep, data = dat)

dat$vote_resid <- abs(residuals(vote_reg))

dat$dif_resid <- dat$id_resid - dat$vote_resid

dat_residlong <- dat %>% pivot_longer(cols = c(id_resid, vote_resid))

## combo model for fun

combo_reg <- lm(Beta.Hat ~ Percent.Vote.Rep + Ideology, data = dat)
summary(combo_reg)

# range of betas

missed_predict_cutoff <- sd(abs(dat$Beta.Hat))

## creating orders
dat_residlong <- dat_residlong %>%
  mutate(
    name = case_when(
      str_detect(name, "id_resid") ~ "Ideology",
      str_detect(name, "vote_resid") ~ "Vote"),
    name = fct_relevel(name, "Ideology", "Vote")) %>%
  arrange(name, value) %>%
  mutate(order = row_number())

resid_plot <- ggplot(dat_residlong, aes(order, value)) +
  geom_hline(yintercept = missed_predict_cutoff, linetype = 2, color = "gray50") +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ name, scales = "free_y") +
  scale_x_continuous(
    breaks = dat_residlong$order,
    labels = dat_residlong$Group,
    expand = c(0,0)
  ) +
  coord_flip() +
  labs(y = "Absolute Value of Residual", x = NULL) +
  theme_cowplot() +
  theme(axis.text.y = element_text(size = 8))

ggsave("figures/resid_plot.pdf", resid_plot, width = 7, height = 4)

#dif plot

dat_dif <- dat %>% 
  mutate(Group = fct_reorder(Group, dif_resid),
         better = ifelse(dif_resid > 0, "Vote", "Ideology"))

dif_plot <- ggplot(dat_dif, aes(x = Group, y = dif_resid, fill = better)) +
  geom_bar(stat = "Identity") +
  scale_fill_brewer(type = "qual", palette = 2) +
  coord_flip() +
  labs(y = "Residual Difference", x = NULL, fill = "Which makes\nbetter predictions?") +
  theme_cowplot() +
  theme(axis.text.y = element_text(size = 8),
        legend.position = c(.475, .15),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))

ggsave("figures/dif_plot.pdf", dif_plot, width = 3.5, height = 4)

resid_plot + dif_plot + plot_layout(widths = c(2.5, 1)) + plot_annotation(tag_levels = 'A')

ggsave("figures/combined_resid_plot.pdf", width = 10, height = 4)

# association between vote and ideology

ggplot(dat, aes(x = Ideology, y = Percent.Vote.Rep)) + 
  geom_vline(xintercept = .50, color = "gray50", linetype = 2) +
  geom_hline(yintercept = .50, color = "gray50", linetype = 2) +
  stat_poly_line() +
  stat_correlation(label.x = "left", small.r = TRUE) +
  geom_point() + 
  geom_text_repel(size = 3, aes(label = Group)) + 
  labs(y = "Proportion Voting Republican",
       x = "Ideology") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_cowplot()

ggsave("figures/predictor_scatter.pdf", width = 5.5, height = 5)

