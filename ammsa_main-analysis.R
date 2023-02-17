################################################################################

# AMMSA Swedish Validation -- Main Analysis

################################################################################

# Setup ------------------------------------------------------------------------

packages <- c("dplyr", "ggplot2", "readr", "lavaan", "semPlot", "EGAnet", "psychonetrics", "qgraph", "cowplot", "IsingSampler")

lapply(packages, library, character.only = TRUE)

source("ammsa_helper-functions.R")

with_demo <- FALSE

# Load cleaned data ------------------------------------------------------------

if (with_demo == TRUE) {
  
  ammsa <- read_csv("./data/ammsa_data_clean.csv")
  
} else if (with_demo == FALSE) {
  
  ammsa <- read_csv("./data/ammsa_data_clean_no-demographics.csv") 
  
}

# Remove attention check failures 

ammsa <- ammsa %>% 
  filter(att_control_8_text == "+" | att_control_8_text == "ett plustecken")

# Extract only AMMSA items for later examination -------------------------------
  
ammsa_scale <- ammsa %>% 
  select(starts_with("ammsa"))

# Load item text

item_text <- read_csv("./data/ammsa-swe_scale-item-text.csv")

# Reverse code relevant variables ----------------------------------------------
  
ammsa <- ammsa %>% 
  mutate(
    asi_03_disaster_r    = 8 - asi_03_disaster_r,
    asi_06_rominvlvd_r   = 8 - asi_06_rominvlvd_r,
    asi_07_feminists_r   = 8 - asi_07_feminists_r,
    asi_13_complete_r    = 8 - asi_13_complete_r,
    asi_18_kickteasing_r = 8 - asi_18_kickteasing_r,
    
    sc_10_sexualoutlet_r = 8 - sc_10_sexualoutlet_r,
    
    sdo_09_groupsequal_r = 8 - sdo_09_groupsequal_r,
    sdo_10_ourideal_r    = 8 - sdo_10_ourideal_r,
    sdo_11_equalchance_r = 8 - sdo_11_equalchance_r,
    sdo_12_eqlizeconds_r = 8 - sdo_12_eqlizeconds_r,
    sdo_13_increased_r   = 8 - sdo_13_increased_r,
    sdo_14_fewerprblms_r = 8 - sdo_14_fewerprblms_r,
    sdo_15_eqlincomes_r  = 8 - sdo_15_eqlincomes_r,
    sdo_16_dominate_r    = 8 - sdo_16_dominate_r
  )

# Create total score variables -------------------------------------------------

ammsa <- ammsa %>%
  mutate(
    ammsa_total = rowMeans(
      select(ammsa, starts_with("ammsa")), 
      na.rm = FALSE),
    asi_total = rowMeans(
      select(ammsa, starts_with("asi")), 
      na.rm = FALSE),
    asi_hostile = rowMeans(
      select(ammsa, asi_02_spclfavours, asi_04_inncntrem, asi_05_offended, asi_07_feminists_r, asi_10_appreciate, asi_11_controlmen, asi_14_exgrtwork, asi_15_tightleash, asi_16_faircomp, asi_18_kickteasing_r, asi_21_reasondemands), 
      na.rm = FALSE),
    asi_benevolent = rowMeans(
      select(ammsa, asi_01_nottrlycmplte, asi_03_disaster_r, asi_06_rominvlvd_r, asi_08_qualpurity, asi_09_chersihed, asi_12_adore, asi_13_complete_r, asi_17_pedestal, asi_19_moralsensib, asi_20_sacrifice, asi_22_refinedsense), 
      na.rm = FALSE),
    asi_paternalism = rowMeans(
      select(ammsa, asi_03_disaster_r, asi_09_chersihed, asi_17_pedestal, asi_20_sacrifice), 
      na.rm = FALSE),
    asi_genderdiff = rowMeans(
      select(ammsa, asi_08_qualpurity, asi_19_moralsensib, asi_22_refinedsense), 
      na.rm = FALSE),
    asi_heterointim = rowMeans(
      select(ammsa, asi_01_nottrlycmplte, asi_06_rominvlvd_r, asi_12_adore, asi_13_complete_r), 
      na.rm = FALSE),
    irma_total = rowMeans(
      select(ammsa, starts_with("irma")), 
      na.rm = FALSE),
    irma_asked_for_it = rowMeans(
      select(ammsa, irma_01_whiledrunk, irma_02_sluttycloth, irma_03_roomalone, irma_04_actslut, irma_05_nounclear, irma_06_girlinitiate), 
      na.rm = FALSE),
    irma_didnt_mean_to = rowMeans(
      select(ammsa, irma_07_strongdesire, irma_08_carriedaway, irma_09_sexdriveooc, irma_10_drunkunint, irma_11_didntrealize, irma_12_bothdrunk), 
      na.rm = FALSE),
    irma_not_rape = rowMeans(
      select(ammsa, irma_13_physicresist, irma_14_physfightbck, irma_15_bruisesmarks, irma_16_noweapon, irma_17_deosntsayno), 
      na.rm = FALSE),
    irma_she_lied = rowMeans(
      select(ammsa, irma_18_agreeregret, irma_19_getbckatguys, irma_20_ledonregrets, irma_21_emotionalprb, irma_22_caughtcheat), 
      na.rm = FALSE),
    sc_total = rowMeans(
      select(ammsa, starts_with("sc")), 
      na.rm = FALSE),
    sdo_total = rowMeans(
      select(ammsa, starts_with("sdo")), 
      na.rm = FALSE)
  )

# Basic visualization ----------------------------------------------------------

hist_ammsa <- 
  ggplot(ammsa,
         aes(
           x = ammsa_total
         )) +
  geom_histogram(
    binwidth = .25,
    color = "black",
    fill = "grey"
  ) +
  scale_x_continuous(
    limits = c(1, 7),
    breaks = 1:7
  ) +
  labs(
    x = "AMMSA Total",
    y = "Frequency",
    title = "AMMSA"
  ) +
  theme_classic()

hist_irma <- 
  ggplot(ammsa,
         aes(
           x = irma_total
         )) +
  geom_histogram(
    binwidth = .25,
    color = "black",
    fill = "grey"
  ) +
  scale_x_continuous(
    limits = c(1, 5),
    breaks = 1:5
  ) +
  labs(
    x = "IRMA Total",
    y = "Frequency",
    title = "IRMA"
  ) +
  theme_classic()

hist_grid <- plot_grid(hist_ammsa, hist_irma, nrow = 2)

# Confirmatory factor analysis -------------------------------------------------

# Unidimensional model

## Model specification

ammsa_cfa_1 <- 
'

# Measurement model

ammsa =~ ammsa_01_menlead + ammsa_02_misgivings + ammsa_03_emancipated + ammsa_04_custfalsacc + ammsa_05_battlesexes + ammsa_06_bionecessit + ammsa_07_amplesuppor + ammsa_08_mediadepict + ammsa_09_cupcoffee + ammsa_10_suggremarks + ammsa_11_darkalleys + ammsa_12_assertright + ammsa_13_praiselooks + ammsa_14_fascination + ammsa_15_playcoy + ammsa_16_exaggerate + ammsa_17_urgepartner + ammsa_18_singleinvit + ammsa_19_politicians + ammsa_20_marital + ammsa_21_steamboil + ammsa_22_retailiate + ammsa_23_onthejob + ammsa_24_hitbreaks + ammsa_25_robbery + ammsa_26_alcohol + ammsa_27_wllmntgest + ammsa_28_shelters + ammsa_29_envdestr + ammsa_30_punish

'

## Model fitting

if (!file.exists("./rda/ammsa_cfa_1_fit.rda")) {
  
  ammsa_cfa_1_fit <- cfa(ammsa_cfa_1,
                         data = ammsa_scale,
                         se = "bootstrap",
                         bootstrap = 5000,
                         meanstructure = TRUE)
  
  save(ammsa_cfa_1_fit, file = "./rda/ammsa_cfa_1_fit.rda")
  
} else {
  
  load("./rda/ammsa_cfa_1_fit.rda")
  
}

## Visualization

ammsa_cfa_1_figure <- 
  semPlot::semPaths(ammsa_cfa_1_fit, 
                    what = "paths",
                    whatLabels = "std",
                    intercepts = FALSE,
                    residuals = FALSE,
                    curvePivot = TRUE,
                    layout = "circle",
                    sizeLat = 4,
                    sizeMan = 3,
                    groups = "latents",
                    nodeLabels = c(1:30, "RMA"))

## Invariance testing

if (!file.exists("./rda/ammsa_cfa_1_invar_1.rda")) {
  
  ammsa_cfa_1_invar_1 <- cfa(ammsa_cfa_1,
                             data = ammsa %>% 
                               select(starts_with("ammsa"), dem_01_gender) %>% 
                               filter(dem_01_gender == 1 | dem_01_gender == 2),
                             se = "bootstrap",
                             bootstrap = 5000,
                             group = "dem_01_gender",
                             meanstructure = TRUE)
  
  ammsa_cfa_1_invar_2 <- cfa(ammsa_cfa_1,
                             data = ammsa %>% 
                               select(starts_with("ammsa"), dem_01_gender) %>% 
                               filter(dem_01_gender == 1 | dem_01_gender == 2),
                             se = "bootstrap",
                             bootstrap = 5000,
                             group = "dem_01_gender",
                             group.equal = c("loadings"),
                             meanstructure = TRUE)
  
  ammsa_cfa_1_invar_3 <- cfa(ammsa_cfa_1,
                             data = ammsa %>% 
                               select(starts_with("ammsa"), dem_01_gender) %>% 
                               filter(dem_01_gender == 1 | dem_01_gender == 2),
                             se = "bootstrap",
                             bootstrap = 5000,
                             group = "dem_01_gender",
                             group.equal = c("loadings", "intercepts"),
                             meanstructure = TRUE)
  
  ammsa_cfa_1_invar_4 <- cfa(ammsa_cfa_1,
                             data = ammsa %>% 
                               select(starts_with("ammsa"), dem_01_gender) %>% 
                               filter(dem_01_gender == 1 | dem_01_gender == 2),
                             se = "bootstrap",
                             bootstrap = 5000,
                             group = "dem_01_gender",
                             group.equal = c("loadings", "intercepts", "residuals"),
                             meanstructure = TRUE)
  
  save(ammsa_cfa_1_invar_1, file = "./rda/ammsa_cfa_1_invar_1.rda")
  save(ammsa_cfa_1_invar_2, file = "./rda/ammsa_cfa_1_invar_2.rda")
  save(ammsa_cfa_1_invar_3, file = "./rda/ammsa_cfa_1_invar_3.rda")
  save(ammsa_cfa_1_invar_4, file = "./rda/ammsa_cfa_1_invar_4.rda")
  
} else {
  
  load("./rda/ammsa_cfa_1_invar_1.rda")
  load("./rda/ammsa_cfa_1_invar_2.rda")
  load("./rda/ammsa_cfa_1_invar_3.rda")
  load("./rda/ammsa_cfa_1_invar_4.rda")
  
}



invar_cfa_1_fitind_1 <- fitmeasures(ammsa_cfa_1_invar_1)
invar_cfa_1_fitind_2 <- fitmeasures(ammsa_cfa_1_invar_2)
invar_cfa_1_fitind_3 <- fitmeasures(ammsa_cfa_1_invar_3)
invar_cfa_1_fitind_4 <- fitmeasures(ammsa_cfa_1_invar_4)

# Seven-factor model

## Model specificaiton

ammsa_cfa_7 <- 
'

# Measurement model

INV =~ ammsa_02_misgivings + ammsa_09_cupcoffee + ammsa_11_darkalleys + ammsa_18_singleinvit

CNH =~ ammsa_06_bionecessit + ammsa_08_mediadepict + ammsa_21_steamboil + ammsa_26_alcohol

EXG =~ ammsa_03_emancipated + ammsa_04_custfalsacc + ammsa_16_exaggerate + ammsa_22_retailiate

GNR =~ ammsa_01_menlead + ammsa_15_playcoy + ammsa_13_praiselooks + ammsa_24_hitbreaks + ammsa_12_assertright

NHP =~ ammsa_05_battlesexes + ammsa_10_suggremarks + ammsa_23_onthejob + ammsa_27_wllmntgest + ammsa_17_urgepartner + ammsa_20_marital

ENG =~ ammsa_07_amplesuppor + ammsa_25_robbery + ammsa_28_shelters + ammsa_30_punish

NP =~ ammsa_14_fascination + ammsa_19_politicians + ammsa_29_envdestr


'

## Model fitting

if (!file.exists("./rda/ammsa_cfa_7_fit.rda")) {
  
  ammsa_cfa_7_fit <- cfa(ammsa_cfa_7,
                         data = ammsa_scale,
                         se = "bootstrap",
                         bootstrap = 5000,
                         meanstructure = TRUE)
  
  save(ammsa_cfa_7_fit, file = "./rda/ammsa_cfa_7_fit.rda")
  
} else {
  
  load("./rda/ammsa_cfa_7_fit.rda")
  
}

## Visualization

node_names <- c(2, 9, 11, 18, 6, 8, 21, 26, 3, 4, 16, 22, 1, 15, 13, 24,
                12, 5, 10, 23, 27, 17, 20, 7, 25, 28, 30, 14, 19, 29,
                "INV", "CNH", "EXG", "GNR", "NHP", "ENG", "NP")

ammsa_cfa_7_figure <- 
  semPlot::semPaths(ammsa_cfa_7_fit, 
                    what = "paths",
                    whatLabels = "std",
                    intercepts = FALSE,
                    residuals = FALSE,
                    curvePivot = TRUE,
                    layout = "circle",
                    sizeLat = 4,
                    sizeMan = 3,
                    groups = "latents",
                    nodeLabels = node_names)

## Invariance testing

if (!file.exists("./rda/ammsa_cfa_7_invar_1.rda")) {

  ammsa_cfa_7_invar_1 <- cfa(ammsa_cfa_7,
                             data = ammsa %>% 
                               select(starts_with("ammsa"), dem_01_gender) %>% 
                               filter(dem_01_gender == 1 | dem_01_gender == 2),
                             se = "bootstrap",
                             bootstrap = 5000,
                             group = "dem_01_gender",
                             meanstructure = TRUE)
  
  ammsa_cfa_7_invar_2 <- cfa(ammsa_cfa_7,
                             data = ammsa %>% 
                               select(starts_with("ammsa"), dem_01_gender) %>% 
                               filter(dem_01_gender == 1 | dem_01_gender == 2),
                             se = "bootstrap",
                             bootstrap = 5000,
                             group = "dem_01_gender",
                             group.equal = c("loadings"),
                             meanstructure = TRUE)
  
  ammsa_cfa_7_invar_3 <- cfa(ammsa_cfa_7,
                             data = ammsa %>% 
                               select(starts_with("ammsa"), dem_01_gender) %>% 
                               filter(dem_01_gender == 1 | dem_01_gender == 2),
                             se = "bootstrap",
                             bootstrap = 5000,
                             group = "dem_01_gender",
                             group.equal = c("loadings", "intercepts"),
                             meanstructure = TRUE)
  
  ammsa_cfa_7_invar_4 <- cfa(ammsa_cfa_7,
                             data = ammsa %>% 
                               select(starts_with("ammsa"), dem_01_gender) %>% 
                               filter(dem_01_gender == 1 | dem_01_gender == 2),
                             se = "bootstrap",
                             bootstrap = 5000,
                             group = "dem_01_gender",
                             group.equal = c("loadings", "intercepts", "residuals"),
                             meanstructure = TRUE)
  
  save(ammsa_cfa_7_invar_1, file = "./rda/ammsa_cfa_7_invar_1.rda")
  save(ammsa_cfa_7_invar_2, file = "./rda/ammsa_cfa_7_invar_2.rda")
  save(ammsa_cfa_7_invar_3, file = "./rda/ammsa_cfa_7_invar_3.rda")
  save(ammsa_cfa_7_invar_4, file = "./rda/ammsa_cfa_7_invar_4.rda")

} else {
  
  load("./rda/ammsa_cfa_7_invar_1.rda")
  load("./rda/ammsa_cfa_7_invar_2.rda")
  load("./rda/ammsa_cfa_7_invar_3.rda")
  load("./rda/ammsa_cfa_7_invar_4.rda")
  
}

invar_cfa_7_fitind_1 <- fitmeasures(ammsa_cfa_7_invar_1)
invar_cfa_7_fitind_2 <- fitmeasures(ammsa_cfa_7_invar_2)
invar_cfa_7_fitind_3 <- fitmeasures(ammsa_cfa_7_invar_3)
invar_cfa_7_fitind_4 <- fitmeasures(ammsa_cfa_7_invar_4)

# Model comparison

cfa_lrt <- lavTestLRT(ammsa_cfa_1_fit, ammsa_cfa_7_fit)

# Correlations -----------------------------------------------------------------

ammsa_cor <- ammsa_scale %>% 
  cor(use = "pairwise.complete")

ammsa_cor_ci <- cor_ci(ammsa_cor, nrow(ammsa_scale))

ammsa_cov <- ammsa_scale %>% 
  cov(use = "pairwise.complete")

if (!file.exists("./rda/ammsa_cor_matrix_n386.rda")) {
  
  save(ammsa_cor, file = "./rda/ammsa_cor_matrix_n386.rda")
  save(ammsa_cov, file = "./rda/ammsa_cov_matrix_n386.rda")
  
}

if (with_demo == TRUE) {
  
  convergent <- 
    ammsa %>% 
    select(
      ammsa_total,
      asi_total,
      asi_hostile,
      asi_benevolent,
      asi_paternalism,
      asi_genderdiff,
      asi_heterointim,
      irma_total,
      irma_asked_for_it,
      irma_didnt_mean_to,
      irma_not_rape,
      irma_she_lied,
      sc_total,
      sdo_total,
      age,
      dem_02_edu,
      dem_04_living,
      dem_05_rightleft_1) %>% 
    cor(use = "pairwise.complete")
  
  covergent_ci <- cor_ci(convergent, nrow(ammsa))
  
} else if (with_demo == FALSE) {
  
  convergent <- 
    ammsa %>% 
    select(
      ammsa_total,
      asi_total,
      asi_hostile,
      asi_benevolent,
      asi_paternalism,
      asi_genderdiff,
      asi_heterointim,
      irma_total,
      irma_asked_for_it,
      irma_didnt_mean_to,
      irma_not_rape,
      irma_she_lied,
      sc_total,
      sdo_total) %>% 
    cor(use = "pairwise.complete")
  
  covergent_ci <- cor_ci(convergent, nrow(ammsa))
  
}

plot_asi  <- scatter_plot("asi_total", "Ambivalent Sexism", c(1, 7))
plot_hs   <- scatter_plot("asi_hostile", "Hostile Sexism", c(1, 7))
plot_bs   <- scatter_plot("asi_benevolent", "Benevolent Sexism", c(1, 7))
plot_irma <- scatter_plot("irma_total", "IRMA Total", c(1, 5))
plot_sc   <- scatter_plot("sc_total", "Sexual Conservatism", c(1, 7))
plot_sdo  <- scatter_plot("sdo_total", "Social Dominance Orientation", c(1, 7))

scatter_grid <- plot_grid(plot_irma, plot_sc, plot_sdo, 
                          plot_asi, plot_hs, plot_bs, 
                          nrow = 2)

# Parallel analysis ------------------------------------------------------------

parallel_ammsa <- psych::fa.parallel(ammsa_scale)

# Exploratory graph analysis ---------------------------------------------------

# set.seed(333)
# 
# ammsa_ega <- 
#   EGAnet::EGA(data = ammsa_scale,
#               model = "glasso",
#               algorithm = "walktrap"
#   )
# 
# ega_cfa <- 
#   EGAnet::CFA(ammsa_ega, 
#               estimator = "ML",
#               se = "bootstrap",
#               bootstrap = 1000,
#               meanstructure = TRUE,
#               data = ammsa_scale,
#               plot.CFA = FALSE)
# 
# ega_cfa_figure <- 
#   semPlot::semPaths(ega_cfa$fit, 
#                     what = "paths",
#                     whatLabels = "std",
#                     intercepts = FALSE,
#                     residuals = FALSE,
#                     curvePivot = TRUE,
#                     layout = "circle",
#                     sizeLat = 4,
#                     sizeMan = 3,
#                     groups = "latents")

# Network analysis -------------------------------------------------------------

if (!file.exists("./rda/ammsa_network_model.rda")) {
  
  network_model <- varcov(data = ammsa_scale,
                          type = "ggm",
                          omega = "Full")
  
  network_model_pruned <- network_model %>%
    setoptimizer(optimizer = "cpp_Nelder-Mead") %>% 
    prune(alpha = 0.05, recursive = TRUE)
  
  network_model_final <- network_model_pruned %>% 
    modelsearch( # This process is computationally intensive
      criterion  = "bic",
      prunealpha = .05,
      addalpha   = .05
    )
  
  save(network_model_final, file = "./rda/ammsa_network_model.rda")
  
} else {
  
  load("./rda/ammsa_network_model.rda") 
  # Unless you absolutely need to rerun the model search procedure,
  # loading the premade skeleton is probably a good idea.
  
}

network_fit <- 
  network_model_final %>% 
  runmodel()

network_pars    <- parameters(network_fit)

network_fit_ind <- fit(network_fit)

network_graph <- 
  qgraph(getmatrix(network_model_final, "omega"),
         labels = 1:30,
         layout = "spring",
         vsize = 4,
         edge.labels = TRUE,
         edge.label.cex = .40,
         edge.label.bg = "white",
         edge.label.position = .50,
         edge.color = "#151414",
         vTrans = 200,
         negDashed = FALSE)

cent_ammsa <- qgraph::centralityPlot(network_graph,
                                    include = c("Strength", "Closeness", "Betweenness"))

ammsa_strength <- cent_ammsa$data %>% 
  filter(measure == "Strength") %>% 
  arrange(desc(value)) %>% 
  mutate(
    node = as.numeric(node)
  ) %>% 
  left_join(filter(item_text, scale == "ammsa"), by = c("node" = "item"))

ammsa_between <- cent_ammsa$data %>% 
  filter(measure == "Betweenness") %>% 
  arrange(desc(value)) %>% 
  mutate(
    node = as.numeric(node)
  ) %>% 
  left_join(filter(item_text, scale == "ammsa"), by = c("node" = "item"))

## Simulated persuasion

set.seed(9898)

### Baseline

# This simulation assumes mild disagreement with all items, to get a distribution
# similar to the empirical distribution

ammsa_sim_base <- IsingSampler(10000, 
                               getmatrix(network_model_final, "omega"), 
                               c(rep(-.10, 30)), 
                               responses = c(0, 1))

ammsa_sim_base <- ammsa_sim_base %>% 
  as.data.frame()

ammsa_sim_base$total <- rowSums(ammsa_sim_base)

ammsa_sim_base_hist <- 
ggplot(ammsa_sim_base,
       aes(
         x = total
       )) +
  geom_histogram(
    binwidth = 1
  ) +
  scale_x_continuous(
    limits = c(-5, 35),
    breaks = seq(0, 30, 5)
  ) +
  theme_classic()

### Persuasion on Item 23

ammsa_sim_pers <- IsingSampler(10000, 
                               getmatrix(network_model_final, "omega"), 
                               c(rep(-.10, 22), -1, rep(-.1, 7)), 
                               responses = c(0, 1))

ammsa_sim_pers <- ammsa_sim_pers %>% 
  as.data.frame()

ammsa_sim_pers$total <- rowSums(ammsa_sim_pers)

ammsa_sim_pers_hist <- 
  ggplot(ammsa_sim_pers,
         aes(
           x = total
         )) +
  geom_histogram(
    binwidth = 1
  ) +
  scale_x_continuous(
    limits = c(-5, 35),
    breaks = seq(0, 30, 10)
  ) +
  theme_classic()

## Latent variable model for comparison

# manifests <- colnames(ammsa_scale)
# 
# lvar <- c("INV", "CNH", "EXG", "GNR", "NHP", "ENG", "NP")
# 
# ### Latent variable model
# 
# latent_lambda <- matrix(
#   c(
#   # INV CNH EXG GNR NHP ENG NP
#     0,  0,  0,  1,  0,  0,  0,
#     1,  0,  0,  0,  0,  0,  0,
#     0,  0,  1,  0,  0,  0,  0,
#     0,  0,  1,  0,  0,  0,  0,
#     0,  0,  0,  0,  1,  0,  0,
#     0,  1,  0,  0,  0,  0,  0,
#     0,  0,  0,  0,  0,  1,  0,
#     0,  1,  0,  0,  0,  0,  0,
#     1,  0,  0,  0,  0,  0,  0,
#     0,  0,  0,  0,  1,  0,  0,
#     1,  0,  0,  0,  0,  0,  0,
#     0,  0,  0,  1,  0,  0,  0,
#     0,  0,  0,  1,  0,  0,  0,
#     0,  0,  0,  0,  0,  0,  1,
#     0,  0,  0,  1,  0,  0,  0,
#     0,  0,  1,  0,  0,  0,  0,
#     0,  0,  0,  0,  1,  0,  0,
#     1,  0,  0,  0,  0,  0,  0,
#     0,  0,  0,  0,  0,  0,  1,
#     0,  0,  0,  0,  1,  0,  0,
#     0,  1,  0,  0,  0,  0,  0,
#     0,  0,  1,  0,  0,  0,  0,
#     0,  0,  0,  0,  1,  0,  0,
#     0,  0,  0,  1,  0,  0,  0,
#     0,  0,  0,  0,  0,  1,  0,
#     0,  1,  0,  0,  0,  0,  0,
#     0,  0,  0,  0,  1,  0,  0,
#     0,  0,  0,  0,  0,  1,  0,
#     0,  0,  0,  0,  0,  0,  1,
#     0,  0,  0,  0,  0,  1,  0
#   ),
#   ncol = length(lvar),
#   byrow = TRUE,
#   dimnames = list(manifests, lvar)
# )
# 
# latent_lvm <- lvm(data = ammsa_scale,
#                   lambda = latent_lambda,
#                   identification = "variance")
# 
# latent_lvm_fit <-
#   latent_lvm %>%
#   setoptimizer(optimizer = "cpp_Nelder-Mead") %>%
#   runmodel()
# 
# latent_lvm_pars    <- parameters(latent_lvm_fit)
# 
# latent_lvm_fit_ind <- fit(latent_lvm_fit)

# Save figures -----------------------------------------------------------------

save_plot("./figures/ammsa-irma.png", hist_grid, base_height = 5, base_width = 5)
save_plot("./figures/ammsa-irma.tiff", hist_grid, base_height = 5, base_width = 5)

save_plot("./figures/ammsa-scatters.png", scatter_grid, base_height = 6, base_width = 12)
save_plot("./figures/ammsa-scatters.tiff", scatter_grid, base_height = 6, base_width = 12)

png("./figures/ammsa-1-factor.png", height = 9.5, width = 14, units = "in", res = 1500)
plot(ammsa_cfa_1_figure)
dev.off()

tiff("./figures/ammsa-1-factor.tiff", height = 9.5, width = 14, units = "in", res = 1500)
plot(ammsa_cfa_1_figure)
dev.off()

png("./figures/ammsa-7-factor.png", height = 9.5, width = 14, units = "in", res = 1500)
plot(ammsa_cfa_7_figure)
dev.off()

tiff("./figures/ammsa-7-factor.tiff", height = 9.5, width = 14, units = "in", res = 1500)
plot(ammsa_cfa_7_figure)
dev.off()

png("./figures/ammsa-network.png", height = 5.5, width = 9.6, units = "in", res = 1500)
plot(network_graph)
dev.off()

tiff("./figures/ammsa-network.tiff", height = 5.5, width = 9.6, units = "in", res = 1500)
plot(network_graph)
dev.off()