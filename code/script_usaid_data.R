if(Sys.info()['user'] == "Maheep'"){
  base_dir = 'D:/Documents/2020 TUM/Sem 2/Development/paper/Code/'
  data_dir = paste0(base_dir, 'USAID_Land_Tenure_Evaluation_Data_ELTAP-ELAP-Complete/')
  out_dir =   paste0(base_dir, 'output/')
  setwd(base_dir)
} else { 
  base_dir = ''
  print(paste0("Manually set base dir in script for this user - ", Sys.info()['user']))
}

#######################################
# load packages
#######################################
library(foreign)
library(sandwich)
library(lmtest)
library(stargazer)
library(lme4)
library(censReg)
library(VGAM)
library(summarytools)
library(knitr)
library(stringr)
library(pdftools)
library(mfx)
library(kableExtra)
library(tidyr)
library(dplyr)
library(texreg)
library(ggplot2)

#######################################
# define functions
#######################################
read_view_names_labels <- function(file_path) {
  print(paste0('#########   ',file_path,'   #########'))
  data <- read.csv(file_path)
  return(data)
}

#######################################
# define vars
#######################################
l_cols_id = c('hh_id', 'kebele', 'woreda', 'zone', 'killil')
l_cols_demo = c('memint', 'relhead', 'age', 'sex', 'edu')
l_cols_plotid = c('parclu', 'parclunm', 'parchow')
l_cols_plotreg = c('parcreg', 'parcb', 'parc2cer') # suffix 1-22
# "717" "parcreg1" "parcreg1::3.1 Has PARCEL 1 been surveyed or certified at all?
# "718" "parcb1" "parcb1::3.2 Do you have 1st level certification for PARCEL 1?
# "722" "parc2sur1" "parc2sur1::3.6 Has PARCEL 1 been surveyed for 2nd level certification?
# "724" "parc2cer1" "parc2cer1::3.8 Do you have a 2nd level certificate for PARCEL 1?
l_cols_plotrights = c('parcruse', 'parcrout', 'parcrher', 'parcrsel', 'parcrcol') # suffix 1-22
l_cols_wealth = c('ironroof2', 'mobile2', 'taperec2', 'radio2', 'sofa2', 'barrel2', 'cart2', 'plow2', 'jewelty2','townhouse2')
l_cols_credit = c('credamt', 'credwho', 'credlc', 'credlcty') # suffix 1-4
l_cols_rental = c('pout', 'rentin2')
l_cols_disputes = c('dispute2', 'redland')

# for regression
ivs = c('cert', 'cert_rnd_one', 'cert_rnd_two')
controls = c('f_head','edu_head', 'dispute', 'land_red', 'age_head', 'm_ad', 'f_ad', 'n_plots', 'wealth_score')
custom.coef.map = list("cert" = "Land certification",
                         "cert_rnd_one" = "Phase 1 Land certification",
                         "cert_rnd_two" = "Phase 2 Land certification",
                         "age_head" = "Age of head of hh",
                         "edu_head" = "Literate head of hh",
                         "f_head" = "Female-headed hh",
                         "m_ad" = "Male adult members in hh",
                         "f_ad" = "Female adult members in hh",
                         "n_plots" = "Plots owned by the hh",
                         "dispute" = "Land dispute",
                         "wealth_score" = "Wealth",
                         "land_red" = "Experienced land redistribution")

#######################################
# load data
#######################################
file_path = paste0(data_dir, 'USAID_Land_Tenure_EE_IE-Endline_Household_Data.csv')
data_raw = read_view_names_labels(file_path)

#######################################
####### extract var labels from codebook
#######################################
##### UNCOMMENT TO GET VAR LABELS
# file_path = paste0(data_dir, 'USAID_Land_Tenure_EE_IE-Endline_Household_Codebook.pdf')
# data_raw_cb = pdf_text(file_path)
# # create a vector with each line of codebook as an element
# lines_cb = c()
# for (pg in data_raw_cb) {
#   lines_pg = str_split(pg, pattern = '\n')
#   lines_cb = c(lines_cb, lines_pg[[1]])
# }
# # grep out var labels from the new vector
# var_labels = c()
# for (var in colnames(data_raw)) {
#   list_matches = str_which(string = lines_cb, pattern = var)
#   line_match = lines_cb[list_matches[1]]
#   var_label = line_match %>% str_replace(pattern = regex("^ +"), '') %>% str_replace(pattern = regex("  +"), '::')
#   var_labels = c(var_labels, var_label)
#   lines_cb = lines_cb[- seq(1,list_matches[1])]
# }
# attributes(data_raw)$var_labels = var_labels
##### UNCOMMENT TO GET VAR LABELS

#######################################
# prepare data for regression
#######################################

# inspect data
#######################################
# see who the respondents of the survey are, want to know if it is the hh head always
df_hh_members = data_raw %>% group_by(hh_id) %>% summarise(n_members = n())
sum(df_hh_members$n_members) == nrow(data_raw) # this is TRUE, means only one respondent per hh
data_raw %>% dplyr::select(hh_id, starts_with('memint'), starts_with('relhead')) %>% filter(relhead1 != 1) # 2 hh where member #1 is not the head => for both member #2 is the head
sum(data_raw$memint1) == nrow(data_raw) # not all are primary respondents
data_raw %>% dplyr::select(hh_id, starts_with('memint')) %>% filter(memint1 == 0) # display hh where member #1 is not primary respondent => mostly members #2-6

# HH-wise demographic data
#######################################
# need to pivot data to make it computation-friendly
df_demo_raw = data_raw %>% dplyr::select(l_cols_id, starts_with(l_cols_demo)) %>% 
                        pivot_longer(cols = c(starts_with(l_cols_demo)),
                                    names_to = c('demo_var', 'member_no'),
                                    names_pattern = "([A-Za-z]+)(\\d+)",
                                    values_to = 'val') %>% 
                        filter(val != -98) %>%
                        pivot_wider(names_from = 'demo_var', 
                                    values_from = 'val')

df_demo = df_demo_raw %>% filter(relhead == 1) %>% 
                          mutate(age_head = age, f_head = ifelse(sex == 2, 1, 0), edu_head = ifelse(edu > 1, 1, 0)) %>% 
                          dplyr::select(l_cols_id, age_head, f_head, edu_head) # age_head, sex_head, edu_head
df_temp = df_demo_raw %>% filter(age >= 18) %>% 
                          dplyr::select(l_cols_id, member_no, sex) %>% 
                          group_by(hh_id) %>% count(sex) %>% 
                          pivot_wider(names_from = 'sex', values_from = 'n', values_fill = list(n = 0)) %>%
                          rename(m_ad = '1', f_ad = '2') # number of male, female adult members (m_ad, f_ad)
df_demo = full_join(df_demo,df_temp)
df_temp = df_demo_raw %>% dplyr::select(l_cols_id, member_no) %>% 
                          group_by(hh_id) %>% 
                          summarise(hh_size = n()) # hh_size
df_demo = full_join(df_demo,df_temp)

df_demo %>% filter_all(any_vars(is.na(.))) # to check if any rows have NA

# HH-wise plot data
#######################################
# pivot data_raw to HH-wise plots
l_cols_plot = c(l_cols_plotid, l_cols_plotreg, l_cols_plotrights)
df_plot_raw = data_raw %>% dplyr::select(l_cols_id, starts_with(l_cols_plot)) %>%
                          pivot_longer(cols = c(starts_with(l_cols_plot)),
                                       names_to = c('plot_var', 'plot_no'),
                                       names_pattern = "([A-Za-z]+[0-9]*[A-Za-z]*)(\\d+$)",
                                       values_to = 'val') %>%
                          filter(!is.na(val)) %>%
                          pivot_wider(names_from = 'plot_var',
                                      values_from = 'val')
df_plot_raw$plot_no = as.integer(df_plot_raw$plot_no) # fix col type

df_plot = df_plot_raw %>% dplyr::select(l_cols_id) %>% distinct()
df_temp = df_plot_raw %>% dplyr::select(l_cols_id, plot_no) %>% 
                          group_by(hh_id) %>% 
                          summarise(n_plots = n()) # no of plots
df_plot = right_join(df_plot, df_temp)

df_temp = df_plot_raw %>% dplyr::select(hh_id, l_cols_plotreg) %>% 
                          group_by(hh_id) %>% mutate(cert_rnd_two = ifelse(1 %in% parc2cer, 1, 0),
                                                     cert_rnd_one = ifelse(1 %in% parcb, 1, 0),
                                                     cert = ifelse(cert_rnd_one | cert_rnd_two, 1, 0)) %>% 
                          dplyr::select(-all_of(l_cols_plotreg)) %>% distinct() # land certified in phase 1 or 2 
df_plot = full_join(df_plot, df_temp)

# HH-wise wealth data
#######################################
# create wealth_index =  scores from PCA of wealth-indicator vars 
df_wealth = data_raw %>% dplyr::select(l_cols_id, l_cols_wealth)
f_pca = as.formula(paste("", paste(l_cols_wealth, collapse = '+'), sep = '~'))
pca = prcomp(f_pca, data = df_wealth, na.action = na.exclude)
df_wealth$wealth_score = pca$x[,'PC1']


# HH-wise credit data
#######################################
df_credit_raw = data_raw %>% dplyr::select(l_cols_id, starts_with(l_cols_credit)) %>% 
                            pivot_longer(cols = c(starts_with(l_cols_credit)),
                                         names_to = c('credit_var', 'credit_no'),
                                         names_pattern = "([A-Za-z]+)(\\d+)",
                                         values_to = 'val') %>% 
                            filter(val != -98) %>%
                            pivot_wider(names_from = 'credit_var', 
                                        values_from = 'val')
df_credit_raw$credit_no = as.integer(df_credit_raw$credit_no) # fix col type

df_credit = data_raw %>% dplyr::select(l_cols_id, cred)
df_temp = df_credit_raw %>% dplyr::select(hh_id, credamt) %>% 
                            group_by(hh_id) %>% 
                            summarise(sum_credamt = sum(credamt)) %>% 
                            dplyr::select(hh_id, sum_credamt) %>% 
                            distinct() # get total credit amount per hh
df_credit = full_join(df_credit, df_temp)
df_credit = df_credit %>% mutate(sum_credamt = ifelse(is.na(sum_credamt), 0, sum_credamt * cred)) %>%
                          dplyr::select(-cred) %>%
                          rename(credit_amt = 'sum_credamt') # clean up total credit amount per hh
# create df for credit summary stats table
df_credit_sum = data.frame('name' = 'Total Households with credit (last 2 years)', 'n' = length(unique(df_credit_raw$hh_id)))
df_credit_sum = df_credit_sum %>% add_row(name = 'Total Loans Obtained (last 2 years)', 
                                          n = nrow(df_credit_raw))
df_credit_sum = df_credit_sum %>% add_row(name = c('Micro-finance Institution', 'Individual', 'Savings and Credit Association', 'Other'), 
                                         n = df_credit_raw %>% group_by(credwho) %>% 
                                                               count() %>%
                                                               pull())
df_credit_sum = df_credit_sum %>% add_row(name = c('First phase', 'Second phase', 'Both', 'None'),
                                          n = df_credit_raw %>% group_by(credlcty) %>% 
                                                                count() %>% 
                                                                pull())
df_credit_sum = df_credit_sum %>% mutate(perc = round(100*n/nrow(df_credit_raw), 1))
df_credit_sum[1,'perc'] = NA
colnames(df_credit_sum) = c('Variable', 'Count', 'Percentage')


#######################################
####### compile processed data
#######################################
df_reg = full_join(df_demo, df_plot)
df_reg = full_join(df_reg, df_wealth)
df_reg = full_join(df_reg, df_credit)

df_reg = df_reg %>% mutate(credit = ifelse(credit_amt > 0, 1, 0)) # make credit a binary var - credit taken or not
df_reg = data_raw %>% dplyr::select(l_cols_id, l_cols_disputes) %>% 
                      right_join(df_reg) %>% 
                      rename(dispute = 'dispute2', land_red = 'redland' ) # add binary var for 'land under dispute', 'experienced land redistribution
df_reg[l_cols_id] <- lapply(df_reg[l_cols_id], as.factor) # fix col types

# CODE TO WRITE OUT THE COMPILED, PROCESSED DATA, NOT RUN
dir.create(out_dir, recursive = TRUE)
zz <- file(paste0(out_dir, "processed_data.csv"), open = "wt")
sink(zz)
sink(zz, type = "message")
write.csv(df_reg, zz, col.names = TRUE, row.names = FALSE)
sink(type = "message")
sink()
close(zz)

#######################################
# descriptive stats
#######################################
# 1. Table to give glimpse of data set
options(knitr.kable.NA = '')
df_sum1 = df_reg %>% group_by(killil) %>% 
                    summarise(n_woredas = n_distinct(woreda), n_kebeles = n_distinct(kebele), n_hh = n_distinct(hh_id)) %>%
                    arrange(n_hh)
df_sum1$killil = c('SNNP', 'Amhara', 'Oromia', 'Tigray')

## ---- sum_table1 ----
kbl(df_sum1, booktabs = T, col.names = c('Killil (Region)', 'Woreda (District)','Kebele (Village)', 'Household'), caption = 'Summary of data set - Regions, Districts, Villages and Households') %>%
  kable_styling(latex_options = 'hold_position') %>%
  add_header_above(c(" " = 1, "Count" = 3))
## ---- end-sum_table1 ----

# 2. table that gives summary of vars used in regression
df_sum2 = df_reg %>% filter_all(all_vars(!is.na(.))) %>% 
                    dplyr::select(c(all_of(ivs), all_of(controls))) %>% 
                    summarise_each(funs = c(sum , mean)) %>%
                    pivot_longer(everything(),
                                 names_to = c('var', 'function'),
                                 names_pattern = "([A-Z_a-z]+)_fn(\\d+)",
                                 values_to = 'val') %>%
                    filter_all(all_vars(!is.na(.))) %>% 
                    pivot_wider(names_from = 'function', 
                                values_from = 'val') %>%
                    mutate(var = unlist(custom.coef.map[var], use.names = FALSE))
df_sum2[match(c(ivs, controls), df_sum2$var),]
colnames(df_sum2) = c('Variable', 'Count of hh', 'Mean')
df_sum2$Mean = round(df_sum2$Mean, 2)
df_sum2[1:7, 'Mean'] = NA
df_sum2[8:12, 'Count of hh'] = NA
df_sum2['Percentage of hh'] = round(100*df_sum2['Count of hh']/nrow(df_reg), 1) 
df_sum2 = df_sum2[,c('Variable', 'Count of hh', 'Percentage of hh', 'Mean')]

## ---- sum_table2 ----
kbl(df_sum2, booktabs = T, caption = 'Summary of data set - Variables of interest') %>%
  kable_styling(latex_options = 'hold_position')
## ---- end-sum_table2 ----

## ---- sum_table_credit ----
kbl(df_credit_sum, booktabs = T, caption = 'Summary of credit-related variables') %>%
  kable_styling(latex_options = 'hold_position') %>%
  pack_rows("Sources of Credit", 2, 6) %>%
  pack_rows("Land certificate used to obtain credit", 7, 10)
## ---- end-sum_table_credit ----

# plot to show relationship between wealth and credit
plt = ggplot(data = df_reg, aes(x = wealth_score, y = credit), na.rm = TRUE) +
  geom_point(color='blue', na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
  labs(title="Wealth score and credit",x="Wealth score", y = "Credit obtained or not", na.rm = TRUE)
#suppressMessages(print(plt))

#######################################
# regression
#######################################
outcome = "credit"
# m1 - use cert (not pooled by phase), no fixed effects 
vars = c(ivs, controls)
vars = setdiff(vars, c('cert_rnd_one', 'cert_rnd_two'))
f = as.formula(paste(outcome, paste(vars, collapse = '+'), sep = '~'))
m1 = probitmfx(f, data = df_reg) # marginal effects
# m2 - pool cert, no fixed effects 
vars = c(ivs, controls)
vars = setdiff(vars, c('cert'))
f = as.formula(paste(outcome, paste(vars, collapse = '+'), sep = '~'))
m2 = probitmfx(f, data = df_reg) # marginal effects
# m3 - pool cert, with village-level fixed effects 
vars = append(vars, 'as.factor(kebele)')
f = as.formula(paste(outcome, paste(vars, collapse = '+'), sep = '~'))
m3 = probitmfx(f, data = df_reg) # marginal effects

# print latex table
mods = list(m1$fit, m2$fit, m3$fit)
latex_table_header_1 = "\\begin{table}[h]
\\begin{center}
\\caption{Does land registration affect credit?}
\\label{table:reg_table1}"
latex_table_footer_1 = "\\end{center}
\\end{table}"
note_table_footer_1 = "Model 3 uses village (kebele) level fixed effects"
## ---- reg_table1 ----
writeLines(latex_table_header_1)
texreg(mods,
       digits = 3,
       stars = c(0.01, 0.05, 0.10),
       caption.above = TRUE,
       caption = "Does land registration affect credit?",
       custom.coef.map = custom.coef.map,
       custom.header = list("Dependent Variable: Obtained credit" = 1:3),
       custom.note = paste0(note_table_footer_1,"%stars"),
#       custom.model.names = c('(1)', '(2)', '(3)'),
       table = FALSE
)
writeLines(latex_table_footer_1)
## ---- end-reg_table1 ----

coefs = list(c(0, m1$mfxest[, 1]), c(0, m2$mfxest[, 1]), c(0, m3$mfxest[, 1]))
ses = list(c(0, m1$mfxest[, 2]), c(0, m2$mfxest[, 2]), c(0, m3$mfxest[, 2]))
pvals = list(c(0, m1$mfxest[, 4]), c(0, m2$mfxest[, 4]), c(0, m3$mfxest[, 4]))
latex_table_header_2 = "\\begin{table}[h]
\\begin{center}
\\caption{Does land registration affect credit? - Marginal effects}
\\label{table:reg_table2}"

## ---- reg_table2 ----
writeLines(latex_table_header_2)
texreg(mods,
       override.coef = coefs,
       override.se = ses,
       override.pval = pvals,
       omit.coef = "(Intercept)",
       caption.above = TRUE,
       caption = "Does land registration affect credit? - Marginal effects",
       stars = c(0.01, 0.05, 0.10),
       digits = 3,
       custom.coef.map = custom.coef.map,
       custom.header = list("Dependent Variable: Obtained credit" = 1:3),
       custom.note = paste0(note_table_footer_1,"%stars"),
       custom.model.names = c('(1)', '(2)', '(3)'),
       table = FALSE
)
writeLines(latex_table_footer_1)
## ---- end-reg_table2 ----

par(mfrow = c(2, 2))
plot(m2$fit)
