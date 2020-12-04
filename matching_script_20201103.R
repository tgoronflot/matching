# --- # data set example

# load("Y:/CIC/EC-CIC/13-SCRIPTS R/matching_functions/matching_20201027.RData")

# The data set is derived from the CORONADO study (https://www.diabetes-covid.org/en/study/)
# ID numbers and centers are factice

#####---------#####
##### FUNCTION

apparier <- function(
  
  data, # the data set to work on
  id, # character variable of identifiers for observations
  group, # binary variable that define cases/controls, or exposed/unexposed. Should be factor or numeric. 
  lvl.first = TRUE, # If TRUE (default), the first level of group variable is considered as controls. You can check for it with levels(data$group)
  cat.var, # vector of character for categorical variables to match on. Should be factors variables
  cont.var, # vector of character for continuous variables to match on. Should be numeric variables
  tolerance, # vector of numeric to define tolerance allowed to find control regarding continuous variables. set 0 for no tolerance. NA not permitted
  # time.var, # vector of character for time variables. Should be "Date" variables {lubridate}
  # time.unit, # vector of character defining desired units for duration computation regarding time variables. Should be 'seconds', 'hours', 'days', 'weeks' or 'years'. Same size as time.var
  # time.close = rep(TRUE, length(time.var)), # Do controls have to be sorted starting with the smallest(TRUE)/greatest(FALSE) calculated period for each time variables ? Default is TRUE
  get.list = FALSE, # if TRUE, returns the list of all potential controls by case without doing actions anymore. Default is FALSE
  optimal = TRUE, # does optimal matching should be performed ? If not, pairs are selected the same order the cases are presented in the data set. Default is TRUE
  n_controls = Inf # How many controls are allowed by case ? by default, there is no limit 
  # same_n_controls = FALSE # Set the same number of controls for each case, determined by the lowest control potential found for a case. Default is FALSE
  
){

# --- # packages
  
library(tidyverse)
library(lubridate)
library(data.table)
# library(rlang)
  
# --- # transform data set into data.table

setDT(data)

# --- # transform modalities into numeric for categorical variables
  
data[, (cat.var) := lapply(.SD, as.integer), .SDcols = cat.var]
  
# --- # Create list of results
  
resultat <- list(NULL)

# # --- # checking arguments
# 
# # group is binary
# if(length(table(data[, ..group])) != 2) stop("group variable has more than 2 modalities")
# # cat.var involved only factors
# if(length(cat.var) > 1){
#   if(all(apply(data[, ..cat.var], 2, is.factor))) stop("At least one of the categorical variables is not a factor")}else{
#     if(is.factor(data[, ..cat.var])) stop("Categorical variable is not a factor")}
# # cont.var involved only numerics
# if(length(cont.var) > 1){
#   if(all(apply(data[, ..cont.var], 2, is.numeric))) stop("At least one of the continuous variables is not numeric")}else{
#     if(is.numeric(data[, ..cont.var])) stop("Continuous variable is not a numeric")}
# # tolerance is a vector of numeric
# if(length(tolerance) != length(cont.var)) stop("tolerance vector is not the same size as cont.var. Please use 0 if no tolerance is defined")
# if(! is.numeric(tolerance)) stop("tolerance vector is not numeric")
# # # time.var
# # try(if(all(sapply(data[, time.var], is.Date))) stop("At least one of the time variables is not a Date object"))

# --- # separate the data in two data.tables: one for cases and another one for controls 

lvl.grp <- names(table(data[, ..group]))

if(nrow(data[group == lvl.grp[1]]) == 0){
  # if group is a numeric
  if(isTRUE(lvl.first)){
    data.control <- data %>% filter(.[[group]] == as.numeric(lvl.grp[1]))
    data.case <- data %>% filter(.[[group]] == as.numeric(lvl.grp[2]))
  }else{
    data.control <- data %>% filter(.[[group]] == as.numeric(lvl.grp[2]))
    data.case <- data %>% filter(.[[group]] == as.numeric(lvl.grp[1]))
  }
}else{
  # if group is a factor
  if(isTRUE(lvl.first)){
    data.control <- data %>% filter(.[[group]] == lvl.grp[1])
    data.case <- data %>% filter(.[[group]] == lvl.grp[2])
  }else{
    data.control <- data %>% filter(.[[group]] == lvl.grp[2])
    data.case <- data %>% filter(.[[group]] == lvl.grp[1])
  }
}

# --- # creates the list for potential controls

pot.controls <- vector("list", data.case[,.N])
# elements names are identifiers of cases
names(pot.controls) <- data.case %>% select(id) %>% pull()

# --- # find out all potential controls for cases, according to variables to match on

for(i in 1:length(pot.controls)){
  
pot.controls[[i]] <- data.control %>%
# Selecting potential controls
filter_(paste(cat.var, "==", data.case[i, ..cat.var], collapse = " & "),
       paste(cont.var, ">=", data.case[i, ..cont.var] - tolerance, " & ", cont.var, "<=", data.case[i, ..cont.var] + tolerance, collapse = " & "))
# compute differences for continuous variable
# mutate(
#   diff.age.years = abs(age - base.match.exposed$age[[i]]),
#   diff.date.jours = abs(interval(date.hospit, base.match.exposed$date.hospit[[i]])) %/% days(1)
# ) %>% 
# # ranking potential controls
# arrange(diff.date.jours, diff.age.years)

}

# --- # Cases without any matching

no.matches.cases <- pot.controls %>% discard(function(x) nrow(x) > 0) %>% names()
perc.no.match <- round(length(no.matches.cases)/data.case[,.N] * 100, 1)

# remove them from pot.controls list
pot.controls <- pot.controls %>% discard(function(x) nrow(x) == 0)

# --- # Possibilities for return

if(isTRUE(get.list)){
  
return(pot.controls) # to get the list of potential controls ...

  }else{ # ...or continue the process according to the arguments provided 

  ## create matching table (final selected pairs)
  matching.table <- NULL
  
  # initialize the matching process
  pot.table.init <- data.frame(
    case.id = rep(names(pot.controls), map(pot.controls, nrow) %>% unlist() %>% unname()), # cases ID in the pair
    cont.id = map(pot.controls, id) %>% unlist() %>% unname() # control ID in the pair
  ) %>% as_tibble()
  # vector of id for cases
  pot.cases.id <- pot.table.init %>% distinct(case.id) %>% pull()
  # attribute a random number to each pair for ties
  pot.table.init <- pot.table.init %>% mutate(rand.nb = sample(seq(-10,10,0.0001), nrow(pot.table.init), replace = F))
  # potential for cases and controls
  setDT(pot.table.init)[, case.pot := .N, by = .(case.id)][, cont.pot := .N, by = .(cont.id)]
  
    if(isFALSE(optimal)){
      
      while(nrow(pot.table.init) > 0){
        
        # add next pair to final sample
        matching.table <- rbindlist(l = list(matching.table, setDT(pot.table.init[1, c(1:2)])))
        # remove other potential pairs concerning this control from pot.table.init
        pot.table.init <- pot.table.init[cont.id != matching.table[.N, cont.id]]
        
      }
      
      if(n_controls < Inf){
        # limit the number of controls for cases to n_controls
        matching.table <- matching.table[, head(.SD, n_controls), by = case.id]

      }else{
      # no limitation in the number of controls for a case
      }
      
    }else{
      
      while(nrow(pot.table.init) > 0){
        
        # (re)arrange according to potentials
        pot.table.init <- pot.table.init[order(case.pot, -rank(cont.pot), rand.nb)]
        # add next pair to final sample
        matching.table <- rbindlist(l = list(matching.table, setDT(pot.table.init[1, c(1:2)])))
        # remove other potential pairs concerning this control from pot.table.init
        pot.table.init <- pot.table.init[cont.id != matching.table[.N, cont.id]]
        
      }
      
      if(n_controls < Inf){
        # limit the number of controls for cases to n_controls
        matching.table <- matching.table[, head(.SD, n_controls), by = case.id]

      }else{
        # no limitation in the number of controls for a case
      }
      
    }
    
  }

# --- # ties

ties.cases <- pot.cases.id[! pot.cases.id %in% matching.table$case.id]
perc.ties <- round(length(ties.cases)/data.case[,.N] * 100, 1)

# --- # create 'pairs' variable(s) into initial data set

n_pairs_var <- unname(sort(table(matching.table$case.id), decreasing = T))[1]
new.data <- data
setindexv(new.data, id) # register rows order given by id
matching.table[, numero := sequence(.N)]

for(i in 1:n_pairs_var){

new_col_names <- paste0("pair_", i)  
colnames(matching.table)[3] <- new_col_names

new.data <- na.omit(matching.table[, .SD[i], by = case.id]) %>% gather(group, identity, -paste0("pair_", i)) %>% select(-group) %>% 
  rename(!!id := identity) %>% right_join(new.data, by = eval(id)) %>% setDT()

}

to.put.last <- new.data %>% select(starts_with("pair_")) %>% names() %>% sort()
new.data <- new.data %>% relocate(starts_with("pair_"), .after = last_col()) %>% select(-to.put.last, to.put.last)
setkey(new.data) # returns new.data in the same order as the initial dataset with respect to id 

# --- # gather results into final list

resultat[[1]] <- no.matches.cases
resultat[[2]] <- perc.no.match # percentage of unmatched cases
resultat[[3]] <- ties.cases
resultat[[4]] <- perc.ties # percentage of unmatched cases because of ties
resultat[[5]] <- 100 - (resultat[[2]] + resultat[[4]]) # percentage of matched cases
resultat[[6]] <- matching.table[, c(1,2)] # final matrix of selected pairs
resultat[[7]] <- new.data # initial dataset including variable(s) allowing to retrieve the constituted pairs 

names(resultat) <- c("cases.id.no.match", "percentage.no.match", "cases.id.ties", "percentage.ties", "percentage.match", "pairs", "new.dataset")

# --- # end

return(resultat)

}

# # --- # Results
# 
# res.appar <- apparier(data = base.match, id = "ident", group = "exposure", lvl.first = TRUE, cat.var = c("sex", "center"),
#                    cont.var = "age", tolerance = 5, get.list = FALSE, optimal = TRUE, n_controls = 5)
# res.appar
# res.appar$pairs

