library(tidyverse)
library(lme4)
library(afex)
library(emmeans)
library(simr)

fit_algo <- function(new_call, newData=NULL, type="lmer")
{
  if(type!="lmer" && type!= "glmer")
  {
    stop("type must be lmer or glmer")
  }
  new_call["start"] <- NULL
  new_call["control"] <- parse(text=paste0(type, 'Control(optimizer ="bobyqa", optCtrl = list(maxfun=1000000))'))
  error_msg <- tryCatch({
    rval <- eval(new_call)
    if(length(rval@optinfo$conv$lme4)!=0)
    {
      "convergence"
    }
    else
    {
      "ok"
    }
  }, error = function(e) {
    "error"
  })
  count <- 0
  while(error_msg!="ok" && count<4)
  {
    if(error_msg=="convergence")
    {
      new_call["start"] <- parse(text="list(fixef = fixef(rval))")
    }
    count <- count + 1
    error_msg <- tryCatch({
      rval <- eval(new_call)
      if(length(rval@optinfo$conv$lme4)!=0)
      {
        "convergence"
      }
      else
      {
        "ok"
      }
    }, error = function(e) {
      "error"
    })
  }
  new_call["start"] <- NULL
  new_call["control"] <- parse(text=paste0(type, 'Control(optimizer ="Nelder_Mead", optCtrl = list(maxfun=1000000))'))
  count <- 0
  while(error_msg!="ok" && count<5)
  {
    count <- count + 1
    error_msg <- tryCatch({
      rval <- eval(new_call)
      if(length(rval@optinfo$conv$lme4)!=0)
      {
        "convergence"
      }
      else
      {
        "ok"
      }
    }, error = function(e) {
      "error"
    })
    if(error_msg=="convergence")
    {
      new_call["start"] <- parse(text="list(fixef = fixef(rval))")
    }
    
  }
  new_call["start"] <- NULL
  new_call["control"] <- NULL
  count <- 0
  while(error_msg!="ok" && count<5)
  {
    count <- count + 1
    error_msg <- tryCatch({
      rval <- eval(new_call)
      if(length(rval@optinfo$conv$lme4)!=0)
      {
        "convergence"
      }
      else
      {
        "ok"
      }
    }, error = function(e) {
      "error"
    })
    if(error_msg=="convergence")
    {
      new_call["start"] <- parse(text="list(fixef = fixef(rval))")
    }
    
  }
  if(!exists("rval"))
  {
    eval(new_call)
  }
  return(rval)
}

## Dataset loading
datasets       <- list.files(file.path("power analysis", "study2_pilot_data"), pattern="*.csv", full.names = TRUE)
datasets       <- lapply(datasets, read_csv, show_col_types = FALSE)

d <- datasets[[1]]
for (i in seq(from=2, to=length(datasets)))
{
  d <- rbind(d, datasets[[i]])
}

## Dataset preparation
d <- d %>% mutate(Subject=rep(seq_len(length(datasets)), each=100))

d_final <- d %>% filter(Dwell.Time > 1500) %>%
  mutate(Agreement.Scaled = (Agreement-mean(Agreement))/sd(Agreement),
         Squared.Agreement.Scaled=Agreement.Scaled^2,
         Agreement.Contacts.Scaled = (Agreement.Contacts-mean(Agreement.Contacts))/sd(Agreement.Contacts),
         Squared.Agreement.Contacts.Scaled=Agreement.Contacts.Scaled^2,
         Index=as.factor(Index), Subject=as.factor(Subject))

## Statistical models
### Agreement
fit_personal_call <- parse(text='lmer(formula=Dwell.Time ~ Agreement.Scaled + Squared.Agreement.Scaled + Condition_Share + Agreement.Scaled*Condition_Share + Squared.Agreement.Scaled*Condition_Share +
                              (1 | Index) +
                              (1 | Subject),
                            data=d_final)')[[1]]

fit_personal <- fit_algo(fit_personal_call)

### Agreement (contacts)

fit_contacts_call <- parse(text='lmer(formula=Dwell.Time ~ Agreement.Contacts.Scaled + Squared.Agreement.Contacts.Scaled + Condition_Share + Agreement.Contacts.Scaled*Condition_Share + Squared.Agreement.Contacts.Scaled*Condition_Share +
                              (1 | Index) +
                              (1 | Subject),
                            data=d_final)')[[1]]

fit_contacts <- fit_algo(fit_contacts_call)

fit_personal@call["start"] <- NULL
fit_personal_random@call["start"] <- NULL
fit_contacts@call["start"] <- NULL

sim_personal_random <- extend(fit_personal_random, along="Subject", n=140)
sim_personal <- extend(fit_personal, along="Subject", n=140)
sim_contacts <- extend(fit_contacts, along="Subject", n=140)

### this is assuming a vertex at 0, a maximum difference
### between vertex and extremes of ~-100ms, and a complete reversal between conditions
### (going from -100ms to +100ms)
fixef(sim_personal)['Agreement.Scaled'] <- 0
fixef(sim_personal)['Agreement.Scaled:Condition_Share'] <- 0
fixef(sim_personal)['Squared.Agreement.Scaled'] <- -25
fixef(sim_personal)['Squared.Agreement.Scaled:Condition_Share'] <- 50

### Same for contacts
fixef(sim_contacts)['Agreement.Contacts.Scaled'] <- 0
fixef(sim_contacts)['Agreement.Contacts.Scaled:Condition_Share'] <- 0
fixef(sim_contacts)['Squared.Agreement.Contacts.Scaled'] <- -25
fixef(sim_contacts)['Squared.Agreement.Contacts.Scaled:Condition_Share'] <- 50