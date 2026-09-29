source(file.path("power analysis", "power_analysis_preparation.R"))

doTest <- function(object, test=fixed(getDefaultXname(object)), ...) {
  
  opts <- simrOptions(...)
  on.exit(simrOptions(opts))
  test <- wrapTest(test)
  pval <- test(object)
  
  if(!is.numeric(pval) || length(pval)!= 1 || is.na(pval)) stop("Test did not return a p-value")
  if(fixef(object)["Squared.Agreement.Contacts.Scaled"] <0)
  {
    pval <- pval/2
  }
  else
  {
    pval <- 1 - pval/2
  }
  rval <- structure(pval,
                    
                    text = str_c("p-value", substring(attr(test, "text")(object, object), 6)),
                    description = attr(test, "description")(object, object)
  )
  
  class(rval) <- "test"
  return(rval)
}

environment(doTest) <- asNamespace("simr")
assignInNamespace("doTest", doTest, ns="simr")

### Power analysis
for(i in c(-100, -75, -25))
{
  fixef(sim_contacts)['Squared.Agreement.Contacts.Scaled'] <- i
  fixef(sim_contacts)['Squared.Agreement.Contacts.Scaled:Condition_Share'] <- -i*2
  pc_contacts_quad <- powerSim(sim_contacts, nsim=50, test=fixed("Squared.Agreement.Contacts.Scaled"), seed=0)
  saveRDS(pc_contacts_quad, file.path("power analysis", paste0("power_contacts_quad_list_", i, ".rds")))
}

fixef(sim_contacts)['Squared.Agreement.Contacts.Scaled'] <- -100
fixef(sim_contacts)['Squared.Agreement.Contacts.Scaled:Condition_Share'] <- 200
pc_contacts_interaction <- powerSim(sim_contacts, nsim=50, test=fixed("Squared.Agreement.Contacts.Scaled:Condition_Share"), seed=0)
saveRDS(pc_contacts_interaction, file.path("power analysis", paste0("power_contacts_interaction_list_", 200, ".rds")))