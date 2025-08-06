#-------------------------------------------------------------------------------
# Utility functions for the analysis
#-------------------------------------------------------------------------------

rename_effects = function(effects, label_map) {
  sapply(effects, function(term) {
    vars = strsplit(term, ":")[[1]]
    if (all(vars %in% names(label_map))) {
      paste(label_map[vars], collapse = " * ")
    } else {
      term  # fallback to original if unmatched
    }
  }, USE.NAMES = FALSE)
}


library(lme4)

effect_contributions_hierarchical = function(full_model, data) {
  # Extract fixed effects terms
  full_terms = attr(terms(full_model), "term.labels")
  
  # Map of terms to higher-order interactions
  term_dependencies = lapply(full_terms, function(term) {
    dependent_terms = full_terms[sapply(full_terms, function(t) {
      all(strsplit(term, ":")[[1]] %in% strsplit(t, ":")[[1]]) && term != t
    })]
    c(term, dependent_terms)
  })
  names(term_dependencies) = full_terms
  
  # Initialize result table
  results = data.frame(
    Effect = character(),
    Terms_Dropped = character(),
    Pseudo_R2_Contribution = numeric(),
    delta_AIC = numeric(),
    delta_BIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  full_logLik = logLik(full_model)
  n = nobs(full_model)
  
  for (term in full_terms) {
    print(paste("Processing term:", term))
    # Drop the term and any dependent interactions
    drop_terms = unique(unlist(term_dependencies[[term]]))
    kept_terms = c(setdiff(full_terms, drop_terms), "(1 | ID)")  # Always keep the random intercept

    
    # Rebuild the formula
    new_formula = reformulate(kept_terms, response = as.character(formula(full_model)[[2]]))
    
    # Try refitting
    reduced_model = try(update(full_model, formula = new_formula), silent = TRUE)
    
    if (!inherits(reduced_model, "try-error")) {
      reduced_logLik = logLik(reduced_model)
      r2_drop = 1 - exp((2 * (reduced_logLik - full_logLik)) / n)
      delta_AIC = AIC(reduced_model) - AIC(full_model)
      delta_BIC = BIC(reduced_model) - BIC(full_model)
      
      results = rbind(
        results,
        data.frame(
          Effect = term,
          Terms_Dropped = paste(drop_terms, collapse = ", "),
          Pseudo_R2_Contribution = round(r2_drop, 4),
          delta_AIC = round(delta_AIC, 4),
          delta_BIC = round(delta_BIC, 4),
          stringsAsFactors = FALSE
        )
      )
    } else {
      results = rbind(
        results,
        data.frame(
          Effect = term,
          Terms_Dropped = paste(drop_terms, collapse = ", "),
          Pseudo_R2_Contribution = NA,
          delta_AIC = NA,
          delta_BIC = NA,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  return(results)
}



