#' A function for Type III Anova (generates the same output as SPSS does)
#' requires installation of the car and dplyr package
#' @param linearmodel A linear model.
#' @keywords type3 anova
#' @export
#' @examples
#' type3anova(lm(y ~ x1 * x2, data=dataset))

type3anova <- function(linearmodel) {

  library(dplyr)

  # get initial contrast options:
  initial.options <- options()
  initial.options.contrasts <- initial.options$contrasts

  # variables
  variables <- all.vars(linearmodel$call) # what are the variables in our model
  variables2 <- variables[-c(1,length(variables))] # remove first (dv) and last (data)

  # set contrasts for each variable
  variables.list <- as.list(variables2)
  for (i in seq(length(variables.list))){
    variables.list[[i]] <- contr.sum
  }
  names(variables.list) <- variables2

  # perform ANOVA
  options(contrasts=c("contr.sum", "contr.poly"))
  dt <- linearmodel$model
  if (class(linearmodel)[1] == "lm"){ # to handle GLM
    new.linearmodel <- lm(linearmodel$call, data=dt)
  } else if (class(linearmodel)[1] == "glm"){
    fam <- linearmodel$family$family
    new.linearmodel <- glm(linearmodel$call, data=dt, family=fam)
  }

  type3_anova.temp <- car::Anova(new.linearmodel, contrasts=variables.list, type = 3)
  type3_anova <- tibble::as_tibble(type3_anova.temp) %>%
    dplyr::mutate(term = rownames(type3_anova.temp)) %>%
    dplyr::rename("df1" = "Df")

  if (class(linearmodel)[1] == "lm"){ # to handle GLM
    type3_anova <- type3_anova %>%
      dplyr::rename("ss" = `Sum Sq`, "f" = "F value", "pvalue" = `Pr(>F)`) %>% # rename columns
      dplyr::mutate(df2 = max(summary(linearmodel)$df)) %>%
      dplyr::select(term, ss, df1, df2, f, pvalue)

    } else if (class(linearmodel)[1] == "glm"){
    type3_anova <- type3_anova %>%
      dplyr::rename("LR_chisq" = `LR Chisq`, "pvalue" = `Pr(>Chisq)`) %>% # rename columns
      dplyr::mutate(df2 = linearmodel$df.residual) %>%
      dplyr::select(term, LR_chisq, df1, df2, pvalue)
    }

  type3_anova <- type3_anova %>% mutate(pvalue = round(pvalue, 3))

  # reset contrast options
  options(contrasts=initial.options.contrasts)    # Set contrast coding to initial contrast options

  return(type3_anova)
}

