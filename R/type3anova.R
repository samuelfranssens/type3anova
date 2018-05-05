#' A function for Type III Anova (generates the same output as SPSS does)
#' @param linearmodel A linear model.
#' @keywords type3 anova
#' @export
#' @examples
#' type3anova(lm(y ~ x1 * x2, data=dataset))

type3anova <- function(linearmodel) {

  # get initial contrast options:
  initial.options <- options()
  initial.options.contrasts <- initial.options$contrasts

  # set contrasts for each variable
  variables <- all.vars(linearmodel$call)
  variables2 <- variables[-c(1,length(variables))]
  variables.list <- as.list(variables2)
  for (i in seq(length(variables.list))){
    variables.list[[i]] <- contr.sum
  }
  names(variables.list) <- variables2

  # perform ANOVA
  options(contrasts=c("contr.sum", "contr.poly"))
  dt <- linearmodel$model
  new.linearmodel <- lm(linearmodel$call, data=dt)
  type3_anova <- car::Anova(new.linearmodel, contrasts=variables.list, type = 3)

  # reset contrast options
  options(contrasts=initial.options.contrasts)    # Set contrast coding to initial contrast options

  return(type3_anova)
}

