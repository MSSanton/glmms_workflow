#---------------------------------------------------#
#  A versatile workflow for linear modelling in R
#  Matteo Santon, Fraenzi Korner-Nievergelt, Nico Michiels, Nils Anthes
#  Version date: 27 March 2023
#  -> SUPPORT FUNCTIONS FOR THE TEMPLATE
#---------------------------------------------------#

#-###############################-#
# 1. Install and load packages ----
#-###############################-#

# Modified from: https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
  ## Installed package will be activated. 
  ## Those that are not will be installed from CRAN, and activated.
  
## List of packages required:
packages <- c("dplyr", "tidyr", "glmmTMB", "emmeans", "performance",
              "MASS", "DHARMa", "arm", "ggplot2", "ggdist",
              "lattice", "ggbeeswarm", "gridExtra", "mgcv", 
              "reshape2", "ggmosaic", "cdata", "car", "stringr",
              "reshape", "parameters", "RColorBrewer", "grDevices","geoR")

## Load or install & load all these:
inst <- packages[1:24] %in% installed.packages()
if(length(packages[!inst]) > 0) install.packages(packages[!inst])

# Load packages into session
lapply(packages, require, character.only = TRUE)


#-#########################################-#
# 2. Plot settings: Basic ggplot themes  ----
#-#########################################-#

# This is our main ggplot theme.
# Modify as needed following ggplot2 specifications.
# WARNING: Objects need to retain their names "plot0.1" and "plot0".

# Theme with a legend:
plot0.1 <- theme(axis.title.x = 
                   element_text(
                     margin = margin(t = 10),
                     color = "Black", 
                     size = 15), 
                 axis.title.y = 
                   element_text(
                     margin = margin(r = 20),
                     color = "Black", 
                     size = 15), 
                 axis.text = 
                   element_text(
                     color = "Black",
                     size = 12), 
                 axis.line = 
                   element_line(
                     color = "Black",
                     linewidth = 0.5), 
                 axis.ticks = element_line(color = "Black"),
                 panel.background = element_rect(fill = "grey98"),
                 panel.grid.minor = element_line(color = "White"),
                 plot.title = 
                   element_text(
                     size = 15,
                     face = "bold",
                     hjust = 0.5),
                 plot.subtitle = 
                   element_text(
                     size = 15,
                     hjust = 0.5),
                 strip.text.x = 
                   element_text(
                     size = 15,
                     color = "black", 
                     face = "bold"),
                 strip.background = 
                   element_rect(
                     color = "white",
                     fill = "white", 
                     linewidth = 1, 
                     linetype = "solid"),
                 legend.background = element_blank(), 
                 legend.title = 
                   element_text(
                     colour = "black",
                     size = 15, 
                     face = "bold"), 
                 legend.text = 
                   element_text(
                     colour = "black",
                     size = 12,
                     face = "bold.italic"),
                 legend.key.width = unit(1.5, "lines"), 
                 legend.key = element_rect(fill = NA))

# Same theme without a legend:
plot0 <- plot0.1 + theme(legend.position = "none")


#-###############################-#
# Functions not called by user ----
#-###############################-#

#========================#
# * Expand grid unique ---
#========================#

# From https://stackoverflow.com/questions/17171148/non-redundant-version-of-expand-grid
expand.grid.unique <- function(x, y, include.equals = FALSE) {
  x <- unique(x)
  y <- unique(y)
  equals <- function(i) {
    z <- setdiff(y, x[seq_len(i-include.equals)])
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  do.call(rbind, lapply(seq_along(x), equals))
}

  
#===========================#
# * Inverse z-standardisation ----
#===========================#

back_z_transform <- function(predictor, z_predictor) {
  (z_predictor * sd(predictor)) + mean(predictor)
}


#-##############################-#
# 3. Definition of variables  ----
#-##############################-#

#=========================#
# * 3.5 Missing values ----
#=========================#

remove_NAs <- function(data, variables) {
  
  data <- as.data.frame(data)
  
  newdata <- data[complete.cases(data[, na.omit(variables)]),] 
  N <- nrow(data) - nrow(newdata)
  message(paste("You have deleted", N, "row(s) from the dataset. Please check that such observations occur at random."))
  return(newdata)
} 


#-###############################-#
# 4. Raw data exploration ----
#-###############################-#

#=========================#
# * 4.1 Extreme values ----
#=========================#

#---------------------------------#
# ** 4.1.1 NUMERICAL variables ----
#---------------------------------#

dotplot_num <- function(data, variables) {
  
  data <- as.data.frame(data)
  
  variables <- na.omit(variables)
  
  if(length(variables) == 0) stop("ERROR: No variable has been specified. Skip this step if no NUMERIC variable is available.")
  
  if(sum(sapply(data[variables], FUN = is.numeric)) != length(variables)) {
    stop("Incorrect assignment of variables. Specify NUMERIC variables only. Skip this step if none is available.")
    }
    
  dat_dotplot <- reshape2::melt(data, measure.vars = variables)
  limits <- dat_dotplot %>% group_by(variable) %>% 
    summarise(min = round(min(value), digits = 3),
              max = round(max(value), digits = 3), 
              empty = NA)
  
    ggplot(dat_dotplot) + 
    labs(x = "Value of variable", 
         y = "Observation order in dataframe", 
         title = "") +
    scale_y_continuous() + 
    scale_x_continuous() +
    geom_point(data = dat_dotplot, 
               aes(x = value, 
                   y = rep(seq(1, nrow(data), 1), length(variables))),
               col = "black") + 
    geom_point(data = limits , 
               aes(x = max, y = as.numeric(empty)), 
               na.rm = TRUE) +
    facet_wrap(~ variable, 
               scales = "free", 
               nrow = ceiling(log(length(variables)) + 0.1)) +
    plot0
}


#------------------------------#  
# ** 4.1.2 FACTOR variables ----
#------------------------------#

barplot_fac <- function(data, variables) { 
  
  data <- as.data.frame(data)
  
  variables <- na.omit(variables)
  
  if(length(variables) == 0) stop("ERROR: No FACTOR variable has been specified. Skip this step if none is available.")
  
  if(sum(sapply(data[variables], FUN = is.factor)) + sum(sapply(data[variables], FUN = is.character)) != length(variables)) {
    stop("ERROR: Incorrect assignment of variables. Specify FACTOR variables only. Skip this step if none is available.")
    } else {

    dat_plot <- suppressWarnings(reshape2::melt(data, measure.vars = c(variables)))
  
  suppressWarnings(print(ggplot(dat_plot) + 
    labs(x = "Factor levels", 
         y = "Number of observations", 
         title = "") +
    scale_y_continuous() + 
    scale_x_discrete() +
    geom_histogram(data = dat_plot, 
                   aes(x = value), 
                   stat = "count",
                   col = "black") + 
    facet_wrap(~ variable, scales = "free", 
               nrow = ceiling(log(length(variables)) + 0.1))  +  
    #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # OPTION for better readability of labels.
    coord_flip() +  
    plot0))
  }
}


#================================#
#* 4.2 Predictor collinearity ----
#================================#

#----------------------------------#
#** 4.2.1: Graphical inspection ----
#----------------------------------#

# NUMERIC predictors: Scatter plots
coll_num <- function(data, predictors) { 
  
  data <- as.data.frame(data)
  
  predictors <- na.omit(predictors)
  
  if(length(predictors) < 2) stop("HINT: Less than two numeric predictors have been specified. Skip this step.")
  
  if(sum(sapply(data[predictors], FUN = is.numeric)) != length(predictors) ) {
    stop("ERROR: Incorrect assignment of predictors. Specify NUMERIC predictors only. Skip this step if less than 2 are available.")
    } 
  else {
  
  num <- colnames(dplyr::select_if(data[predictors], is.numeric))
  
  dat_plot <- data.frame(expand.grid.unique(num, num))
  colnames(dat_plot) <- c("y","x")
  dat_plot <- cbind(data.frame(pair_key = paste(dat_plot[[1]], dat_plot[[2]],  sep = " against "), stringsAsFactors = F), dat_plot)
  
  dat_plot <- cdata::rowrecs_to_blocks(data, dat_plot)
  
 ggplot(dat_plot, aes(x = x, y = y)) + 
    geom_point() +
    facet_wrap(~ pair_key, 
               scales = "free") + 
    labs(y = "Value", 
         x = "Value", 
         title = "") +
    plot0
  }
  }


# FACTOR against NUMERIC predictors: Boxplots
coll_num_fac <- function(data, predictors) { 
  
  data <- as.data.frame(data)
  
  predictors <- na.omit(predictors)
  
  if(length(predictors) <  2) stop("HINT: Less than two predictors have been specified. Skip this step.")
  
  if(sum(sapply(data[predictors], FUN = is.numeric)) < 1)
     {
    stop("ERROR: No NUMERIC predictor has been specified. If correct, skip this step, otherwise specify at least 1 numeric predictor.")
    } else {
    if(sum(sapply(data[predictors], FUN = is.factor)) < 1) 
      {
      stop("ERROR: No FACTOR predictor has been specified. If correct, skip this step, otherwise specify at least 1 factor predictor.")
      } else {
        
  num <- colnames(dplyr::select_if(data[predictors], is.numeric))
  fac <- colnames(dplyr::select_if(data[predictors], is.factor))
  
  dat_plot <- data.frame(expand.grid.unique(num, fac))
  colnames(dat_plot) <- c("value", "levels")
  dat_plot <- cbind(data.frame(pair_key = paste(dat_plot[[1]], dat_plot[[2]],  sep = " against "), stringsAsFactors = F), dat_plot)
  
  dat_plot <- cdata::rowrecs_to_blocks(data, dat_plot) 
  
plot <-  lapply(levels(as.factor(dat_plot$pair_key)), 
                function(pair_key_value)  {
                  ggplot(subset(dat_plot, pair_key == pair_key_value), 
                         aes(x = levels, y = value)) +
                    geom_boxplot(outlier.shape = NA) +  
                    geom_quasirandom(method = "smiley") +
                    labs(y = sub('\\against.*', '', pair_key_value), 
                         x = sub('.*against ', '', pair_key_value), 
                         title = pair_key_value) +
                    plot0
                  })

do.call("grid.arrange", c(plot, 
                          nrow = ceiling(log(length(plot) + 0.1))))
      }
    }
  }


# FACTOR predictors: Mosaic plots
coll_fac <- function(data, predictors) { 
  
  data <- as.data.frame(data)
  
  predictors <- na.omit(predictors)
  
  if(length(predictors) < 2) stop("HINT: Less than two FACTOR predictors have been specified. Skip this step.")
  
  if(sum(sapply(data[predictors], FUN = is.factor)) != length(predictors)) {
    stop("ERROR: Incorrect assignment of predictors. Specify FACTOR predictors only. Skip this step if < 2 Factor predictors are available.")
    } else {
  
  fac <- colnames(dplyr::select_if(data[predictors], is.factor))
  
  dat_plot <- data.frame(expand.grid.unique(fac, fac))
  colnames(dat_plot) <- c("y", "x")
  dat_plot <- cbind(data.frame(pair_key = paste(dat_plot[[1]], dat_plot[[2]], sep = " against "), stringsAsFactors = F), dat_plot)
  
  dat_plot <- cdata::rowrecs_to_blocks(data, dat_plot) 
  
  plot <- lapply(levels(as.factor(dat_plot$pair_key)), function(pair_key_value)  {
    ggplot(subset(dat_plot, pair_key == pair_key_value)) +
      geom_mosaic(
        aes(x = product(y,x), fill = y)) +
      labs(y = sub('\\against.*', '', pair_key_value), 
           x = sub('.*against ', '', pair_key_value), 
           title = pair_key_value) +
      plot0})
  
  do.call("grid.arrange", c(plot,
                            nrow = ceiling(log(length(plot) + 0.1))))
    }
  }


#-----------------------------------------------#
#** 4.2.2: Variance Inflation Factors (VIFs) ----
#-----------------------------------------------#
# core function from Zuur, Ieno & Elphick (2010).

corvif <- function(data, variables) {
  
  variables <- na.omit(variables)
  
  if(length(variables) < 2) stop("HINT: Variance inflation factors can only be calculated with at least 2 predictor variables. You have currently specified one predictor, only.")
  
  dataz <- as.data.frame(data[variables])
  
  #vif part
  form    <- formula(paste("fooy ~ ", paste(strsplit(names(dataz)," "),  collapse = " + ")))
  dataz   <- data.frame(fooy = 1 + rnorm(nrow(dataz)), dataz)
  lm_mod  <- lm(form, dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}


# Support function for VIF from Zuur, Ieno & Elphick 2010
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: VIFs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor) <- 0
    if (any(tmp_cor == 1.0)){
      return("Sample size is too small, 100% collinearity is present.")
    } else {
      return("Sample size is too small.")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "data", "GVIF^(1/2data)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF = result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}


#==========================================#
#* 4.3 Predictor-response relationships ----
#==========================================#

relat_single <- function(data, response, predictors, y.limits, y.breaks) { 
  
  data <- as.data.frame(data)
  
  predictors <- na.omit(predictors)
  
  if(length(predictors) == 0) stop("ERROR: No predictor has been specified. Skip this step if none is available.")
  
  num <- colnames(dplyr::select_if(data[predictors], is.numeric))
  fac <- colnames(dplyr::select_if(data[predictors], is.factor))
  
  if(length(num) > 1) {suppressWarnings(reshape2::melt(data, measure.vars = c(num), value.name = "value", variable.name = "num"))} -> dat_plot
  
  if(missing(y.limits)) {
    y.limits <- c(min(data[,response]),max(data[,response]))} 
  
  if(missing(y.breaks)) {
    y.breaks <- waiver()} 
  
  if(length(num) > 1) {plot1 <- lapply(levels(as.factor(dat_plot$num)), function(num_value) { 
    ggplot(subset(dat_plot, num == num_value), aes(x = value, y = !!sym(response))) + 
      scale_y_continuous(limits = y.limits, breaks = y.breaks) + # this sets y axis 
     # geom_smooth(method = "loess", span = 3,se = F) + 
      geom_jitter(width = 0.0, height = 0, alpha = 0.6) +
      labs(y = response, x = num_value, title = "") +
      plot0})} else {
        
        if(length(num) == 1) {
          plot1 <- list(ggplot(data, aes(x = !!sym(num), y = !!sym(response))) + 
                          scale_y_continuous(limits = y.limits, breaks = y.breaks) + # this sets y axis 
                       #   geom_smooth(method = "loess", span = 3,se = F) + 
                          geom_jitter(width = 0.0, height = 0, alpha = 0.6) +
                          labs(y = response, x = num, title = "") +
                          plot0)} else {plot1 <- NULL}}
  
  if(length(fac) > 1) {suppressWarnings(reshape2::melt(data, measure.vars = c(fac), value.name = "levels", variable.name = "fac"))} -> dat_plot
  
  if(missing(y.limits)) {
    y.limits <- c(min(data[,response]),max(data[,response]))} 
  
  if(missing(y.breaks)) {
    y.breaks <- waiver()} 
  
  if(length(fac) > 1) {plot2 <- lapply(levels(as.factor(dat_plot$fac)), function(fac_value) { 
    ggplot(subset(dat_plot, fac == fac_value), aes(x = levels, y = !!sym(response))) + 
      scale_y_continuous(limits = y.limits, breaks = y.breaks) + # this sets y axis  
      geom_boxplot(outlier.shape = NA) + 
      geom_jitter(width = 0.1, height = 0, alpha = 0.6) +
      labs(y = response, x = fac_value, title = "") +
      plot0})} else {
        
        if(length(fac) == 1) {
          plot2 <- list(ggplot(data, aes(x = !!sym(fac), y = !!sym(response))) + 
                          scale_y_continuous(limits = y.limits, breaks = y.breaks) + # this sets y axis 
                          geom_boxplot(outlier.shape = NA) + 
                          geom_jitter(width = 0.1, height = 0, alpha = 0.6) +
                          labs(y = response, x = fac, title = "") +
                          plot0)} else {plot2 <- NULL}}
  
  plot <- c(plot1,plot2)
  
  plot <- plot[!unlist(lapply(plot,is.null))]
  
  if(length(plot) > 9) {
    mp <- marrangeGrob(grobs = plot, ncol = 3, nrow = 3)
    ggsave("Predictor_response_relationships.pdf", mp, height = 20, width = 30, units = "cm")} else {
      
      suppressWarnings(grid.arrange(grobs = plot, ncol = round(sqrt(length(plot))), nrow = ceiling(length(plot)/round(sqrt(length(plot))))))
    }
}


#===============================#
#* 4.4 Response distribution ----
#===============================#

distr_response <- function (data, response) {
  
  data <- as.data.frame(data)
  
  ggplot(data, aes(x = !!sym(response))) + 
  geom_histogram(col = "white") +
  labs(y = "Number of observations", x = paste("Response values:", response), title = "") +
  plot0
  }

     
#-#######################-#
# 5. Model formulation ----
#-#######################-#

#===============================================#
#* 5.1 Standardisation of numeric predictors ----
#===============================================#

z_transform <- function(data, predictors) {
  
  data <- as.data.frame(data)
  
  predictors <- na.omit(predictors)
  
  if(length(predictors) == 0) stop("ERROR: No NUMERIC predictor has been specified. Skip this step if none is available.")
  
  if(sum(sapply(data[predictors], FUN = is.numeric)) != length(predictors)) 
    { stop("ERROR: You either have no NUMERIC predictors (skip this step), or some specified predictors are not numeric (specify numeric predictors only).")} else {
  
  num <- colnames(dplyr::select_if(data[predictors], is.numeric))
  
  temp_list <- lapply(as.list(data[num]), scale)
  names(temp_list) <- paste(num,"_z", sep = "") 
  return(temp_list)
    }
  }

#-######################-#
# 6. Model assessment ----  
#-######################-#

#========================#
#* 6.1 Residual checks ----
#========================#

#----------------------------------------#
#** 6.1.1: Distribution of residuals -----
#----------------------------------------#

residual_plots <- function(data, modelTMB, response) {
  
  data <- as.data.frame(data)
  
  residuals <- data[,FALSE]
  
  residuals$mod.res <- simulateResiduals(fittedModel = modelTMB)$scaledResiduals 
  residuals$mod.fit <- simulateResiduals(fittedModel = modelTMB)$fittedPredictedResponse 
  residuals$mod.fit.rank <- rank(residuals$mod.fit, ties.method = "average") 
  residuals$mod.fit.rank <- residuals$mod.fit.rank / max(residuals$mod.fit.rank)
  
  formula <- paste(gsub(".~","", formula(modelTMB, fixed.only = T))[3], collapse = "")
  
  # Remove offset from model formulation if present:
  if(length(modelTMB$call$offset) > 0) { 
    offset <- paste("+ offset(", paste(as.character(modelTMB$call$offset), collapse = "("), ")", sep = "")
    formula <- paste(gsub(offset, "", formula, fixed = TRUE), collapse = "")
    formula <- paste(gsub(")","",formula, fixed = TRUE), collapse = "")
  }
  
  temp <- unlist(str_split(formula, "\\+"))
  temp <- unlist(str_split(temp, "\\*"))         # solves interaction if expressed with "\\*"
  temp <- str_replace_all(temp, fixed(" "), "")  # remove ""
  
  remove_me <- temp[str_detect(temp, pattern = (":"))] # extract interactions 
  
  if(length(remove_me) > 0) {
    fixed.pred <- temp[-match(remove_me, temp)]
  } else {
    fixed.pred <- temp
  }
  
  poly_form <- fixed.pred[str_detect(fixed.pred, pattern = "poly")] # find poly formula if present.
  
  if(length(poly_form) > 0) {
    poly_col <- gsub(" ", "", gsub("poly\\(|\\)|x\\=|[[:digit:]])||degree = [[:digit:]],|,|degree=[[:digit:]],|, |[[:digit:]],|, 
                   raw=TRUE|raw=T|raw=FALSE|raw=F", "", poly_form))
    
    for (i in 1:length(poly_form)) {
      fixed.pred <- str_replace_all(fixed.pred, fixed(poly_form[i]), poly_col[i])
    }
  } # change name of the formula to real column in list of fixed predictors
  
  # QQ-plot of model residuals 
  plot1 <- list(ggplot(residuals, aes(sample = mod.res)) +
                  scale_y_continuous(limits = c(0,1)) +
                  labs(x = "Standardised expected quantiles", 
                       y = "Standardised observed quantiles", 
                       title = "QQ-plot of standardised residuals") +
                  geom_qq(distribution = stats::qunif) +
                  geom_abline() +
                  theme(aspect.ratio = 1) +
                  plot0
  )
  
  # Residual against fitted value plot - should show no pattern:
  plot2 <- list(ggplot(data = residuals, aes(x = mod.fit.rank, y = mod.res)) +
                  {if (length(fixed.pred) == 1 && is.factor(data[,fixed.pred]) && length(fixef(modelTMB)$zi) == 0) 
                    geom_boxplot(outlier.shape = NA, 
                                 data = residuals, 
                                 aes(x = mod.fit.rank, 
                                     y = mod.res, 
                                     group = as.factor(mod.fit.rank)), 
                                 col = "black", alpha = 1)} +
                  {if (is.factor(data[,fixed.pred]) == F) 
                    geom_smooth(data = residuals, 
                                aes(x = mod.fit.rank, y = mod.res), 
                                method = "glm", se = F, col = "blue")} +
                  scale_y_continuous(limits = c(0,1)) +
                  labs(x = "Model predictions (ranked)", 
                       y = "Standardised residuals", 
                       title = "Residuals against fitted values") +
                  geom_hline(yintercept = c(0.25, 0.50, 0.75), lty = 2, col = "grey60") +
                  geom_quasirandom(method = "smiley", alpha = 0.6) +
                  theme(aspect.ratio = 1) +
                  plot0
  )

  
  # QQ-plot of model random intercepts 
  if(length(mod$sdr$diag.cov.random) != 0) { 
  rand <- ranef(modelTMB)$cond
  
  if (length(ranef(modelTMB)$cond) > 0) {
    plot3 <- lapply(1:length(ranef(modelTMB)$cond), function(ran) { 
      ggplot(rand[[ran]], aes(sample = rand[[ran]][,1])) +
        #scale_y_continuous(limits = c(0,1)) +
        labs(x = "Theoretical quantiles", 
             y = paste("Random intercept deviations for", names(ranef(modelTMB)$cond)[ran]), 
             title = paste(paste("QQ-plot for random intercept", names(ranef(modelTMB)$cond)[ran]))) +
        geom_qq(distribution = stats::qnorm) +
        stat_qq_line() +
        theme(aspect.ratio = 1) +
        plot0
    }
    )
  }} else {plot3 <- NULL
  }
  
  plot <- c(plot1, plot2, plot3)
  
  plot <- plot[!unlist(lapply(plot,is.null))]
  
  do.call("grid.arrange", c(plot, nrow = ceiling(log(length(plot) + 0.1))))
  
}


#---------------------------------------------------#
#** 6.1.2: Residuals against possible PREDICTORS ----
#---------------------------------------------------#

residual_plots_predictors <- function(data, modelTMB, predictors) {
  
  data <- as.data.frame(data)

if(length(na.omit(predictors)) < 1) {
  stop("ERROR: Please enter at least one predictor.")
  } else {
  
  predictors <- na.omit(predictors)  
  num <- colnames(dplyr::select_if(data[predictors], is.numeric))
  fac <- colnames(dplyr::select_if(data[predictors], is.factor))
  data$mod.res <- simulateResiduals(fittedModel = modelTMB)$scaledResiduals # from DHARMa
  
if(length(num) > 1) {
  suppressWarnings(reshape2::melt(data, measure.vars = c(num), value.name = "value", variable.name = "num"))} -> dat_plot

if(length(num) > 1) {
  plot1 <- lapply(levels(as.factor(dat_plot$num)), function(num_value) {
    ggplot(subset(dat_plot, num == num_value), aes(x = value, y = mod.res)) + 
      scale_y_continuous(limits = c(0,1)) +
      geom_point() +
      geom_smooth(method = "gam", se = F) +
      geom_hline(yintercept = c(0.25, 0.50, 0.75), lty = 2, col = "grey60") +
      labs(y = "Standardised residuals", 
           x = num_value, 
           title = "") +
      theme(aspect.ratio = 1) +
      plot0
    })
  } else {
  
  if(length(num) == 1) {
    plot1 <- list(ggplot(data, aes(x = !!sym(num), y = mod.res)) + 
                    scale_y_continuous(limits = c(0,1)) +
                    geom_point() +
                    geom_smooth(method = "glm", se = F) +
                    geom_hline(yintercept = c(0.25, 0.50, 0.75), lty = 2, col = "grey60") +
                    labs(y = "Standardised residuals", 
                         x = num, 
                         title = "") +
                    theme(aspect.ratio = 1) +
                    plot0
                  )
    }  else {plot1 <- NULL}}

if(length(fac) > 1) {
  suppressWarnings(reshape2::melt(data, measure.vars = c(fac), value.name = "levels", variable.name = "fac"))} -> dat_plot
  
if(length(fac) > 1)  {
  plot2 <- lapply(levels(as.factor(dat_plot$fac)), function(fac_value) {
    ggplot(subset(dat_plot, fac == fac_value), aes(x = levels, y = mod.res)) + 
      scale_y_continuous(limits = c(0,1)) +
      geom_boxplot() + 
      geom_quasirandom(method = "smiley", alpha = 0.6) +
      labs(y = "Standardised residuals", x = fac_value, title = "") +
      theme(aspect.ratio = 1) +
      plot0
    }
    )
  } else {

  if(length(fac) == 1) {
    plot2 <- list(ggplot(data, aes(x = !!sym(fac), y = mod.res)) + 
                    geom_boxplot() + 
                    geom_quasirandom(method = "smiley", alpha = 0.6) +
                    scale_y_continuous(limits = c(0,1)) +
                    labs(y = "Standardised residuals", x = fac, title = "") +
                    theme(aspect.ratio = 1) +
                    plot0)
    }  else{plot2 <- NULL}}
  
plot <- c(plot1, plot2)

plot <- plot[!unlist(lapply(plot,is.null))]

do.call("grid.arrange", c(plot, nrow = ceiling(log(length(plot) + 0.1))))
  }
  }


#------------------------------------------------------------------------------#  
#** 6.1.3: Residuals against possible TWO-WAY INTERACTIONS AMONG PREDICTORS ----
#------------------------------------------------------------------------------#   

residual_plots_interactions <- function(data, modelTMB, predictors) { 
  
  data <- as.data.frame(data)
  
  if(length(na.omit(predictors)) < 2) {stop("ERROR: At least two predictors are required")} else {
    
    predictors <- na.omit(predictors)
    
    num <- colnames(dplyr::select_if(data[predictors], is.numeric))
    fac <- colnames(dplyr::select_if(data[predictors], is.factor))
    
    data$mod.res <- simulateResiduals(fittedModel = modelTMB)$scaledResiduals # from DHARMa
    
    if(length(num) > 1) {
      dat_plot <- data.frame(expand.grid.unique(num, num))
      colnames(dat_plot) <- c("y", "x")
      dat_plot <- cbind(data.frame(pair_key = paste(dat_plot[[1]], dat_plot[[2]], sep = " : "), stringsAsFactors = F), dat_plot)
      
      dat_plot <- cdata::rowrecs_to_blocks(data, dat_plot, controlTableKeys = "pair_key", columnsToCopy = "mod.res") 
      dat_plot$pair_key <- as.factor(dat_plot$pair_key)

      plot1 <- lapply(levels(dat_plot$pair_key), function(pair_key_value)  {
        
        sub <- subset(dat_plot, pair_key == pair_key_value)
        
        sub <- as.data.frame(sub %>% mutate(x = case_when(x <= quantile(x, probs = 0.33) ~ 1, between(x, quantile(x, probs = 0.33),quantile(x, probs = 0.66)) ~ 2,
                                                          x >= quantile(x, probs = 0.66) ~ 3)))
        sub$x <- as.factor(sub$x)
        levels(sub$x) <- c("33% quantile","50% quantile","66% quantile")
        
        ggplot(data = sub, aes(x = y, y = mod.res, color = x, group = x)) + 
          scale_y_continuous(limits = c(0,1)) +
          geom_smooth(method = "loess", span = 3, se = F) + 
          geom_point(data = sub, aes(x = y, y = mod.res, color = x, group = x), position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
          labs(y = "Standardised residuals", x = sub('\\:.*', '', pair_key_value), title = pair_key_value) +
          guides(color = guide_legend(title = sub('.*: ', '', pair_key_value))) +
          theme(legend.justification = c(0.02, 0.98), legend.position = c(0.02, 0.98), legend.key.size = unit(0.2, 'cm')) + 
          plot0.1
      })
    } else {plot1 <- NULL}
    
    if(length(num) != 0 && length(fac) != 0) {dat_plot <- data.frame(expand.grid.unique(num, fac))
    colnames(dat_plot) <- c("y","x")
    dat_plot <- cbind(data.frame(pair_key = paste(dat_plot[[1]], dat_plot[[2]], sep = " : "), stringsAsFactors = F), dat_plot)
    
    dat_plot <- cdata::rowrecs_to_blocks(data, dat_plot, controlTableKeys = "pair_key", columnsToCopy = "mod.res")
    dat_plot$pair_key <- as.factor(dat_plot$pair_key)
    
    plot2 <- lapply(levels(dat_plot$pair_key), function(pair_key_value)  {
      ggplot(subset(dat_plot, pair_key == pair_key_value), aes(x = y, y = mod.res, color = x)) + 
        scale_y_continuous(limits = c(0,1)) +
        geom_smooth(method = "loess", span = 3, se = F) + 
        geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
        labs(y = "Standardised residuals", x = sub('\\:.*', '', pair_key_value), title = pair_key_value) +
        guides(color = guide_legend(title = sub('.*: ', '', pair_key_value))) +
        theme(legend.justification = c(0.02, 0.98), legend.position = c(0.02, 0.98), legend.key.size = unit(0.2, 'cm')) + 
        plot0.1})} else {plot2 <- NULL}
    
    if(length(fac) > 1) {dat_plot <- data.frame(expand.grid.unique(fac, fac))
    colnames(dat_plot) <- c("y","x")
    dat_plot_pairs <- cbind(data.frame(pair_key = paste(dat_plot[[1]], dat_plot[[2]], sep = " : "), stringsAsFactors = F), dat_plot)
    
    dat_plot <- cdata::rowrecs_to_blocks(data, dat_plot_pairs, controlTableKeys = "pair_key", columnsToCopy = "mod.res") 
    dat_plot$pair_key <- as.factor(dat_plot$pair_key)
    
    plot3 <- lapply(levels(dat_plot$pair_key), function(pair_key_value)  {
      dat_plot <- subset(dat_plot, pair_key ==  pair_key_value)
      dat_plot$y <- factor(dat_plot$y, levels = levels(data[,subset(dat_plot_pairs, pair_key == pair_key_value)[,"y"]]))
      dat_plot$x <- factor(dat_plot$x, levels = levels(data[,subset(dat_plot_pairs, pair_key == pair_key_value)[,"x"]]))
      ggplot(dat_plot, aes(x = y, y = mod.res, color = x)) + 
        scale_y_continuous(limits = c(0,1)) +
        geom_boxplot(outlier.shape = NA, position = position_dodge(0.8)) + 
        geom_quasirandom(method = "smiley", alpha = 0.6, dodge.width = 0.8) +
        labs(y = "Standardised residuals", x = sub('\\:.*', '', pair_key_value), title = pair_key_value) +
        guides(color = guide_legend(title = sub('.*: ', '', pair_key_value))) +
        theme(legend.justification = c(0.02, 0.98), legend.position = c(0.02, 0.98), legend.key.size = unit(0.2, 'cm')) + 
        plot0.1})} else {plot3 <- NULL}
    
    plot <- c(plot1, plot2, plot3)
    
    plot <- plot[!unlist(lapply(plot,is.null))]
    
    if(length(plot) > 9) {
      mp <-  marrangeGrob(grobs = plot, 
                          ncol = 2, 
                          nrow = 2)
      ggsave("Two-way_interactions.pdf", mp, width = 12, height = 12, units = "in")} else {
        
        grid.arrange(grobs = plot, 
                     ncol = round(sqrt(length(plot))), 
                     nrow = ceiling(length(plot)/round(sqrt(length(plot)))))
      }
  }
}


#------------------------------------------------------------------------#
#** 6.1.4: Residuals against PREDICTORS split by RANDOM FACTOR LEVELS ----
#------------------------------------------------------------------------#

residual_plots_random_slopes <- function(data, modelTMB, fixed_eff, random_eff) {
  
  data <- as.data.frame(data)
  
  if(length(ranef(modelTMB)$cond) < 1) 
  {stop("ERROR: Your model does not contain random factors, skip this step.")} else {
    
  if(length(na.omit(fixed_eff)) < 1 || length(na.omit(random_eff)) < 1) 
    {stop("ERROR: Please enter at least one predictor and random factor.")} else {
    
  mod.res <- simulateResiduals(fittedModel = modelTMB)$scaledResiduals # from DHARMa
  
  fixed_eff <- na.omit(fixed_eff)
  
  num <- colnames(dplyr::select_if(data[fixed_eff], is.numeric))
  fac <- colnames(dplyr::select_if(data[fixed_eff], is.factor))
  
  lapply(random_eff, function(random_eff_value){
    
    if(length(num) > 0) {plot1 <- lapply(num, function(x_value){
      ggplot(data, aes(x = !!as.name(x_value), y = mod.res)) + 
        scale_y_continuous(limits = c(0,1)) +
        geom_point() +
        geom_smooth(se = F, method = "glm") + # or loess?
        facet_wrap(~ data[,random_eff_value]) + 
        labs(y = "Standardised residuals", 
             x = as.name(x_value), 
             title = paste("Std residuals as a function of", x_value, "split by", random_eff_value, collapse = " ")) +
        theme(panel.spacing.x = unit(1, "lines")) +
        plot0
    })} else {plot1 <- NULL}
    
    if(length(fac) > 0) {plot2 <- lapply(fac, function(x_value){
      ggplot(data, aes(x = !!as.name(x_value), y = mod.res)) + 
        scale_y_continuous(limits = c(0,1)) +
        geom_boxplot(outlier.shape = NA) +
        geom_quasirandom(method = "smiley", alpha = 0.6, col = "grey40", cex = 0.7) +
        facet_wrap(~ data[,random_eff_value]) + 
        labs(y = "Standardised residuals", 
             x = as.name(x_value), 
             title = paste("Std residuals as a function of", x_value, "split by", random_eff_value, collapse = " ")) +
        theme(panel.spacing.x = unit(1, "lines")) +
        plot0
    })} else {plot2 <- NULL}
    
    
    plot <- c(plot1, plot2)
    
    plot <- plot[!unlist(lapply(plot,is.null))]
    
    devAskNewPage(TRUE)
    capture.output(plot)
    devAskNewPage(FALSE)})
    }
  }
  }


#=====================================#
#* 6.2 Posterior predictive checks ----
#=====================================# 

#-----------------------#
#** 6.2.1 Dispersion ----
#-----------------------#

dispersion_simulation <- function(data, modelTMB, response, predictor, n.sim) {
  
  data <- as.data.frame(data)
  
  sims <- simulate(modelTMB, nsim = n.sim, seed = 9) 
  
  if(!missing(predictor) && !is.null(predictor) && length(na.omit(predictor)) > 0) { 
    
    if(length(na.omit(predictor)) > 1) {warning(call. = F, "Multiple predictors detected: the first has been taken by default. Explicitly specify others if preferred.")}
    
    if(length(predictor) > 1) {
      predictor <- na.omit(predictor)[1]
    }
    
    if(sum(sapply(data[predictor], FUN = is.numeric)) == length(predictor))  {stop("ERROR: The predictor must be a factor.")} else {
      
      if (modelTMB$modelInfo$family[1] == "binomial" | modelTMB$modelInfo$family[1] == "betabinomial") {
        list_sims <- lapply(sims, function(x) {
          cbind("estimate" = x[,1]/(x[,1] + x[,2]), data %>% dplyr::select(!!sym(predictor)))
        }
        )}
      else {
        list_sims <- lapply(sims, function(x) { # lapply applies function to each vector and returns it as a list.
          cbind("estimate" = x, data %>% dplyr::select(!!sym(predictor)))
        }
        )}
      
      sims_summary <- lapply(list_sims, function(x) {
        as.data.frame(x %>% group_by(!!sym(predictor)) %>% 
                        summarize(variance = round(var(estimate), digits = 5)))
      }
      )
      all_sims <- do.call(rbind, sims_summary)
      temp <- data %>% group_by(!!sym(predictor)) %>% 
        summarize(variance = var(!!sym(response)))
      
      # The following checks the quantile of observed parameters against simulated ones.
      # Depending on quantiles, observed vlines are coloured blue, orange, or red.
      
      grouped_sims <- cbind(all_sims, temp[2])
      names(grouped_sims) <- c(predictor, "variance", "obs_variance")
      
      grouped_sims <- grouped_sims %>% group_by(!!sym(predictor))
      
    }}  else { # not grouped by a predictor
      
      if (modelTMB$modelInfo$family[1] == "binomial" | modelTMB$modelInfo$family[1] == "betabinomial") {
        list_sims <- lapply(sims, function(x) {
          "estimate" = x[,1]/(x[,1] + x[,2])
        }
        )}
      else {
        list_sims <- lapply(sims, function(x) {
          "estimate" = x
        }
        )}
      
      sims_summary <- lapply(list_sims, function(x) {
        as.data.frame(list(variance = round(var(x), digits = 5)))
      })
      all_sims <- do.call(rbind, sims_summary) # do.call constructs and executes a function call from a name or 
      temp <- data %>% summarize(variance = var(!!sym(response)))
      
      # The following checks the quantile of observed parameters against simulated ones.
      # Depending on quantiles, observed vlines are coloured blue, orange, or red.
      
      grouped_sims <- cbind(all_sims, temp)
      names(grouped_sims) <- c("variance","obs_variance")
    }
  
  prop <- as.data.frame(grouped_sims %>% summarize(extreme_variance = sum(variance > obs_variance)/n.sim, equal_variance = sum(variance == obs_variance)/n.sim))
  
  col.vline_variance <- ifelse(between(prop$extreme_variance, 0.05, 0.95) | prop$equal_variance > 0.1, "blue",  
                               ifelse(between(prop$extreme_variance, 0.005, 0.995) | prop$equal_variance > 0.01, "orange", "red"))
  
  plot1 <- ggplot() + 
    labs(x = "Variance: simulated (bars) and observed (line)", y = "Frequency", title = "") +
    geom_histogram(data = all_sims, aes(x = variance), col = "white", fill = "grey70", bins = 30) + 
    geom_vline(data = temp, aes(xintercept = variance), col = col.vline_variance, linewidth = 1.5) +
    plot0 +
    if(!missing(predictor) && !is.null(predictor)  && length(na.omit(predictor)) > 0) {facet_wrap(as.formula(paste("~", predictor)), scales = "free")}
  
  if(sum(col.vline_variance == "blue")  == length(col.vline_variance)) {
    message("OPTIMAL FIT: All observed variances fall within central 90% of simulated data. No issues.")} else {
      if(sum(col.vline_variance == "orange") == length(col.vline_variance)) {
        warning(call. = F, "SUB-OPTIMAL FIT: All observed variances (orange) fall marginal within central 99% of simulated data. Check if better model fit can be achieved.")} else {
          if(sum(col.vline_variance == "red") == length(col.vline_variance)) {
            warning(call. = F, "POOR FIT: All observed variances (red) are outside the central 99% of simulated data. Your model likely provides insufficient fit to your data.")} else {
      if(sum(col.vline_variance == "blue") + sum(col.vline_variance == "orange") == length(col.vline_variance)) {
          warning(call. = F, "SUB-OPTIMAL FIT: Some observed variances (orange) fall marginal within central 99% of simulated data. Check if better model fit can be achieved.")} else {
      if(sum(col.vline_variance == "blue") + sum(col.vline_variance == "red") == length(col.vline_variance)) {
        warning(call. = F, "POOR FIT: Some observed variances (red) are outside the central 99% of simulated data. Your model likely provides insufficient fit to your data.")} else {
          if(sum(col.vline_variance == "orange") + sum(col.vline_variance == "red") == length(col.vline_variance)) {
            warning(call. = F, "SUB-OPTIMAL FIT: Some observed variances (orange) fall marginal within central 99% of simulated data. Check if better model fit can be achieved.")
            warning(call. = F, "POOR FIT: Some observed variances (red) are outside the central 99% of simulated data. Your model likely provides insufficient fit to your data.")} else {
              if(sum(col.vline_variance == "blue") + sum(col.vline_variance == "orange") + sum(col.vline_variance == "red") == length(col.vline_variance)) {   
                warning(call. = F, "SUB-OPTIMAL FIT: Some observed variances (orange) fall marginal within central 99% of simulated data. Check if better model fit can be achieved.")
                warning(call. = F, "POOR FIT: Some observed variances (red) are outside the central 99% of simulated data. Your model likely provides insufficient fit to your data.")
                }}}}}}}
    plot1
  }


#---------------------------#
#** 6.2.2 Zero inflation ---- 
#---------------------------#

zero_simulation <- function(data, modelTMB, response, predictor, n.sim) {
  
  data <- as.data.frame(data)
  
  sims <- simulate(modelTMB, nsim = n.sim, seed = 9) # simulate data from the model
  
  if(!missing(predictor) && !is.null(predictor) && length(na.omit(predictor)) > 0) { 
    
    if(length(na.omit(predictor)) > 1) {warning(call. = F, "Multiple predictors detected: the first has been taken by default. Explicitly specify others if preferred.")}
    
    if(length(predictor) > 1) {
      predictor <- na.omit(predictor)[1]
    }
    
    if(sum(sapply(data[predictor], FUN = is.numeric)) == length(predictor)) {stop("ERROR: The predictor must be a factor")} else {
      
      if (modelTMB$modelInfo$family[1] == "binomial" | modelTMB$modelInfo$family[1] == "betabinomial") { # ... for binomial distribution
        list_sims <- lapply(sims, function(x) { 
          cbind("estimate" = x[,1]/(x[,1] + x[,2]), data %>% dplyr::select(!!sym(predictor)))
        })
      } else { # ... and for non-binomial distribution
        list_sims <- lapply(sims, function(x) { 
          cbind("estimate" = x, data %>% dplyr::select(!!sym(predictor)))
        })
      }
      sims_summary <- lapply(list_sims, function(x) {
        as.data.frame(x %>% group_by(!!sym(predictor)) %>% summarize(zeros = sum(estimate == 0)))
      })
      all_sims <- do.call(rbind, sims_summary)
      temp <- data %>% group_by(!!sym(predictor)) %>% summarize(zeros = sum(!!sym(response) == 0))
      
      # The following checks the quantile of observed zero counts against simulated zeros.
      # Depending on quantiles, observed vlines are coloured blue, orange, or red.
      
      grouped_sims <- cbind(all_sims, temp[2])
      names(grouped_sims) <- c(predictor,"zeros","obs_zeros")
      
      prop <- as.data.frame(grouped_sims %>% group_by(!!sym(predictor)) %>% summarize(extreme = sum(zeros > obs_zeros)/n.sim, 
                                                                                      equal = sum(zeros == obs_zeros)/n.sim))
      
      col.vline <- ifelse(between(prop$extreme, 0.05, 0.95) | prop$equal > 0.1, "blue",  
                          ifelse(between(prop$extreme, 0.005, 0.995) | prop$equal > 0.01, "orange", "red"))
      
    }} else { # factor to facet absent ...
      if(modelTMB$modelInfo$family[1] == "binomial" | modelTMB$modelInfo$family[1] == "betabinomial") {
        list_sims <- lapply(sims, function(x) { # ... for binomial distribution
          "estimate" = x[,1] / (x[,1] + x[,2])
        })
      } else {list_sims <- lapply(sims, function(x) { # ... and for non-binomial distribution
        "estimate" = x
      })
      } 
      all_sims <- sapply(list_sims, function(x) {
        list(rbind("zeros" = sum(x == 0)))
      })
      all_sims <- do.call(rbind.data.frame, all_sims)
      names(all_sims)[1] <- paste("zeros") 
      temp <- as.data.frame(sum(data[, response] == 0))
      names(temp)[1] <- paste("zeros") 
      
      # The following checks the quantile of observed zero counts against simulated zeros.
      # Depending on quantiles, observed vlines are coloured blue, orange, or red.
      grouped_sims <- cbind(all_sims, temp)
      names(grouped_sims) <- c("zeros","obs_zeros")
      prop <- as.data.frame(grouped_sims %>%  summarize(extreme = sum(zeros > obs_zeros)/n.sim, 
                                                        equal = sum(zeros == obs_zeros)/n.sim))
      
      col.vline <- ifelse(between(prop$extreme, 0.05, 0.95) | prop$equal > 0.1, "blue",  
                          ifelse(between(prop$extreme, 0.005, 0.995) | prop$equal > 0.01, "orange", "red"))
      
    }
  
  plot.output <- ggplot() +
    labs(x = "N. zeros: simulated (bars) and observed (line)", y = "Frequency", title = "") +
    geom_histogram(data = all_sims, aes(x = zeros), col = "white", fill = "grey70", bins = 30) + 
    geom_vline(data = temp, aes(xintercept = zeros), col = col.vline, linewidth = 1.2, lty = 1) +
    scale_x_continuous() +
    plot0
  if(!missing(predictor) && !is.null(predictor) && length(na.omit(predictor)) > 0) {
    plot.output <- plot.output + facet_wrap(as.formula(paste("~", predictor)), scales = "free")
  }
  

    if(sum(col.vline == "blue")  == length(col.vline)) {
    message("OPTIMAL FIT: All observed zeros fall within central 90% of simulated data. No issues.")} else {
      if(sum(col.vline == "orange") == length(col.vline)) {
        warning(call. = F, "SUB-OPTIMAL FIT: All observed zeros (orange) fall marginal within central 99% of simulated data. Check if better model fit can be achieved.")} else {
          if(sum(col.vline == "red") == length(col.vline)) {
            warning(call. = F, "POOR FIT: All observed zeros (red) are outside the central 99% of simulated data. Your model likely provides insufficient fit to your data.")} else {
              if(sum(col.vline == "blue") + sum(col.vline == "orange") == length(col.vline)) {
                warning(call. = F, "SUB-OPTIMAL FIT: Some observed zeros (orange) fall marginal within central 99% of simulated data. Check if better model fit can be achieved.")} else {
                  if(sum(col.vline == "blue") + sum(col.vline == "red") == length(col.vline)) {
                    warning(call. = F, "POOR FIT: Some observed zeros (red) are outside the central 99% of simulated data. Your model likely provides insufficient fit to your data.")} else {
                      if(sum(col.vline == "orange") + sum(col.vline == "red") == length(col.vline)) {
                        warning(call. = F, "SUB-OPTIMAL FIT: Some observed zeros (orange) fall marginal within central 99% of simulated data. Check if better model fit can be achieved.")
                        warning(call. = F, "POOR FIT: Some observed zeros (red) are outside the central 99% of simulated data. Your model likely provides insufficient fit to your data.")} else {
                          if(sum(col.vline == "blue") + sum(col.vline == "orange") + sum(col.vline == "red") == length(col.vline)) {   
                            warning(call. = F, "SUB-OPTIMAL FIT: Some observed zeros (orange) fall marginal within central 99% of simulated data. Check if better model fit can be achieved.")
                            warning(call. = F, "POOR FIT: Some observed zeros (red) are outside the central 99% of simulated data. Your model likely provides insufficient fit to your data.")
                            }
                        }
                    }
                }
            }
        }
    }
 
   if(family(modelTMB)[1] %in% c("gaussian")) {
     warning(call. = F, "NOTE: Model uses GAUSSIAN family, where simulated data will almost never contain true zeros. Warnings for 'poor fit' may here be ignored if observed zeros fall well within the entire raw data range. Check carefully.")
   }
    plot.output
  }


#------------------------------#
#** 6.2.3 Data distribution ----
#------------------------------# 

ppcheck_fun <- function(data, modelTMB, response, predictor, n.sim) {
  
  data <- as.data.frame(data)
  
  sims <- simulate(modelTMB, nsim = n.sim, seed = 9) # simulate data from the model
  
  if(!missing(predictor) && !is.null(predictor) && length(na.omit(predictor)) > 0) { 
    
    if(length(na.omit(predictor)) > 1) {warning(call. = F, "Multiple predictors detected: the first has been taken by default. Explicitly specify others if preferred.")}
    
    if(length(predictor) > 1) {
      predictor <- na.omit(predictor)[1]
    }
    
    if(sum(sapply(data[predictor], FUN = is.numeric)) == length(predictor)) {stop("ERROR: The predictor must be a factor")} else {
      
      if (modelTMB$modelInfo$family[1] == "binomial" | modelTMB$modelInfo$family[1] == "betabinomial") {
        simulations <- lapply(sims, function(x) {
          "value" = x[,1]/(x[,1] + x[,2])})}
      else {
        simulations <- lapply(sims, function(x) {
          "value" = x})
      }
      simulations <- cbind(reshape::melt(simulations), data %>% dplyr::select(!!sym(predictor)), row.names = NULL)
    }} else {
      if (modelTMB$modelInfo$family[1] == "binomial" | modelTMB$modelInfo$family[1] == "betabinomial") {
        simulations <- lapply(sims, function(x) {
          "value" = x[,1]/(x[,1] + x[,2])})
      } else {
        simulations <- lapply(sims, function(x) {
          "value" = x})
      }
      simulations <- reshape::melt(simulations)
    }
  
  plot.output <- ggplot() + 
    geom_density(data = simulations, 
                 aes(x = value, group = L1), 
                 col = "grey60") + # is geom_density what we are looking for?
    geom_density(data = data, 
                 aes(x = !!sym(response)), 
                 col = "blue", size = 1.5) +
   # scale_x_continuous(limits = c(0, quantile(simulations$value, prob = 0.999))) +
    labs(x = paste(response, ": simulated (grey) and observed (blue)", sep = ""), 
         y = "Density", 
         title = "") + 
    plot0
  
  if(!missing(predictor) && !is.null(predictor) && length(na.omit(predictor)) > 0) {
    plot.output <- plot.output + facet_wrap(as.formula(paste("~", predictor)), scales = "free")
  }
  plot.output
  }


#================================#
#* 6.3 Autocorrelation checks ---- 
#================================#

autocor_check <- function(data,  modelTMB, variable, grouping, maxlag, n.sim) {
  
  data <- as.data.frame(data)
  
  if(missing(variable) || is.null(variable) ||  is.na(variable)) 
  {stop("ERROR: Temporal or spatial predictor missing. Please specify one, otherwise skip this step.")
  } else {
    
    if(length(variable) > 1) {   # dealing with spatial dependency here
      
      dists <- dist(data[, variable]) # this computes distance matrix 
      coord <- cbind(data[, variable[1]], data[, variable[2]])
      
      if(missing(maxlag) || is.null(maxlag) ||  is.na(maxlag)) {
        lags <- seq(min(dists), max(dists), length = 10) # set 10 lags at which to calculate semivariance  
      } else {lags <- seq(min(dists), max(maxlag), length = 10)}
      
      
    } else {
      
      if(!missing(grouping) && !is.null(grouping) &&  !is.na(grouping)) {  # dealing with multiple time series here  
        
        if(missing(maxlag) || is.null(maxlag) ||  is.na(maxlag)) {
          maxlag <- max(as.data.frame(data %>%
                                        group_by_at(grouping) %>%
                                        summarise(lag = max(!!sym(variable)) - min(!!sym(variable))))[,2])}
        
        new_time <- data[, variable] + maxlag * 3 * (as.integer(data[, grouping])-1)
        data$new_time <- new_time
        
        dists <- dist(cbind(new_time, rep(0, nrow(data)))) # this computes distance matrix   
        coord <- cbind(data$new_time, rep(0, nrow(data)))
        
        lags <- seq(min(dists), maxlag, length = 10) # set 10 lags at which to calculate semivariance  
        
      } else { # dealing with single time series here
        
        dists <- dist(cbind(data[, variable], rep(0, nrow(data)))) # this computes distance matrix   
        coord <- cbind(data[, variable], rep(0, nrow(data)))
        
        if(missing(maxlag) || is.null(maxlag) ||  is.na(maxlag)){
          lags <- seq(min(dists), max(dists), length = 10) # set 10 lags at which to calculate semivariance  
        } else {
          lags <- seq(min(dists), maxlag, length = 10)}
      }}
    
    data.Var <- as.data.frame(variog(coords = coord, 
                                   data = scale(residuals(modelTMB)), 
                                   breaks = lags)[c("u","v","n")])
    
    # Here we permute the observed semivariances across the observed pairwise distances (= grey lines in graph)
    data.rand.Var <- replicate(n.sim, variog(coords = coord, 
                                         data = sample(scale(residuals(modelTMB)), replace = F),
                                         breaks = lags)[c("v")])
    
    names(data.rand.Var) <- as.character(c(1:n.sim))
    data.rand.Var <- do.call(cbind.data.frame, data.rand.Var)
    data.rand.Var <- reshape::melt(data.rand.Var)
    data.rand.Var$u <- data.Var$u
    colnames(data.rand.Var) <- c("rep.run","v.rand","u")
    
    ggplot(data = data.Var, 
           aes(x = u, y = v)) + # this extracts distance lags and semi variance
      geom_line(data = data.rand.Var,
                aes(x = u, y = v.rand, 
                    group = rep.run), col = "grey80", size = 1, alpha = 0.5) +
      geom_point(col = "blue", size = 5, alpha = 0.5) +
      geom_smooth(method = "loess", se = F, col = "blue", size = 1) +
      geom_hline(yintercept = 1, col = "red", lty = 2) +
      xlab("Distance between observations") + 
      ylab("Standardised semivariance") +
      scale_y_continuous() +
      scale_x_continuous(breaks = data.Var$u, labels = formatC(round(data.Var$u, digits = 2), 2, format = "f")) +
      annotate("text", x = data.Var$u, y = min(c(data.rand.Var$v.rand,data.Var$v)) - 0.02*max(c(data.rand.Var$v.rand,data.Var$v)), label = c(paste("N =",data.Var$n[1]), data.Var$n[-1]), size = 5) +
      plot0
  }}


#-###########################################-#
# 7. Model results and parameter estimates ----
#-###########################################-#

#============================================================#
#* 7.1 Overall coefficient estimates and model performance ---  
#============================================================#

comp_int <- function(modelTMB, ci_range, effects, component) {
  
  if(missing(ci_range)) ci_range = 0.95
  if(missing(effects)) effects = "all"
  if(missing(component)) component = "all"
 
  if(effects == "random") {
    message("NOTE: you selected to display random effects, only. This 
will generate an error message in case your model does not contain any 
random effect.")}
  # We use the parameters function (from package 'parameters') to extract model coefficients:
  temp <- parameters(modelTMB,
                     ci = ci_range,
                     iterations = 10000,
                     effects = effects,
                     component = component)
  
  # We simplify the output to effect estimates and their SE / CI, but remove null hypothesis test info:
  if(effects == "all") {
    output.cols <- c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "Effects", "Group", "Component")     # c(1:3,5:6,10:12)
    if(length(temp) < 12) {  # length can be 10 or 11 in the absence of random component.
      output.cols <- c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "Effects")  # c(1:3,5:6,10)
      message("HINT: Model contains no random component")
    }
  }
  
  if(effects == "random") {
    output.cols <- c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "Effects", "Group", "Component")   # c(1:3,5:9)
  }
  if(effects == "fixed") {
    output.cols <- c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "Effects")  # c(1:3,5:6, 10)
  }
  print(temp[, output.cols])
  
}


#=========================================#
#* 7.2 Estimation of regression slopes ---- 
#=========================================#

slope_estimates <- function(data, modelTMB, num_predictor, fac_predictors) {
  
  data <- as.data.frame(data)
  
  set.seed(99) # Just to make results reproducible
  
  #if (contrasts == "within") {contrasts <- "|"}


if(missing(num_predictor) || is.null(num_predictor) ||  is.na(num_predictor)){
  stop("ERROR: No NUMERIC predictor has been specified. Skip this step if none is available.")}

if(sum(sapply(data[num_predictor], FUN = is.numeric)) != length(num_predictor)) {
  stop("ERROR: Incorrect assignment of the numeric predictor. Specify a NUMERIC predictor only. Skip this step if none is available.")}

if(length(num_predictor) > 1) {
  stop("ERROR: Multiple numeric predictors detected, specify one at a time. Note that this function does not work when two or more numeric predictors are part of the same interaction.")}

if (missing(fac_predictors)) {
  stop("ERROR: No factor predictor has been specified. Skip this step if none is available.")}

formula <- paste(gsub(".~", "", formula(modelTMB, fixed.only = T))[3], collapse = "") 
temp_1 <- unlist(str_split(formula, "\\+"))
temp_1 <- str_replace_all(temp_1, fixed(" "), "")  # remove ""
temp <- unlist(str_split(temp_1, "\\*"))         # solves interaction if expressed with "\\*"
  
remove_me <- temp[str_detect(temp, pattern = (":"))] # extract interactions 
interactions <-  temp_1[str_detect(temp_1, pattern = (":|\\*"))] # extract interactions
  
int_check <- sapply(interactions,str_detect,fac_predictors, simplify = F)
int_check <- names(int_check)[str_detect(names(int_check), num_predictor)]
  
if(sum(str_detect(int_check, fac_predictors)) != length(fac_predictors)) {
    stop("ERROR: One or more factor predictors are not in interaction with the numeric predictor specified.")}  
  

pairs <-  eval(str2expression(paste("pairwise ~", fac_predictors[1], sep = " ")))
if (length(fac_predictors) > 1) {
  #  if (contrasts == "all") { 
  pairs <-  eval(str2expression(paste("pairwise ~", paste(fac_predictors[1], fac_predictors[2], sep = " * "), sep = " ")))} 
#else {
#      pairs <-  eval(str2expression(paste("pairwise ~", paste(fac_predictors[1], fac_predictors[2], sep = " | "), sep = " ")))
#  }
#}

output <- as.data.frame(emtrends(object = modelTMB, 
                                 specs = pairs, 
                                 var = num_predictor, adjust = "none")$emtrends)

if (length(fac_predictors) == 1) {
  output <- output[,c(1,2,3,5,6)] 
  colnames(output)[1] <- "slope"
  colnames(output)[colnames(output) == paste(num_predictor, "trend", sep = ".")] <- "estimate"
}

if (length(fac_predictors) > 1) {
  output$slope <- levels(interaction(output[,1], output[,2], sep = ": "))
  output <- output[,c(8,3,4,6,7)]
  colnames(output)[colnames(output) == paste(num_predictor, "trend", sep = ".")] <- "estimate"}

colnames(output)[c(4,5)] <- c("lower.CI","upper.CI")


plot.output <- ggplot(output, aes(x = estimate, y = slope)) + # plots contrasts only with 95% compatibility interval
  geom_point() +
  geom_errorbarh(aes(xmax = upper.CI, 
                     xmin = lower.CI, 
                     height = 0)) 

plot.output <- plot.output + geom_vline(xintercept = 0, linetype = 2, col = "blue") 

plot.output <- plot.output + 
  {if(modelTMB$modelInfo$family$link == "identity") 
    labs(x = paste(num_predictor, "slope [absolute mean +/- 95% CI]", sep = " "),
         y = NULL)} +
{if(modelTMB$modelInfo$family$link == "log") 
  labs(x = paste(num_predictor, "slope [change in log(response) per unit increase +/- 95% CI]", sep = " "),
       y = NULL)} +
  {if(modelTMB$modelInfo$family$link == "logit") 
    labs(x =  paste(num_predictor, "slope [change in log(Odds) per unit increase +/- 95% CI]", sep = " "),
         y = NULL)} +
  {if(!(modelTMB$modelInfo$family$link %in% c("identity", "log", "logit"))) 
    labs(x = paste(num_predictor, "slope [response change on link scale +/- 95% CI]",sep = " "),
         y = NULL)} +
  theme(axis.ticks.y = element_blank()) + 
  plot0 

print(plot.output)

if(modelTMB$modelInfo$family$link == "log") {
  colnames(output)[2] <- c("log.ratio")
  colnames(output)[c(4,5)] <- c("log.ratio_lower.CI","log.ratio_upper.CI")
  output$ratio <- exp(output$log.ratio)
  output$ratio.lower.CI <- exp(output$log.ratio_lower.CI)
  output$ratio.upper.CI <- exp(output$log.ratio_upper.CI)
}
if(modelTMB$modelInfo$family$link == "logit") {
  colnames(output)[2] <- c("log.odds")
  colnames(output)[c(4,5)] <- c("log.odds_lower.CI","log.odds_upper.CI")
  output$odds.ratio <- exp(output$log.odds)
  output$odds.ratio_lower.CI <- exp(output$log.odds_lower.CI)
  output$odds.ratio_upper.CI <- exp(output$log.odds_upper.CI)
}

output

}


#=========================================================#
#* 7.3 Estimation of differences between factor levels ---- 
#=========================================================#

pairwise_comparisons <- function(data, modelTMB, predictors, component, dispScale, contrasts) {
  
  data <- as.data.frame(data)

#  on dispScale, i.e. "type" in original emmeans call
#  As in predict.emmGrid, this determines whether we want to inverse-transform the predictions (type = "response") 
#  or not (any other choice). The default is "link", unless the "predict.type" option is in force; see emm_options. 
#  In addition, the user may specify type = "scale" to create a transformed scale for the vertical axis 
#  based on object’s response transformation or link function.
  
  if (missing(dispScale)) {dispScale <- "response"} # can also be link or scale
  if (missing(component)) {component <- "cond"} # can also be zi
  if (missing(contrasts)) {contrasts <- "all"} # can also be within
  
  if (contrasts == "within") {contrasts <- "|"}
  
  set.seed(99) # Just to make results reproducible
  
  if(length(predictors) == 0) stop("ERROR: No FACTOR predictor has been specified. Skip this step if none is available.")
  
  if(length(predictors) - sum(sapply(data[predictors], FUN = is.numeric)) !=  length(predictors)) {
    stop("ERROR: Incorrect assignment of predictors. Specify FACTOR predictors only. Skip this step if not available.")}
  
  pairs <-  eval(str2expression(paste("pairwise ~", predictors[1], sep = " ")))
  if (length(predictors) > 1) {
    if (contrasts == "all") { 
      pairs <-  eval(str2expression(paste("pairwise ~", paste(predictors[1], predictors[2], sep = " * "), sep = " ")))} else {
        pairs <-  eval(str2expression(paste("pairwise ~", paste(predictors[1], predictors[2], sep = " | "), sep = " ")))
      }
  }
  
  emm <- emmeans(object = modelTMB, # Name of model
                 specs = pairs, # Replace with the name of your factor predictor
                 type = dispScale, 
                 component = component, # Computes contrasts for the conditional model ("cond"), zero-inflation part of the model ("zi")
                 adjust = "none") 
  
  comparisons <- summary(object = emm, # object from emmeans function
                         infer = T, adjust = "none") # displays comp intervals
  
  if (contrasts == "all" | length(predictors) == 1) {
    comparisons <- comparisons$contrasts[,c(1,2,3,5,6)] # columns c("contrast", "ratio|estimate|odds.ratio", "SE", "lower.CL", "upper.CL")
  }
  if (contrasts == "|" && length(predictors) > 1) {
    comparisons <- comparisons$contrasts[,c(1,2,3,4,6,7)] # columns c("contrast", var.fac[2], "ratio|estimate|odds.ratio", "SE", "lower.CL", "upper.CL")
    comparisons$contrast <- levels(interaction(comparisons[,2], comparisons[,1], sep = ": "))
    comparisons <- comparisons[,c(1,3,4,5,6)]}
  
  colnames(comparisons)[c(4,5)] <- c("lower.CI","upper.CI")
  
  plot.output <- ggplot(comparisons, aes(x = !!sym(colnames(comparisons)[2]), y = contrast)) + # plots contrasts only with 95% compatibility interval
    geom_point() +
    geom_errorbarh(aes(xmax = upper.CI, 
                       xmin = lower.CI, 
                       height = 0)) +
    theme(axis.ticks.y = element_blank()) +
    plot0
  
  if (colnames(comparisons)[2] == "estimate" & modelTMB$modelInfo$family$link == "identity") {
    plot.output <- plot.output + geom_vline(xintercept = 0, linetype = 2, col = "blue") +
      labs(x = "Pairwise differences: mean +/- 95% CI",
           y = NULL)}
  if (colnames(comparisons)[2] == "estimate" & modelTMB$modelInfo$family$link != "identity" & modelTMB$modelInfo$family$link != "logit" & modelTMB$modelInfo$family$link != "log") {
    plot.output <- plot.output + geom_vline(xintercept = 0, linetype = 2, col = "blue") +
      labs(x = "Pairwise difference on link scale: mean +/- 95% CI",
           y = NULL)}
  if (colnames(comparisons)[2] == "estimate" & modelTMB$modelInfo$family$link == "logit") {
    plot.output <- plot.output + geom_vline(xintercept = 0, linetype = 2, col = "blue") +
      labs(x = "Pairwise log(odds.ratio): mean +/- 95% CI",
           y = NULL)}
  if (colnames(comparisons)[2] == "estimate" & modelTMB$modelInfo$family$link == "log" & component == "cond") {
    plot.output <- plot.output + geom_vline(xintercept = 0, linetype = 2, col = "blue") +
      labs(x = "Pairwise log.ratio: mean +/- 95% CI",
           y = NULL)}
  if (colnames(comparisons)[2] == "estimate" & modelTMB$modelInfo$family$link == "log" & component == "zi") {
    plot.output <- plot.output + geom_vline(xintercept = 0, linetype = 2, col = "blue") +
      labs(x = "Pairwise log(odds.ratio): mean +/- 95% CI",
           y = NULL)}
  if (colnames(comparisons)[2] == "ratio") {
    plot.output <- plot.output + geom_vline(xintercept = 1, linetype = 2, col = "blue") +
      labs(x = "Pairwise response ratios: mean +/- 95% CI",
           y = NULL)}
  if (colnames(comparisons)[2] == "odds.ratio") {
    plot.output <- plot.output + geom_vline(xintercept = 1, linetype = 2, col = "blue") +
      labs(x = "Pairwise odds ratios: mean +/- 95% CI",
           y = NULL)}

  if(colnames(comparisons)[2] == "estimate" & modelTMB$modelInfo$family$link == "log" & component == "cond") {
    colnames(comparisons)[2] <- c("log.ratio")
    colnames(comparisons)[c(4,5)] <- c("log.ratio_lower.CI","log.ratio_upper.CI")
  }
  if(colnames(comparisons)[2] == "estimate" & modelTMB$modelInfo$family$link == "log" & component == "zi") {
    colnames(comparisons)[2] <- c("log.odds.ratio")
    colnames(comparisons)[c(4,5)] <- c("log.odds.ratio_lower.CI","log.odds.ratio_upper.CI")
  }
  if(colnames(comparisons)[2] == "estimate" & modelTMB$modelInfo$family$link == "logit") {
    colnames(comparisons)[2] <- c("log.oddsratio")
    colnames(comparisons)[c(4,5)] <- c("log.odds.ratio_lower.CI","log.odds.ratio_upper.CI")
  }
  
  print(comparisons)
  
  plot.output
  
}


#-############################################-#
# 8. Graphical display of model predictions ----
#-############################################-#

#==================================#
#* 8.2 Derive model predictions ---- 
#==================================#

# routine from Korner-Nievergelt et al. 2015; Brooks et al. 2017
# More detail: https://stackoverflow.com/questions/61902110/extracting-posterior-modes-and-compatibility-intervals-from-glmmtmb-output

post_predict <- function(data, modelTMB, plot_predictors, offset, component) { 
    
  if (missing(offset)) {offset <- NA}
  
  if(length(modelTMB$call$offset) > 0 && is.na(offset)) {
    stop("ERROR: Your model contains an offset term. Please specify it in the function call.")}
    
  
  data <- as.data.frame(data)
  
    # First, we create a grid...
  
  set.seed(99) # Just to make results reproducible
  
  formula <- paste(gsub(".~", "", formula(modelTMB, fixed.only = T))[3], collapse = "")
  
  temp <- unlist(str_split(formula, "\\+"))
  temp <- unlist(str_split(temp, "\\*"))         # solves interaction if expressed with "\\*"
  temp[grep("offset",temp)] <- offset
  temp <- str_replace_all(temp, fixed(" "), "")  # remove ""

  remove_me <- temp[str_detect(temp, pattern = (":"))] # extract interactions 
  
  if(length(remove_me) > 0) {
    fixed.pred <- temp[-match(remove_me, temp)]
  } else {
    fixed.pred <- temp
  }
  
  poly_form <- fixed.pred[str_detect(fixed.pred, pattern = "poly")] # find poly formula if present
  
  if(length(poly_form) > 0) {
   
    poly_col <- gsub(" ", "", gsub("poly\\(|\\)|x\\=|[[:digit:]])||degree = [[:digit:]],|,|degree=[[:digit:]],|, |[[:digit:]],|, 
                   raw=TRUE|raw=T|raw=FALSE|raw=F", "", poly_form))
    
    for (i in 1:length(poly_form)) {
      fixed.pred <- str_replace_all(fixed.pred, fixed(poly_form[i]), poly_col[i])
    }
  } # change name of the formula to real column in list of fixed predictors
  
  # Define the intended value ranges for each predictor:
  nd.list <- lapply(fixed.pred, function(x) { 
    if(is.factor(data[, x])) {levels(droplevels(data[, x])) 
    } else {if(x %in% plot_predictors) {
      seq(min(data[, x]), max(data[, x]), 
          length = 20) # can also be set to 50                     
    } else {if(length(poly_form) > 0) {    
      if(grepl(x, poly_form)) {             
        seq(min(data[, x]), max(data[, x]), 
            length = 20)    # can also be set to 50                
      } else {
        median(data[, x])
      }
    } else {
      median(data[, x])
    }
    }
    }
  })
  
  # Now we generate the grid
  grid <- expand.grid(nd.list)  
  names(grid) <- fixed.pred
  
  # Now we generate model predictions from simulated data for data ranges defined in the grid
  # The procedure follows the one presented in GLMMTMB paper from Brooks 2017
  
  if (missing(component)) {component <- "all"}
  
  # Simulation for ALL model parts:
  if (component == "all") {
    bsim_cond <- mvrnorm(10000, 
                         mu = fixef(modelTMB)$cond, 
                         Sigma = vcov(modelTMB)$cond) # equivalent to sim from package arm.
 
       formula <- eval(str2expression(paste(gsub(".~","", formula(modelTMB, fixed.only = T))[c(1,3)], collapse = "")))
    
    
    Xmat_cond <- model.matrix(formula, data = grid)
    fitmatrix_all <- modelTMB$modelInfo$family$linkinv(Xmat_cond %*% t(bsim_cond)) 
    pred_mod  <- modelTMB$modelInfo$family$linkinv(Xmat_cond %*% fixef(modelTMB)$cond)
    
    # simulation for the zi-part of model, if present:
    if (length(fixef(modelTMB)$zi != 0)) {
      bsim_zi <- mvrnorm(10000, mu = fixef(modelTMB)$zi, Sigma = vcov(modelTMB)$zi)
      Xmat_zi <- model.matrix(modelTMB$modelInfo$terms$zi$fixed, data = grid)
      fitmatrix_zi <- 1 - plogis((Xmat_zi %*% t(bsim_zi)))
      fitmatrix_all <- fitmatrix_all * fitmatrix_zi
      pred_mod <- modelTMB$modelInfo$family$linkinv(Xmat_cond %*% fixef(modelTMB)$cond) * (1- plogis(Xmat_zi %*% fixef(modelTMB)$zi))
      }
    }
  
  # Simulation just for the CONDITIONAL model part:
  if (component == "cond") { 
    bsim_cond <- mvrnorm(10000, 
                         mu = fixef(modelTMB)$cond, 
                         Sigma = vcov(modelTMB)$cond) # equivalent of sim from package arm
    formula <- eval(str2expression(paste(gsub(".~","", formula(modelTMB, fixed.only = T))[c(1,3)], collapse = "")))
    
     Xmat_cond <- model.matrix(formula, data = grid)
    fitmatrix_all <- modelTMB$modelInfo$family$linkinv(Xmat_cond %*% t(bsim_cond)) 
    pred_mod  <- modelTMB$modelInfo$family$linkinv(Xmat_cond %*% fixef(modelTMB)$cond)
    }
  
  # Simulation just for ZI model part:
  if (component == "zi") {
    bsim_zi <- mvrnorm(10000, mu = fixef(modelTMB)$zi, Sigma = vcov(modelTMB)$zi)
    Xmat_zi <- model.matrix(modelTMB$modelInfo$terms$zi$fixed, data = grid)
    fitmatrix_all <- 1 - plogis((Xmat_zi %*% t(bsim_zi)))
    pred_mod <- 1 - plogis(Xmat_zi %*% fixef(modelTMB)$zi)
    }
  
  grid$median <- apply(fitmatrix_all, 1, quantile, prob = 0.5)  # extracts median
  grid$pred_mod <-  pred_mod
  grid$lower <- apply(fitmatrix_all, 1, quantile, prob = 0.025) # extracts lower compatibility interval
  grid$upper <- apply(fitmatrix_all, 1, quantile, prob = 0.975) # ... and the upper compatibility interval
  
  
  # Project z-transformed predictors to raw scale:
  if(length(grep("_z", names(grid))) > 0) {
    z_predictors <- names(grid)[grep("_z", names(grid))]
    predictors <- str_replace_all(z_predictors[grep("_z", z_predictors)], fixed("_z"), "")
    
    temp2 <- NULL
    for(i in 1:(length(predictors))) {
      temp1 <- back_z_transform(data[,predictors[i]], grid[,z_predictors[i]]) 
      temp2 <- as.data.frame(cbind(temp2, temp1))
      }
    
    grid[, z_predictors] <- temp2
    names(grid) <- str_replace_all(names(grid), fixed("_z"), "")
    }
  
  plot_predictors <- str_replace_all(plot_predictors, fixed("_z"), "")
  
  if (!is.na(offset)) {
        plot_predictors <- c(plot_predictors, offset)}
  
  # We AVERAGE the grid across the values for the predictors of interest:
  filled_grid <- data.frame(
    grid  %>%  
        group_by_at(all_of(plot_predictors))  %>%
          summarize(median = median(median), pred_mod = median(pred_mod), lower = median(lower), upper = median(upper))
  )
  
  return(filled_grid)
}


#============================#
#* 8.3 Summarise raw data ---- 
#============================#

# Average RAW data per true replicate at display level:
display_raw <- function(data, modelTMB, plot_predictors, plot_random, response, offset, component) {
  
  data <- as.data.frame(data)
  
  if(missing(plot_random)) {plot_random <- NA}
  if(missing(offset)) {offset <- NA}
  
  predictors <- c(plot_predictors, plot_random)
  predictors <- na.omit(predictors)
  predictors <- str_replace_all(predictors, fixed("_z"), "") # this removes the "_z" from predictor names, so that raw data values are used for plotting.
  
  if(!is.na(plot_random[1])) {
    if (missing(component)) {component <- "all"}
    if (length(grep("truncated_", modelTMB$modelInfo$family, fixed = T)) > 0) {
      if (component == "cond") {
        if(!is.na(offset)) {
          if(length(modelTMB$call$offset) == 0) {stop("ERROR: Your model does not have an offset term.")
            }
      dat_summary <- as.data.frame(data %>% 
                                     filter(!!sym(response) > 0) %>%
                                     mutate(mean = !!sym(response)/!!sym(offset))  %>%
                                     group_by_at(all_of(predictors)) %>% 
                                     summarise(mean = mean(mean)))
      } else {
        dat_summary <- as.data.frame(data %>% 
                                       filter(!!sym(response) > 0) %>%
                                       group_by_at(all_of(predictors)) %>% 
                                       summarize(mean = mean(!!sym(response))))
        }
        # next line adds some zeros to conditional model when some replicate levels are missing
        dat_summary <- as.data.frame(left_join(unique(data[,predictors]), dat_summary) %>% 
                      mutate(mean = replace(mean, is.na(mean), 0)))
        }
      if (component == "zi") {
        dat_summary <- as.data.frame(data %>% 
                                       mutate(zi_resp = case_when(!!sym(response) > 0 ~  1, !!sym(response) == 0 ~  0))  %>%
                                       group_by_at(all_of(predictors))  %>% 
                                       summarize(mean = mean(zi_resp)))
        }
      if (component == "all") {
        if(!is.na(offset)) {
          if(length(modelTMB$call$offset) == 0) {stop("ERROR: Your model does not have an offset term.")
            }
          dat_summary <- as.data.frame(data %>% 
                                         mutate(mean = !!sym(response)/!!sym(offset))  %>%
                                         group_by_at(all_of(predictors)) %>% 
                                         summarise(mean = mean(mean)))
          } else {
            dat_summary <- as.data.frame(data %>% group_by_at(all_of(predictors))  %>% summarize(mean = mean(!!sym(response))))
            }
        }
      } else {
        if (component == "all" | component == "cond") {
          if(!is.na(offset)) {
            if(length(modelTMB$call$offset) == 0) {stop("ERROR: Your model does not have an offset term.")
              } 
            dat_summary <- as.data.frame(data %>% 
                                           mutate(mean = !!sym(response)/!!sym(offset))  %>%
                                           group_by_at(all_of(predictors)) %>% 
                                           summarise(mean = mean(mean)))
            } else {
              dat_summary <- as.data.frame(data %>% 
                                             group_by_at(all_of(predictors)) %>%
                                             summarize(mean = mean(!!sym(response))))
            }
          }
        }
  } else {
      if (missing(component)) {component <- "all"}
    if (length(grep("truncated_", modelTMB$modelInfo$family, fixed = T)) > 0) {
      if (component == "cond") {
        if(!is.na(offset)) {
          if(length(modelTMB$call$offset) == 0) {stop("ERROR: Your model does not have an offset term.")
            }
          dat_summary <- as.data.frame(data %>%  
                                         filter(!!sym(response) > 0) %>% 
                                         mutate(mean = !!sym(response)/!!sym(offset))  %>%
                                         group_by_at(all_of(predictors)) %>% 
                                         summarise(mean = mean(mean)))
          } else {
            dat_summary <- as.data.frame(data  %>%  
                                           filter(!!sym(response) > 0) %>% 
                                           group_by_at(all_of(predictors)) %>% 
                                           mutate(mean = !!sym(response)))
            }
        # next line adds some zeros to conditional model when some replicate levels are missing
        dat_summary <- as.data.frame(dat_summary %>%
                  mutate(mean = replace(mean, is.na(mean), 0)))
        }
      if (component == "zi") {
        dat_summary <- as.data.frame(data %>% 
                                       mutate(zi_resp = case_when(!!sym(response) > 0 ~  1, !!sym(response) == 0 ~  0))  %>%
                                       group_by_at(all_of(predictors))  %>% 
                                       mutate(mean = zi_resp))
        }
      if (component == "all") {
        if(!is.na(offset)) {
          if(length(modelTMB$call$offset) == 0) {stop("ERROR: Your model does not have an offset term.")
            }
          dat_summary <- as.data.frame(data %>% 
                                         mutate(mean = !!sym(response)/!!sym(offset))  %>%
                                         group_by_at(all_of(predictors)) %>% 
                                         summarise(mean = mean(mean)))
          } else {
            dat_summary <- as.data.frame(data %>% group_by_at(all_of(predictors))  %>%  mutate(mean = !!sym(response)))
          }
        }
      } else {
        if (component == "all" | component == "cond") {
          if(!is.na(offset)) {
            if(length(modelTMB$call$offset) == 0) {stop("ERROR: Your model does not have an offset term.")
              }
            dat_summary <- as.data.frame(data %>% 
                                           mutate(mean = !!sym(response)/!!sym(offset))  %>%
                                           group_by_at(all_of(predictors)) %>% 
                                           summarise(mean = mean(mean)))  
            } else {
              dat_summary <- as.data.frame(data %>% 
                                             group_by_at(all_of(predictors))  %>% 
                                             mutate(mean = !!sym(response)))
            }
          }
        }}
  
  return(dat_summary)
  }


#============================#
#* 8.4 Produce final plot ---- 
#============================#

final_plotting <- function(data_summary, predictions, predictors, response, interaction.lines, y.limits, y.breaks, leg.pos, x.lab, y.lab, plot.title,
                           size.points, shape.points, col.fill.points, col.outline.points, alpha.points, jitter.points, width.error.bars, size.error.bars, 
                           col.error.bars, alpha.error.bars, size.means, grouped.shape.points, grouped.errorbar.col, grouped.fill.col, grouped.linetype, leg.title, A.ratio) {
  
  if (missing(interaction.lines)) {interaction.lines = F}
  if (missing(size.points)) {size.points = 2.5}
  if (missing(shape.points)) {shape.points = 21}
  if (missing(col.fill.points)) {col.fill.points = "grey50"}
  if (missing(col.outline.points)) {col.outline.points = "black"}
  if (missing(alpha.points)) {alpha.points = 0.6}
  if (missing(jitter.points)) {jitter.points = 0.1}
  if (missing(width.error.bars)) {width.error.bars = 0.12}
  if (missing(size.error.bars)) {size.error.bars = 0.75}
  if (missing(col.error.bars)) {col.error.bars = "black"}
  if (missing(alpha.error.bars)) {alpha.error.bars = 0.6}
  if (missing(size.means)) {size.means = 3.5}
  if (missing(plot.title)) {plot.title = NULL}
  if (missing(grouped.errorbar.col)) {grouped.errorbar.col = NULL}
  if (missing(grouped.shape.points)) {grouped.shape.points = NULL}
  if (missing(grouped.fill.col)) {grouped.fill.col = NULL}
  if (missing(grouped.linetype)) {grouped.linetype = NULL}
  if (missing(leg.pos)) {leg.pos = "inside.top.left"}
  if (missing(leg.title)) {leg.title = NULL}
  if (missing(y.limits)) {y.limits = NULL}
  if (missing(y.breaks)) {y.breaks = NULL}
  if (missing(y.lab)) {y.lab = NULL}
  if (missing(x.lab)) {x.lab = NULL}
  if (missing(A.ratio)) {A.ratio = 1}
  
  
  predictors <- str_replace_all(predictors, fixed("_z"), "")
  
  # Now we start generating final plots, either for 1 or 2 predictors
  if(length(predictors) == 1) {
    plot.1pred <- ggplot(data = data_summary, 
                         aes(x = data_summary[, predictors],
                             y = mean)) 
    
    if(is.factor(data_summary[, predictors])) {
    plot.1pred <- plot.1pred +
      geom_point(aes(shape = data_summary[, predictors]), fill = col.fill.points, position = position_jitter(width = jitter.points, height = 0),
                 size = size.points, col = col.outline.points, alpha = alpha.points) +
      geom_errorbar(data = predictions, 
                    aes(ymax = upper, ymin = lower, 
                        y = pred_mod,
                        x = predictions[, predictors], col = predictions[, predictors]),
                    width = width.error.bars, size = size.error.bars) + 
      geom_point(data = predictions, 
                 aes(y = pred_mod,
                     x = predictions[, predictors], fill = predictions[, predictors], shape = predictions[, predictors],
                     col = predictions[, predictors]), size = size.means) 
    } else {
      plot.1pred <- plot.1pred +
        geom_ribbon(data = predictions, aes(x = predictions[, predictors], 
                                            y = pred_mod, 
                                            ymin = lower, ymax = upper), 
                    alpha = alpha.error.bars, bg = col.error.bars) +
        geom_line(data = predictions, aes(x = predictions[, predictors], y = pred_mod), 
                  size = 0.8, linetype = "dashed", colour = col.error.bars) +
        geom_point(size = size.points, shape = shape.points, bg = col.fill.points, col = col.outline.points, alpha = alpha.points,
                   position = position_jitter(width = jitter.points, height = 0)) 
    }
    
      
    if(is.null(leg.title)) {
      leg.title = predictors[1]}
    
    colours <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    colours <- sample(colours, nlevels(predictions[, predictors[1]]))
    
    if (is.null(grouped.errorbar.col)) {grouped.errorbar.col = colours}
    if (is.null(grouped.shape.points)) {grouped.shape.points = rep(21, nlevels(predictions[,predictors[1]]))}
    if (is.null(grouped.fill.col))  {grouped.fill.col = colours}
    if (is.null(grouped.linetype)) {grouped.linetype = rep(1, nlevels(predictions[, predictors]))}
    
    plot.1pred <- plot.1pred +
      labs(x = predictors, y = response) +
      scale_color_manual(values = grouped.errorbar.col, name = leg.title) +
      scale_fill_manual(values = grouped.fill.col, name = leg.title) +
      scale_shape_manual(values = grouped.shape.points, name = leg.title) +
      guides(fill = "none", shape = "none", col = "none") +
      plot0
    
    plot.final <- plot.1pred
    
  } else {
    
    # Now the plot for 2 predictors:
    if(length(predictors) == 2) {
      if(is.factor(data_summary[, predictors[1]])) {
        pred.x1 <- predictors[2]
        pred.x2 <- predictors[1]
      } else {
        pred.x1 <- predictors[1]
        pred.x2 <- predictors[2]
      }
      
      if(!is.factor(data_summary[, pred.x2])) {
        # OPTION A: Plot for 2 numeric or integer predictors.
        # Here, we display the curves for 5%, 50% and 95% quantile observed values on pred.x2
        
        data.sub <- c(unique(predictions[, pred.x2])[round(length(unique(predictions[, pred.x2]))*0.05)],
                      unique(predictions[, pred.x2])[round(length(unique(predictions[, pred.x2]))*0.50)],
                      unique(predictions[, pred.x2])[round(length(unique(predictions[, pred.x2]))*0.95)])
        predictions <- subset(predictions, predictions[, pred.x2] %in% data.sub)
        predictions[, pred.x2] <- droplevels(as.factor(predictions[, pred.x2]))
        levels(predictions[,pred.x2]) <- c("5% quantile","50% quantile","95% quantile")
        #levels(predictions[,pred.x2]) <- round(as.numeric(levels(predictions[,pred.x2])), digits = 2)
        # From here, plotting proceeds as if pred.x2 was a factor
      }
      
      colours <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
      colours <- sample(colours, nlevels(predictions[, pred.x2]))
      
      if (is.null(grouped.errorbar.col)) {grouped.errorbar.col = colours}
      if (is.null(grouped.shape.points)) {grouped.shape.points = rep(21, nlevels(predictions[, pred.x2]))}
      if (is.null(grouped.fill.col)) {grouped.fill.col = colours}
      if (is.null(grouped.linetype)) {grouped.linetype = rep(1, nlevels(predictions[, pred.x2]))}
      
      # Now we start the plotting routine, which boils down to only 2 options:
      plot.2pred <- ggplot(data = predictions,
                           aes(x = as.numeric(predictions[, pred.x1]),
                               y = pred_mod), 
                           group = predictions[, pred.x2]) 
      
      if(is.factor(data_summary[, pred.x1])) {  # OPTION B: Plot for 2 factors
        
        if (interaction.lines == T) {
          plot.2pred <- plot.2pred + 
            geom_line(position = position_dodge(0.7), linetype = "dashed", size = 0.5, alpha = 0.6,
                      aes(group = predictions[, pred.x2], col = predictions[, pred.x2]))}  # interaction lines if needed
        
        plot.2pred <- plot.2pred +
          geom_errorbar(aes(ymax = upper, 
                            ymin = lower, 
                            col = predictions[, pred.x2]),
                        width = width.error.bars, 
                        size = size.error.bars, 
                        position = position_dodge(0.7)) +
          geom_point(aes(col = predictions[, pred.x2], 
                         shape = predictions[, pred.x2], 
                         fill = predictions[, pred.x2]),  
                     size = size.means, 
                     position = position_dodge(0.7))  +
          scale_x_continuous(breaks = c(1:length(levels(data_summary[, pred.x1]))),         
                             labels = levels(data_summary[, pred.x1]), 
                             limits = c(0.6, length(levels(data_summary[, pred.x1])) + 0.4))} 
      else {# OPTION C: Plot for 1 factor + 1 numeric/int
        plot.2pred <- plot.2pred +
          geom_ribbon(aes(ymin = lower, 
                          ymax = upper, 
                          bg = predictions[, pred.x2]),
                      alpha = alpha.error.bars) +
          geom_line(aes(col = predictions[, pred.x2],
                        linetype = predictions[, pred.x2]),
                    size = 0.8)
        }
      
      dodg <- ifelse(is.factor(data_summary[, pred.x1]), 0.7, 0) # amount of dodging varies with x-vector class
      
      if(is.factor(data_summary[, pred.x1])) {
        plot.2pred <- plot.2pred +    # ADDED 31 Mar 22: Beeswarm plotting
          geom_beeswarm(data = data_summary,  # geom_quasirandom
                        aes(x = as.numeric(data_summary[, pred.x1]),
                            y = mean,
                            group = data_summary[, pred.x2],
                            fill = data_summary[, pred.x2],
                            col = data_summary[, pred.x2],
                            shape = data_summary[, pred.x2]),
                        size = size.points,
                        col = col.outline.points, 
                        alpha = alpha.points,
                        cex = jitter.points,
                        #varwidth = T,
                        #width = 0.1,
                        priority = "density",
                    #    method = "square",  # "quasirandom", "smiley", "compactswarm", "hex", "square"
                        dodge.width = dodg, 
                        groupOnX = TRUE)
        
  # The following lines can go if we agree on sticking to beeswarm:     
  #      plot.2pred <- plot.2pred +
  #        geom_point(data = data_summary,  
  #                   aes(x = as.numeric(data_summary[, pred.x1]),
  #                       y = mean,
  #                       group = data_summary[, pred.x2],
  #                       fill = data_summary[, pred.x2],
  #                       col = data_summary[, pred.x2],
  #                       shape = data_summary[, pred.x2]),
  #                   size = size.points,  #fill = col.fill.points, # shape = shape.points,
  #                   position = position_jitterdodge(dodge.width = dodg, jitter.width = jitter.points),   
  #                   col = col.outline.points, alpha = alpha.points)
        
      }
      if(!is.factor(data_summary[, pred.x1])) {
        if(!is.factor(data_summary[, pred.x2])) {
        plot.2pred <- plot.2pred +
          geom_point(data = data_summary, 
                     aes(x = as.numeric(data_summary[, pred.x1]), 
                         y = mean),   
                     size = size.points, 
                     position = position_jitter(width = jitter.points, height = 0),   
                     col = col.outline.points, 
                     alpha = alpha.points)
        } else {
          plot.2pred <- plot.2pred +
            geom_point(data = data_summary, 
                       aes(x = as.numeric(data_summary[, pred.x1]), 
                           y = mean,  
                           group = data_summary[, pred.x2],   # shape = data_summary[, pred.x2]))
                           shape = data_summary[, pred.x2],   
                           fill = data_summary[, pred.x2]),   
                       size = size.points, 
                       position = position_jitter(width = jitter.points, height = 0),   
                       col = col.outline.points, 
                       alpha = alpha.points)
          
        }
      }
      
      plot.final <- plot.2pred +
        labs(x = pred.x1, y = response, title = plot.title)
      
      if(is.null(leg.title)) {
        leg.title = pred.x2}
      
      plot.final <- plot.final +
        labs(x = pred.x1, y = response, title = plot.title) +
        scale_color_manual(values = grouped.errorbar.col, name = leg.title) +
        scale_fill_manual(values = grouped.fill.col, name = leg.title) +
        scale_shape_manual(values = grouped.shape.points, name = leg.title) +
        scale_linetype_manual(values = grouped.linetype, name = leg.title) + 
        guides(fill = guide_legend(override.aes = list(fill = grouped.fill.col)))
    }
  }
  
  if(!is.null(y.limits) && !is.null(y.breaks)) { # setting y axis breaks and limits
    plot.final <- plot.final + scale_y_continuous(limits = y.limits, breaks = y.breaks)} else {
      
      if(!is.null(y.limits)) {
        plot.final <- plot.final + scale_y_continuous(limits = y.limits)}
      
      if(!is.null(y.breaks)) {
        plot.final <- plot.final + scale_y_continuous(breaks = y.breaks)}}
  
  if(!is.null(y.lab) && !is.null(x.lab)) { # setting y/x axes titles, and plot title
    plot.final <- plot.final + labs(x = x.lab, y = y.lab, title = plot.title)} else {
      
      if(!is.null(y.lab)) {
        plot.final <- plot.final + labs(y = y.lab, title = plot.title)} 
      
      if(!is.null(x.lab)) {
        plot.final <- plot.final + labs(x = x.lab, y = response, title = plot.title)}}
  
  
  if(length(predictors) == 2){ # setting legend position for 2 predictor plots only
    if(is.null(leg.pos)){ 
      plot.final <- plot.final + plot0 +
        theme(legend.position = "none")
    } else {
      if(leg.pos == "inside.top.left") { # top-left inside
        plot.final <- plot.final + plot0.1 +
          theme(legend.justification = c(0.02, 0.98), legend.position = c(0.02, 0.98))
      }
      if(leg.pos == "inside.top.right") { # top-right inside
        plot.final <- plot.final + plot0.1 +
          theme(legend.justification = c(0.98, 0.98), legend.position = c(0.98, 0.98))
      }
      if(leg.pos == "inside.bottom.left") { # bottom-left inside
        plot.final <- plot.final + plot0.1 +
          theme(legend.justification = c(0.02, 0.02), legend.position = c(0.02, 0.02))
      }
      if(leg.pos == "inside.bottom.right") { # bottom-right inside
        plot.final <- plot.final + plot0.1 +
          theme(legend.justification = c(0.98, 0.02), legend.position = c(0.98, 0.02))
      }
      if(leg.pos == "outside.top") { # top outside
        plot.final <- plot.final + plot0.1 +
          theme(legend.position = "top")
      }
      if(leg.pos == "outside.left") { # left outside
        plot.final <- plot.final + plot0.1 +
          theme(legend.position = "left")
      }
      if(leg.pos == "outside.right") { # right outside
        plot.final <- plot.final + plot0.1 +
          theme(legend.position = "right")
      }
      if(leg.pos == "outside.bottom") { # bottom outside
        plot.final <- plot.final + plot0.1 +
          theme(legend.position = "bottom")
      }
    }
  } 
  plot.final <- plot.final + theme(aspect.ratio = A.ratio)
  plot.final
}
