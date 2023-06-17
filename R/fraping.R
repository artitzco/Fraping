is.dataFrap.class <- function(x) list.compareItem(x, "class", "frap.000000uFpZ4K5kgF0kXZ8RaD5PknOYGvqg99t")

#' frapclass
#'
#' Class representing functionality for FRAP (Fluorescence Recovery After Photobleaching).
#'
#' @format \code{R6Class} object.
#'
#' @field name Name of the class.
#' @field bleach Percentage of sample bleaching.
#' @field fmin Minimum fluorescence recovery value.
#' @field length Length of the class.
#' @field seconds Vector of time in seconds.
#' @field photo Vector of time frames.
#' @field time0 Vector of recovery start times.
#' @field getColor Function to get the current color.
#' @field setColor Function to set the color.
#'
#' @section Usage:
#' \preformatted{frap_obj <- frapclass$new(name, bleach)}
#'
#' @examples
#' # Create an instance of the "frapclass" class
#' frap_obj <- frapclass$new("FRAP1", 30)
#'
#' @export
.frapclass <- R6::R6Class("frapclass",
  private = list(
    Color = NULL,
    Seconds = NULL,
    Photo = NULL,
    Time0 = NULL,
    Time = NULL,
    Area = NULL,
    Input = NULL,
    newAdded = NULL
  ),
  public = list(
    name = NULL,
    bleach = NULL,
    fmin = NULL,
    length = NULL,
    seconds = NULL,
    photo = NULL,
    time0 = NULL,
    getColor = NULL,
    setColor = NULL,
    class = "frap.000000uFpZ4K5kgF0kXZ8RaD5PknOYGvqg99t",
    initialize = function(name, bleach) {
      private$Color <- newVec(color())
      private$Seconds <- newVec()
      private$Photo <- newVec()
      private$Time0 <- newVec()
      private$Time <- newList()
      private$Area <- newList()
      private$Input <- newList()
      private$newAdded <- newVec(FALSE)
      self$name <- name
      self$bleach <- bleach
      self$fmin <- 1 - bleach / 100
      self$length <- private$Time$length
      self$seconds <- private$Seconds$get
      self$photo <- private$Photo$get
      self$time0 <- private$Time0$get
      self$getColor <- private$Color$get
      self$setColor <- private$Color$set
    },
    addDataFrame = function(seconds, photo, areaColumn, inputColumn, dataR, dataC, dataB = NULL) {
      tryCatch(
        {
          dataB <- isnt.null(dataB, {
            lista <- list()
            lista[[areaColumn]] <- rep(1, nrow(dataR))
            lista[[inputColumn]] <- rep(0, nrow(dataR))
            data.frame(lista)
          })
          areaR <- dataR[[areaColumn]][1]
          areaC <- dataC[[areaColumn]][1]
          areaB <- dataB[[areaColumn]][1]
          inputR <- dataR[[inputColumn]]
          inputC <- dataC[[inputColumn]]
          inputB <- dataB[[inputColumn]]
          allright <- is.numeric(areaR) & is.numeric(areaC) & is.numeric(areaB) &
            is.numeric(inputR) & is.numeric(inputC) & is.numeric(inputB) &
            length(inputR) == length(inputC) & length(inputC) == length(inputB)
          if (allright) {
            time <- seq(0, seconds, seconds / (nrow(dataR) - 1))
            private$Time$add(time)
            private$Area$add(list(R = areaR, C = areaC, B = areaB))
            private$Input$add(list(R = inputR, C = inputC, B = inputB))
            private$Seconds$add(seconds)
            private$Photo$add(photo)
            private$Time0$add(time[photo])
            private$newAdded$set(TRUE)
            return()
          }
        },
        error = function(err) {
          message("Error: Failed to update the database")
        }
      )
      message("Error: Failed to update the database")
    },
    addFile = function(seconds, photo, areaColumn, inputColumn, fileR, fileC, fileB = "") {
      tryCatch(
        {
          dataR <- read.csv(fileR)
          dataC <- read.csv(fileC)
          dataB <- if (fileB != "") {
            read.csv(fileB)
          } else {
            lista <- list()
            lista[[areaColumn]] <- rep(1, nrow(dataR))
            lista[[inputColumn]] <- rep(0, nrow(dataR))
            data.frame(lista)
          }
          areaR <- dataR[[areaColumn]][1]
          areaC <- dataC[[areaColumn]][1]
          areaB <- dataB[[areaColumn]][1]
          inputR <- dataR[[inputColumn]]
          inputC <- dataC[[inputColumn]]
          inputB <- dataB[[inputColumn]]
          allright <- is.numeric(areaR) & is.numeric(areaC) & is.numeric(areaB) &
            is.numeric(inputR) & is.numeric(inputC) & is.numeric(inputB) &
            length(inputR) == length(inputC) & length(inputC) == length(inputB)
          if (allright) {
            time <- seq(0, seconds, seconds / (nrow(dataR) - 1))
            private$Time$add(time)
            private$Area$add(list(R = areaR, C = areaC, B = areaB))
            private$Input$add(list(R = inputR, C = inputC, B = inputB))
            private$Seconds$add(seconds)
            private$Photo$add(photo)
            private$Time0$add(time[photo])
            private$newAdded$set(TRUE)
          } else {
            message(paste("Error: Failed to update the database with",
              "\n\t R:", fileR,
              "\n\t C:", fileC,
              "\n\t B:", fileB,
              sep = ""
            ))
          }
        },
        error = function(err) {
          message(paste("Error: Failed to update the database with",
            "\n\t R:", fileR,
            "\n\t C:", fileC,
            "\n\t B:", fileB,
            sep = ""
          ))
        }
      )
    },
    addDir = function(seconds, photo, areaColumn, inputColumn, dirR, dirC, dirB = "") {
      DIRR <- paste(dirR, list.files(dirR), sep = "/")
      DIRC <- paste(dirC, list.files(dirC), sep = "/")
      DIRB <- paste(dirB, list.files(dirB), sep = "/")
      n <- min(c(length(DIRR), length(DIRC), length(DIRB)))
      if (n > 0) {
        for (i in 1:n) {
          self$addFile(
            seconds, photo, areaColumn, inputColumn,
            DIRR[i], DIRC[i], (if (dirB == "") "" else DIRB[i])
          )
        }
      }
    },
    time = function(index = NA, AB = FALSE) {
      if (self$length() == 0) {
        return(NULL)
      }
      if (is.na(index[1])) index <- 1:self$length()
      if (length(index) > 1) {
        return(funList(self$time, index, AB = AB))
      }
      if (!AB) {
        return(private$Time$get(index))
      }
      private$Time$get(index)[
        1:(length(private$Time$get(index)) - private$Photo$get(index) + 1)
      ]
    },
    area = function(index = NA, type = "R") {
      if (self$length() == 0) {
        return(NULL)
      }
      if (is.na(index[1])) index <- 1:self$length()
      if (length(index) > 1) {
        return(funVec(self$area, index, type = type))
      }
      private$Area$get(index)[[type]]
    },
    recover = function(index = NA, type = "RCB", area = TRUE, stand = TRUE, AB = FALSE) {
      if (self$length() == 0) {
        return(NULL)
      }
      if (is.na(index[1])) index <- 1:self$length()
      if (length(index) > 1) {
        return(funList(self$recover, index, type = type, area = area, stand = stand, AB = AB))
      }

      getIntDen <- function(index, type = "R", area = TRUE) {
        private$Input$get(index)[[type]] / (if (area) self$area(index, type) else 1)
      }
      recover <- if (type == "R" | type == "C" | type == "B") {
        getIntDen(index, type, area)
      } else if (type == "RC") {
        getIntDen(index, "R", area) / getIntDen(index, "C", area)
      } else if (type == "RB") {
        getIntDen(index, "R", area) - getIntDen(index, "B", area)
      } else if (type == "CB") {
        getIntDen(index, "C", area) - getIntDen(index, "B", area)
      } else if (type == "RCB") {
        (getIntDen(index, "R", area) - getIntDen(index, "B", area)) /
          (getIntDen(index, "C", area) - getIntDen(index, "B", area))
      } else {
        NULL
      }
      if (stand) {
        recover <- reparam(recover, c(recover[private$Photo$get(index)], recover[1]), c(self$fmin, 1))
      }
      if (AB) {
        recover <- recover[private$Photo$get(index):length(private$Time$get(index))]
      }
      return(recover)
    },
    table = function(index = NA, type = "RCB", area = TRUE, stand = TRUE, AB = FALSE) {
      tim <- self$time(index = index, AB = AB)
      rec <- self$recover(index = index, type = type, area = area, stand = stand, AB = AB)
      tab <- tableLinesInter(
        if (!is.list(tim)) list(tim) else tim,
        if (!is.list(rec)) list(rec) else rec
      )
      private$newAdded$set(FALSE)
      return(tab)
    }
  )
)

#' @title newDataFrap
#'
#' @description Create a new instance of the "frapclass" class.
#'
#' @param name Name of the FRAP object.
#' @param bleach Percentage of sample bleaching.
#'
#' @return An instance of the "frapclass" class.
#'
#' @examples
#' # Create a new FRAP object
#' frap_obj <- newDataFrap("FRAP1", 30)
#'
#' @keywords FRAP class object
#'
#' @export
#'
newDataFrap <- function(name, bleach) {
  .frapclass$new(name, bleach)
}

#' @title newFit
#'
#' @description Create a new instance of the "Fit" class for a given function.
#'
#' @param name Name of the fit.
#' @param fun The fitting function.
#' @param param The parameter(s) of the fitting function.
#' @param interval The interval(s) for each parameter.
#'
#' @return An instance of the "Fit" class.
#'
#' @examples
#' # Create a new Fit object for the Exponential distribution
#' fit_exp <- newFit("Exponential", pexp, "rate", list(c(0, 1)))
#'
#' # Create a new Fit object for the Pareto distribution
#' ppareto <- function(x, shape, scale) {
#'   if (any(x < scale)) return(rep(0, length(x)))
#'   shape * scale^shape / x^(shape + 1)
#' }
#' fit_pareto <- newFit("Pareto", ppareto, c("shape", "scale"), list(c(0, 5), c(0, 100)))
#'
#' @keywords Fit class object
#'
#' @export
#'
newFit <- function(name, fun, param, interval) {
  fn <- newFunction("fun")
  fn$add("fit<-function(", join(param, sep = ", "), ", alpha, fmin, x)", " fmin+alpha*(1-fmin)*fun(x, ", join(pst(param, "=", param), sep = ", "), ")")
  fn$add("function(", join(param, sep = ", "), ", alpha, fmin, x, y) sum(abs(y-fit(", join(param, sep = ", "), ", alpha, fmin, x)))")
  error <- fn$run(fun)
  param <- c(param, "alpha")
  interval <- c(interval, list(c(0, 1)))
  n <- length(param)
  function(data, type = "RCB", area = T, stand = T) { ######### No perder de vista
    if (is.character(data)) {
      return(name)
    }
    code <- join("\\fit", data$name, name, param, round(listToVec(interval, 1:length(interval)), 10), type, area, stand, sep = "_")
    op <- globalVar(code)
    if (!base::is.null(op)) {
      return(op)
    }
    table <- data.frame()
    for (i in 1:data$length()) {
      x <- data$time(i, AB = T)
      y <- data$recover(i, type = type, area = area, stand = stand, AB = T)
      op <- TRY(optima(error, param, interval, fmin = data$fmin, x = x, y = y), {
        print("Optimization failed")
        rep(NA, n + 1)
      })
      table <- rbind(table, op)
    }
    names(table) <- c(param, "error")
    error <- table$error
    table$error <- NULL
    table$fmax <- data$fmin + table$alpha * (1 - data$fmin)
    table$UF <- (1 - table$fmax) / (1 - data$fmin)
    table$MF <- 1 - table$UF
    table$error <- error
    evaluate <- function(table) {
      fn <- newFunction("fun")
      fn$add("fit<-list()")
      fn$add("prob<-list()")
      for (i in 1:nrow(table)) {
        fn$add(
          "fit[[", i, "]]<-function(x) ", data$fmin, "+", table[i, n] * (1 - data$fmin), "*fun(x, ",
          join(pst(param[1:(n - 1)], "=", table[i, 1:(n - 1)]), sep = ", "), ")"
        )
        fn$add("prob[[", i, "]]<-function(x) fun(x, ", join(pst(param[1:(n - 1)], "=", table[i, 1:(n - 1)]), sep = ", "), ")")
      }
      fn$add("return(list(fit=fit, prob=prob))")
      fn$run(fun)
    }
    eva <- evaluate(table)
    op <- list(
      name = name, data = data, evaluate = evaluate, table = table,
      fit = eva$fit, prob = eva$prob, saveable = TRUE, simulated = FALSE, code = code
    )
    globalVar(code, op)
    return(op)
  }
}

#' @title simFit
#'
#' @description Simulate fits based on a given Fit object.
#'
#' @param fit A Fit object obtained from the `newFit` function.
#' @param Nsim Number of simulations to perform.
#' @param seed Seed for reproducibility. Default is NA.
#' @param saveable Boolean indicating whether the simulation results should be saved. Default is !is.na(seed).
#'
#' @return A simulated Fit object.
#'
#' @examples
#' # Simulate fits for the Exponential distribution Fit object
#' sim_exp <- simFit(fit_exp, Nsim = 100, seed = 123)
#'
#' # Simulate fits for the Pareto distribution Fit object
#' sim_pareto <- simFit(fit_pareto, Nsim = 50, seed = 456)
#'
#' @keywords Simulation Fit class object
#'
#' @export
#'
simFit <- function(fit, Nsim = 50, seed = NA, saveable = !is.na(seed)) {
  code <- NULL
  if (!is.na(seed)) set.seed(seed)
  if (saveable & !is.na(seed)) {
    saveable <- TRUE
    code <- join("\\simfit", fit$data$name, fit$name, Nsim, seed, fit$code, sep = "_")
    sft <- globalVar(code)
    if (!base::is.null(sft)) {
      return(sft)
    }
  }
  n <- sum((names(fit$table) == "alpha") * (1:ncol(fit$table))) - 1
  tab <- subset.data.frame(fit$table, select = 1:(n + 1))
  fn <- newFunction("tab")
  fn$add("lm(tab$alpha~", join(pst("tab$", colnames(tab)[1:n]), sep = "+"), ")$coefficients")
  coef <- fn$run(tab)

  one <- 1
  table <- cbind(one, simTable(subset.data.frame(fit$table, select = 1:n), Nsim))
  alpha <- NULL
  for (i in 1:nrow(table)) {
    alpha[i] <- sum(table[i, ] * coef)
  }
  table$alpha <- alpha
  table$one <- NULL

  table$fmax <- fit$data$fmin + table$alpha * (1 - fit$data$fmin)
  table$UF <- (1 - table$fmax) / (1 - fit$data$fmin)
  table$MF <- 1 - table$UF
  eva <- fit$evaluate(table)
  sft <- list(
    name = pst(fit$name, "Sim"), data = fit$data, table = table,
    fit = eva$fit, prob = eva$prob, saveable = saveable, simulated = TRUE, code = code
  )
  if (saveable) globalVar(code, sft)
  return(sft)
}

#' @title tableFit
#'
#' @description Generate a table of fitted values based on a given Fit object.
#'
#' @param fit A Fit object obtained from the `newFit` function.
#' @param index Indices of the fits to include in the table. Default is NA (all fits).
#' @param timelim Time limits for the table. Default is NULL (uses full time range).
#' @param npoints Number of points in the table. Default is 100.
#' @param displacement Boolean indicating whether to include time displacement. Default is FALSE.
#'
#' @return A table of fitted values.
#'
#' @examples
#' # Generate a table of fitted values for all fits in the Fit object
#' table_all <- tableFit(fit)
#'
#' # Generate a table of fitted values for specific fits in the Fit object
#' table_subset <- tableFit(fit, index = c(1, 3, 5))
#'
#' @keywords Table Fit class object
#'
#' @export
#'
tableFit <- function(fit, index = NA, timelim = NULL, npoints = 100, displacement = F) {
  code <- NULL
  if (fit$saveable) {
    code <- join("\\fitable", fit$data$name, fit$name, index, timelim, npoints, displacement, index, fit$code, sep = "_")
    ft <- globalVar(code)
    if (!base::is.null(ft)) {
      return(ft)
    }
  }
  n <- nrow(fit$table)
  if (!is.null(timelim)) {
    minTime <- timelim[1]
    maxTime <- timelim[2]
  } else {
    minTime <- 0
    maxTime <- max(fit$data$seconds() - fit$data$time0())
  }
  if (is.na(index[1])) index <- 1:n
  DISP <- IF(displacement & !fit$simulated, fit$data$time0(), rep(0, n))
  tim <- function(i) seq(minTime + DISP[i], maxTime + min(DISP), (maxTime - minTime) / (npoints - 1))
  TIM <- funList(function(i) tim(i), index)
  FIT <- funList(function(i) (fit$fit[[i]])(tim(i) - DISP[i]), index)
  ft <- list(TIM = TIM, FIT = FIT, table = tableLinesInter(TIM, FIT), code = code)
  if (fit$saveable) globalVar(code, ft)
  return(ft)
}

#' @title plotRecover
#'
#' @description Plot the recovery curves for multiple data sets.
#'
#' @param ... One or more data sets to plot, each of class `dataFrap.class`.
#' @param index Indices of the data sets to include in the plot. Default is NA (all data sets).
#' @param type A vector or list of recovery curve types. Default is "RCB".
#' @param area A vector or list indicating whether to use area normalization. Default is TRUE.
#' @param stand A vector or list indicating whether to apply standardization. Default is TRUE.
#' @param AB A vector or list indicating whether to include pre-bleach data. Default is FALSE.
#' @param new.plot Boolean indicating whether to create a new plot. Default is TRUE.
#' @param plot.lines Boolean indicating whether to plot recovery curves as lines. Default is FALSE.
#' @param plot.points Boolean indicating whether to plot recovery curves as points. Default is FALSE.
#' @param plot.shadow Boolean indicating whether to plot recovery curve shadows. Default is FALSE.
#' @param plot.mean Boolean indicating whether to plot the mean recovery curve. Default is FALSE.
#' @param col Colors to use for the plot. If NULL, colors are automatically assigned.
#' @param getGroup Boolean indicating whether to return the group function. Default is FALSE.
#'
#' @return If `getGroup` is TRUE, returns a function for grouping data. Otherwise, generates the plot.
#'
#' @examples
#' # Generate a plot of recovery curves for all data sets
#' plotRecover(data1, data2, data3)
#'
#' # Generate a plot of recovery curves for specific data sets
#' plotRecover(data1, data2, data3, index = c(1, 3, 5))
#'
#' # Return the group function for custom grouping
#' group_func <- plotRecover(getGroup = TRUE)
#' custom_group <- group_func(data1, data2)
#'
#' @keywords Plot Recovery curves multiple
#'
#' @export
#'
plotRecover <- function(..., index = NA, type = "RCB", area = T, stand = T, AB = F, # you########No perder de vista
                        # Plot
                        new.plot = T, plot.lines = F, plot.points = F,
                        plot.shadow = F, plot.mean = F, col = NULL, getGroup = F) {
  type <- iterVector(type, cyclic = T)
  area <- iterVector(area, cyclic = T)
  stand <- iterVector(stand, cyclic = T)
  AB <- iterVector(AB, cyclic = T)
  groupRecover <- function(data) {
    list(
      X = data$time(index = index, AB = AB$This()),
      Y = data$recover(index = index, type = type$This(), area = area$This(), stand = stand$This(), AB = AB$This()),
      table = data$table(index = index, type = type$Next(), area = area$Next(), stand = stand$Next(), AB = AB$Next()),
      col = fixCol(data$getColor()), class = group.class()
    )
  }
  if (getGroup) {
    return(groupRecover)
  }
  ret <- substrae.LIST.DAT(...,
    CONDITION = is.dataFrap.class,
    TRANSFORM = function(x) groupRecover(x)
  )
  ret$lista.123$xlab <- "Time (min)"

  ret$lista.123$ylab <- "Fluorescence"

  plotGroup(
    lista.123 = ret$lista.123, data.123 = ret$data.123,
    new.plot = new.plot, plot.lines = plot.lines, plot.points = plot.points,
    plot.shadow = plot.shadow, plot.mean = plot.mean, col = col
  )
}

#' @title plotFit
#'
#' @description Plot the fitted or simulated recovery curves for multiple data sets.
#'
#' @param ... One or more data sets to plot, each of class `dataFrap.class`.
#' @param fit An optional fit object obtained from the `newFit` function.
#' @param index Indices of the data sets to include in the plot. Default is NA (all data sets).
#' @param type A vector or list of recovery curve types. Default is "RCB".
#' @param area A vector or list indicating whether to use area normalization. Default is TRUE.
#' @param stand A vector or list indicating whether to apply standardization. Default is TRUE.
#' @param simulated A vector or list indicating whether to plot simulated recovery curves. Default is FALSE.
#' @param Nsim A vector or list specifying the number of simulations to perform. Default is 50.
#' @param seed A vector or list specifying the seed for the random number generator. Default is NA (no seed).
#' @param npoints Number of points to use for generating the fitted/simulated curves. Default is 100.
#' @param displacement Boolean indicating whether to displace the curves by their respective time0 values. Default is FALSE.
#' @param new.plot Boolean indicating whether to create a new plot. Default is TRUE.
#' @param plot.lines Boolean indicating whether to plot the fitted/simulated curves as lines. Default is FALSE.
#' @param plot.points Boolean indicating whether to plot the fitted/simulated curves as points. Default is FALSE.
#' @param plot.shadow Boolean indicating whether to plot the shadow area around the fitted/simulated curves. Default is FALSE.
#' @param plot.mean Boolean indicating whether to plot the mean fitted/simulated curve. Default is FALSE.
#' @param col Colors to use for the plot. If NULL, colors are automatically assigned.
#'
#' @examples
#' # Generate a plot of fitted recovery curves for all data sets
#' plotFit(data1, data2, data3, fit = myFit)
#'
#' # Generate a plot of fitted recovery curves for specific data sets
#' plotFit(data1, data2, data3, fit = myFit, index = c(1, 3, 5))
#'
#' # Generate a plot of simulated recovery curves
#' plotFit(data1, data2, data3, fit = myFit, simulated = TRUE, Nsim = 100)
#'
#' @keywords Plot Fitted recovery curves simulated multiple
#'
#' @export
#'
plotFit <- function(..., fit, index = NA, type = "RCB", area = T, stand = T, ######### No perder de vista
                    simulated = F, Nsim = 50, seed = NA,
                    npoints = 100, displacement = F,
                    new.plot = T, plot.lines = F, plot.points = F,
                    plot.shadow = F, plot.mean = F, col = NULL) {
  type <- iterVector(type, cyclic = T)
  area <- iterVector(area, cyclic = T)
  stand <- iterVector(stand, cyclic = T)
  simulated <- iterVector(simulated, cyclic = T)
  Nsim <- iterVector(Nsim, cyclic = T)
  seed <- iterVector(seed, cyclic = T)
  fit <- iterList(IF(is.list(fit), fit, list(fit)), cyclic = T)
  groupFit <- function(data) {
    type <- type$Next()
    area <- area$Next()
    stand <- stand$Next()
    simulated <- simulated$Next()
    Nsim <- Nsim$Next()
    seed <- seed$Next()
    col <- data$getColor()
    fit <- fit$Next()
    if (is.null(fit)) {
      return(plotRecover(index = index, type = type, area = area, stand = stand, AB = !displacement, col = col, getGroup = T)(data))
    }
    ft <- fit(data, type = type, area = area, stand = stand)
    if (simulated) ft <- simFit(ft, Nsim = Nsim, seed = seed)
    tab <- tableFit(ft, index = index, timelim = list(...)$xlim, npoints = npoints, displacement = displacement)
    list(X = tab$TIM, Y = tab$FIT, table = tab$table, col = fixCol(col), class = group.class())
  }
  ret <- substrae.LIST.DAT(...,
    CONDITION = is.dataFrap.class,
    TRANSFORM = function(x) groupFit(x)
  )
  ret$lista.123$xlab <- "Time (min)"
  ret$lista.123$ylab <- "Fluorescence"
  plotGroup(
    lista.123 = ret$lista.123, data.123 = ret$data.123,
    new.plot = new.plot, plot.lines = plot.lines, plot.points = plot.points,
    plot.shadow = plot.shadow, plot.mean = plot.mean, col = col
  )
}

#' @title compareMean
#'
#' @description Compare the mean recovery curves between two data sets.
#'
#' @param data1 The first data set, of class `dataFrap.class`.
#' @param data2 The second data set, of class `dataFrap.class`.
#' @param fit An optional fit object obtained from the `newFit` function.
#' @param type A vector or list of recovery curve types. Default is "RCB".
#' @param area A vector or list indicating whether to use area normalization. Default is TRUE.
#' @param stand A vector or list indicating whether to apply standardization. Default is TRUE.
#' @param simulated A vector or list indicating whether to perform simulations. Default is FALSE.
#' @param Nsim A vector or list specifying the number of simulations to perform. Default is 50.
#' @param seed A vector or list specifying the seed for the random number generator. Default is NA (no seed).
#' @param saveable A vector or list indicating whether to save the simulation results. Default is TRUE if seed is provided, otherwise FALSE.
#' @param conf.level Confidence level for hypothesis testing. Default is 0.95.
#' @param alternative The alternative hypothesis for hypothesis testing. Default is "two.sided".
#' @param return Boolean indicating whether to return the test results. Default is FALSE.
#' @param p.value Boolean indicating whether to plot the p-value. Default is TRUE.
#' @param npoints Number of points to use for generating the comparison plot. Default is 100.
#' @param new.plot Boolean indicating whether to create a new plot. Default is TRUE.
#' @param plot.lines Boolean indicating whether to plot the comparison as lines. Default is TRUE.
#' @param plot.points Boolean indicating whether to plot the comparison as points. Default is FALSE.
#' @param col Colors to use for the plot. If NULL, colors are automatically assigned.
#' @param ... Additional plot parameters.
#'
#' @return If `return` is TRUE, returns the test results. Otherwise, generates the comparison plot.
#'
#' @examples
#' # Compare the mean recovery curves between two data sets
#' compareMean(data1, data2, fit = myFit)
#'
#' # Compare the mean recovery curves and return the test results
#' result <- compareMean(data1, data2, fit = myFit, return = TRUE)
#' print(result)
#'
#' @keywords Compare Mean recovery curves data sets hypothesis test
#'
#' @export
#'
compareMean <- function(data1, data2, fit,
                        type = "RCB", area = T, stand = T,
                        simulated = F, Nsim = 50, seed = NA, saveable = !is.na(seed),
                        conf.level = 0.95, alternative = "two.sided",
                        return = F, p.value = T, npoints = 100,
                        new.plot = T, plot.lines = T, plot.points = F, col = NULL,
                        ...) {
  lst <- list(...)
  fit <- iterList(IF(is.list(fit), fit, list(fit)), cyclic = T)
  type <- iterVector(type, cyclic = T)
  area <- iterVector(area, cyclic = T)
  stand <- iterVector(stand, cyclic = T)
  fit1 <- fit$Next()(data1, type = type$Next(), area = area$Next(), stand = stand$Next())
  fit2 <- fit$Next()(data2, type = type$Next(), area = area$Next(), stand = stand$Next())

  saveable <- iterVector(saveable, cyclic = T)
  seed <- iterVector(seed, cyclic = T)
  Nsim <- iterVector(Nsim, cyclic = T)
  simulated <- iterVector(simulated, cyclic = T)
  # simulated$Restart()
  if (simulated$Next()) fit1 <- simFit(fit1, Nsim = Nsim$Next(), seed = seed$Next(), saveable = saveable$Next())
  if (simulated$Next()) fit2 <- simFit(fit2, Nsim = Nsim$Next(), seed = seed$Next(), saveable = saveable$Next())
  if (is.null(lst$xlim)) {
    lst$xlim <- c(0, min(
      fit1$data$seconds() - fit1$data$time0(),
      fit2$data$seconds() - fit2$data$time0()
    ))
  }
  table1 <- tableFit(fit1, timelim = lst$xlim, npoints = npoints, displacement = F)
  table2 <- tableFit(fit2, timelim = lst$xlim, npoints = npoints, displacement = F)
  if (p.value) {
    pval <- NULL
    ttest <- NULL
    for (i in 1:npoints) {
      group1 <- table1$table$tab[i, ]
      group2 <- table2$table$tab[i, ]
      ttest[[i]] <- TRY(t.test(group1, group2, conf.level = conf.level, alternative = alternative), list(p.value = NA))
      pval[i] <- ttest[[i]]$p.value
    }
    line <- pval
    if (new.plot) {
      refy <- rep(1 - conf.level, 2)
      if (is.null(lst$ylim)) lst$ylim <- c(0, max(refy, pval, na.rm = T))
      lst$xlab <- "Time (min)"
      lst$ylab <- "Probability"
    }
  } else {
    line <- table1$table$mean - table2$table$mean
    if (new.plot) {
      if (is.null(lst$ylim)) lst$ylim <- c(min(line, na.rm = T), max(line, na.rm = T))
      lst$xlab <- "Time (min)"
      lst$ylab <- "Difference"
      refy <- rep(0, 2)
    }
  }
  if (return) {
    return(if (p.value) list(t.test = ttest, p.value = line) else line)
  }

  if (new.plot) {
    G1 <- list(X = lst$xlim, Y = refy, col = color("#333333"), class = group.class())
    G2 <- list(X = table1$table$z, Y = line, col = color("blue"), class = group.class())
    plotGroup(
      data.123 = newList(G1, G2), lista.123 = lst,
      plot.lines = plot.lines, plot.points = plot.points,
      plot.shadow = F, plot.mean = F, col = col
    )
  } else {
    G2 <- list(X = table1$table$z, Y = line, col = color("blue"), class = group.class())
    linesGroup(
      data.123 = newList(G2), lista.123 = lst,
      plot.lines = plot.lines, plot.points = plot.points,
      plot.shadow = F, plot.mean = F, col = col
    )
  }
}

#' @title compareParam
#'
#' @description Compare a specific parameter between two data sets.
#'
#' @param data1 The first data set, of class `dataFrap.class`.
#' @param data2 The second data set, of class `dataFrap.class`.
#' @param fit An optional fit object obtained from the `newFit` function.
#' @param param The name of the parameter to compare.
#' @param type A vector or list of recovery curve types. Default is "RCB".
#' @param area A vector or list indicating whether to use area normalization. Default is TRUE.
#' @param stand A vector or list indicating whether to apply standardization. Default is TRUE.
#' @param simulated A vector or list indicating whether to perform simulations. Default is FALSE.
#' @param Nsim A vector or list specifying the number of simulations to perform. Default is 50.
#' @param seed A vector or list specifying the seed for the random number generator. Default is NA (no seed).
#' @param conf.level Confidence level for hypothesis testing. Default is 0.95.
#' @param alternative The alternative hypothesis for hypothesis testing. Default is "two.sided".
#' @param return Boolean indicating whether to return the test results. Default is FALSE.
#' @param new.plot Boolean indicating whether to create a new plot. Default is TRUE.
#' @param plot.lines Boolean indicating whether to plot the comparison as lines. Default is TRUE.
#' @param plot.points Boolean indicating whether to plot the comparison as points. Default is FALSE.
#' @param col Colors to use for the plot. If NULL, colors are automatically assigned.
#' @param ... Additional plot parameters.
#'
#' @return If `return` is TRUE, returns the test results. Otherwise, generates the comparison plot.
#'
#' @examples
#' # Compare the parameter between two data sets
#' compareParam(data1, data2, fit = myFit, param = "alpha")
#'
#' # Compare the parameter and return the test results
#' result <- compareParam(data1, data2, fit = myFit, param = "alpha", return = TRUE)
#' print(result)
#'
#' @keywords Compare Parameter data sets hypothesis test
#'
#' @export
#'
compareParam <- function(data1, data2, fit, param,
                         type = "RCB", area = T, stand = T,
                         simulated = F, Nsim = 50, seed = NA,
                         conf.level = 0.95, alternative = "two.sided", return = F,
                         new.plot = T, plot.lines = T, plot.points = F, col = NULL,
                         ...) {
  lst <- list(...)
  fit <- iterList(IF(is.list(fit), fit, list(fit)), cyclic = T)
  type <- iterVector(type, cyclic = T)
  area <- iterVector(area, cyclic = T)
  stand <- iterVector(stand, cyclic = T)
  fit1 <- fit$Next()(data1, type = type$Next(), area = area$Next(), stand = stand$Next())
  fit2 <- fit$Next()(data2, type = type$Next(), area = area$Next(), stand = stand$Next())
  simulated <- iterVector(simulated, cyclic = T)
  Nsim <- iterVector(Nsim, cyclic = T)
  seed <- iterVector(seed, cyclic = T)
  if (simulated$Next()) fit1 <- simFit(fit1, Nsim = Nsim$Next(), seed = seed$Next())
  if (simulated$Next()) fit2 <- simFit(fit2, Nsim = Nsim$Next(), seed = seed$Next())
  p <- (0:100) / 100
  n1 <- sum((names(fit1$table) == param) * (1:ncol(fit1$table)))
  n2 <- sum((names(fit2$table) == param) * (1:ncol(fit2$table)))
  param1 <- fit1$table[, n1]
  param2 <- fit2$table[, n2]
  name1 <- join(names(fit1$table)[n1], fit1$name, fit1$data$name, sep = "_")
  name2 <- join(names(fit2$table)[n2], fit2$name, fit2$data$name, sep = "_")
  if (name1 == name2) {
    name1 <- pst(name1, "1", sep = "_")
    name2 <- pst(name2, "2", sep = "_")
  }
  fn <- newFunction(name1, name2)
  fn$add("t.test(", name1, ", ", name2, ", conf.level=", conf.level, ", alternative=\"", alternative, "\")")
  # fn$getCode()
  ##### Plot
  if (return) {
    return(fn$run(param1, param2))
  }
  name1 <- join(names(fit1$table)[n1], fit1$name, fit1$data$name, sep = " ")
  name2 <- join(names(fit2$table)[n2], fit2$name, fit2$data$name, sep = " ")
  if (name1 == name2) {
    name1 <- pst(name1, "1", sep = " ")
    name2 <- pst(name2, "2", sep = " ")
  }
  q1 <- quantile(param1, p)
  q2 <- quantile(param2, p)
  if (new.plot) {
    lim <- c(min(q1, q2), max(q1, q2))
    if (is.null(lst$xlim)) lst$xlim <- lim
    if (is.null(lst$ylim)) lst$ylim <- lim
    lst$xlab <- name1
    lst$ylab <- name2
    ref <- c(max(lst$xlim), min(lst$ylim))

    G1 <- list(X = ref, Y = ref, col = color("#333333"), class = group.class())
    G2 <- list(X = q1, Y = q2, col = color("blue"), class = group.class())
    plotGroup(
      data.123 = newList(G1, G2), lista.123 = lst,
      plot.lines = plot.lines, plot.points = plot.points,
      plot.shadow = F, plot.mean = F, col = col
    )
  } else {
    G2 <- list(X = q1, Y = q2, col = color("blue"), class = group.class())
    linesGroup(
      data.123 = newList(G2), lista.123 = lst,
      plot.lines = plot.lines, plot.points = plot.points,
      plot.shadow = F, plot.mean = F, col = col
    )
  }
}

#' @title compareFit
#'
#' @description Compare the fit of multiple models using histograms and Q-Q plots.
#'
#' @param data The data set, of class `dataFrap.class`.
#' @param fit A single fit object or a list of fit objects obtained from the `newFit` function.
#' @param col Colors to use for the plots. If NULL, colors are automatically assigned.
#' @param digits Number of digits to round the error values. Default is 4.
#' @param ... Additional parameters passed to the underlying functions.
#'
#' @keywords Compare Fit models histograms Q-Q plot
#'
#' @examples
#' # Compare the fit of multiple models using histograms and Q-Q plots
#' compareFit(data, fit = list(fit1, fit2, fit3))
#'
#' @export
#'
compareFit <- function(data, fit, col = NULL, digits = 4, ...) {
  if (!is.list(fit)) fit <- list(fit)
  myhist <- function(x, digits, ...) {
    lst <- list(...)
    lst$alp.abline <- lst$alp.mean
    lst$col.abline <- lst$col.mean
    lst$lty.abline <- lst$lty.mean
    lst$lwd.abline <- lst$lwd.mean
    lst$col.abline.auxiliar <- lst$col.mean.auxiliar

    lst$alp.lin <- lst$alp.mean
    lst$col.lin <- lst$col.mean
    lst$lty.lin <- lst$lty.mean
    lst$lwd.lin <- lst$lwd.mean
    lst$col.lin.auxiliar <- lst$col.mean.auxiliar

    mhist(x, param = lst, xlab = "Error", ylab = "Density", xdigits = digits, ydigits = digits)
    mn <- round(mean(x), digits = digits)
    mabline(v = mn, sld.abline = F, param = lst)
    mlegend(isnt.null(lst$pos.legend, "top"), join("Mean = ", mn), param = lst)
  }
  n <- length(fit)
  col <- fixCol(isnt.null(col, color.rainbow(n)))
  colInv <- col$inverse()
  par(mfrow = c(n, n))
  for (i in 1:n) {
    cls <- col$getCol()
    clsInv <- colInv$getCol()
    for (j in 1:n) {
      if (i == j) {
        myhist(fit[[i]](data)$table$error, digits,
          col.mean.auxiliar = clsInv,
          col.hist.auxiliar = cls,
          main = pst("Histogram of error\n", fit[[i]]("")),
          ...
        )
      } else {
        compareParam(data, data,
          fit = l(fit[[i]], fit[[j]]),
          xlab = fit[[i]](""),
          ylab = fit[[j]](""),
          main = "Q-Q Plot",
          xdigits = digits,
          ydigits = digits,
          param = "error", ...
        )
      }
    }
  }
  par(mfrow = c(1, 1))
}
