# Run the EVPI calculation
# import EVPI from another file
source("functions/empirical_EVPI.R")

multi_EVPI <-
  function (mc,
            first_out_var,
            write_table = FALSE,
            outfolder = NA,
            n_cores = max(1L, parallel::detectCores() - 1L),
            mc_samples = NULL)
  {
    if (mean(mc[, 1] == 1:nrow(mc)) == 1)
      mc <- mc[, 2:ncol(mc)]

    # Optionally subsample rows before EVPI calculation.
    # EVPI estimates are stable with ~2000-3000 rows and LOESS cost scales
    # super-linearly with N, so subsampling gives large speed gains with
    # minimal accuracy loss (Luedeling & Shepherd, 2016).
    if (!is.null(mc_samples) && mc_samples < nrow(mc)) {
      set.seed(42)
      mc <- mc[sample(nrow(mc), mc_samples), ]
    }

    # which and colnames from base R
    outvar1pos <- which(colnames(mc) == first_out_var)
    outvars <- colnames(mc)[outvar1pos:ncol(mc)]
    # print and paste from base R
    print(paste(
      "Processing",
      length(outvars),
      "output variables using",
      n_cores,
      "cores. This can take some time."
    ))
    outputs <- list()
    for (out_var in outvars) {
      # Parallelise over input variables using forked processes (mclapply).
      # Falls back to sequential on Windows (mc.cores is ignored there).
      input_vars <- colnames(mc)[1:(outvar1pos - 1)]
      raw <- parallel::mclapply(
                          input_vars,
                          function(x)
                            empirical_EVPI(mc, x, out_var)[c("expected_gain",
                                                             "EVPI_do", "EVPI_dont")],
                          mc.cores = n_cores)
      names(raw) <- input_vars
      # Convert list-of-lists to named matrix matching t(sapply(...)) output
      results <- t(sapply(raw, function(r) r))
      outs <- data.frame(
        variable = rownames(results),
        expected_gain = unlist(results[, "expected_gain"]),
        EVPI_do = unlist(results[, "EVPI_do"]),
        EVPI_dont = unlist(results[, "EVPI_dont"])
      )
      decision <- ifelse(mean(outs$expected_gain, na.rm = TRUE) >
                           0, "do", "dont")
      if (decision == "do")
        res <- outs[, c("EVPI_do")]
      if (decision == "dont")
        res <- outs[, c("EVPI_dont")]
      names(res) <- outs[, c("variable")]
      outputs[[out_var]] <- outs
      outputs[[out_var]]$EVPI <- res
      outputs[[out_var]]$decision <- decision
      if (write_table) {
        if (is.na(outfolder))
          outfolder <- "."
        write.csv(outs, paste(file.path(
          outfolder,
          paste("EVPI_table_",
                out_var, ".csv", sep = "")
        )))
      }
      print(paste(
        "Output variable ",
        which(out_var == outvars),
        " (",
        out_var,
        ") completed.",
        sep = ""
      ))
    }
    attr(outputs, "class") <- c("EVPI_outputs", "list")
    return(invisible(outputs))
  }