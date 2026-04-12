extract_anti_diags <- function(mat) {
  # Elements on the same bottom-left to top-right diagonal
  # have the same sum of row and column indices.
  diag_groups <- row(mat) + col(mat)
  
  # Split the matrix elements into a list based on these sums
  diags <- split(mat, diag_groups)
  
  # Optional: Remove names from the list for cleaner output
  names(diags) <- NULL
  return(diags)
}

plot_bs <- function(case_ratios, stan_object, x_lim) {
  par(mfcol = c(2, 1))
  hist(x = case_ratios, main = 'Observed Case Reserve Ratios', xlab = '', 
    axes = FALSE, xlim = x_lim)
  axis(side = 1, at = axTicks(side = 1), labels = easyr::fmat(x = 
      axTicks(side = 1), type = '%'))
  axis(side = 2, las = 2)
  
  hist(x = (rstan::extract(object = stan_object)$alpha / 
      rstan::extract(object = stan_object)$beta / 
      (1 + rstan::extract(object = stan_object)$trend)), 
    main = 'Fitted Case Reserve Ratios', xlab = '', axes = FALSE, xlim = x_lim)
  axis(side = 1, at = axTicks(side = 1), labels = easyr::fmat(x = 
      axTicks(side = 1), type = '%'))
  axis(side = 2, las = 2)
}


bs_ult <- function(rptd, paid, row_names = NULL) {
  no_cols <- dim(rptd)[2]
  df_mat <- rptd[, 2:no_cols] / rptd[, 1:(no_cols - 1)]
  ata <- apply(X = df_mat, MARGIN = 2, FUN = mean, na.rm = TRUE)
  atu <- c(1, ata |> rev() |> cumprod())
  latest_rptd <- sapply(X = 1:no_cols, function(x) rptd[x, no_cols + 1 - x])
  latest_paid <- sapply(X = 1:no_cols, function(x) paid[x, no_cols + 1 - x])
  ultimate <- latest_rptd * atu
  matrix_out <- matrix(data = c(latest_rptd, atu, ultimate, latest_paid),
    ncol = 4, byrow = FALSE)
  matrix_out <- cbind(matrix_out, matrix_out[,1] * matrix_out[,2] -
      matrix_out[,4])
  colnames(matrix_out) <- c('rptd', 'atu', 'ult', 'paid', 'reserve')
  rownames(matrix_out) <- row.names(rptd)
  return(matrix_out)
}


