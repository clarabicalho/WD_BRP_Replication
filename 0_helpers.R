
# Cleaning functions

expand_mchoice <- function(var_name){
  var <- dat[[var_name]]
  max_elements <- max(nchar(as.character(var)), na.rm = TRUE)
  n_cols <- ceiling(max_elements/2)
  v <- lapply(str_split(var,  pattern = ","), as.numeric)
  max_value <- max(unlist(lapply(v, max)), na.rm = TRUE)
  # v <- str_split_fixed(as.character(var),  pattern = ",", n = n_cols)
  out <- do.call(rbind,
                 lapply(v, function(x) sapply(1:max_value, function (i) as.numeric(i %in% x))))
  out <- as.data.frame(out)
  # out <- apply(out, 2, as.numeric)
  colnames(out) <- paste0(var_name, "_", 1:max_value)
  return(out)
}

mchoice_long <- function(var = "fact_trueforyou_"){
  w_vname <- startsWith(names(dat), var) & !endsWith(names(dat), "_TEXT")
  vname <- names(dat)[w_vname]
  dat_out <- dat[,c("country", "ResponseId", vname)] %>%
    pivot_longer(cols = vname, values_to = gsub("_$", "", var),
                 values_drop_na= TRUE)
  return(dat_out)
}

# Coding text variable functions

code_terms <- function(terms, var, ...){
  has <- sapply(terms, function(t){
    a <- grepl(t, var, ignore.case = TRUE, ...)
    a[is.na(var)] <- NA
    a
  })
  1*apply(has, 1, any)
}


# Wranglers ---------------------------------------------------------------
#' Wrap string by other strings
#' @param vector A character vector. Elements to be wrapped.
#' @param left A string. String pasted to the left of each element.
#' @param right A string. String pasted to the right of each vector.
#' @export
#' @example
#' se  <- round(runif(3), 3)
#' wrap_str(se)
wrap_str <- function(vector, left = "(", right = ")"){
  sapply(vector, function(v)
    ifelse(is.na(v) || v == "", "", paste0(left, v , right)))
}

#' Weave elements of two matrices of vectors
#' Weaves two matrices or vectors whereby rows (elements) of first matrix (vector) are interspersed with rows (elements) of the second matrix, one by one.
#' @param a A vector of matrix. Rows (elements) to be weaved.
#' @param b A vector or matrix. Rows (elements) to weave into \code{a}.
#' @param inpar_b Logical. Whether to enclose elements in \code{b} with parenthesis.
#' @param rnames A character vector. Names of elements in each row of \code{a} (assumed the same in \code{b}).
#' @param excl_0 Logical. Whether to exclude values of `0` in \code{b}.
#' @param within_col Logical. If \code{a} and \code{b} are vectors, `TRUE` weaves elements into one column and `FALSE` returns \code{a} in the first row, and \code{b} on the second row.
#' @export
#' @examples
#' weave(letters[1:5], 1:5, inpar_b = FALSE)
#' means <- round(runif(3), 3)
#' sds <- round(runif(10), 3)
#' weave(means, sds, rnames = c("var1", "var2", "var3"))
weave <- function(a, b, inpar_b = TRUE, rnames = NULL, excl_0 = TRUE, within_col = TRUE){
  if(!identical(dim(a), dim(b))) stop("Input matrices should be the same length")
  if(is.vector(a)){
    if(within_col){
      a <- matrix(a, ncol = 1)
      b <- matrix(b, ncol = 1)
    }else{
      a <- matrix(a, nrow = 1)
      b <- matrix(b, nrow = 1)
    }
  }
  if(!is.matrix(a)) a <- as.matrix(a)
  if(!is.matrix(b)) b <- as.matrix(b)
  
  matout <- matrix(NA, nrow(a)*2, ncol(a))
  for(i in 1:nrow(a)){
    matout[(2*i-1),] <- a[i,]
    if(inpar_b)
      matout[(2*i),] <- wrap_str(b[i,])
    else
      matout[(2*i),] <- b[i,]
  }
  
  if(!is.null(rnames)){
    rnames <- rep(rnames, each = 2)
    rmn <- 1:length(rnames)%%2 == 0
    rnames[rmn] <- ""
  }
  
  if(!is.null(colnames(a))) colnames(matout) <- colnames(a)
  matout <- cbind(rnames, matout)
  matout <- gsub("NaN", "", matout, fixed = TRUE)
  if(excl_0) matout <- gsub("(0)", "", matout, fixed = TRUE)
  
  return(matout)
}

#' Format table
#'
#' Wrapper for \code{xtable} with additional formatting options.
#' @param tab
#' @param add.note
#' @param caption
#' @param type
#' @param column.sep.width
#' @param header
#' @param table.placement
#' @param omit.table.layout
#' @param include.colnames
#' @param include.rownames
#' @param comment
#' @param ...
#'
format_table <- function(tab, add.note = "",
                         caption = "", print.sign = FALSE,
                         type = "latex",
                         column.sep.width = "1pt",
                         header = F, table.placement = "htb",
                         omit.table.layout = "n",
                         include.colnames = FALSE,
                         include.rownames = FALSE,
                         comment = FALSE, ...){
  to_print <- capture.output(print(
    xtable(tab, caption = caption, ...),
    type = type,
    column.sep.width = column.sep.width,
    header = header, table.placement = table.placement,
    omit.table.layout = omit.table.layout,
    include.colnames = include.colnames,
    include.rownames = include.rownames,
    comment = comment, caption.placement = "top", ...))
  
  if(type == "latex"){
    ptext <- ifelse(print.sign, "$^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$.", "")
    notes <- paste("\\begin{flushleft}\\textit{Note:}", add.note, ptext, "\\end{flushleft}")
    if(add.note == "" && !print.sign) notes <- ""
    to_print <- c(to_print[1:(length(to_print)-1)], notes, to_print[length(to_print)])
  }
  
  to_print
}

write_table <- function(to_export, output_file){
  writeLines(to_export, file(output_file))
  close.connection(file(output_file))
}

render_table <- function(table_file, output_file = "test_table.pdf"){
  Rmd <- gsub("pdf","Rmd", output_file, fixed = TRUE)
  writeLines(paste0("\\input{", table_file, "}"), file(Rmd))
  close.connection(file(Rmd))
  rmarkdown::render(Rmd, output_format = "pdf_document")
  file.remove(Rmd)
  closeAllConnections()
}

star <- function(pvalue){
  sign <- case_when(pvalue <= 0.001 ~ "***",
                    pvalue > 0.001 & pvalue <= 0.01 ~ "**",
                    pvalue > 0.01 & pvalue <= 0.05 ~ "*",
                    pvalue > 0.05 ~ "")
  if(any(is.na(sign))){
    sign[is.na(sign)] <- ""
    message("Some estimates are missing p-value")
  }
  return(sign)
}

# format_output <- function(..., notes = ""){
#   t <- capture.output(stargazer(..., df = FALSE, column.sep.width = "1pt",
#                                 omit = c("Constant", "m"),
#                                 omit.stat = c("adj.rsq", "ser"),
#                                 header = F, table.placement = "htb",
#                                 omit.table.layout = "n"))
#   notes <- paste("\\begin{flushleft}\\textit{Note:}", notes, "$^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$. \\end{flushleft}")
#   c(t[1:(length(t)-1)], notes, t[length(t)])
# }

# Summary stats -----------------------------------------------------------




# Main Results ------------------------------------------------------------

make_wt <- function(vars, data = dat){
  df <- data %>% drop_na(vars)
  df <- df %>%
    group_by(country) %>%
    dplyr::mutate(ctry_sum_wt = n(),
                  wt = 1/(3 * ctry_sum_wt)) %>% 
    dplyr::select(-ctry_sum_wt) %>%
    ungroup()
}
  
estimate_dim <- function(condition1 = "C", 
                         condition2 = "T1", 
                         country = NULL, 
                         y = "hypo_answer_scaled",
                         weights = TRUE){
  df <- dat %>% 
    tidyr::drop_na(hypo_condition, country) %>%
    tidyr::drop_na(y) %>%
    filter(hypo_condition %in% c(condition1, condition2))
  
  if(is.null(country)){
    df <- df %>%
      group_by(country) %>%
      dplyr::mutate(ctry_sum_wt = n(),
                    wt = 1/(3 * ctry_sum_wt)) %>% 
      dplyr::select(-ctry_sum_wt) %>%# create weight
      ungroup()
  }
  
  if(is.null(country) & weights){
    estimatr::difference_in_means(
      as.formula(paste0(y , "~ hypo_condition")), 
      data = df,
      weights = wt,
      blocks = country,
      condition1 = condition1,
      condition2 = condition2)
  } else {
    if(is.null(country) & !weights){
      estimatr::difference_in_means(
        as.formula(paste0(y , "~ hypo_condition")), 
        data = df,
        blocks = country,
        condition1 = condition1,
        condition2 = condition2)
    } else{
      estimatr::difference_in_means(
        hypo_answer ~ hypo_condition, 
        data = df[df$country == country,],
        condition1 = condition1,
        condition2 = condition2)
    }
  }
}

estimate_lin <- function(
  formula = hypo_answer_scaled ~ hypo_condition, 
  data, 
  cntry = NULL, 
  cov = c("dem_age", "female", 
          "dem_edu", "dem_religiosity", #"dem_rel",
          # "rel_catholic", "rel_protestant", 
          # "rel_evangelical", "rel_muslim", 
          "occ_student", 
          "occ_midlev", "occ_uprlev", "occ_never", "occ_manual", "loc_urbrur", 
          "voted_copartisan", "lockdown", "country"))
  {
  
  if(is.null(cntry)){
    
    dat_lin <- make_wt(vars = c(all.vars(formula), cov), data)
    mod <- lm_lin(formula,
                  covariates = as.formula(paste0("~ ", paste(cov, collapse = " + "))),
                  data = dat_lin,
                  weights = wt)
  }else{
    if(!is.null(cntry) & cntry == "Nigeria"){
      cov <- setdiff(cov, "country")
    }else{
      cov <- setdiff(cov, c("lockdown", "country"))
    }
    mod <- lm_lin(formula,
                  covariates = as.formula(paste0("~ ", paste(cov, collapse = " + "))),
                  data = dat,
                  subset = country == cntry) 
  }
  return(mod)
}

make_lin_tab <- function(mods,
                         snames = c("Pooled", "Kenya", "Nigeria", "Uganda")){
  ltab <- lapply(mods, function(m){
    est <- c(coef(m)["hypo_conditionT1"], coef(m)["hypo_conditionT2"])
    se <- c(m$std.error["hypo_conditionT1"], m$std.error["hypo_conditionT2"])
    sign <- star(c(m$p.value["hypo_conditionT1"], m$p.value["hypo_conditionT2"]))
    top <- weave(paste0(round(est, 3), sign), round(se, 3))
    bottom <- c(round(c(m$r.squared, m$adj.r.squared), 3), 
                as.character(paste0(m$nobs))) %>% as.matrix(., ncol = 1)
    rbind(top, bottom)
  })
  tabout <- do.call(rbind, ltab)
  rnames <- lapply(snames, function(n) c("", n, rep("", 5))) %>% unlist
  cbind(rnames, rep(c("T1", "", "T2", "", "R^2", "Adj. R^2", "N"), length(mod)), 
        tabout)
}

make_lm_robust <- function(mods,
                           mod_names = c("Pooled", "Kenya", "Nigeria", "Uganda")){
  ltab <- lapply(mods, function(m){
    est <- coef(m)
    se <- m$std.error
    sign <- star(m$p.value)
    top <- weave(paste0(round(est, 3), sign), round(se, 3))
    bottom <- rbind(round(m$r.squared, 3), round(m$adj.r.squared, 3), m$nobs)
    rbind(top, bottom)
  })
  tabout <- do.call(cbind, ltab)
  # rnames <- lapply(snames, function(n) c("", n, rep("", 5))) %>% unlist
  # cbind(rnames, rep(c("T1", "", "T2", "", "R^2", "Adj. R^2", "N"), length(mod)),
        # tabout)
tabout
  }

# Plot functions ----------------------------------------------------------

plot_config <- function(){

}

plot_label <- function(title = "", question, scale){
  q <- stringi::stri_wrap(question, width = 60)
  q <- paste(q, collapse = "\n")
  labs(title = title,
       x = paste0(q, "\n", scale),#" (n = ", n, ")"),
       y = "")
}

hist_plot <- function(var, question, scale, df = dat, show_mean = FALSE){

  df$y <- df[[var]]
  dp <- df %>%
    group_by(country) %>%
    mutate(n_responses = sum(!is.na(y))) %>%
    group_by(country, n_responses, y) %>% 
    summarize(count = n()) %>%
    mutate(pct = count/n_responses) %>%
    filter(!is.na(y)) #%>%
  
  if(show_mean)
    dp <- dp %>% mutate(mean = weighted.mean(y, pct))
  
  plot <- ggplot(dp) + 
    geom_bar(aes(x = dp$y, y = pct, group = 1), stat = "identity") +
    facet_grid(.~country) +
    ylim(c(0,1)) +
    plot_label(question = question, scale = scale) +
    theme_minimal() +
    scale_fill_grey() +
    theme(plot.title = element_text(hjust = .5),
          axis.title.x = element_text(size = 8),
          legend.position = "none")
  if(show_mean)
    plot <- plot + geom_vline(aes(xintercept = mean), linetype = "dashed")
  return(plot)
}

mchoice_plot <- function(var, question, scale = "", df = dat, recode_df){
 
  q <- stringi::stri_wrap(question, width = 60)
  q <- paste(q, collapse = "\n")

  # df$y <- df[[var]]
  recode_df[,1] <- paste0(var, "_", recode_df[,1])
  recode_df <- as.data.frame(recode_df)
  colnames(recode_df) <- c("name", "value")
  
  #create new line in answer values when too long
  recode_df$value <- sapply(recode_df$value, function(x){
    e <- stringi::stri_wrap(x, width = 40)
    e <- paste(e, collapse = "\n")
    e
  })
  
  dp <- df %>% filter(!is.na(country)) %>%
    group_by(country) %>%
    mutate(n_responses = length(unique(ResponseId))) %>%
    group_by(country, n_responses, name) %>% 
    summarize(count = sum(fact_trueforyou == 1)) %>%
    mutate(pct = count/n_responses) %>%
    left_join(., recode_df, by = "name")
  
  plot <- ggplot(dp, mapping = aes(x = reorder(value, pct), y = pct)) + 
    geom_bar(aes(group = 1), stat = "identity") +
    ylim(c(0,1)) +
    coord_flip() +
    facet_grid(.~country) +
    labs(title = "",
         y = q,
         x = "") +
    # plot_label(question = question, scale = scale) +
    theme_minimal() +
    scale_fill_grey() +
    theme(plot.title = element_text(hjust = .5),
          axis.title.y = element_text(size = 8),
          legend.position = "none")

  return(plot)
}

apply_hist <- function(df_detail){
  Map(hist_plot, var = df_detail$var, 
      scale = df_detail$scale, 
      question = df_detail$question,
      show_mean = df_detail$show_mean)
}


# Map respondents subnational ---------------------------------------------

map_respondents <- function(df = ng_sf, title = "Nigeria"){
  p <- ggplot(df) + 
    geom_sf(aes(fill = share_s), alpha = .5) +
    # scale_colour_viridis_c() +
    scale_fill_grey(start = .9, end = 0, na.value = "white", drop = FALSE) +
    # scale_fill_gradient(low = "#ffffff", high = "black", 
                        # na.value = "#000000") +
    # scale_fill_gradient(low = "#FFFFFF",
    #                     high = "#000000",
    #                     space = "Lab",
    #                     na.value = "red") +
    # geom_point(aes(cpoints$X, cpoints$Y)) +
    labs(fill = "") +
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position = "bottom",
          # legend.key.width = unit(1, "cm"),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank()) + 
  labs(title = title)
  return(p)
}




