my_quantile <- function(x, n_quant_groups, txt = "pct") {
  # Calculate the quantile probabilities based on the number of groups
  probs <- seq(0, 1, length.out = n_quant_groups + 1)[-c(1, n_quant_groups + 1)]
  quan <- quantile(na.omit(x), probs = probs)

  # Function to determine the group for a single value
  find_group <- function(value, quan, n_quant_groups) {
    if (is.na(value)) {
      return(NA)
    } else {
      for (i in seq_along(quan)) {
        if (value < quan[i]) {
          return(paste0(
            (round((i - 1) * 100 / n_quant_groups)),
            "%-",
            round((i * 100 / n_quant_groups)),
            "%"
          ))
        }
      }
      return(paste0(
        (round((n_quant_groups - 1) * 100 / n_quant_groups)),
        "%-100%"
      ))
    }
  }

  # Apply the function to each element in x
  final <- sapply(x, find_group, quan, n_quant_groups)

  if (txt == "pct") {
    return(final)
  }

  temp <- tibble(x = x, txt = final) %>%
    group_by(txt) %>%
    mutate(
      num = paste0(min(x, na.rm = TRUE), "-", max(x, na.rm = TRUE)),
      txt_num = paste0(
        txt,
        '<br><span style="font-size: 10pt;">*',
        min(x, na.rm = TRUE),
        "-",
        max(x, na.rm = TRUE),
        "*</span>"
      )
    )

  if (txt == "num") {
    return(temp$num)
  }

  if (txt == "both") {
    return(temp$txt_num)
  }
}
