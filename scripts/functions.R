
get_samp_cols = function(x, cols) {
  grps = cut(x, length(cols), include.lowest = T)
  return(data.frame(grps, col=cols[grps]))
}