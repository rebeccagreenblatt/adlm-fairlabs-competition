make_funnelplot <- function(dat){
  
  names = colnames(dat)[2:length(colnames(dat))]
  
  fig <- plot_ly(
    type = "funnel",
    name = dat[1,1],
    orientation = "h",
    y = names,
    x = unlist(dat[1,2:ncol(dat)]),
    textinfo = "x+percent initial",
    hoverinfo = "x+percent previous+percent initial")
  
  for(i in 2:nrow(dat)){
    fig <- fig %>%
      add_trace(
        type = "funnel",
        name = dat[i,1],
        orientation = "h",
        y = names,
        x = unlist(dat[i,2:ncol(dat)]),
        textinfo = "x+percent initial",
        hoverinfo = "x+percent previous+percent initial")
  }
  
  fig <- fig %>%
    layout(yaxis = list(categoryarray = names))
  
  return(fig)
  
}