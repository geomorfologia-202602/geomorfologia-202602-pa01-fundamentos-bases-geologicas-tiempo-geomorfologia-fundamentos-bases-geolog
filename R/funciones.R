estimacion_riqueza_chao <- function(mc, n_raras = 10) {
  library(RColorBrewer)
  library(SpadeR)
  library(iNEXT)
  mc_lista <- sapply(
    rownames(mc), function(x) as.numeric(mc[x,]), simplify = F)
  asin_resumen_estimadores <- if(is.list(mc))
    sapply(
      mc_lista,
      function(x) 
        SpadeR::ChaoSpecies(x, datatype = 'abundance', k = n_raras, conf = 0.95),
      simplify = F)
  else
    SpadeR::ChaoSpecies(mc, datatype = 'abundance', k = n_raras, conf = 0.95)
  nasin_raref <- iNEXT::iNEXT(
    if(is.list(mc)) mc_lista else mc,
    q=0,
    knots = 50,
    datatype ="abundance")
  acumulacion_especies <- iNEXT::ggiNEXT(nasin_raref, type=1) +
    theme_bw() +
    theme(
      text = element_text(size = 20),
      panel.background = element_rect(fill = 'white', colour = 'black'),
      panel.grid.major = element_line(colour = "grey", linetype = "dashed", size = 0.25)
    ) +
    ylab('Riqueza de especies') +
    xlab('Número de individuos') +
    scale_y_continuous(breaks = seq(0,80, length.out = 9)) +
    scale_color_manual(values = brewer.pal(8, 'Set2')) +
    scale_fill_manual(values = brewer.pal(8, 'Set2'))
  return(list(
    asintoticos_estimacion = asin_resumen_estimadores,
    no_asintoticos_rarefaccion_extrapolacion = nasin_raref,
    no_asintoticos_rarefaccion_extrapolacion_grafico = acumulacion_especies))
}
