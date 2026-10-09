#' Drop terminal-year indices in retrospective peels
#'
#' Refits each retrospective peel of a WHAM model with the selected indices and index
#' age-composition data removed from the terminal year, then compares the resulting
#' NAA deviations and SSB retrospective patterns against the base model. Saves a
#' NAA deviation plot per peel and an SSB retrospective plot to the working directory.
#'
#' @param model A WHAM rds output file from a run that included NAA random effects and a retrospective. The rds cannot include projections.
#' @param drop A list with two elements (`indices` and `index_paa`) giving the numeric index values
#'   of the indices and age compositions to drop from the terminal years.
#'
#' @return A list containing the results of each peel fit (one WHAM fit per peel). Also saves
#'   a png plot of NAA devs from each fit (`Peel_<r>_lostTermI_NAAdevs.jpeg`) and the SSB retro
#'   plot (`SSB_retro_relative_lostTermI.png`) to the working directory.
#'
#' @examples
#' \dontrun{
#' # drops all indices from a fit
#' droptermIndex <- drop_term_index_retro(model = mod, drop = list(indices = 1:mod$input$data$n_indices, index_paa = 1:mod$input$data$n_indices))
#' # drop whatever index 2 is from a fit
#' droptermIndex <- drop_term_index_retro(model = mod, drop = list(indices = 2, index_paa = 2))
#' }
#' @export
#############################
#drop index observations from the terminal year of retro peels to check the effect
drop_term_index_retro=function(model=NULL,drop=NULL){
  n.peels=length(model$peels)
  for(r in 1:n.peels){
    if(r==1) peel=list(fit_hindcast(model=model$peels[[r]],peel=1,drop=drop))
    if(r>1) peel[[r]]=fit_hindcast(model=model$peels[[r]],peel=1,drop=drop)
    
    # Trim the years vector to match the shortened peel
    peel[[r]]$input$years_full=model$input$years_full[1:(length(model$input$years_full)-r)]
    peel[[r]]$model_name=paste0("Peel-",r)
    
    naaplot=plotNAAfxn(mods=peel[[r]],cor="NA")
    ggsave(paste0("Peel_",r,"_lostTermI_NAAdevs.jpeg"),naaplot,path=getwd())
  }
  graphics.off()
  
  
  # Relative SSB difference from the base model for each peel, over the years each peel covers
  ssbretrolist=lapply(peel,function(x) {
    nyrs=x$input$data$n_years_model
    (x$rep$SSB-model$rep$SSB[1:nyrs])/model$rep$SSB[1:nyrs]
  }
  )
  
  years=1:(model$input$data$n_years_model-1)
  n_years=max(years)
  # Common y-axis limits across all peels
  miny=min(unlist(lapply(ssbretrolist,function(x) min(x))))
  maxy=max(unlist(lapply(ssbretrolist,function(x) max(x))))
  
  plot.colors = RColorBrewer::brewer.pal(n.peels + 1,"Set1")
  tcol = col2rgb(plot.colors)
  png(filename = "SSB_retro_relative_lostTermI.png",width=960,height=960)
  plot(years, ssbretrolist[[1]], lwd = 1, col = "black", 
       type = "l", xlab = "Year", ylab = "Mohn's rho(SSB)", ylim = c(miny,maxy))
  points(years[n_years], ssbretrolist[[1]][n_years], pch = 16, col = "black")
  grid(col = gray(0.7), lty = 2)
  for (i in 1:(n.peels-1)) {
    lines(years[1:(n_years - i)], ssbretrolist[[i + 1]], 
          col = tcol[i + 1])
    points(years[n_years - i], ssbretrolist[[i + 1]][n_years - 
                                                       i], pch = 16, col = tcol[i + 1])
  }
  abline(h=0,col="black")
  graphics.off()
  
  return(peel)
}
