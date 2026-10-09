#' Create a PowerPoint presentation of figures
#'
#' @description Generic R code to pull all figures from a folder into a PowerPoint. By default this code generates a PowerPoint presentation including all .png files in the specified plotStorage directory. Slides are labeled numerically. The presentation is written as a Quarto (.qmd) file and rendered with Quarto.
#'
#' @param plotStorage A file path to the folder containing all figures to include in a powerpoint, this folder can contain sub-directories whose images will also be included
#' @param figureExtension A string specifying the file extension for the type of figures to include, default = ".png"
#' @param slideTitle A title for the PowerPoint slides, default = "Supplemental figures"
#' @param format A string indicating the type of format to use ("pptx" or "revealjs"), default = "pptx"
#' @param outdir A file path for the directory where the final presentation file should be saved, no default.
#' @param filename A string for the final presentation file name (without extension), no default. The .qmd extension is added automatically.
#'
#' @return Called for its side effects: writes a `.qmd` file to `outdir` and renders it with Quarto.
#' @export


makePresentation <- function(plotStorage = NULL,
                             figureExtension = ".png",
                             slideTitle = "Supplemental figures",
                             format = "pptx",
                             outdir = NULL,
                             filename = NULL) {
  # List all matching figures in plotStorage (recursive); full paths are used for the slide image links
  plotList <- list.files(path = plotStorage, pattern = figureExtension, recursive = TRUE, full.names = TRUE)
  nfigure <- length(plotList)

  slideOut <- paste0(outdir, "/", filename, ".qmd")

  # ---- Title (YAML header) ----
  write("---", slideOut, append = FALSE)
  write(paste0("title: '", slideTitle, "'"), slideOut, append = TRUE)

  # Set slide format
  if (format == "revealjs") {
    write(paste0("format: "), slideOut, append = TRUE)
    write(paste0("  ", format, ":"), slideOut, append = TRUE)
    write("    width: '150%'", slideOut, append = TRUE) # Increases size of plots
  } else if (format == "pptx") {
    write(paste0("format: ", format), slideOut, append = TRUE)
  }
  write("---", slideOut, append = TRUE) # End YAML

  # ---- One slide per figure ----
  for (ifigure in 1:nfigure) {
    write(paste0("## Figure", ifigure), slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotList[ifigure]), ")"), slideOut, append = TRUE)
    write("", slideOut, append = TRUE)
  }

  quarto:::quarto_render(slideOut)
}
