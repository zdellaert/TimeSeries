save_ggplot <- function(plot, filename, width = 10, height = 7, units = "in", dpi = 300,bg = "white") {
  png_path <- file.path(outdir_plots, paste0(filename, ".png"))
  pdf_dir <- file.path(outdir_plots, "pdf_figs")
  pdf_path <- file.path(pdf_dir, paste0(filename, ".pdf"))
  
  # Ensure the pdf_figs directory exists
  if (!dir.exists(pdf_dir)) dir.create(pdf_dir, recursive = TRUE)
  
  # Save plots
  ggsave(filename = png_path, plot = plot, width = width, height = height, units = units, dpi = dpi,bg = bg)
  ggsave(filename = pdf_path, plot = plot, width = width, height = height, units = units, dpi = dpi,bg = bg)
}

treat_colors <- c("C" = "lightblue4", "H" = "#D55E00")

time_colors <- colorRampPalette(c("#ffffcc","#0c2c84"))(7)
names(time_colors) <- c("0", "1", "3", "12", "24", "72", "120")

species_colors <- c("Pacuta" = "#5E65BEFF", "Mcap" = "#D29C44FF", "Pcomp" = "#7EC5F4FF",
                    "POC" = "#5E65BEFF", "MON" = "#D29C44FF", "POR" = "#7EC5F4FF")

# for legend making, this is the color scheme used in mfuzz.plot2 
# colmap <- c( "#FF8F00", "#FFA700", "#FFBF00",
#              "#FFD700", "#FFEF00", "#F7FF00", "#DFFF00", "#C7FF00",
#              "#AFFF00", "#97FF00", "#80FF00", "#68FF00", "#50FF00",
#              "#38FF00", "#20FF00", "#08FF00", "#00FF10", "#00FF28",
#              "#00FF40", "#00FF58", "#00FF70", "#00FF87", "#00FF9F",
#              "#00FFB7", "#00FFCF", "#00FFE7", "#00FFFF", "#00E7FF",
#              "#00CFFF", "#00B7FF", "#009FFF", "#0087FF", "#0070FF",
#              "#0058FF", "#0040FF", "#0028FF", "#0010FF", "#0800FF",
#              "#2000FF", "#3800FF", "#5000FF", "#6800FF", "#8000FF",
#              "#9700FF", "#AF00FF", "#C700FF", "#DF00FF", "#F700FF",
#              "#FF00EF", "#FF00D7", "#FF00BF", "#FF00A7", "#FF008F",
#              "#FF0078", "#FF0060", "#FF0048", "#FF0030", "#FF0018")
# 
# x <- seq(0,1,0.01)
# k <- 11
# x.small<-seq(x[1], x[length(x)],length=k)
# image(x, 1, matrix(x,length(x),1), axes=FALSE, xlab="", ylab="", col=colmap,main="")
# axis(1, at=rev(x.small), labels=signif(rev(x.small),2), srt=270)
# box()