#' @noRd

#font_import() ## use this once to load up the fonts to C://Windows/Fonts

## On Windows machines need to explicitly load specific fonts to use in the scripts
#loadfonts(device = "win")
#windowsFonts(`Arial Narrow` = windowsFont("Arial Narrow"))
#windowsFonts(Times=windowsFont("TT Times New Roman"))
#windowsFonts(Helvetica=windowsFont("Helvetica"))

# Theming scripts to use in publications

theme_pem <- function(base_size=12, base_family="Helvetica") {
  thm <- theme_pem_foundation(base_size = base_size, base_family = base_family)
  thm
}

theme_pem_facet <- function(base_size = 12, base_family = "Helvetica") {

  theme_pem_foundation(base_size = base_size, base_family = base_family) +
    theme(
      panel.spacing = unit(.6,"lines"),
      panel.border = element_rect(colour = "black", fill = NA),
      strip.background = element_rect(colour = "black", fill = "grey95"))

}

theme_pem_foundation <- function(base_size, base_family) {
  theme_few(
    base_size = base_size,
    base_family = base_family) +
    theme(
      text = element_text(colour = "black"),
      line = element_line(colour = "black", size = 0.5,
                          linetype = 1, lineend = "butt"),
      rect = element_rect(fill = "white", colour = "black",
                          size = 0.5, linetype = 1),
      axis.line = element_line(colour = "black"),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      axis.text = element_text(colour = 'black'),
      axis.text.y = element_text(hjust = 1),
      axis.ticks = element_blank(),
      plot.title = element_text(vjust = 2),
      legend.title = element_text(face = "plain"),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.y = element_line(colour = "grey85",size = 0.5),
      #panel.grid.minor = element_blank(),
      #panel.grid.major = element_line(colour = "grey80",size = 0.5),
      axis.title.y = element_text(vjust = 1, angle = 90),
      axis.title.x = element_text(vjust = 0),
      #panel.spacing = unit(0.25, "lines"),
      plot.background = element_blank(),
      legend.key = element_blank()#,
      #complete = TRUE
    )
}

# colour themes: options
#scale_fill_discrete_sequential(palette = "Light Grays")
#scale_fill_discrete_sequential(palette = "Blues")
#require(flextable)
init_flextable_defaults()
set_flextable_defaults(
  font.family = "Helvetica",
  font.size = 10,
  font.color = 'black',
  text.align = 'centre',
  padding = NULL,
  padding.bottom = NULL,
  padding.top = NULL,
  padding.left = NULL,
  padding.right = NULL,
  border.color = NULL,
  background.color = NULL,
  line_spacing = NULL,
  table.layout = NULL,
  cs.family = NULL,
  eastasia.family = NULL,
  hansi.family = NULL,
  decimal.mark = NULL,
  big.mark = NULL,
  digits = NULL,
  na_str = NULL,
  nan_str = NULL,
  fmt_date = NULL,
  fmt_datetime = NULL,
  extra_css = NULL,
  fonts_ignore = NULL,
  theme_fun = NULL,
  post_process_pdf = NULL,
  post_process_docx = NULL,
  post_process_html = NULL,
  post_process_pptx = NULL
)

#init_flextable_defaults()

#p3 <- ggplot(diamonds, aes(y = carat, x = price, fill = cut)) +
#  #geom_bar(stat = "identity") +
#  geom_boxplot() +
# facet_wrap(~clarity)

#p3 + theme_pem()

#p3 + theme_pem_facet()

#p3 + theme_ipsum()

bgc_colours <- function(which = NULL) {
  cols <- c(BAFA = "#E5D8B1",
            SWB  = "#A3D1AB",
            BWBS = "#ABE7FF",
            ESSF = "#9E33D3",
            CMA  = "#E5C7C7",
            SBS  = "#2D8CBD",
            MH   = "#A599FF",
            CWH  = "#208500",
            ICH  = "#85A303",
            IMA  = "#B2B2B2",
            SBPS = "#36DEFC",
            MS   = "#FF46A3",
            IDF  = "#FFCF00",
            BG   = "#FF0000",
            PP   = "#DE7D00",
            CDF  = "#FFFF00")

  if (is.null(which)) {
    return(cols)
  } else {
    if (!all(which %in% names(cols))) stop("Unknown Biogeoclimatic Zone code(s) specified", call. = FALSE)
    return(cols[which])
  }
}

###from BBC package for saving plots to files
save_plot <- function (plot_grid, width, height, filename) {
  grid::grid.draw(plot_grid)
  #save it
  ggplot2::ggsave(filename = filename,
                  plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
}

#Left align text
left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

create_footer <- function (source_name, logo_image_path) {
  #Make the footer
  footer <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.1, "npc")),
                           grid::textGrob(source_name,
                                          x = 0.004, hjust = 0, gp = grid::gpar(fontsize=16)),
                           grid::rasterGrob(png::readPNG(logo_image_path), x = 0.944))
  return(footer)

}

finalise_plot <- function(plot_name,
                          #source_name,
                          #save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.pdf"),
                          filename,
                          width_pixels=640,
                          height_pixels=450)#,
  #logo_image_path = file.path(system.file("data", package = 'bbplot'),"placeholder.png"))

{

  #export_plot <- function(plot_name, source, save_filepath, width_pixels = 640, height_pixels = 450) {
  theme_pem() #+

  #Draw your left-aligned grid
  plot_left_aligned <- left_align(plot_name, c("subtitle", "title", "caption"))
  plot_grid <- ggpubr::ggarrange(plot_left_aligned, #footer,
                                 ncol = 1, nrow = 1)#,
  #                                heights = c(1, 0.045/(height_pixels/450)))
  ## print(paste("Saving to", save_filepath))
  #save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
  save_plot(plot_left_aligned, width_pixels, height_pixels, filename)
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  invisible(plot_grid)
}


finalise_facet_plot <- function(plot_name,
                                #source_name,
                                #save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.pdf"),
                                filename,
                                width_pixels=640,
                                height_pixels=450)#,
  #logo_image_path = file.path(system.file("data", package = 'bbplot'),"placeholder.png"))

{

  #export_plot <- function(plot_name, source, save_filepath, width_pixels = 640, height_pixels = 450) {
  theme_pem_facet() #+

  #Draw your left-aligned grid
  plot_left_aligned <- left_align(plot_name, c("subtitle", "title", "caption"))
  plot_grid <- ggpubr::ggarrange(plot_left_aligned, #footer,
                                 ncol = 1, nrow = 1)#,
  #                                heights = c(1, 0.045/(height_pixels/450)))
  ## print(paste("Saving to", save_filepath))
  #save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
  save_plot(plot_left_aligned, width_pixels, height_pixels, filename)
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  invisible(plot_grid)
}



# calculate predicted vs obs pc for balancing types

png_retina <- function(filename = "Rplot%03d.png", width = 480, height = 480,
                       units = "px", pointsize = 12, bg = "white",  res = NA,
                       ..., type = c("cairo", "cairo-png", "Xlib", "quartz"),
                       antialias) {

  width <- width * 2
  height <- height * 2
  res <- ifelse(is.na(res), 144, res * 2)

  grDevices::png(filename = filename, width = width, height = height, units = units,
                 pointsize = pointsize, bg = bg, res = res, ...,
                 type = type, antialias = antialias)
}

multi_plot <- function(plotdata, filename){
  #  svg_px(paste0(filename, ".svg"), width = 400, height = 300)
  #  plot(plotdata)
  #  dev.off()
  png_retina(paste0(filename, ".png"), width = 700, height = 700,
             units = "px", type = "cairo-png", antialias = "default")
  plot(plotdata)
  dev.off()
}

