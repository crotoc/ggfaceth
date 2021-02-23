#' Split facet_grid from boxploth over multiple plots
#'
#' This extension to [ggplot2::facet_grid()] will allow you to split
#' a facetted plot over multiple pages. You define a number of rows and columns
#' per page as well as the page number to plot, and the function will
#' automatically only plot the correct panels. Usually this will be put in a
#' loop to render all pages one by one.
#'
#' @inheritParams ggplot2::facet_grid
#' @param ncol Number of columns per page
#' @param nrow Number of rows per page
#' @param page The page to draw
#' @param byrow Should the pages be created row-wise or column wise
#' @param x.variable Specify the x variable to caculate how many boxes for each panel
#' @note If either `ncol` or `nrow` is `NULL` this function will
#' fall back to the standard `facet_grid` functionality.
#'
#' @family ggfacet 
#'
#' @export
#' @import
#' ggplot2
#' rlang
#' @importFrom ggplot2 facet_grid ggproto
#'
#' @examples
#' # Draw a small section of the grid
#' ggplot(diamonds) +
#'   geom_point(aes(carat, price), alpha = 0.1) +
#'   facet_gridh_paginate(color~cut:clarity, ncol = 1, nrow = 3, page = 4, space="free_y", scales="free_y", x.variable="variable")
#'
facet_gridh_paginate <- function(facets, margins = FALSE, scales = "fixed",
    space = "fixed", shrink = TRUE, labeller = "label_value", as.table = TRUE,
    switch = NULL, drop = TRUE, ncol = NULL, nrow = NULL, page = 1,
    byrow = TRUE,x.variable="variable") {
    facet <- facet_grid(facets, margins = margins, scales = scales,
                        space = space, shrink = shrink, labeller = labeller,
                        as.table = as.table, switch = switch, drop = drop)
    if (is.null(nrow) || is.null(ncol)) {
        facet
    } else {
        ggproto(NULL, FacetGridhPaginate, shrink = shrink,
                params = c(
                    facet$params,
                    list(ncol = ncol, nrow = nrow, page = page, byrow = byrow, x.variable=x.variable)
                ))
    }
}

#' @rdname facet_gridh_paginate
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto FacetWrap
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @export
FacetGridhPaginate <- ggproto("FacetGridhPaginate", FacetGrid,
                             compute_layout = function(self,data, params) {
                                 layout <- FacetGrid$compute_layout(data, params)
                                 print(layout)
                                 row_bin <- ceiling(layout$ROW / params$nrow)
                                 print(row_bin)
                                 col_bin <- ceiling(layout$COL / params$ncol)
                                 print(col_bin)
                                 bin_layout <- matrix(seq_len(max(row_bin) * max(col_bin)),
                                                      nrow = max(row_bin), byrow = params$byrow)
                                 print(bin_layout)
                                 layout$page <- bin_layout[(col_bin - 1) * nrow(bin_layout) + row_bin]
                                 print(bin_layout)
                                 ##calculate how many panels and spaces should be added to each page
                                 data_list <- split(data[[2]],data[[2]][[names(params$rows)]])
                                 #print(data_list)
                                 variable_n <- unlist(lapply(data_list,function(x){length(unique(x[[params$x.variable]]))}))
                                 print(variable_n)
                                 layout$n <- variable_n[which(layout[[names(params$rows)]]==names(variable_n))]
                                 null_per_page <- unlist(lapply(split(layout,layout$page),function(x){sum(x$n)}))
                                 panel_per_page <- unlist(lapply(split(layout,layout$page),function(x){length(x$n)}))
                                 fill_info <- data.frame(page=names(null_per_page),null_per_page,panel_per_page)
                                 print(fill_info)
                                 fill_info$fill_space <- max(fill_info$panel_per_page)-fill_info$panel_per_page
                                 fill_info$fill_null <- max(fill_info$null_per_page)-fill_info$null_per_page
                                 self$fill_info <- fill_info
                                 layout
                             },
                             draw_panels = function(self,panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
                                 include <- which(layout$page == params$page)
                                 ## Format whole_plot width
                                 whole_plot <- FacetGrid$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
                                 widths <- whole_plot$widths
                                 # print(whole_plot$layout)
                                 # print(widths)
                                 stripidx <- grep("strip",whole_plot$layout$name)
                                 ##print(stripidx)
                                 ##print(whole_plot$grobs)
                                 strip_grob <- whole_plot$grobs[stripidx]
                                 strip_width <- strip_grob[[1]]$width
                                 ## print(strip_grob)
                                 ## print(strip_width)
                                 
                                 panels <- panels[include]
                                 ##ranges <- ranges[include]
                                 ranges <- ranges[include]
                                 layout <- layout[include, , drop = FALSE]
                                 layout$PANEL <- 1:length(layout$PANEL)
                                 layout$ROW <- layout$ROW - min(layout$ROW) + 1
                                 layout$COL <- layout$COL - min(layout$COL) + 1
                                 x_scale_ind <- unique(layout$SCALE_X)
                                 x_scales <- x_scales[x_scale_ind]
                                 layout$SCALE_X <- match(layout$SCALE_X, x_scale_ind)
                                 y_scale_ind <- unique(layout$SCALE_Y)
                                 y_scales <- y_scales[y_scale_ind]
                                 layout$SCALE_Y <- match(layout$SCALE_Y, y_scale_ind)
                                 ## print(layout)
                                 table <- FacetGrid$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)

                                 ## Format the widths of axis, panel and strip
                                 ## Format whole_plot width
                                 widths <- whole_plot$widths
                                 ## print(table$widths)
                                 table$widths <- widths
                                 ## print(widths)

                                 ## Format strip background width 
                                 stripidx <- grep("strip",whole_plot$layout$name)
                                 strip_grob <- whole_plot$grobs[stripidx]
                                 strip_width <- strip_grob[[1]]$width
                                 for(i in 1:length(table$grobs)) {
                                     if(table$grobs[[i]]$name == 'strip') table$grobs[[i]]$widths <- strip_width
                                 }

                                 ## fill null and panel space
                                 fill_info <- self$fill_info[which(self$fill_info$page==params$page),]
                                 print(fill_info)
                                 print(fill_info$fill_null)
                                 if(!is.na(fill_info$fill_null[1]) & fill_info$fill_null[1]!=0){
                                     ## Because a one-row panel has 1.2 null in total, when there is two panels,there is 0.2 null extra comparing to a two-row plot. So When adding one panel we add 0.8null space 
                                     table <- gtable_add_rows(table, unit(fill_info$fill_null[1] * 0.8, 'null'))
                                     #table <- gtable_add_rows(table, unit(0 * 0.8, 'null'))
                                     ## rect <- rectGrob(gp = gpar(fill = "black"))
                                     ## table <- gtable_add_grob(table, rect, 1, 2, clip = "off", name = paste0("myrect"), z = 3)
                                     ## gtable_add_grob(table, rect,t = 1, l = 2,name="panel-3")
                                 }
                                 
                                 spacing <- theme$panel.spacing.y %||% theme$panel.spacing
                                 if(fill_info$fill_space[1]!=0){
                                     table <- gtable_add_rows(table, unit(as.numeric(spacing) * fill_info$fill_space[1],"pt"))
                                 }
                                 table

                             }                     
                             )


