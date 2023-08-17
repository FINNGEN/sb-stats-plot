#!/usr/bin/env Rscript

library("optparse")
library("ggplot2")
library("RColorBrewer")
library("cowplot")
library("gridExtra")

ts <- format(Sys.time(), "%d%m%yT%H%M")

args <- list(
  make_option(c("--path"), type="character", default=NULL, 
              help="Path to the location with files containing statistics gathered.", 
              metavar="character"),
  make_option(c("--out"), type="character", default=paste0("plots_", ts, ".pdf"), 
              help="Full path to the output file. Default: \"./plots_<TIMESTAMP>.pdf\"", metavar="character"),
  make_option(c("--size"), type="integer", default=18, 
              help="Text size [default= %default]", metavar="integer"),
  make_option(c("--width"), type="integer", default=25, 
              help="PDF document width [default= %default]", metavar="integer"),
  make_option(c("--height"), type="integer", default=15, 
              help="PDF document height [default= %default]", metavar="integer"),
  make_option(c("--sb_project"), type="character",  default=NULL, 
              help=paste("Google Cloud Project ID containing Datastore with 'SandboxConfing' entity storing Sandbox names.",
                         "If omitted, no Sandbox names matching is performed [default %default]."), 
              metavar="character"),
  make_option(c("--remove_unmatched"), default=FALSE, type="logical",
              help="Remove Sandboxes with unmatched Sanbox Name from the report [default %default]."),
  make_option(c("--plot_legacy_vm_profiles_separately"), default=FALSE, type="logical",
              help="Plot summary of the legacy VM profiles on a separate plot (i.e. 'Basic Machine') [default %default]."),
  make_option(c("--add_nodata"), default=FALSE, type="logical",
              help="Add info on the side of the plot with sb names omitted from the plot if no stats data exists [default %default]."),
  make_option(c("--max_sb_plots_per_page"), type="integer", default=20, 
              help="Max number of SB figures per page in the overview plots [default= %default]", metavar="integer")
)


# Args parser ----------------------------------------------------------------

args_parser <- OptionParser(option_list=args)
opt <- parse_args(args_parser)
if (is.null(opt$path)){
  print_help(args_parser)
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

cat("Saving report using the following: \n")
opt_print <- opt[1:(length(opt)-1)]
str <- paste0('\t- ', paste0(names(opt_print), ": ", opt_print))
cat(paste0(str, collapse = "\n"), "\n")

# assign parameters
data_path <- opt$path
fout <- opt$out
width <- opt$width
height <- opt$height
sb_project <- opt$sb_project
remove_unmatched <- opt$remove_unmatched
plot_legacy_vm_profiles_separately <- opt$plot_legacy_vm_profiles_separately
add_nodata <- opt$add_nodata
max_plot_per_page <- opt$max_sb_plots_per_page

TEXT_SIZE <- opt$size
MARGIN <- max(c(floor(width / 10), floor(height / 10)))
LEGMAR <- 0.2


# Functions ------------------------------------------------------------------

# add nas to the data frame
add_nas <- function(x, date_levels){
  d <- date_levels[!date_levels %in% x$date_char]
  m <- as.data.frame(
    matrix(NA, 
           ncol = ncol(x), 
           nrow = length(unique(x$attr))))
  colnames(m) <- colnames(x)
  m$attr <- unique(x$attr)
  for (date in d){
    p <- m
    p$date_char <- date
    p$value <- NA
    p$sb <- unique(x$sb)
    x <- rbind(x, p)
  }
  return(x)
}

get_sb_names <- function(sb_project) {
  
  message("Retreive sandbox information from the datastore")
  
  # get path of the python script used to fetch data from the datastore
  py_script <- get_py_script_path()
  
  message("Getting sandbox names from the datastore")
  
  if (!is.null(sb_project)){
    cmd <- paste("python3", py_script, "-p", sb_project)
    out <- system(cmd, intern = TRUE)
    
    # check if some error occurred
    if ( length(grep('ERROR', out)) == 0 ){
      sb_names <- as.data.frame(do.call(rbind, sapply(strsplit(out, ";"), strsplit, ",")))
      colnames(sb_names) <- c("sandbox_name", "ProjectID")
      rownames(sb_names) <- sb_names$ProjectID
      return(sb_names)
    }
    
  } else {
    message("WARN :: Sandbox project is is not specified - no Sandbox name matching will be performed.")
  }
  
  return(NULL)
  
}

get_py_script_path <- function(){
  init_opt <- commandArgs(trailingOnly = FALSE)
  file_arg_name <- "--file="
  script_name <- sub(file_arg_name, "", init_opt[grep(file_arg_name, init_opt)])
  script_dir <- file.path(dirname(script_name))
  py_script <- file.path(script_dir, "get_sb_name_datastore.py")
  return(py_script)
}

read_and_combine_data <- function(data_path){
  
  # list files in the input directory
  files <- list.files(data_path, full.names = T, pattern = ".txt")
  
  # combine data into a singe data frame
  dat <- rbind()
  for (f in files) {
    x <- read.delim(f, header = F, sep = "\n")
    x$filename <- basename(f)
    dat <- rbind(dat, x)
  }
  
  # format the date %Y-%m-%d
  m <- do.call(rbind, strsplit(dat$filename, "_"))[,3]
  y <- do.call(rbind, strsplit(dat$filename, "_"))[,2]
  date <-  as.Date(paste0(y, "-", sprintf("%02s", m), '-01'), 
                   format = "%Y-%m-%d")
  dat$date <- date
  dat <- dat[order(dat$date), ]
  
  return(dat)
  
}

filter_not_available_metrics <- function(dat){
  
  message("Removing entries with 'Metrics not available/No available data' status")
  
  dates <- unique(dat$date)
  dat <- dat[grep("not available", dat$V1, invert = T), ]
  dat <- dat[grep("No available data", dat$V1, invert = T), ]
  removed_dates <- setdiff(as.character(dates), as.character(unique(dat$date)))
  if (length(removed_dates) > 0){
    message(paste("The following date(s) removed after filtering not available data:", removed_dates))
  }
  
  return(dat)
  
}

format_data <- function(dat, sb_names){
  
  message("Prepare data for the sandbox-level description")
  
  # format the first column which contains sb id
  lst <- do.call(rbind, strsplit(as.character(dat$V1), "] "))
  dat$name_full <- paste0(lst[, 1], "]")
  dat$attr <- do.call(rbind, strsplit(lst[, 2], ": "))[, 1]
  dat$value <- as.numeric(do.call(rbind, strsplit(lst[, 2], ": "))[, 2])
  
  # split into tot- and sb-level
  dat$sb <- gsub("]", "", gsub(
    pattern = '\\[fg-production-master]\\[', 
    replacement = "", dat$name_full)
  )
  
  # add sb_names and format date
  if (! is.null(sb_names)){
    dat$sandbox_name <- as.character(sb_names[dat$sb, 'sandbox_name'])
  } else {
    dat$sandbox_name <- dat$sb
  }
  
  # format date
  dat$date_formatted <- format(dat$date,format="%b,%y")
  df_levels <- unique(dat$date_formatted)
  dat$date_formatted <- factor(dat$date_formatted, levels = df_levels)
  
  return(dat)
}

split_into_sb_and_master_level <- function(dat, remove_unmatched, sb_names){
  
  # get unmatched sb names
  unmatched_ids = grepl("fg-production-sandbox-(\\d+)$", dat$sb) & is.na(dat$sandbox_name)
  if(sum(unmatched_ids) > 0){
    message("The following sandboxes didn't have matching name in the Datastore:\n\t ", 
            paste0(unique(dat[unmatched_ids, 'sb']), collapse = "\n\t "))
  }
  
  # remove unmatched sb if specified
  if (remove_unmatched & !is.null(sb_names)){
    message("\tRemove unmatched from the report!")
    dat <- dat[!unmatched_ids, ]
  } else {
    message("\tLeave unmatched in the report!")
    dat[unmatched_ids, 'sandbox_name'] <- dat[unmatched_ids, 'sb']
  }
  
  master <- dat[is.na(dat$sandbox_name), ]
  dat_sb <- dat[!is.na(dat$sandbox_name), ]
  
  # sandbox level
  sb <- dat_sb[grep(pattern = "fg-production-sandbox", x = dat_sb$name_full), ]
  
  # add sb number
  sb <- add_sb_number(sb)
  
  return(list(master = master, sb = sb))
  
}

add_sb_number <- function(sb){
  n <- do.call(rbind, strsplit(sb$sb, "-"))
  sb$sb_numb <- as.numeric(n[, ncol(n)])
  sb <- sb[order(sb$sb_numb), ]
  sb$sb <- factor(sb$sb, levels = unique(sb$sb))
  sb$attr <- factor(sb$attr, levels = unique(sb$attr))
  return(sb)
}

split_master_into_general_and_vm_level <- function(data){
  
  message("Prepare data for the general-level description")
  
  tot <- data[grep(pattern = "fg-production-sandbox", x = data$name, invert = T), ]
  tot$name <- gsub("]", "", gsub(pattern = '\\[fg-production-master]\\[', 
                                 replacement = "", tot$name_full))
  tot$name[tot$name == ""] <- NA
  
  # split into master and profiles
  inds <- tot$name == "fg-production-master" | is.na(tot$name)
  general_level <- tot[which(inds), ]
  vm_profiles_level <- tot[which(!inds), ]
  
  return(list(general = general_level, vm_profiles = vm_profiles_level))
}

plot_fg_master_overview <- function(master, plot_id=NULL){
  
  message("Prepare plot ", plot_id, ": fg-production-master overview")
  
  # append zeros for the missing data to the 
  m <- master[, c('date', 'date_formatted', 'value', 'attr')]
  all_attr <- unique(m$attr)
  
  # fill nas for the missing dates 
  date_fill_nas <- names(table(m$date_formatted)[table(m$dateformatted) < length(all_attr)])
  for (d in date_fill_nas){
    x <- m[m$date == d, ]
    missing_attr <- setdiff(all_attr, unique(x$attr))
    df <- as.data.frame(matrix(NA, nrow = length(missing_attr), ncol = ncol(m)))
    colnames(df) <- colnames(m)
    
    df$date_formatted <- d
    df$value <- 0
    df$attr <- missing_attr
    
    m <- rbind(m, df)
  }
  
  p <- ggplot(as.data.frame(m), aes(date_formatted, value)) + 
    geom_bar(fill="steelblue", color = "white", 
             stat="identity", linewidth = 0.2) + 
    facet_wrap(~attr, scales = "free_y") + 
    ggtitle("Overview (fg-prodiction-master)") +
    theme_bw() + theme(text = element_text(size = TEXT_SIZE),
                       plot.margin = unit(c(t = MARGIN, r = MARGIN, 
                                            b = MARGIN, l = MARGIN), "cm"),
                       axis.text.x = element_text(size = TEXT_SIZE,
                         angle = 90, vjust = 0.5, hjust=1)) +
    ylab("") + xlab("") 
  
  return(p)
  
}

plot_fg_master_profiles <- function(vm_profiles, labs = NULL, 
                                    plot_id=NULL, title_prefix=NULL){
  
  message("Prepare plot ", plot_id, ": Profiles of fg-production-master")
  if (!is.null(labs)){
    vm_profiles <- vm_profiles[vm_profiles$name %in% labs, ]
  }
  
  p <- ggplot(vm_profiles) + 
    geom_bar(aes(date_formatted, value), 
             fill="steelblue", color = "white", 
             stat="identity", linewidth = 0.2) +
    theme_bw() + 
    theme(text = element_text(size = TEXT_SIZE),
          legend.margin = margin(LEGMAR, LEGMAR, LEGMAR, LEGMAR),
          plot.margin = unit(c(t = MARGIN, r = MARGIN, 
                               b = MARGIN, l = MARGIN), "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                     hjust=1, size = TEXT_SIZE)) +
    facet_wrap(~name) + 
    ylab("") + xlab("") 
  
  if(!is.null(title_prefix)){
    p <- p + ggtitle(paste0(title_prefix, " Profiles (fg-production-master)")) 
  } else {
    p <- p + ggtitle("Profiles (fg-production-master)")
  }
  
  return(p)
  
}

plot_overview_across_sb <- function(sb, plot_id=NULL){
  
  message("Prepare plot ", plot_id, ": Overview across sandboxes")
  
  p <- ggplot(sb) + 
    geom_bar(aes(date_formatted, value, 
                 fill = factor(sandbox_name)), 
             color = "gray30", stat="identity", 
             linewidth = 0.2) + 
    theme_bw() + theme(
      legend.title = element_text(size = floor(TEXT_SIZE/1.1)),
      legend.text = element_text(size = floor(TEXT_SIZE/1.2)),
      text = element_text(size = TEXT_SIZE), 
      legend.margin = margin(LEGMAR, LEGMAR, LEGMAR, LEGMAR),
      axis.text.x = element_text(size = TEXT_SIZE,
        angle = 90, vjust = 0.5, hjust=1)) +
    facet_wrap(~attr, scales = "free") + 
    guides(fill=guide_legend(ncol=1)) +
    xlab("") + ylab("") + labs(fill = "Sandbox") + 
    ggtitle("Overview across sandboxes") 
  
  return(p)
  
}

plot_overview_across_terms_sb <- function(
    sb, max_plot_per_page = 20, 
    add_free_y = FALSE,
    plot_id=NULL) {
  
  message("Prepare plot ", plot_id, ": Overview across terms in sb")

  l <- length(unique(sb$sandbox_name))
  x <- unique(sb[, c('sb', 'sandbox_name')])
  nr <- nrow(x)
  n <- ceiling(nr / 2)
  if (l > max_plot_per_page){
    y <- split(x, rep(1:ceiling(nr/n), each=n, length.out=nr))
  } else {
    y <- list('1' = x)
  }
  
  plist <- list()
  for (k in 1:length(y)){
    ssb <- sb[which(sb$sandbox_name %in% y[[k]]$sandbox_name), ]
    
    p <- ggplot(ssb) + 
      geom_bar(
        aes(date_formatted, value, fill = attr), 
        color = "gray30", stat="identity", 
        linewidth = 0.1) + 
      theme_bw() + theme(
        strip.text.x = element_text(size = floor(TEXT_SIZE/1.2)),
        legend.title = element_text(size = floor(TEXT_SIZE/1.1)),
        legend.text = element_text(size = floor(TEXT_SIZE/1.2)),
        legend.margin = margin(LEGMAR, LEGMAR, LEGMAR, LEGMAR),
        text = element_text(size = TEXT_SIZE),
        axis.text.x = element_text(size = TEXT_SIZE,
          angle = 90, vjust = 0.5, hjust=1)) +
      facet_wrap(~sandbox_name) + ylab("") + xlab("") +
      labs(fill = "Type") + 
      ggtitle(paste0("Overview across terms (part ", k, ")")) 
    
    if (add_free_y){
      p <- p + facet_wrap(~sandbox_name, scales = "free_y") + 
        ggtitle(paste0("Overview across terms, not fixed y-axis (part ", k, ")")) 
    }
    
    plist[[k]] <- p
    
  }
  
  return(plist)
  
}

plot_amount_of_sb_per_event <- function(sb, plot_id=NULL){
  
  message("Prepare plot ", plot_id, ": Amount of sandboxes per event")
  
  sb$binary_value <- as.numeric(sb$value > 0)
  sb_count <- aggregate(sb$binary_value, list(sb$date_formatted, sb$attr), sum)
  sb_count$Group.2 <- as.character(sb_count$Group.2)
  sb_count$Group.2[
    which(sb_count$Group.2 == "VM Running Time (h)")] = 
    "With non-zero VM Running Time"
  sb_count$Group.2[
    which(sb_count$Group.2 == "Pipelines started")] = 
    "Started pipelines"
  sb_count$Group.2[
    which(sb_count$Group.2 == "Share requests")] = 
    "Performing share requests"
  sb_count$Group.2[
    which(sb_count$Group.2 == "Terms accepted")] = 
    "With non-zero accepted terms"
  sb_count$Group.2[
    which(sb_count$Group.2 == "Download requests")] = 
    "Performing download requests"
  
  p <- ggplot(sb_count)  + 
    geom_bar(aes(Group.1, x), stat = "identity", fill="steelblue") +
    facet_wrap(~Group.2, ncol=3, scales = "free_y") +
    theme_bw() + ylab("Sandbox count") + xlab("") + 
    ggtitle("Total number of sandboxes") + 
    theme(text = element_text(size = TEXT_SIZE),
          strip.text.x = element_text(size = floor(TEXT_SIZE/1.1)),
          plot.margin = unit(c(t = MARGIN, r = MARGIN, 
                               b = MARGIN, l = MARGIN), "cm"),
          legend.margin = margin(LEGMAR, LEGMAR, LEGMAR, LEGMAR),
          axis.text.x = element_text(size = TEXT_SIZE,
                                     angle = 90, vjust = 0.5, hjust=1))
  
  return(p)
  
}

plot_monthly_totals_per_sb <- function(sb, plot_id){
  
  message("Prepare plot ", plot_id, ": Monthly totals")

  sb_totals <- aggregate(sb$value, list(sb$date_formatted, sb$attr), sum)
  
  p <- ggplot(sb_totals)  + 
    geom_bar(aes(Group.1, x), stat = "identity", fill="steelblue") +
    facet_wrap(~Group.2, ncol=3, scales = "free_y") +
    theme_bw() + ylab("") + xlab("") + 
    ggtitle("Monthly totals over sandboxes") +
    theme(text = element_text(size = TEXT_SIZE),
          plot.margin = unit(c(t = MARGIN, r = MARGIN, 
                               b = MARGIN, l = MARGIN), "cm"),
          legend.margin = margin(LEGMAR, LEGMAR, LEGMAR, LEGMAR),
          axis.text.x = element_text(size = TEXT_SIZE,
                                     angle = 90, vjust = 0.5, hjust=1)) 
  return(p)
  
}

plot_stats_per_term <- function(sb, plist, add_nodata=TRUE, plot_id = NULL){
  
  message("Prepare plot ", plot_id, ": Stats per term")
  
  attributes <- unique(as.character(sb$attr))
  
  for (a in attributes){
    sb_attr = sb[as.character(sb$attr) == as.character(a), ]
    sb_not_on_plt = setdiff(sb$sandbox_name, sb_attr$sandbox_name)
    tbl <- data.frame( "removed" = sb_not_on_plt)
    g <- ggplot(sb_attr) + geom_bar(
      aes(date_formatted, value), fill = "steelblue", 
      color = "white", stat="identity") + 
      theme_bw() + theme(
        text = element_text(size = TEXT_SIZE),
        legend.margin = margin(LEGMAR, LEGMAR, LEGMAR, LEGMAR),
        axis.text.x = element_text(
          angle = 90, vjust = 0.5, hjust=1)) +
      facet_wrap(~sandbox_name) + ylab("") + xlab("") + 
      labs(fill = "Type") + ggtitle(a)
    
    # table containing sb not in the plot
    theme <- ttheme_minimal(core = list(fg_params = list(hjust = 0, x = 0.1)), 
                            base_size = TEXT_SIZE - 2)
    
    g_tbl <- tableGrob(tbl, theme=theme)
    
    # combined
    if (add_nodata) {
      gfull <-  ggdraw() + draw_plot(g, 0, 0, 0.8, 1) + 
        draw_plot(g_tbl, 0.81, 0, 0.19, 1) + 
        theme(plot.background = element_rect(fill="white", color = NA))
    } else {
      gfull <-  g
    }

    # add list to the plot
    plist[[(length(plist) + 1)]] <- gfull
  }
  
  return(plist)
}

aggregate_data <- function(sb){
  
  aggr <- aggregate(sb$value, list(sb$attr, sb$sandbox_name), mean)
  aggr$sb_name <- aggr$Group.2
  
  # add sandbox names
  ord <- aggr$sb_name[order((aggr$x), decreasing = T)]
  ord <- as.character(unique(ord))
  aggr$sb_name <- factor(aggr$sb_name, levels = ord)
  
  return(aggr)
}

plot_aggregated_stats1 <- function(aggr, date_min, date_max, plot_id){
  
  message("Prepare plot ", plot_id, ": Aggregated plots 1/3")
  
  # colors
  attributes <- unique(as.character(aggr$Group.1 ))
  colors <- brewer.pal(8, "Set1")
  colors <- colors[1:length(attributes)]
  names(colors) <- attributes
  
  p <- ggplot(aggr) + 
    geom_bar(aes(sb_name, x, fill=Group.1), stat="identity") +
    theme_bw() + theme(
      strip.text.x = element_text(size = floor(TEXT_SIZE/1.5)),
      legend.title = element_text(size = floor(TEXT_SIZE/1.1)),
      legend.text = element_text(size = floor(TEXT_SIZE/1.2)),
      text = element_text(size = TEXT_SIZE),
      legend.margin = margin(LEGMAR, LEGMAR, LEGMAR, LEGMAR),
      axis.text.x = element_text(angle = 90, size = TEXT_SIZE, # size=14, 
                                 vjust = 0.5, hjust=1)) +
    labs(fill = "Type") + xlab("") + ylab("") + 
    scale_fill_manual(values = colors) + 
    ggtitle(paste0("Aggregated stats over period ", date_min, " - ", date_max)) 
  
  return(p)
  
}

plot_aggregated_stats2 <- function(aggr, plist, date_min, date_max, plot_id, add_nodata=TRUE){
  
  message("Prepare plot ", plot_id, ": Aggregated plots 2/3")
  
  for (t in unique(as.character(aggr$Group.1))){
    term_aggr <- aggr[as.character(aggr$Group.1) == t, ]
    
    term_aggr_ord <- term_aggr[order(term_aggr$x, decreasing = T), ]
    levels <- unique(as.character(term_aggr_ord$sb_name))
    term_aggr$sb_name <- factor(as.character(term_aggr$sb_name), levels = levels)
    
    g <- ggplot(term_aggr) + 
      geom_bar(aes(sb_name, x), fill= "gray70", stat="identity") +
      theme_bw() + theme(
        text = element_text(size = TEXT_SIZE),
        strip.text.x = element_text(size = floor(TEXT_SIZE/1.3)),
        legend.title = element_text(size = floor(TEXT_SIZE/1.1)),
        legend.text = element_text(size = floor(TEXT_SIZE/1.3)),
        axis.text.x = element_text(angle = 90, size = TEXT_SIZE, # size=14, 
                                   vjust = 0.5, hjust=1)) +
      labs(fill = "Type") + xlab("") + ylab("") +
      ggtitle(paste0("Overall ", t, " between ", date_min, ' - ', date_max)) 
    
    sb_not_on_plt = setdiff(sb$sandbox_name, term_aggr$sb_name)
    tbl <- data.frame( "no_data" = sb_not_on_plt)
    
    # table containing sb not in the plot
    theme <- ttheme_minimal(core = list(fg_params = list(hjust = 0, x = 0.1)), 
                            legend.margin = margin(LEGMAR, LEGMAR, LEGMAR, LEGMAR),
                            base_size = TEXT_SIZE - 2)
    
    g_tbl <- tableGrob(tbl, theme=theme)
    
    # combined
    if (add_nodata) {
      gfull <-  ggdraw() + draw_plot(g, 0, 0, 0.8, 1) + 
        draw_plot(g_tbl, 0.81, 0, 0.19, 1) + 
        theme(plot.background = element_rect(fill="white", color = NA))
    } else {
      gfull <-  g
    }
    
    # add list to the plot
    plist[[(length(plist) + 1)]] <- gfull
  }
  
  return(plist)
}

plot_aggregated_stats3 <- function(sb, aggr, plot_id){
  
  message("Prepare plot ", plot_id, ": Aggregated plots 3/3")
  
  # colors
  attributes <- unique(as.character(aggr$Group.1 ))
  colors <- brewer.pal(8, "Set1")
  colors <- colors[1:length(attributes)]
  names(colors) <- attributes
  
  aggr$logx <- log(aggr$x + 1)
  
  a <- aggregate(aggr$logx, list(aggr$sb_name), sum)
  a <- a[order(a$x,  decreasing = T),]
  aggr$sb_name <- factor(
    aggr$sb_name, 
    levels = a[, 1])
  cols <- colors[as.character(unique(sb$attr))] 
  
  g <- ggplot(aggr) + 
    geom_bar(aes(sb_name, logx, fill=Group.1), stat="identity") +
    theme_bw() + scale_fill_manual(values = cols) + 
    theme(
      text = element_text(size = TEXT_SIZE),
      strip.text.x = element_text(size = floor(TEXT_SIZE/1.1)),
      legend.title = element_text(size = floor(TEXT_SIZE/1.1)),
      legend.text = element_text(size = floor(TEXT_SIZE/1.1)),
      legend.margin = margin(LEGMAR, LEGMAR, LEGMAR, LEGMAR),
      axis.text.x = element_text(angle = 90, size = TEXT_SIZE, # size=14,
                                 vjust = 0.5,  hjust=1)) +
    labs(fill = "Type") + 
    ylab("log(value)") + xlab("") +
    ggtitle("Overall stats (logarithmic)") 
  
  return(g)
}

plot_stats_per_sb <- function(sb, position="dodge", plot_id){
  
  message("Prepare plot ", plot_id, ": Stats per Sandbox (part 1)")
  
  # colors
  attributes <- unique(as.character(sb$attr ))
  colors <- brewer.pal(8, "Set1")
  colors <- colors[1:length(attributes)]
  names(colors) <- attributes
  
  levels <- unique(as.character(sb$date))
  
  sb_list <- split(sb, sb$sb)
  for (x in sb_list){
    cat(paste0("\tPrepare figure for sandbox ", unique(x$sb)), "\n")
    x$date_char = as.character(x$date)
    s <- add_nas(x, levels)
    
    title <- unique(s$sandbox_name)
    
    # select colors - preserve color for each type across all sb plots
    cols <- colors[as.character(unique(s$attr))]
    
    # prepare the plot
    s$date_char <- factor(s$date_char, levels = levels)
    g <- ggplot(s, aes(date_char, value, fill = attr)) + 
      # geom_col(position = position_dodge2(width = 1, preserve = "single")) +
      geom_bar(aes(date_char, value, fill = attr),
               stat="identity", color = "white", 
               position = position) +
      theme_bw() + xlab("") + ylab("") + 
      scale_fill_manual(values=colors) +
      ggtitle(title) + labs(fill = "Type") +
      theme(text = element_text(size = TEXT_SIZE),
            strip.text.x = element_text(size = floor(TEXT_SIZE/1.1)),
            legend.title = element_text(size = floor(TEXT_SIZE/1.1)),
            legend.text = element_text(size = floor(TEXT_SIZE/1.1)),
            legend.margin = margin(LEGMAR, LEGMAR, LEGMAR, LEGMAR),
            plot.margin = unit(c(t = MARGIN, r = MARGIN, 
                                 b = MARGIN, l = MARGIN), "cm"),
            axis.text.x = element_text(size = TEXT_SIZE,
                                       angle = 90, vjust = 0.5, hjust=1))
    
    # add to a list of plots
    plist[[(length(plist) + 1)]] <- g
  }
  
  return(plist)
}


# Step 1: Get sandbox names from the Google Cloud Datastore ------------------

sb_names <- get_sb_names(sb_project)


# Step 2: Read & munge the data ----------------------------------------------

# read and combine
dat <- read_and_combine_data(data_path)

# filter 'Metrics not available records'
dat <- filter_not_available_metrics(dat)

# extract attr, values, dates, etc.
dat <- format_data(dat, sb_names)


# Step 3:Split into sb and (vm) profiles -------------------------------------

# split into profiles and sb
res <- split_into_sb_and_master_level(dat, remove_unmatched, sb_names)
master <- res$master
sb <- res$sb


# Step 4: Get general level description  -------------------------------------

res <- split_master_into_general_and_vm_level(master)
master_general <- res$general 
master_vm_profiles <- res$vm_profiles


# Step 5: Plotting general overviews  ----------------------------------------

g1 <- plot_fg_master_overview(master_general, 1)

# vm profiles
if(plot_legacy_vm_profiles_separately) {
  
  labs <- c(grep("CPU", unique(master_vm_profiles$name), value = T),
            grep("Custom", unique(master_vm_profiles$name), value = T))
  g21 <- plot_fg_master_profiles(master_vm_profiles, labs, 2)
  
  # legacy vm profiles
  labs_legacy <- unique(master_vm_profiles$name)[
    !unique(master_vm_profiles$name) %in%  labs
  ]
  g22 <- plot_fg_master_profiles(master_vm_profiles, labs_legacy, 3,
                                 title_prefix="Legacy")
  master_plot_list <- list(g1, g21, g22)
} else {
  g2 <- plot_fg_master_profiles(master_vm_profiles, plot_id = 2)
  master_plot_list <- list(g1, g2)
}


# Step 6: Plotting sb overviews  ---------------------------------------------

g3 <- plot_overview_across_sb(sb, 4)

g4 <- plot_overview_across_terms_sb(sb, add_free_y=FALSE, 
                                    max_plot_per_page = max_plot_per_page, 
                                    plot_id = 5)

g5 <- plot_overview_across_terms_sb(sb, add_free_y=T,
                                    max_plot_per_page = max_plot_per_page, 
                                    plot_id = 6)

g6 <- plot_amount_of_sb_per_event(sb, 7)

g7 <- plot_monthly_totals_per_sb(sb, 8)

# add plots to the list of plots
plist <- c(master_plot_list, list(g6, g7), list(g3), c(g4, g5))


# Step 7: Stats per term -----------------------------------------------------

# add plots per term
plist <- plot_stats_per_term(sb, plist, add_nodata=F,  9)

# Step 8: Plot aggregated ----------------------------------------------------

# aggregate data
aggr <- aggregate_data(sb)

# plot aggregated stats 1/3
date_min <- as.character(format(min(sb$date), format = "%b,%y"))
date_max <- as.character(format(max(sb$date), format = "%b,%y"))
g8 <- plot_aggregated_stats1(aggr, date_min, date_max, 10)
plist[[(length(plist) + 1)]] <- g8

# plot aggregated stats 2/3
plist <- plot_aggregated_stats2(aggr, plist, date_min, date_max, 11, add_nodata)

# plot aggregated stats 3/3
g9 <- plot_aggregated_stats3(sb, aggr, 12)
plist[[(length(plist) + 1)]] <- g9


# Step 9: Plot stats per term ------------------------------------------------

plist <- plot_stats_per_sb(sb, position = "stack", 13)


# Step 10: Save the plots ----------------------------------------------------

ggsave(
  filename = fout, 
  plot = marrangeGrob(plist, nrow=1, ncol=1), 
  width = width, height = height
)

print(warnings())

if (file.exists("Rplots.pdf")){
  res <- file.remove("Rplots.pdf")
}

cat("\n\nSaved reports to", fout, "\n")

