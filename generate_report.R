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
  make_option(c("--size"), type="integer", default=14, 
              help="Text size [default= %default]", metavar="integer"),
  make_option(c("--width"), type="integer", default=23, 
              help="PDF document width [default= %default]", metavar="integer"),
  make_option(c("--height"), type="integer", default=15, 
              help="PDF document height [default= %default]", metavar="integer")
); 

args_parser <- OptionParser(option_list=args);
opt <- parse_args(args_parser);
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
text_size <- opt$size
width <- opt$width
height <- opt$height
margin <- max(c(floor(width / 5), floor(height / 5)))

# add nas to the data frame
add_nas <- function(s, date_levels){
  d <- date_levels[!date_levels %in% x$date_char]
  m <- as.data.frame(
    matrix(NA, 
           ncol = ncol(s), 
           nrow = length(unique(s$attr))))
  colnames(m) <- colnames(s)
  m$attr <- unique(s$attr)
  for (date in d){
    p <- m
    p$date_char <- date
    p$value <- NA
    p$sb <- unique(x$sb)
    s <- rbind(s, p)
  }
  return(s)
}

# list files in the input directory
files <- list.files(data_path, full.names = T)

# combine data into a singe data frame
dat <- rbind()
for (f in files) {
  x <- read.delim(f, header = F, sep = "\n")
  x$filename <- basename(f)
  dat <- rbind(dat, x)
}

# filter out
dat <- dat[grep("not available", dat$V1, invert = T), ]
dat <- dat[grep("No available data", dat$V1, invert = T), ]

# format the date
date <- do.call(rbind, strsplit(dat$filename, "_"))[,4]
date <- as.Date(date, format = "%Y%m%d")
dat$date <- date

# format the first column
lst <- do.call(rbind, strsplit(dat$V1, "] "))
dat$name_full <- paste0(lst[, 1], "]")
dat$attr <- do.call(rbind, strsplit(lst[, 2], ": "))[, 1]
dat$value <- as.numeric(do.call(rbind, strsplit(lst[, 2], ": "))[, 2])

# split into tot- and sb-level
message("Prepare data for the sandbox-level description")

# sandbox level
sb <- dat[grep(pattern = "fg-production-sandbox", 
               x = dat$name_full), ]
sb$sb <- gsub("]", "", gsub(
  pattern = '\\[fg-production-master]\\[', 
  replacement = "", sb$name_full)
)

# add sandbox number
n <- do.call(rbind, strsplit(sb$sb, "-"))
sb$sb_numb <- as.numeric(n[, ncol(n)])
sb <- sb[order(sb$sb_numb), ]
sb$sb <- factor(sb$sb, levels = unique(sb$sb))
sb$attr <- factor(sb$attr, levels = unique(sb$attr))

message("Prepare data for the general-level description")
tot <- dat[grep(pattern = "fg-production-sandbox", 
                x = dat$name, invert = T), ]
tot$name <- gsub("]", "", 
  gsub(pattern = '\\[fg-production-master]\\[', 
       replacement = "", tot$name_full))
tot$name[tot$name == ""] <- NA
ids <- tot$name == "fg-production-master" | is.na(tot$name)
master <- tot[which(ids), ]
profiles <- tot[which(!ids), ]

# plot 1: fg-production-master
message("Prepare plot 1: fg-production-master overview")
g1 <- ggplot(master) + 
  geom_bar(aes(date, value), 
           fill="steelblue", color = "gray40", 
           stat="identity", size = 0.2) +
  theme_bw() + 
  theme(text = element_text(size = text_size),
    axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~attr, scales = "free") + 
  ylab("") + xlab("") +
  scale_fill_discrete(na.value="gray92") + 
  ggtitle("fg-production-master") +
  scale_x_date(date_labels="%b, %y", 
               date_breaks  ="1 month")

# plot 2
message("Prepare plot 2: Profiles of fg-production-master")
g2 <- ggplot(profiles) + 
  geom_bar(aes(date, value), 
           fill="steelblue", color = "gray40", 
           stat="identity", size = 0.2) +
  theme_bw() + 
  theme(
    text = element_text(size = text_size),
    axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~name, ncol=5) + 
  ylab("") + xlab("") +
  scale_fill_discrete(na.value="gray92") + 
  ggtitle("Profiles (fg-production-master)") +
  scale_x_date(date_labels="%b, %y", 
               date_breaks  ="1 month")

# plot 3
message("Prepare plot 3: Overview across terms")
g3 <- ggplot(sb) + 
  geom_bar(aes(date, value, fill = factor(sb_numb)), 
           color = "gray30", stat="identity", 
           size = 0.2) + 
  theme_bw() + theme(
    text = element_text(size = text_size),
    axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~attr, scales = "free") + 
  xlab("") + ylab("") + labs(fill = "SB numb.") + 
  ggtitle("Overview across terms") +
  scale_x_date(date_labels="%b, %y", 
               date_breaks  ="1 month")

# plot 4
message("Prepare plot 4: Overview across terms")
g4 <- ggplot(sb) + 
  geom_bar(
    aes(date, value, fill = attr), 
    color = "gray30", stat="identity", 
    size = 0.2) + 
  theme_bw() + theme(
    text = element_text(size = text_size),
    axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~sb) + ylab("") + xlab("") +
  labs(fill = "Type") + 
  ggtitle("Overview across sandboxes") +
  scale_x_date(date_labels="%b, %y", 
               date_breaks  ="1 month")

# plot 5: 
message("Prepare plot 5: Overview across sandboxes (free y axis)")
g5 <- ggplot(sb) + geom_bar(
  aes(date, value, fill = attr), 
  color = "gray30", stat="identity", size = 0.2) + 
  theme_bw() + 
  ggtitle("Overview across sandboxes (free y axis)") + 
  theme(
    text = element_text(size = text_size),
    axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~sb, scales = "free_y") + 
  ylab("") + xlab("") + labs(fill = "Type") +
  ggtitle("Overview across sandboxes (free y axis)") +
  scale_x_date(date_labels="%b, %y", 
               date_breaks  ="1 month") 

# plot 6: Total SB with non-zero VM running time
message("Prepare plot 6: Total number of sandboxes per month overview")
sb$binary_value <- as.numeric(sb$value > 0)
sb_count <- aggregate(sb$binary_value, list(sb$date, sb$attr), sum)
sb_count$Group.2 <- as.character(sb_count$Group.2)
sb_count$Group.2[
  which(sb_count$Group.2 == "VM Running Time (h)")] = 
  "Non-zero VM Running Time"
# sb_count$Group.2 <- paste0(sb_count$Group.2, " > 0")
g6 <- ggplot(sb_count)  + 
  geom_bar(aes(Group.1, x), stat = "identity", fill="steelblue") +
  facet_wrap(~Group.2, ncol=3, scales = "free_y") +
  theme_bw() + ylab("Sandbox count") + xlab("") + 
  ggtitle("Total number of sandboxes per month") +
  theme(text = element_text(size = text_size),
        plot.margin = unit(c(t = margin, r = margin, 
                             b = margin, l = margin), "cm"),
        axis.text.x = element_text(size = text_size,
                                   angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_date(date_labels="%b, %y", date_breaks  ="1 month")

# plot 6: Monthly totals across sandboxes
sb_totals <- aggregate(sb$value, list(sb$date, sb$attr), sum)
g7 <- ggplot(sb_totals)  + 
  geom_bar(aes(Group.1, x), stat = "identity", fill="steelblue") +
  facet_wrap(~Group.2, ncol=3, scales = "free_y") +
  theme_bw() + ylab("Count") + xlab("") + 
  ggtitle("Monthly totals") +
  theme(text = element_text(size = text_size),
        plot.margin = unit(c(t = margin, r = margin, 
                             b = margin, l = margin), "cm"),
        axis.text.x = element_text(size = text_size,
                                   angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_date(date_labels="%b, %y", date_breaks  ="1 month")

# add plots to the list of plots
plist <- list(g1, g2, g3, g4, g5, g6, g7)

message("Prepare plot 7: Stats per term")
attributes <- unique(as.character(sb$attr))
for (a in attributes){
  sb_attr = sb[as.character(sb$attr) == as.character(a), ]
  sb_not_on_plt = setdiff(sb$sb, sb_attr$sb)
  tbl <- data.frame( "nodata" = sb_not_on_plt)
  g <- ggplot(sb_attr) + geom_bar(
    aes(date, value), fill = "steelblue", 
    color = "white", stat="identity") + 
    theme_bw() + theme(
      text = element_text(size = text_size),
      axis.text.x = element_text(
        angle = 90, vjust = 0.5, hjust=1)) +
    facet_wrap(~sb) + ylab("") + xlab("") + 
    labs(fill = "Type") + ggtitle(a) +
    scale_x_date(date_labels="%b, %y", 
                 date_breaks  ="1 month")
  
  # table containing sb not in the plot
  g_tbl <- tableGrob(tbl, theme=ttheme_minimal(base_size = text_size - 2))
  
  # combined
  gfull <-  ggdraw() + draw_plot(g, 0, 0, 0.8, 1) + 
    draw_plot(g_tbl, 0.81, 0, 0.19, 1) + 
    theme(plot.background = 
            element_rect(fill="white", color = NA))
  
  # add list to the plot
  plist[[(length(plist) + 1)]] <- gfull
}

# colors
colors <- brewer.pal(8, "Set1")
colors <- colors[1:length(attributes)]
names(colors) <- attributes

# aggregated plots 1/2
message("Prepare plot 8: Aggregated plots 1/2")
aggr <- aggregate(sb$value, list(sb$attr, sb$sb), mean)
ord <- aggr$Group.2[order((aggr$x), decreasing = T)]
ord <- as.character(unique(ord))
aggr$Group.2 <- factor(aggr$Group.2, levels = ord)
g <- ggplot(aggr) + 
  geom_bar(aes(Group.2, x, fill=Group.1), stat="identity") +
  theme_bw() + theme(
    text = element_text(size = text_size),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.text = element_text(size=9.5)) +
  labs(fill = "Type") + xlab("") + ylab("") +
  ggtitle("Aggregated stats over the time per sandbox") 

# add list to the plot
plist[[(length(plist) + 1)]] <- g

# aggregated plot 2/2: logarithmic
message("Prepare plot 9: Aggregated plots 2/2")
aggr$logx <- log(aggr$x + 1)
ord <- order(aggr$logx, decreasing = T)
aggr <- aggr[ord, ]
aggr$Group.2 <- factor(
  aggr$Group.2, 
  levels = as.character(unique(aggr$Group.2)))
cols <- colors[as.character(unique(sb$attr))]
g <- ggplot(aggr) + 
  geom_bar(aes(Group.2, logx, fill=Group.1), stat="identity") +
  theme_bw() + scale_fill_manual(values = cols) + 
  theme(
    text = element_text(size = text_size),
    axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust=1), 
    axis.text = element_text(size=9.5)) +
  labs(fill = "Type") + 
  ylab("log(value)") + xlab("") +
  ggtitle("Aggregated stats over the time per sandbox (logarithmic)")

# add list to the plot
plist[[(length(plist) + 1)]] <- g

# plot per sandbox
message("Prepare plot 10: Stats per Sandbox")
sb_list <- split(sb, sb$sb)
for (x in sb_list){
  cat(paste0("\tPrepare figure for sandbox ", unique(x$sb)), "\n")
  x$date_char = as.character(x$date)
  s <- add_nas(x, unique(sb$date_char))
  # s$date_char <- as.Date(s$date_char, format = "%Y-%m-%d")
  title <- unique(s$sb)
  
  # select colors - preserve color for each type across all sb plots
  cols <- colors[as.character(unique(s$attr))]
  
  # prepare the plot
  g <- ggplot(s) + 
    geom_bar(aes(date_char, value, fill = attr), 
             stat="identity") +
    theme_bw() + xlab("") + ylab("") + 
    scale_fill_manual(values=cols) +
    ggtitle(title) + labs(fill = "Type") +
    theme(text = element_text(size = text_size),
          plot.margin = unit(c(t = margin, r = margin, 
                               b = margin, l = margin), "cm"),
          axis.text.x = element_text(size = text_size,
          angle = 90, vjust = 0.5, hjust=1))
  
  # add to a list of plots
  plist[[(length(plist) + 1)]] <- g
}


# save the plots
ggsave(
  filename = fout, 
  plot = marrangeGrob(plist, nrow=1, ncol=1), 
  width = width, height = height
)

if (file.exists("Rplots.pdf")){
  res <- file.remove("Rplots.pdf")
}

cat("\n\nSaved reports to", fout, "\n")

