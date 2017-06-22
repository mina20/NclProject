################################################################################
# Author: Shihao Yang                                                          #
# Date: Nov 14, 2015                                                           #
# Main script for argo package, also served as an example of package usage     #
#                                                                              #
# Required package: argo, xts, glmnet, boot, xtable                            #
################################################################################

library(argo)

#### loading data ####
all_data <- load_data()
#tail(all_data$CD09)
#tail(all_data$CD10)
tail(all_data$GT)
tail(all_data$CD)
#tail(all_data$GFT)

#### start of meat ####
santillana_etal <- list()
our_argo <- list()

# The script takes around 10 minutes to run, generating the entire results
# Real-case nowcast is available when dtname == "GT"
#for(dtname in c("CD","GT")){
for(dtname in "GT"){
  exogen <- all_data[[dtname]]
  yx_merged <- merge(all_data$CD, exogen, join = "right")
  y <- yx_merged$CD.data
  santillana_etal[[dtname]] <-
    argo(y, exogen = exogen,
         alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 104)

  our_argo[[dtname]] <-argo(logit(y / 100), exogen = log((exogen + 0.5) / 100),
   
         alpha = 1, N_lag=1:52, use_all_previous = FALSE, N_training = 104)
}

ar3 <- argo(all_data$CD, alpha = NA, use_all_previous = FALSE, N_lag=1:3,
            N_training = 104)

ar3_gft <- argo(all_data$CD[zoo::index(all_data$GFT)], exogen = all_data$GFT,
                alpha = NA, use_all_previous = FALSE, N_lag=1:3,
                N_training = 104)


#### presentation and plot ####
#h1n1_start <- which(colnames(our_argo$GC09$coef)=="2009-03-28")
#h1n1_end <- which(colnames(our_argo$GC09$coef)=="2010-05-15")
#ARGO_coef_GC09_h1n1 <- our_argo$GC09$coef[, h1n1_start:h1n1_end]

post2009_start <- which(colnames(our_argo$CD)=="2009-01-04")
post2009_end <- ncol(our_argo$GC)
ARGO_coef_GC10_post2009 <- our_argo$CD[,post2009_start:post2009_end]

GT_coef_start <- post2009_end + 1
GT_coef_end <- ncol(our_argo$GT)
GT_coef_id <- GT_coef_start:GT_coef_end

ARGO_coef_blend <- merge(ARGO_coef_CD_post2009, our_argo$GT[,GT_coef_id],
                         by = "row.names", all=TRUE)
rownames(ARGO_coef_blend) <- ARGO_coef_blend[,1]
ARGO_coef_blend <- data.matrix(ARGO_coef_blend[,-1], rownames.force = TRUE)
ARGO_coef_blend <- ARGO_coef_blend[rownames(ARGO_coef_GC_post2009),]

ARGO_coef_blend <- ARGO_coef_blend[,setdiff(colnames(ARGO_coef_blend),colnames(ARGO_coef_GC_h1n1))]
ARGO_coef_blend <- merge(ARGO_coef_CD09_h1n1, ARGO_coef_blend, by = "row.names", all=TRUE)
rownames(ARGO_coef_blend) <- ARGO_coef_blend[,1]
ARGO_coef_blend <- data.matrix(ARGO_coef_blend[,-1], rownames.force = TRUE)
phrases_both <- intersect(rownames(our_argo$CD), rownames(our_argo$CD))
phrases_GC10 <- setdiff(rownames(our_argo$CD), rownames(our_argo$CD))
phrases_GC09 <- setdiff(rownames(our_argo$GT), rownames(our_argo$GT))
ARGO_coef_blend <- ARGO_coef_blend[c(phrases_both, phrases_GC, phrases_GC),]


pred_xts_blend <- merge(all_data$CD, logit_inv(our_argo$GT)*100, 
                        santillana_etal$GT$pred, ar3_gft$pred, ar3$pred, all=FALSE)
names(pred_xts_blend) <- c("CD.data", "ARGO", "GFT", "Santillana", "GFT_AR3", "AR3")

pred_xts_blend$naive <- c(NA, as.numeric(pred_xts_blend$CDC.data[-nrow(pred_xts_blend)]))

pred_xts_blend$ARGO[zoo::index(our_argo$GC10$pred)] <- logit_inv(our_argo$CD10$pred)*100
pred_xts_blend$ARGO["/2010-05-23"] <- logit_inv(our_argo$CD09$pred["/2010-05-23"])*100

pred_xts_blend$Santillana[zoo::index(santillana_etal$CD10$pred)] <- santillana_etal$GC10$pred
pred_xts_blend$Santillana["/2010-05-23"] <- santillana_etal$CD09$pred["/2010-05-23"]

pred_xts_blend <- pred_xts_blend["2009-03-29/"]

model_names <- c("ARGO", "GFT", "Santillana", "GFT_AR3", "AR3", "naive")
legend_names <- c("ARGO", "GFT (Oct 2014)", "Santillana et al. (2014)", "GFT+AR(3)","AR(3)", "Naive")

zoom_periods <- c("2004-01-04/2009-05-09",
                  "204-05-16/2004-06-27",
                  "2010-01-03/2010-04-18",
                  "2011-01-02/2011-09-11",
                  "2013-09-22/2014-01-26", 
                   )

GC_GT_cut_date <- tail(zoo::index(our_argo$GC10$pred),1)+1

pdf("final_plot.pdf", height=11,width=12)
plot_argo(pred_xts_blend, GC_GT_cut_date, model_names[1:5], legend_names[1:5], zoom_periods)

dev.off()
pdf(paste0("heatmap.pdf"),width=4, height=10)
heatmap_argo(ARGO_coef_blend, 0.1)
dev.off()

all_tab <- summary_argo(pred_xts_blend, model_names, legend_names, zoom_periods, "2009/2015")

corr_header <- all_tab$corr_print[1,,drop=F]
rownames(corr_header) <- "\\textbf{Correlation}\\hfill\\vadjust{}"
corr_header[,] <- NA

rmse_header <- all_tab$rmse_print[1,,drop=F]
rownames(rmse_header) <- "\\textbf{RMSE}\\hfill\\vadjust{}"
rmse_header[,] <- NA

abse_header <- all_tab$abse_print[1,,drop=F]
rownames(abse_header) <- "\\textbf{MAE}\\hfill\\vadjust{}"
abse_header[,] <- NA

mape_header <- all_tab$mape_print[1,,drop=F]
rownames(mape_header) <- "\\textbf{MAPE}\\hfill\\vadjust{}"
mape_header[,] <- NA

corr_diff_header <- all_tab$corr_diff_print[1,,drop=F]
rownames(corr_diff_header) <- "\\textbf{Corr. of increment}\\hfill\\vadjust{}"
corr_diff_header[,] <- NA


big_tab_print <- rbind(
  rmse_header, all_tab$rmse_print,
  abse_header, all_tab$abse_print,
  mape_header, all_tab$mape_print,
  corr_header, all_tab$corr_print,
  corr_diff_header, all_tab$corr_diff_print)



blank_suffix <- sapply(1:(nrow(big_tab_print)/(nrow(all_tab$rmse_print)+1)), function(i)
  paste(rep(" ",i), collapse = ""))
rownames(big_tab_print) <-
  paste0(rownames(big_tab_print), rep(blank_suffix, each=(nrow(all_tab$rmse_print)+1)))

print(xtable::xtable(big_tab_print), sanitize.text.function=identity,
      sanitize.rownames.function=identity)

rela_effi <- sapply(model_names[-1], function(model_bench)
  bootstrap_relative_efficiency(
    na.omit(pred_xts_blend), model_names[1], model_bench, l=52, sim = "geom"))
rela_effi <- t(rela_effi)
colnames(rela_effi) <- c("point estimate",
                         paste(rep(c("basic","normal","percent"), each=2),
                               c("95% CI lower bond", "95% CI upper bond")))
rownames(rela_effi) <- legend_names[-1]
print(xtable::xtable(rela_effi))
