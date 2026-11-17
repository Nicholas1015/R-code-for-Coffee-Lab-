# R-code-for-Coffee-Lab-
> library(ggplot2)
> library(ggrepel)
> 
> # ---- STEP 1. The ppm ranges used for EVERY spectrum ------------------
> # Each row is one range. A peak whose ppm falls between "from" and "to"
> # is labelled with that metabolite. (Spectra are referenced to HDO at
> # 4.70 ppm, so these sit ~0.09 ppm below literature values.)
> ranges <- data.frame(
+     Metabolite = c("Sucrose", "CQA", "Caffeine",
+                    "Trigonelline", "Trigonelline", "Trigonelline"),
+     from       = c(5.27, 6.05, 7.66, 7.85, 8.65, 8.97),
+     to         = c(5.36, 7.60, 7.80, 8.02, 8.87, 9.08))
> calibrant <- "Caffeine"   # set to 1.00 in every spectrum
> n_each    <- 5            # how many arabica and how many robusta to use
> 
> # ---- STEP 2. Read every peak Mnova found (one row per peak) ----------
> # all_peaks.csv is the spreadsheet's peak tables stacked into one list:
> # Sample, Species, ppm, area. If it's missing, rebuild it from the xlsx.
> if (!file.exists("all_peaks.csv"))
+     source("coffee_peaks_to_integrals.R", local = new.env())
Error in file(filename, "r", encoding = encoding) : 
  cannot open the connection
In addition: 
Warning message:
In file(filename, "r", encoding = encoding) :
  cannot open file 'coffee_peaks_to_integrals.R': No such file or directory

> setwd("~/Downloads/coffee_lab_complete 2")
> 
> file.exists("all_peaks.csv")
[1] TRUE
> library(ggplot2)
> library(ggrepel)
> 
> # ---- STEP 1. The ppm ranges used for EVERY spectrum ------------------
> # Each row is one range. A peak whose ppm falls between "from" and "to"
> # is labelled with that metabolite. (Spectra are referenced to HDO at
> # 4.70 ppm, so these sit ~0.09 ppm below literature values.)
> ranges <- data.frame(
+     Metabolite = c("Sucrose", "CQA", "Caffeine",
+                    "Trigonelline", "Trigonelline", "Trigonelline"),
+     from       = c(5.27, 6.05, 7.66, 7.85, 8.65, 8.97),
+     to         = c(5.36, 7.60, 7.80, 8.02, 8.87, 9.08))
> calibrant <- "Caffeine"   # set to 1.00 in every spectrum
> n_each    <- 5            # how many arabica and how many robusta to use
> 
> # ---- STEP 2. Read every peak Mnova found (one row per peak) ----------
> # all_peaks.csv is the spreadsheet's peak tables stacked into one list:
> # Sample, Species, ppm, area. If it's missing, rebuild it from the xlsx.
> if (!file.exists("all_peaks.csv"))
+     source("coffee_peaks_to_integrals.R", local = new.env())
> peaks <- read.csv("all_peaks.csv")
> 
> # ---- STEP 3. Choose the samples --------------------------------------
> first   <- function(sp, n) head(unique(peaks$Sample[peaks$Species == sp]), n)
> unknown <- first("unknown", 1)
> arabica <- first("arabica", n_each)
> robusta <- first("robusta", n_each)
> cat("Unknown:", unknown, "\nArabica:", arabica, "\nRobusta:", robusta, "\n\n")
Unknown: U 
Arabica: A1 A2 A3 A4 A5 
Robusta: R15 R16 R17 R18 R19 

> 
> # ---- STEP 4. Label each peak using the ranges from STEP 1 ------------
> # The same loop runs over every peak of every spectrum, so every
> # spectrum is analysed with exactly the same ranges.
> peaks$Metabolite <- NA
> for (i in seq_len(nrow(ranges))) {
+     inside <- peaks$ppm >= ranges$from[i] & peaks$ppm <= ranges$to[i]
+     peaks$Metabolite[inside] <- ranges$Metabolite[i]
+ }
> labelled <- peaks[!is.na(peaks$Metabolite), ]   # drop peaks outside all ranges
> write.csv(labelled, "final_labelled_peaks.csv", row.names = FALSE)
> 
> cat("Number of peaks labelled in each spectrum:\n")
Number of peaks labelled in each spectrum:
> print(table(labelled$Sample, labelled$Metabolite)[c(unknown, arabica, robusta), ])
     
      Caffeine CQA Sucrose Trigonelline
  U          1  47       7           12
  A1         1  42       5           14
  A2         1  28       2           12
  A3         2  50       6           16
  A4         3  51       5           15
  A5         1  31       1           11
  R15        1  62       4           17
  R16        1  29       2           13
  R17        1  35       2           13
  R18        1  43       2           14
  R19        1  46       3           15
> 
> # ---- STEP 5. Add up the peak areas for each metabolite ---------------
> # One row per sample, one column per metabolite = the integrals.
> areas <- tapply(labelled$area, list(labelled$Sample, labelled$Metabolite), sum)
> areas <- areas[unique(peaks$Sample), ]          # keep the spreadsheet's order
> if (any(is.na(areas))) stop("A sample has no peaks in one of the ranges.")
> 
> # ---- STEP 6. Calibrate: divide by the calibrant so it equals 1.00 ----
> integrals <- areas / areas[, calibrant]
> dat <- data.frame(Sample  = rownames(integrals),
+                   Species = peaks$Species[match(rownames(integrals), peaks$Sample)],
+                   round(integrals, 4))
> write.csv(dat, "final_integrals.csv", row.names = FALSE)
> cat("\nIntegrals relative to", calibrant, "= 1.00:\n")

Integrals relative to Caffeine = 1.00:
> print(dat[match(c(unknown, arabica, robusta), dat$Sample), ], row.names = FALSE)
 Sample Species Caffeine     CQA Sucrose Trigonelline
      U unknown        1 11.6143  0.5444       2.7494
     A1 arabica        1 14.0686  0.4776       2.3701
     A2 arabica        1  7.9183  0.3347       2.4379
     A3 arabica        1  6.9615  0.3450       2.6685
     A4 arabica        1  8.5560  0.3864       2.5993
     A5 arabica        1  4.4442  0.3991       0.9333
    R15 robusta        1  3.5146  0.1558       0.7731
    R16 robusta        1  4.3064  0.1337       1.0168
    R17 robusta        1  3.7336  0.1692       1.1061
    R18 robusta        1  5.5352  0.1921       0.8694
    R19 robusta        1  4.7602  0.1607       0.9332
> 
> # The calibrant is 1.00 in every sample, so it can't separate anything
> # and is left out of the PCA.
> chems <- setdiff(unique(ranges$Metabolite), calibrant)
> 
> cols   <- c(arabica = "#2a78d6", robusta = "#eb6834", unknown = "#1baf7a")
> shapes <- c(arabica = 16, robusta = 17, unknown = 18)
> labs3  <- c(arabica = "Arabica", robusta = "Robusta", unknown = "Unknown")
> look   <- theme_bw(base_size = 13) +
+     theme(panel.grid.minor = element_blank(), legend.position = "top",
+           plot.title.position = "plot")
> 
> # ---- STEP 7. A function that runs a PCA and draws PC 2 vs PC 1 -------
> run_pca <- function(samples, include_unknown, title, file, equal_axes = FALSE) {
+     d   <- dat[match(samples, dat$Sample), ]
+     # Which samples build the PCA? Known coffees only, unless there are too
+     # few of them (the 3-sample PCA), in which case the unknown is included.
+     fit <- if (include_unknown) rep(TRUE, nrow(d)) else d$Species != "unknown"
+     pca <- prcomp(d[fit, chems], center = TRUE, scale. = TRUE)   # the PCA itself
+     pct <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)          # % variance per PC
+     sc  <- predict(pca, d[, chems])                              # every sample's scores
+     
+     pts <- data.frame(Sample = d$Sample, Species = d$Species, PC1 = sc[, 1], PC2 = sc[, 2])
+     pts$Label <- ifelse(pts$Species == "unknown", "Unknown", pts$Sample)
+     
+     p <- ggplot(pts, aes(PC1, PC2)) +
+         geom_hline(yintercept = 0, colour = "grey70") +
+         geom_vline(xintercept = 0, colour = "grey70") +
+         geom_point(aes(colour = Species, shape = Species), size = 4) +
+         geom_text_repel(aes(label = Label), size = 3.5, colour = "grey20",
+                         fontface = ifelse(pts$Species == "unknown", "bold", "plain"),
+                         box.padding = 0.5, min.segment.length = 0,
+                         segment.colour = "grey60", max.overlaps = Inf, seed = 1) +
+         scale_colour_manual(values = cols, labels = labs3, name = NULL) +
+         scale_shape_manual(values = shapes, labels = labs3, name = NULL) +
+         labs(x = paste0("PC 1 (", pct[1], " %)"), y = paste0("PC 2 (", pct[2], " %)"),
+              title = title, subtitle = paste("Integrals relative to", tolower(calibrant), "= 1.00")) +
+         look
+     if (equal_axes) {   # same scale on both axes, so a tiny PC 2 isn't stretched
+         xr <- range(c(pts$PC1, 0)); xr <- xr + c(-1, 1) * 0.2 * diff(xr)
+         p  <- p + coord_equal(xlim = xr, ylim = c(-1, 1) * max(1, 1.5 * max(abs(pts$PC2))))
+     }
+     print(p)
+     ggsave(file, p, width = 7.5, height = 5.5, dpi = 300, bg = "white")
+     
+     # How far is the unknown from each group's centre?
+     cat("\n", title, sprintf(" (PC 1 %.1f%%, PC 2 %.1f%%)\n", pct[1], pct[2]), sep = "")
+     u <- sc[d$Species == "unknown", ]
+     for (g in c("arabica", "robusta")) {
+         centre <- colMeans(sc[d$Species == g, , drop = FALSE])
+         cat(sprintf("  distance from unknown to %s: %.2f\n", g, sqrt(sum((u - centre)^2))))
+     }
+     list(pca = pca, pct = pct)
+ }
> 
> # ---- STEP 8. The PCA plots ------------------------------------------
> five  <- run_pca(c(unknown, arabica, robusta), FALSE,
+                  "PCA: first 5 of each + unknown", "final_PCA_first5.png")

PCA: first 5 of each + unknown (PC 1 83.6%, PC 2 9.3%)
  distance from unknown to arabica: 1.73
  distance from unknown to robusta: 4.37
> three <- run_pca(c(unknown, arabica[1], robusta[1]), TRUE,
+                  "PCA: our group's 3 samples", "final_PCA_group3.png", equal_axes = TRUE)

PCA: our group's 3 samples (PC 1 96.6%, PC 2 3.4%)
  distance from unknown to arabica: 0.66
  distance from unknown to robusta: 3.03
> whole <- run_pca(dat$Sample, FALSE,
+                  "PCA: whole section", "final_PCA_section.png")

PCA: whole section (PC 1 83.4%, PC 2 11.8%)
  distance from unknown to arabica: 1.73
  distance from unknown to robusta: 3.97
> 
> # ---- STEP 9. Loadings plot for the first-5 PCA -----------------------
> # pca$rotation holds the loadings: how much each metabolite contributes
> # to PC 1 (x) and PC 2 (y).
> ld <- data.frame(Metabolite = rownames(five$pca$rotation),
+                  PC1 = five$pca$rotation[, 1], PC2 = five$pca$rotation[, 2])
> cat("\nLoadings (first-5 PCA):\n"); print(ld, row.names = FALSE, digits = 3)

Loadings (first-5 PCA):
   Metabolite    PC1    PC2
      Sucrose -0.579  0.529
          CQA -0.584  0.262
 Trigonelline -0.569 -0.807
> pad <- function(v) { r <- range(c(v, 0)); r + c(-1, 1) * 0.2 * max(diff(r), 0.4) }
> p_ld <- ggplot(ld, aes(PC1, PC2)) +
+     geom_hline(yintercept = 0, colour = "grey75") +
+     geom_vline(xintercept = 0, colour = "grey75") +
+     geom_point(shape = 18, size = 5, colour = "#1c2e5a") +
+     geom_text(aes(label = Metabolite), hjust = 0, nudge_x = 0.04 * diff(pad(ld$PC1)), size = 4.5) +
+     scale_x_continuous(limits = pad(ld$PC1)) + scale_y_continuous(limits = pad(ld$PC2)) +
+     labs(x = "Loadings PC 1", y = "Loadings PC 2", title = "Loadings plot: first-5 PCA",
+          subtitle = paste0("PC 1 ", five$pct[1], "%, PC 2 ", five$pct[2], "% of variance")) +
+     look
> print(p_ld)
> ggsave("final_loadings_first5.png", p_ld, width = 6.5, height = 5.2, dpi = 300, bg = "white")
> 
> # ---- STEP 10. Bar chart ("histogram") of the first 5 of each ---------
> # One panel per metabolite, each with its own y-axis, because CQA's
> # numbers are far bigger than sucrose's and would hide them on one axis.
> known <- dat[match(c(arabica, robusta), dat$Sample), ]
> unk   <- dat[dat$Sample == unknown, ]
> bars  <- do.call(rbind, lapply(chems, function(m) {
+     a <- known[known$Species == "arabica", m]       # the 5 arabica values
+     r <- known[known$Species == "robusta", m]       # the 5 robusta values
+     p <- t.test(a, r)$p.value                       # Welch t-test: are the means different?
+     data.frame(Metabolite = m, p = p,
+                Panel = paste0(m, "\n", ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))),
+                Group = c("Arabica", "Robusta", "Unknown"),
+                Mean  = c(mean(a), mean(r), unk[[m]]),
+                SD    = c(sd(a), sd(r), NA))
+ }))
> bars$Panel <- factor(bars$Panel, levels = unique(bars$Panel))
> cat("\nMeans (first 5 of each), unknown's value, and t-test p values:\n")

Means (first 5 of each), unknown's value, and t-test p values:
> print(bars[, c("Metabolite", "Group", "Mean", "SD", "p")], row.names = FALSE, digits = 3)
   Metabolite   Group   Mean     SD        p
      Sucrose Arabica  0.389 0.0567 0.000364
      Sucrose Robusta  0.162 0.0212 0.000364
      Sucrose Unknown  0.544     NA 0.000364
          CQA Arabica  8.390 3.5393 0.062629
          CQA Robusta  4.370 0.8140 0.062629
          CQA Unknown 11.614     NA 0.062629
 Trigonelline Arabica  2.202 0.7192 0.016108
 Trigonelline Robusta  0.940 0.1288 0.016108
 Trigonelline Unknown  2.749     NA 0.016108
> 
> is_unk <- bars$Group == "Unknown"
> p_bar <- ggplot(bars, aes(Group, Mean)) +
+     geom_col(data = bars[!is_unk, ], aes(fill = Group), width = 0.7) +
+     geom_errorbar(data = bars[!is_unk, ], aes(ymin = pmax(Mean - SD, 0), ymax = Mean + SD),
+                   width = 0.2, colour = "grey30") +
+     geom_point(data = bars[is_unk, ], shape = 18, size = 6, colour = cols[["unknown"]]) +
+     facet_wrap(~Panel, scales = "free_y") +
+     scale_fill_manual(values = c(Arabica = cols[["arabica"]], Robusta = cols[["robusta"]]),
+                       guide = "none") +
+     scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
+     labs(x = NULL, y = paste0("Integral relative to ", tolower(calibrant), " (= 1.00)"),
+          title = "Mean integrals: first 5 of each",
+          subtitle = "Bars = mean \u00b1 SD (n = 5 each); diamond = unknown\np = Welch t-test, arabica vs robusta") +
+     look + theme(panel.grid.major.x = element_blank(), strip.text = element_text(size = 12))
> print(p_bar)
> ggsave("final_barchart_first5.png", p_bar, width = 8, height = 5, dpi = 300, bg = "white")
> 
> cat("\nDone. Saved: final_PCA_first5.png, final_PCA_group3.png, final_PCA_section.png,\n",
+     "final_loadings_first5.png, final_barchart_first5.png,\n",
+     "final_integrals.csv, final_labelled_peaks.csv\n", sep = "")

Done. Saved: final_PCA_first5.png, final_PCA_group3.png, final_PCA_section.png,
final_loadings_first5.png, final_barchart_first5.png,
final_integrals.csv, final_labelled_peaks.csv
