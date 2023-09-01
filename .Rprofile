# polaR ========================================================================
if (!exists(".polarEnv")) .polarEnv <- new.env()

## Raw Data Path ---------------------------------------------------------------
.polarEnv$rawDataPath  <- "../../../../Data/"

## MEaSURES Data Path ----------------------------------------------------------
.polarEnv$MEaSURES     <- "MEaSURES Boundaries/"

## Monthly RACMO Data Paths ----------------------------------------------------
.polarEnv$rcm$racmoM$rp3   <- list(
  "dir" = "RACMO/RACMO2.3p3_CON_ANT27_monthly/",
  "src" = "10.5281/zenodo.5512076")

.polarEnv$rcm$racmoM$rp2   <- list(
  "dir" = "RACMO/RACMO2.3p2_ANT27_ERA5-3H_monthly/",
  "src" = "10.5281/zenodo.7760490")

.polarEnv$rcm$racmoM$aprp2 <- list(
  "dir" = "RACMO/RACMO2.3p2_XPEN055_monthly/",
  "src" = "10.5281/zenodo.7961732")

## Daily RACMO Data Paths ------------------------------------------------------
.polarEnv$rcm$racmoD$rp3   <- list(
  "dir" = "RACMO/RACMO2.3p3_CON_ANT27_daily/",
  "src" = "10.5281/zenodo.5512076") # matches

## Monthly MAR Data Paths ------------------------------------------------------
.polarEnv$rcm$marD         <- NULL

## Daily MAR Data Paths --------------------------------------------------------
.polarEnv$rcm$marM         <- NULL
# .polarEnv$marD$m31     <- list(
#   "dir" = "MAR/MAR-ERA-Interim/",
#   "src" = "")

## Hourly MAR Data Paths -------------------------------------------------------
.polarEnv$rcm$marH$marMelt <- list(
  "dir" = "MAR/MARv3.11_AP_3H_Melt/",
  "src" = "10.5281/zenodo.6347190")

## Attach .polarEnv ------------------------------------------------------------
.polarEnv$testing <- "Goooood luck & enjoy!!!"
attach(.polarEnv)

# Miscellaneous ================================================================
message("< Hi devDomi, let's go write some cool polaR code!")
