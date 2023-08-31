# Prepare paths for polaR
.polarEnv = new.env()

# Raw Data Path
.polarEnv$rawDataPath  <- "../../../../Data/"

# MEaSURES Data
.polarEnv$MEaSURES     <- "MEaSURES Boundaries/"

# Monthly RACMO Data
.polarEnv$racmoM$rp3   <- list("dir" = "RACMO/RACMO2.3p3_CON_ANT27_monthly/",
                             "src" = "10.5281/zenodo.5512076")

.polarEnv$racmoM$rp2   <- list("dir" = "RACMO/RACMO2.3p2_ANT27_ERA5-3H_monthly/",
                             "src" = "10.5281/zenodo.7760490")

.polarEnv$racmoM$aprp2 <- list("dir" = "RACMO/RACMO2.3p2_XPEN055_monthly/",
                             "src" = "10.5281/zenodo.7961732")

# Daily RACMO Data
.polarEnv$racmoD$rp3   <- list("dir" = "RACMO/RACMO2.3p3_CON_ANT27_daily/",
                             "src" = "10.5281/zenodo.5512076") # matches

# Monthly MAR Data
.polarEnv$marD         <- NULL

# Daily MAR Data
.polarEnv$marM         <- NULL
# .polarEnv$marD$m31     <- list("dir" = "MAR/MAR-ERA-Interim/",
                             # "src" = "")

# Hourly MAR Data
.polarEnv$marH$marMelt <- list("dir" = "MAR/MARv3.11_AP_3H_Melt/",
                             "src" = "10.5281/zenodo.6347190")

# Set defaults
.polarEnv$defaults$marD   <- NULL
.polarEnv$defaults$marM   <- NULL
.polarEnv$defaults$marH   <- NULL
.polarEnv$defaults$racmoD <- NULL
.polarEnv$defaults$racmoM <- NULL

attach(.polarEnv)

message("< Hi devDomi, let's go write some polaR code!")
