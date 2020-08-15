# Make IGME CC profiles
# 2020.08

source("R/CC_funcs.R")
source("../helper_subset dataset.R")
# Load dataset
dt_U5MR <- fread(get.dir_U5MR())[Visible == 1] 
dt_IMR <- fread(get.dir_IMR())[Visible == 1] 
dt_NMR <- fread(get.dir_NMR())[Visible == 1]
dir.20.5_14 <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/IGME 5-14/2020 Round Estimation/input/updateddata.10q5all_b.csv")
file.exists(dir.20.5_14)
dir.20.15_24 <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/IGME 5-14/Estimates 10q15/input/updateddata.10q15all_b.csv")
file.exists(dir.20.15_24)
dt5_14 <- fread(dir.20.5_14)[Visible==1]
dt15_24 <- fread(dir.20.15_24)[Visible==1]


# Get Estimates Data ---- 
dir_cqt_files <- "../2020_CME_Plots/figData/cqt_backup/"
dt_cqt <- get.dt.cqt(dir_cqt_files)

isos <- unique(dt_cqt$ISO.Code)
dt_new_cnames <- readRDS("dt_new_cnames.Rds")

dt1 <- get.table.by.iso(iso0 = "TKM")

# Get Source ---- 
CME_sources <- get.0_4.sources()

CME_sources5_24 <- get.5_24.sources()
CME_sources_vr5_24 <- CME_sources5_24$vr
CME_sources_survey5_14 <- CME_sources5_24$survey1 # as they are seperate cells
CME_sources_survey15_24 <- CME_sources5_24$survey2

# make profile ---- 
fig_dir <- "../2020_CME_Plots/fig2020AfterCC/"
fig_dirs <- list.files(fig_dir, pattern = ".png", recursive = TRUE, full.names = TRUE)
save.CME.CC.profile("TKM", dir_save = here::here("AfterCC"))
invisible(lapply(isos[1:30], save.CME.CC.profile, dir_save = here::here("AfterCC")))
