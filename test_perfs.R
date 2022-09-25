library(arrow)
library(dplyr)
library(vroom)
library(forcats)
library(qs)
library(tictoc)
library(readr)


# Prepare data -------------------------------------------------------------------------------
# Extend timeout to enable download of heavy files
options(timeout = max(300, getOption("timeout")))
# Need to disable SSL certification, as INSEE SSL certificates are self-signed.
options(download.file.method="curl", download.file.extra="-k -L")
# Download home-work travels from 2019 French Census
download.file("https://www.insee.fr/fr/statistiques/fichier/6456056/RP2019_mobpro_csv.zip",
              destfile = "RP2019_mobpro_csv.zip")
# Unzip and remove file
unzip(zipfile = "RP2019_mobpro_csv.zip")
# zip and csv file size
file.size("RP2019_mobpro_csv.zip")
# 85635499
file.size("FD_MOBPRO_2019.csv")
# 787990758

# Read csv with three methods ---------------------------------------------------------------

# Base R (utils)
tic()
mobpro_2019 <- read.csv("FD_MOBPRO_2019.csv")
toc()
# 46.254 sec elapsed
rm(mobpro_2019)

# Readr
tic()
mobpro_2019 <- read_csv("FD_MOBPRO_2019.csv")
toc()
# 5.784 sec elapsed
rm(mobpro_2019)

# Vroom from csv
tic()
mobpro_2019 <- vroom("FD_MOBPRO_2019.csv")
toc()
# 6.35 sec elapsed
rm(mobpro_2019)

# Arrow from csv
tic()
mobpro_2019 <- read_csv_arrow("FD_MOBPRO_2019.csv")
toc()
# 1.992 sec elapsed
rm(mobpro_2019)

# Save with qs
tic()
qsave(mobpro_2019, "mobpro_2019.qs")
time_qsave <- toc()
time_qsave
# 8.702 sec elapsed

# save in parquet with arrow
tic()
write_parquet(mobpro_2019, "mobpro_2019.parquet")
time_write_parquet <- toc()
time_write_parquet
# 2.257 sec elapsed

# file sizes
file.size("mobpro_2019.qs")
# 106517933
file.size("mobpro_2019.parquet")
# 188832719

# Read with qs
tic()
mobpro_2019 <- qread("mobpro_2019.qs")
time_qread <- toc()
time_qread
rm(mobpro_2019)
#5.423 sec elapsed

# Read with parquet
tic()
mobpro_2019 <- read_parquet("mobpro_2019.parquet")
time_read_parquet <- toc()
time_read_parquet
rm(mobpro_2019)
#1.939 sec elapsed
