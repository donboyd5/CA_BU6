


# establish onedrive connection -------------------------------------------

od <- get_personal_onedrive()
od$list_items()
od$list_files("Documents")
# # folder: CA_BU6_OneDrive
a <- od$list_files("Documents/CA_BU6_OneDrive")
a
# str(a)
odfn <- "Boyd_CABU6.xlsx"
odpath <- "Documents/CA_BU6_OneDrive/Boyd_CABU6.xlsx"
# item <- od$get_item("Documents/CA_BU6_OneDrive/Boyd_CABU6.xlsx")

static <- here::here("boyd", "Boyd_CABU6_static.xlsx")



# update parameters -------------------------------------------------------

od$download_file(src=odpath, dest=static, overwrite = TRUE)
prange <- "B2:K49"
df1 <- read_excel(static, sheet="retplan_parameters", range=prange, skip = 1)
df1

df2 <- df1 %>%
  filter(!is.na(col)) %>%
  pivot_longer(cols=-col, names_to = "xletter") %>%
  pivot_wider(names_from = col)
df2

idvars <- c("xletter", "stabbr", "tier", "tname")
lvars <- c("ss_covered", "db_covered", "db_cola_compound", "dc_covered")
cvars <- NULL
nvars <- setdiff(names(df2), c(idvars, lvars, cvars))
nvars
df3 <- df2 %>%
  mutate(across(all_of(lvars), as.logical),
         across(all_of(nvars), as.numeric))
df3


# review parameters -------------------------------------------------------

  


# url <- "https://1drv.ms/x/s!As1z6InC8nJgkAXlu5PwSkglYhqo?e=vsLU52"
# url <- "https://1drv.ms/x/s!As1z6InC8nJgkAXlu5PwSkglYhqo"
# 
# 
# download.file(url, here::here("boyd", "temp.xlsx"), mode="wb")
# 
# read_excel(url, sheet="retplan_parameters", skip = 1)


