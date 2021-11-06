# https://cran.r-project.org/web/packages/Microsoft365R/vignettes/od_sp.html
library(Microsoft365R)
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

od$download_file(src=odpath, dest=here::here("boyd", "Boyd_CABU6_static.xlsx"), overwrite = TRUE)

# str(item)
# item$get_path()
# od$get_path()
# 
# od$download_file(dest="C:/Users/donbo/Downloads/", odpath)
# 
# 
# url <- "https://1drv.ms/x/s!AM1z6InC8nJgkAU"
# 
# url <- "https://onedrive.live.com/edit.aspx?cid=6072f2c289e873cd"
# url <- "https://1drv.ms/x/s!As1z6InC8nJgkAXlu5PwSkglYhqo?e=BA6ipn"
# url <- "https://onedrive.live.com/edit.aspx?cid=6072f2c289e873cd&page=view&resid=6072F2C289E873CD!2053&parId=6072F2C289E873CD!2045&app=Excel"
# read_excel(url, sheet="retplan_parameters", skip = 1)