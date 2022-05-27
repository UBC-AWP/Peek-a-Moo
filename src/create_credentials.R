library(shinymanager)

guest_pw <- Sys.getenv("GUEST_PW")
user_pw <- Sys.getenv("USER_PW")
admin_pw <- Sys.getenv("ADMIN_PW")
passphrase <- Sys.getenv("PASSPHRASE")

# Init DB using credentials data
credentials <- data.frame(
  user = c("guest", "user", "admin"),
  password = c(guest_pw, user_pw, admin_pw),
  admin = c(FALSE, FALSE, TRUE),
  stringsAsFactors = FALSE
)

# you can use keyring package to set database key
# library(keyring)
# key_set("R-shinymanager-key", "obiwankenobi")

# Init the database
create_db(
  credentials_data = credentials,
  sqlite_path = "auth/database.sqlite", # will be created
  # passphrase = key_get("R-shinymanager-key", "obiwankenobi")
  passphrase = passphrase
)
