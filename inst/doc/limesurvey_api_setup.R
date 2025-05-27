## ----eval=FALSE---------------------------------------------------------------
# usethis::edit_r_environ();

## ----eval=FALSE---------------------------------------------------------------
# 
# ### Load the LimeSurvey environment variables into your R session:
# 
# options(
#   lime_api =
#     paste0(
#       "https://", Sys.getenv("LIMESURVEY_HOST"), "/admin/remotecontrol"
#     )
#   );
# options(lime_username = Sys.getenv("LIMESURVEY_USER"));
# options(lime_password = Sys.getenv("LIMESURVEY_PW"));
# 

## ----eval=FALSE---------------------------------------------------------------
# usethis::edit_r_profile();

