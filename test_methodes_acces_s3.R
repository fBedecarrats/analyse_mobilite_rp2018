library(arrow)
library(tidyverse)

my_key <- Sys.getenv("AWS_ACCESS_KEY_ID")
my_key_enc <- URLencode(my_key) # Au cas où on aurait des caractères spéciaux '+', '/'
my_secret <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
my_secret_enc <- URLencode(my_secret)

bucket <- arrow::s3_bucket("minio.lab.sspcloud.fr/fbedecarrats", 
                           region = "",
                           access_key = my_key,
                           secret_key = my_secret)


minio <- S3FileSystem$create(
  access_key = my_key,
  secret_key = my_secret,
  scheme = "https",
  endpoint_override = "localhost:9000"
)


minio$ls()

test <- aws.s3::bucketlist()
test <- aws.s3::get_bucket(bucket = "fbedecarrats")
objects <- aws.s3::get_bucket_df(bucket = "fbedecarrats", region = "")
test <- Sys.getenv()
