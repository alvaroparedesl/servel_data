aws s3 sync website/_book/. s3://analisislegislativo --profile myaws --acl public-read
aws cloudfront create-invalidation --distribution-id E2IGLSZ2P7FESQ --paths / --profile myaws

echo "Deployment complete"
pause