aws ecr get-login-password --region us-west-2 | docker login --username AWS --password-stdin 396408709989.dkr.ecr.us-west-2.amazonaws.com
docker build -t request-reporter .
docker tag request-reporter:latest 396408709989.dkr.ecr.us-west-2.amazonaws.com/request-reporter:latest
docker push 396408709989.dkr.ecr.us-west-2.amazonaws.com/request-reporter:latest



