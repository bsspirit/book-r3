sudo docker pull r-base
sudo docker images
sudo docker run -ti --rm r-base

mkdir ret && cd ret
pwd

sudo docker build -t a.r .
sudo docker run a.r

sudo docker tag 9a48d6dc02fe bsspirit/ret
sudo docker push bsspirit/ret
sudo docker run bsspirit/ret
