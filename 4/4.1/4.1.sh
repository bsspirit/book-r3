cat /etc/issue
cat /proc/cpuinfo 
cat /proc/meminfo 

sudo apt-get install r-base
R --version

sudo apt-get install libopenblas-base
sudo update-alternatives --config libblas.so.3

wget http://brettklamer.com/assets/files/statistical/faster-blas-in-r/R-benchmark-25.R
sudo update-alternatives --config libblas.so.3



