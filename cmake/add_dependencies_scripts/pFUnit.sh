set -e

mkdir -p external_dependencies
cd external_dependencies
if [ -d "pFUnit" ]; then
    echo "pFUnit folder exists"
    cd ..
    exit 0
fi
git clone --recursive https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git
cd pFUnit

mkdir build
cd build
cmake ..
# make tests
make install

cd ../../..
