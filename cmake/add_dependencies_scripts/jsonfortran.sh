set -e

mkdir -p external_dependencies
cd external_dependencies
if [ -d "json-fortran" ]; then
    echo "json-fortran folder exists"
    cd ..
    exit 0
fi
mkdir -p json-fortran
cd json-fortran

git clone https://github.com/jacobwilliams/json-fortran.git

cd ../..
