set -e

mkdir -p external_dependencies
cd external_dependencies
if [ -d "fortran-syntax" ]; then
    echo "fortran-syntax folder exists"
    cd ..
    exit 0
fi
git clone https://github.com/cphyc/fortran-syntax.git
cd fortran-syntax
python -m venv Fortran_linter_environment
source Fortran_linter_environment/bin/activate
pip install . # Please note that depending on your installation, you may have to add sudo

cp ./../../cmake/add_dependencies_scripts/Fortran_linter/check.py ./check.py
# cat > "check.py" << EOL
# ...
# EOL

deactivate

cd ../..
