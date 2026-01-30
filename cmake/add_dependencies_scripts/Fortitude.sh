set -e

mkdir -p external_dependencies
cd external_dependencies
if [ -d "Fortitude" ]; then
    echo "Fortitude folder exists"
    cd ..
    exit 0
fi
mkdir -p Fortitude
cd Fortitude

python -m venv Fortitude_environment
source Fortitude_environment/bin/activate
pip install fortitude-lint
deactivate

cd ../..
