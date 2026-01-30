set -e

mkdir -p external_dependencies
cd external_dependencies
if [ -d "Ford" ]; then
    echo "Ford folder exists"
    cd ..
    exit 0
fi
mkdir -p Ford
cd Ford

python -m venv Ford_environment
source Ford_environment/bin/activate
pip install ford
deactivate

cd ../..
