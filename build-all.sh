echo "Compiling skill..."
npm run build
echo "Successfully compiled skill inside output/"

echo "Compiling skill manifest..."
npm run manifest
echo "Successfully compiled skill.json"

echo "Compiling language model..."
npm run model
echo "Successfully built language model in models/en-US.json"
