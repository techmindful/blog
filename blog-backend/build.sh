echo ">>>Building the app..."
stack install --pedantic

echo ">>>Copying app here..."
mv ~/.local/bin/blog-backend-exe .

