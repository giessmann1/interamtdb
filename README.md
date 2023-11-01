# interamtdb
Project to build a research database based on job advertisements from interamt.de

TODOs before start
Create .secrets/ directory
mkdir .secrets/

Create mongodb_user.txt and mongodb_pwd.txt and set your own username and password (no update in Python scripts necessary). Be aware of newlines, which need to be avoided!

echo "username" > .secrets/mongodb_user.txt
echo "password" > .secrets/mongodb_pwd.txt

Create data repository
mkdir interamtdb-data/