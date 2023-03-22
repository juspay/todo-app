# Create configuration file for postgrest
mkdir data
echo "db-uri = \"postgres://authenticator:mysecretpassword@localhost:5432/$(whoami)\"
db-schemas = \"api\"
db-anon-role = \"todo_user\"" > data/db.conf

# Load DB dump
psql < db.sql
