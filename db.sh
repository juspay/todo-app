# Create configuration file for postgrest
echo "db-uri = \"postgres://authenticator:mysecretpassword@localhost:5432/$(whoami)\"
db-schemas = \"api\"
db-anon-role = \"todo_user\"" > tutorial.conf

# Load DB dump
psql < db.sql
