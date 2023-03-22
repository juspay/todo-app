-- This schema has been taken from https://postgrest.org/en/stable/tutorials/tut0.html
create schema api;

create table if not exists api.todos (
  id serial primary key,
  done boolean not null default false,
  task text not null
);
insert into api.todos (task) values
  ('finish tutorial 0'), ('pat self on back');


create role authenticator noinherit login password 'mysecretpassword';
create role todo_user nologin;
grant todo_user to authenticator;

-- Granting all permissions for tutorial purposes only
-- Follow this to use JWT and secure your api calls: https://postgrest.org/en/stable/tutorials/tut1.html
grant all on schema api to todo_user;
grant all on api.todos to todo_user;
grant all on sequence api.todos_id_seq to todo_user;

-- To reset the sequence
create function api.reset_id()
returns void
language plpgsql
as
$$
begin
   ALTER SEQUENCE api.todos_id_seq RESTART WITH 1;
end;
$$;

-- This is needed because to change ownership of sequence, the same owner has to own the table
-- TODO: Find a better way to do this without changing ownership 
ALTER TABLE api.todos OWNER TO todo_user;
ALTER SEQUENCE api.todos_id_seq OWNER TO todo_user;
