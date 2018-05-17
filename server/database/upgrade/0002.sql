do $$
begin

create table rota
( id                     serial  not null  primary key
, key                    text    not null
);

insert into rota (key)
values ('TESTROTA');

end $$