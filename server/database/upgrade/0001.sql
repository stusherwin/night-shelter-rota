do $$
begin

alter table volunteer add active boolean not null default true;

end $$