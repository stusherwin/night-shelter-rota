do $$
declare rotaId integer;
begin

alter table volunteer add rota_id integer null references rota(id);

select id from rota into rotaId;

update volunteer set rota_id = rotaId;

alter table volunteer alter rota_id set not null;

end $$