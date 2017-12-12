do $$
declare v1Id integer;
declare v2Id integer;
declare v3Id integer;
declare v4Id integer;
begin

drop table if exists volunteer_shift cascade;
drop table if exists volunteer cascade;

create table volunteer
( id                     serial  not null  primary key
, name                   text    not null
, overnight_pref         char(1) null
, overnight_gender_pref  char(1) null
, notes                  text    not null
);

insert into volunteer (name, overnight_pref, overnight_gender_pref, notes)
values ('Fred', '2', null, '')
returning id
into v1Id;

insert into volunteer (name, overnight_pref, overnight_gender_pref, notes)
values ('Alice', null, 'F', '')
returning id
into v2Id;

insert into volunteer (name, overnight_pref, overnight_gender_pref, notes)
values ('Jim', '1', 'M', '')
returning id
into v3Id;

insert into volunteer (name, overnight_pref, overnight_gender_pref, notes)
values ('Mary', null, null, 'Only nice people')
returning id
into v4Id;

create table volunteer_shift
( volunteerId  integer not null references volunteer(id)
, shiftDate    date    not null
, shiftType    char(1) not null
, primary key (volunteerId, shiftDate)
);

insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2017-12-12', v1Id, 'O');
insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2017-12-12', v2Id, 'E');
insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2017-12-12', v3Id, 'O');
insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2017-12-12', v4Id, 'E');

insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2017-12-13', v1Id, 'O');
insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2017-12-13', v3Id, 'O');
insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2017-12-13', v4Id, 'E');

grant all on all tables in schema public to shelter_rota_user;
grant all on all sequences in schema public to shelter_rota_user;
end $$