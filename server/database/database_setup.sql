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
, intro                  text    not null
, overnight_pref         char(1) null
, overnight_gender_pref  char(1) null
, notes                  text    not null
);

insert into volunteer (name, intro, overnight_pref, overnight_gender_pref, notes)
values ('Fred', 'Hi, I''ve been a volunteer here for 10 years, I like fish and I don''t want any trouble.
I should add that I really don''t like Tuesdays, and I have a secret phobia of falling down sideways into black forest gateaus. Or is it gateaux?', '2', null, '')
returning id
into v1Id;

insert into volunteer (name, intro, overnight_pref, overnight_gender_pref, notes)
values ('Alice', '', null, 'F', '')
returning id
into v2Id;

insert into volunteer (name, intro, overnight_pref, overnight_gender_pref, notes)
values ('Jim', 'Hi, I''ve been a volunteer here for 10 years, I like fish and I don''t want any trouble.
I should add that I really don''t like Tuesdays, and I have a secret phobia of falling down sideways into black forest gateaus. Or is it gateaux?', '1', 'M', '')
returning id
into v3Id;

insert into volunteer (name, intro, overnight_pref, overnight_gender_pref, notes)
values ('Mary', '', null, null, 'Only nice people')
returning id
into v4Id;

create table volunteer_shift
( volunteerId  integer not null references volunteer(id)
, shiftDate    date    not null
, shiftType    char(1) not null
, primary key (volunteerId, shiftDate)
);

insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2018-01-07', v1Id, 'O');
insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2018-01-07', v2Id, 'E');
insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2018-01-07', v3Id, 'O');
insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2018-01-07', v4Id, 'E');

insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2018-01-08', v1Id, 'O');
insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2018-01-08', v3Id, 'O');
insert into volunteer_shift (shiftDate, volunteerId, shiftType) values ('2018-01-08', v4Id, 'E');

-- grant all on all tables in schema public to shelter_rota_user;
-- grant all on all sequences in schema public to shelter_rota_user;
end $$