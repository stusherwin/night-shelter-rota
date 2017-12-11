drop table if exists volunteer cascade;

CREATE TABLE volunteer (
    id                     serial primary key NOT NULL,
    name                   text NOT NULL,
    overnight_pref         char(1) NULL,
    overnight_gender_pref  char(1) NULL,
    notes                  text NOT NULL
);

INSERT INTO volunteer (name, overnight_pref, overnight_gender_pref, notes) values ('Fred', '2', null, '');
INSERT INTO volunteer (name, overnight_pref, overnight_gender_pref, notes) values ('Alice', null, 'F', '');
INSERT INTO volunteer (name, overnight_pref, overnight_gender_pref, notes) values ('Jim', '1', 'M', '');
INSERT INTO volunteer (name, overnight_pref, overnight_gender_pref, notes) values ('Mary', null, null, 'Only nice people');