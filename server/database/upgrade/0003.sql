do $$
begin

alter table volunteer_shift rename volunteerId to volunteer_id;
alter table volunteer_shift rename shiftDate to shift_date;
alter table volunteer_shift rename shiftType to shift_type;

end $$