:- dynamic here/1.
:- dynamic have/1.
:- dynamic location/2.
:- dynamic turned_on/1.
:- dynamic opened/2.

:- consult('adventure_larkc_client').

room(kitchen).
room(office).
room(hall).
room(diningRoom).
room(cellar).
door(office, hall).
door(kitchen, office).
door(hall, diningRoom).
door(kitchen, cellar).
door(diningRoom, kitchen).
opened(office, hall).
opened(kitchen, office).
% opened(hall, diningRoom).
key_for_door(key,hall,diningRoom).
opened(kitchen, cellar).
opened(diningRoom, kitchen).
location(desk, office).
location(apple, kitchen).
%% location(flashlight, desk).
location(flashlight, office).
location(washingMachine, cellar).
location(nani, washingMachine).
location(broccoli, kitchen).
location(crackers, kitchen).
location(computer, office).
location(envelope, desk).
location(stamp, envelope).
location(key, envelope).

edible(apple).
edible(crackers).
tastes_yucky(broccoli).
here(kitchen).

where_food(X,Y) :-
    location(X,Y),
    edible(X).
where_food(X,Y) :-
    location(X,Y),
    tastes_yucky(X).

connect(X,Y) :- door(X,Y).
connect(X,Y) :- door(Y,X).

list_things(Place) :-
    list_things_s(Place).
list_things(Place) :-
    location(X, Place),
    tab(2),
    write(X),
    nl,
    fail.
list_things(_).

list_connections(Place) :-
    connect(Place, X),
    tab(2),
    write(X),
    nl,
    fail.
list_connections(_).

look :-
    here(Place),
    write('You are in the '), write(Place), nl,
    write('You can see:'), nl,
    list_things(Place),
    write('You can go to:'), nl,
    list_connections(Place).


look_in(Place) :-
    write('In '), write(Place), write(' are the following:'), nl,
    location(X, Place),
    tab(2), write(X), fail.
look_in(_).

goto(Place):-
    puzzle(goto(Place)),
    can_go(Place),
    move(Place),
    look.

can_go(Place):-
    here(X),
    connect(X,Place),
    is_opened(X,Place).
can_go(Place):-
    here(X),
    connect(X,Place),
    write('The door is shut.'), nl, fail.
can_go(Place):-
    write('You can''t get there from here.'), nl,
    fail.

move(Place):-
    larkc_retract(here(X)),
    larkc_asserta(here(Place)).

take(X):-
    can_take(X),
    take_object(X).

can_take(Thing) :-
    can_take_s(Thing).
can_take(Thing) :-
    here(Place),
    is_contained_in(Thing, Place).
can_take(Thing) :-
    write('There is no '), write(Thing),
    write(' here.'),
    nl, fail.

take_object(X) :-
    larkc_retract(location(X,_)),
    larkc_asserta(have(X)),
    write('taken'), nl.

put(X) :-
    can_put(X),
    put_object(X).

can_put(Thing) :-
    here(Place),
    have(Thing).
can_put(Thing) :-
    write('You cannot place '), write(Thing),
    write(' here.'),
    nl, fail.

put_object(X) :-
    here(Location),
    larkc_retract(have(X)),
    larkc_asserta(location(X,Location)),
    write('put'), nl.

inventory :-
    write('You have the following things:'), nl,
    have(X), tab(3), write(X), nl, fail.
inventory.

turn_on(X) :-
    can_turn_on(X),
    turn_on_object(X).
    
can_turn_on(X) :-
    have(X).
can_turn_on(X) :-
    write('You cannot turn on '), write(Thing),
    write('..'),
    nl, fail.

turn_on_object(X) :-
    larkc_asserta(turned_on(X)).

turn_off(X) :-
    can_turn_off(X),
    turn_off_object(X).
    
can_turn_off(X) :-
    have(X),
    turned_on(X).

can_turn_off(X) :-
    write('You cannot turn off '), write(Thing),
    write('..'),
    nl, fail.

turn_off_object(X) :-
    larkc_retract(turned_on(X)).

open_door(Location,OtherSide) :-
    can_open_door(Location,OtherSide),
    do_open_door(Location,OtherSide).

can_open_door(Location,OtherSide) :-
    here(Location),
    connect(Location,OtherSide).
can_open_door(Location,OtherSide) :-
    write('You cannot open the door from '), write(Location),
    write('.to '), write(OtherSide), write('.'),
    nl, fail.

% can_open_door(Location,OtherSide) :-
%     here(Location),
%     connect(Location,OtherSide).
%     key_for_door(Key,Location,OtherSide).
%     not(have(Key)),
%     write('You need the key'),nl,fail.
% can_open_door(Location,OtherSide) :-
%     here(Location),
%     connect(Location,OtherSide).
%     key_for_door(Key,Location,OtherSide).
%     have(Key).
% can_open_door(Location,OtherSide) :-
%     here(Location),
%     connect(Location,OtherSide).
%     not(key_for_door(Key,Location,OtherSide)).
% can_open_door(Location,OtherSide) :-
%     write('You cannot open the door from '), write(Location),
%     write('.to '), write(OtherSide), write('.'),
%     nl, fail.

is_opened(Location,OtherSide) :-
    opened(Location, OtherSide).
is_opened(Location,OtherSide) :-
    opened(OtherSide,Location).

do_open_door(Location,OtherSide) :-
	%% assert(opened(Location,OtherSide)).
	larkc_asserta(opened(Location,OtherSide)).

close_door(Location,OtherSide) :-
    can_close_door(Location,OtherSide),
    do_close_door(Location,OtherSide).

can_close_door(Location,OtherSide) :-
    here(Location),
    connect(Location,OtherSide),
    is_opened(Location,OtherSide).

can_close_door(Location,OtherSide) :-
    write('You cannot close the door from '), write(Location),
    write('.to '), write(OtherSide), write('.'),
    nl, fail.

do_close_door(Location,OtherSide) :-
    larkc_retract(opened(Location,OtherSide)).
do_close_door(Location,OtherSide) :-
    larkc_retract(opened(OtherSide,Location)).

is_contained_in(T1,T2) :-
    location(T1,T2).
is_contained_in(T1,T2) :-
    location(X,T2),
    is_contained_in(T1,X).

is_contained_in_b(T1,T2) :-
    location(T1,T2).
is_contained_in_b(T1,T2) :-
    location(T1,X),
    is_contained_in_b(X,T2).

% object(candle, red, small, 1).
% object(apple, red, small, 1).
% object(apple, green, small, 1).
% object(table, blue, big, 50). 

location_s(object(candle, red, small, 1), kitchen).
location_s(object(apple, red, small, 1), kitchen).
location_s(object(apple, green, small, 1), kitchen).
location_s(object(table, blue, big, 50), kitchen).

can_take_s(Thing):-
    here(Room),
    location_s(object(Thing, _, small, _), Room).
can_take_s(Thing) :-
    here(Room),
    location_s(object(Thing, _, big, _), Room),
    write('The '), write(Thing),
    write(' is too big to carry.'), nl,
    fail.
can_take_s(Thing) :-
    here(Room),
    not(location_s(object(Thing, _, _, _), Room)),
    write('There is no '), write(Thing), write(' here.'), nl,
    fail.

list_things_s(Place) :-
    location_s(object(Thing, Color, Size, Weight), Place),
    write('A '),write(Size),tab(1),
    write(Color),tab(1),
    write(Thing),write(', weighing '),
    write(Weight), write(' pounds'), nl,
    fail.

puzzle(goto(cellar)) :-
    have(flashlight),
    turned_on(flashlight),
    !.
puzzle(goto(cellar)) :-
    write('It''s dark and you are afraid of the dark.'),
    !, fail.
puzzle(_).
