:- module(adventure, [list_things/1,
                      list_connections/1,
                      look/0,
                      look_in/1,
                      goto/1,
                      move/1,
                      put/1,
                      inventory/0,
                      turn_on/1,
                      turn_off/1,
                      open_door/2,
                      close_door/2,
                      puzzle/1,
                      error_input/0,
                     init_game_state/0]).

:- use_module(library(http/http_session)).
:- use_module(library(pengines)).

:- dynamic current_hold/2.

hold(X) :-
    pengine_self(Session),
    current_hold(Session, X).

retractall_hold(X) :-
    pengine_self(Session),
    retractall(current_hold(Session, X)).

asserta_hold(X) :-
    pengine_self(Session),
    asserta(current_hold(Session, X)).

/*
:- dynamic here/1.
:- dynamic have/1.
:- dynamic location/2.
:- dynamic turned_on/1.
:- multifile opened/2.
:- dynamic opened/2.
*/

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
key_for_door(key,hall,diningRoom).
edible(apple).
edible(crackers).
tastes_yucky(broccoli).

:- dynamic described/2.

longd(office, 'home office, with a desk, computer, work lamp, and chair').
shortd(office, 'home office').

long_description(X, Long) :-
    longd(X, Long),
    !.
long_description(X, X).

short_description(X, Long) :-
    shortd(X, Long),
    !.
short_description(X, X).



describe(X, D) :-
    pengine_self(ID),
    (   \+ described(ID, X)
    ->  asserta(described(ID, X)),
        long_description(X, D)
    ;   short_description(X, D)
    ).

write_description(X) :-
    describe(X, D),
    write(D).


init_game_state :-
    maplist(asserta_hold, [
                opened(office, hall),
                opened(kitchen, office),
                % opened(hall, diningRoom),

                opened(kitchen, cellar),
                opened(diningRoom, kitchen),
                location(desk, office),
                location(apple, kitchen),
                % location(flashlight, desk),
                location(flashlight, office),
                location(washingMachine, cellar),
                location(nani, washingMachine),
                location(broccoli, kitchen),
                location(crackers, kitchen),
                location(computer, office),
                location(envelope, desk),
                location(stamp, envelope),
                location(key, envelope),
                here(kitchen)
            ]).


where_food(X,Y) :-
    hold(location(X,Y)),
    edible(X).
where_food(X,Y) :-
    hold(location(X,Y)),
    tastes_yucky(X).

connect(X,Y) :- door(X,Y).
connect(X,Y) :- door(Y,X).

list_things(Place) :-
    list_things_s(Place).
list_things(Place) :-
    hold(location(X, Place)),
    tab(2),
    write_description(X),
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
    hold(here(Place)),
    describe(Place, Desc),
    write('You are in the '), write(Desc), nl,
    write('You can see:'), nl,
    list_things(Place),
    write('You can go to:'), nl,
    list_connections(Place).


look_in(Place) :-
    write('In '), write_description(Place), write(' are the following:'), nl,
    hold(location(X, Place)),
    tab(2), write_description(X), fail.
look_in(_).

goto(Place):-
    puzzle(goto(Place)),
    can_go(Place),
    move(Place),
    look.

can_go(Place):-
    hold(here(X)),
    connect(X,Place),
    is_opened(X,Place).
can_go(Place):-
    hold(here(X)),
    connect(X,Place),
    write('The door is shut.'), nl, fail.
can_go(_):-
    write('You can''t get there from here.'), nl,
    fail.

move(Place):-
    retractall_hold(here(_)),
    asserta_hold(here(Place)).

take(X):-
    can_take(X),
    take_object(X).

can_take(Thing) :-
    can_take_s(Thing).
can_take(Thing) :-
    hold(here(Place)),
    is_contained_in(Thing, Place).
can_take(Thing) :-
    write('There is no '),
    write_description(Thing),
    write(' here.'),
    nl, fail.

take_object(X) :-
    retractall_hold(location(X,_)),
    asserta_hold(have(X)),
    write('taken'), nl.

put(X) :-
    can_put(X),
    put_object(X).

can_put(Thing) :-
    hold(here(_)),
    hold(have(Thing)).
can_put(Thing) :-
    write('You cannot place '),
    write_description(Thing),
    write(' here.'),
    nl, fail.

put_object(X) :-
    hold(here(Location)),
    retractall_hold(have(X)),
    asserta_hold(location(X,Location)),
    write('put'), nl.

inventory :-
    write('You have the following things:'),
    nl,
    hold(have(X)),
    tab(3),
    write_description(X),
    nl,
    fail.
inventory.

turn_on(X) :-
    can_turn_on(X),
    turn_on_object(X).

can_turn_on(X) :-
    hold(have(X)).
can_turn_on(X) :-
    write('You cannot turn on '),
    write_description(X),
    write('..'),
    nl, fail.

turn_on_object(X) :-
    asserta_hold(turned_on(X)).

turn_off(X) :-
    can_turn_off(X),
    turn_off_object(X).

can_turn_off(X) :-
    hold(have(X)),
    hold(turned_on(X)).

can_turn_off(X) :-
    write('You cannot turn off '),
    write_description(X),
    write('..'),
    nl, fail.

turn_off_object(X) :-
    retractall_hold(turned_on(X)).

open_door(Location,OtherSide) :-
    can_open_door(Location,OtherSide),
    do_open_door(Location,OtherSide).

can_open_door(Location,OtherSide) :-
    hold(here(Location)),
    connect(Location,OtherSide).
can_open_door(Location,OtherSide) :-
    write('You cannot open the door from '),
    write_description(Location),
    write('.to '),
    write_description(OtherSide),
    write('.'),
    nl,
    fail.

% can_open_door(Location,OtherSide) :-
%     hold(here(Location)),
%     connect(Location,OtherSide).
%     key_for_door(Key,Location,OtherSide).
%     not(hold(have(Key))),
%     write('You need the key'),nl,fail.
% can_open_door(Location,OtherSide) :-
%     hold(here(Location)),
%     connect(Location,OtherSide).
%     key_for_door(Key,Location,OtherSide).
%     hold(have(Key)).
% can_open_door(Location,OtherSide) :-
%     hold(here(Location)),
%     connect(Location,OtherSide).
%     not(key_for_door(Key,Location,OtherSide)).
% can_open_door(Location,OtherSide) :-
%     write('You cannot open the door from '), write(Location),
%     write('.to '), write(OtherSide), write('.'),
%     nl, fail.

is_opened(Location,OtherSide) :-
    hold(opened(Location, OtherSide)).
is_opened(Location,OtherSide) :-
    hold(opened(OtherSide,Location)).

do_open_door(Location,OtherSide) :-
	%% assert(opened(Location,OtherSide)).
	asserta_hold(opened(Location,OtherSide)).

close_door(Location,OtherSide) :-
    can_close_door(Location,OtherSide),
    do_close_door(Location,OtherSide).

can_close_door(Location,OtherSide) :-
    hold(here(Location)),
    connect(Location,OtherSide),
    is_opened(Location,OtherSide).

can_close_door(Location,OtherSide) :-
    write('You cannot close the door from '),
    write_description(Location),
    write('.to '),
    write_description(OtherSide),
    write('.'),
    nl, fail.

do_close_door(Location,OtherSide) :-
    retractall_hold(opened(Location,OtherSide)).
do_close_door(Location,OtherSide) :-
    retractall_hold(opened(OtherSide,Location)).

is_contained_in(T1,T2) :-
    hold(location(T1,T2)).
is_contained_in(T1,T2) :-
    hold(location(X,T2)),
    is_contained_in(T1,X).

is_contained_in_b(T1,T2) :-
    hold(location(T1,T2)).
is_contained_in_b(T1,T2) :-
    hold(location(T1,X)),
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
    hold(here(Room)),
    location_s(object(Thing, _, small, _), Room).
can_take_s(Thing) :-
    hold(here(Room)),
    location_s(object(Thing, _, big, _), Room),
    write('The '),
    write_description(Thing),
    write(' is too big to carry.'), nl,
    fail.
can_take_s(Thing) :-
    hold(here(Room)),
    not(location_s(object(Thing, _, _, _), Room)),
    write('There is no '),
    write_description(Thing),
    write(' here.'), nl,
    fail.

list_things_s(Place) :-
    location_s(object(Thing, Color, Size, Weight), Place),
    write('A '),write(Size),tab(1),
    write(Color),tab(1),
    write_description(Thing),
    write(', weighing '),
    write(Weight), write(' pounds'), nl,
    fail.

puzzle(goto(cellar)) :-
    hold(have(flashlight)),
    hold(turned_on(flashlight)),
    !.
puzzle(goto(cellar)) :-
    write('It''s dark and you are afraid of the dark.'),
    !, fail.
puzzle(_).

error_input :-
    write('I can\'t understand that').
