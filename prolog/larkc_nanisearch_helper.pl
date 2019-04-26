loadNaniSearchIntoLarKC :-
	f(here,X1),
	cycAssert(['isa','here','UnaryPredicate'],'BaseKB',X2),
	cycAssert([arity,here,1],'BaseKB',X3),
	f(room,X4),
	cycAssert(['isa','room','Collection'],'BaseKB',X5),
	cycAssert(['arg1Isa','here','room'],'BaseKB',X6),
	f(kitchen,X7),
	cycAssert(['isa','kitchen','room'],'BaseKB',X8),
	f(office,X9),
	cycAssert(['isa','office','room'],'BaseKB',X10),
	f(cellar,X11),
	cycQuery(['isa','cellar','room'],'BaseKB',X12),
	cycAssert(['here','kitchen'],'BaseKB',X13),
	cycQuery(['here','?X'],'BaseKB',X14),
	writeln(X14).

%% %% are([kitchen,office,hall,diningRoom,cellar],room).

%% door(office, hall).
%% door(kitchen, office).
%% door(hall, diningRoom).
%% door(kitchen, cellar).
%% door(diningRoom, kitchen).

%% opened(office, hall).
%% opened(kitchen, office).
%% % opened(hall, diningRoom).
%% key_for_door(key,hall,diningRoom).
%% opened(kitchen, cellar).
%% opened(diningRoom, kitchen).
%% location(desk, office).
%% location(apple, kitchen).
%% location(flashlight, desk).
%% location(washingMachine, cellar).
%% location(nani, washingMachine).
%% location(broccoli, kitchen).
%% location(crackers, kitchen).
%% location(computer, office).
%% location(envelope, desk).
%% location(stamp, envelope).
%% location(key, envelope).
