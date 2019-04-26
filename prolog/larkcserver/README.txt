# Larkc server

first, run the server
1. swipl larkc_server.pl
?- go.

Now start another terminal and start swipl

?- use_module(library(pengines)).
?- pengine_rpc('http://localhost:9880/', do_larkc_server(hi, bye)).



Look in larkc_api, you will see do_larkc_server/2

You can add whatever predicates you want in here. Do 3 things to make them available
from the remote side.

1. define the predicate (duh)
2. export the predicate in the module call at the top.
3. add a clause to sandbox:safe_primitive/1 at the bottom that unifies with any call to your
new predicate.

