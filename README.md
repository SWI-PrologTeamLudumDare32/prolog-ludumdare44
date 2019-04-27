# Team Prolog Ludum Dare 44 Entry - Psyche

In preparation for Ludum Dare 44, Andrew Dougherty mated an old front
end the LGBTLaGrande team had written for LD41 with the nanisearch/
adventure.pl code from Amzi. 

This combination is a cool starting point for hacking adventure games in
Prolog.

The web interface is currently styled to look like an old VT300 terminal.

Thanks Andrew!

# Architecture

The game is served over the web, from a SWI-Prolog server. 
Start the server with

cd prolog/
swipl psyche_server.pl

The game is now served at http://localhost:9870/

The server uses Cyc as a back end. We have a pengines interface to Cyc.
This is served from the server partyserver.rocks

<Andrew, fill in how to run the back end server>


