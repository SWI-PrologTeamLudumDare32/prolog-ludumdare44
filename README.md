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

First ensure you have the genutils pack installed, e.g. by running
`swipl` and then running `pack_install(genutils).`

Start the server with

cd prolog/
swipl psyche_server.pl

The game is now served at http://localhost:9870/

The server uses Cyc as a back end. We have a pengines interface to Cyc.
This is served from the server partyserver.rocks

# Running LarKC_CL's Pengine Server on http://partyserver.rocks:9870/

Become the user aindilis on the partyserver.rocks system.  Then do:

cd /home/aindilis/prolog-ludumdare44/scripts

followed by:

./start-larkc-server.sh

That should start the server.  Note that if you are trying to get it
running on other systems besides partyserver.rocks or ai.frdcsa.org
you will need to change the env vars in

/home/aindilis/prolog-ludumdare44/scripts/envvars.sh

to point to the right location.  Add a conditional for your hostname
and then put the proper values in for LD44ROOT and LARKC_CL

Note that since distribution of LarKC_CL/RCyc is limited we are not
providing installation instructions at this time.  We are however
working to integrate LarKC_CL/OCyc as a drop in replacement.
