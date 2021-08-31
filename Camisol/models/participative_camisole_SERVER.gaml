/***
* Name: participative_camisole_SERVER
* Author: Lucas GROSJEAN, Nicolas MARILLEAU
***/

model participativecamisole

/*
 * todo : trier la liste de liste result pour montrer les meilleurs combinaisons
 * 
 */

global
{
	
	list<list<string>> result <- [];
	
	init{
		create network_species {
			do connect to: "localhost" protocol: "tcp_server" port: 3001 with_name: "Server";
		}
	}
	
}

species network_species skills:[network]
{
	reflex fetch when: has_more_message() { 
	    message mess <- fetch_message();
	    //write name + " fecth this message: " + mess.contents;   
	    
	    list<string> test <- mess.contents;
	    
	    add test to: result;
	    
	    write(test[0] +" "+ test[1] +" "+ test[2] +" "+ test[3] +" "+ test[4]);
	}
}

experiment run
{
	output
	{
		
	}
}