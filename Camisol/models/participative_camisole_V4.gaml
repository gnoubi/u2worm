/***
***/


model participativecamisole

import "camisole_adapter_V2.gaml" as SoilModel

global
{
	
	int end_cycle <- 500;
	int nb_cycle <- 500;
	int sub_simulation_cycle <- 0;
	
	int year <- 0;
	string current_compost <- nil;
	string current_soil <- nil;
	string current_landuse <- nil;
	
	int nb_charette_total;
	
	bool result_updated <- false;
	geometry shape <- square(10#cm);
	bool is_run <- false;
	float plot_area <- 10#m*10#m;
	geometry area_result <- polygon([point(1#cm,5#cm),point(9#cm,5#cm),point(9#cm,9#cm),point(1#cm,9#cm),point(1#cm,5#cm) ]);
	
	csv_file history_file_csv <- csv_file("../includes/history_file.csv");
	matrix history_file_matrix <- matrix(history_file_csv);
	
	action valid_button
	{
		point loc <- #user_location; // contains the location of the mouse in the world 
      	list<Valid_button> selected_agents <- Valid_button overlapping loc; // contains agents clicked by the event 
      	
  		ask world 
       	{
       		if(world.could_start_simulation()){
				do create_subsimulation;
	       		do run_subsimulation;
       		}else{
       			write("please select a soil and a species to produce");
       		}
       	}     
	}
	
	action select_button
	{ 
		point loc <- #user_location; // contains the location of the mouse in the world 
      	list<panel> selected_agents <- panel overlapping loc; // contains agents clicked by the event 
      
      	write("selected agent = "+selected_agents);
      	ask selected_agents
      	{
      		is_selected <- true;
      	}
      	if (empty(selected_agents where(each.image_button=nil or each.is_visible = false)))
     	{
			ask selected_agents
			{
		       	if(self.grid_x = 0)
		       	{
	       			ask panel
		       		{
		       			is_selected <- false;
		       		}
		       		ask world 
		       		{
		       			do select_soil(myself);
		       		}
		       	}	
		       	 
		       	if(self.grid_x = 2) // plantation
		       	{
		       		ask panel where(each.grid_x = 2)
		       		{
		       			is_selected <- false;   	
		       		}
		       		self.is_selected <- true;
		       		current_landuse <- self.usage;
		       	}
		       	
				if(self.grid_x = 4) // animal
		       	{
		       		ask panel where(each.grid_x = 4)
		       		{
		       			is_selected <- false;
		       		}
		       		self.is_selected <- true;
		       	}
				
				if(self.grid_x = 6) // compost
		       	{
		       		ask panel where(each.grid_x = 6)
		       		{
		       			is_selected <- false;
		       		}
		       		self.is_selected <- true;
		       		current_compost <- self.usage;
				}
	       	}
  		}
   	}	 
   
   	reflex print_production
   	{
   		write("production total in kilos : "+SoilModel.Simple[0].get_production_total());
   	}
	 
	bool could_start_simulation
	{
	 	return current_soil != nil and current_landuse != nil;	
	}
	
	action create_subsimulation
	{
		write("create sub simulation with "+current_landuse+" "+current_compost);
 		create SoilModel.Simple with: [soil_characteristics:: evaluate_soil(), species_to_produce::current_landuse, type_apport::current_compost] ;
   }
   
   action select_soil(panel agt)
	{
		agt.is_selected <- true;
		current_soil <- agt.usage;
		ask soil_gui where(each.grid_x=0 and each.grid_y=1) {
			image_button <- agt.image_button;
		}
	   			
		ask panel where(each.image_button != nil){
			is_visible <- !is_visible;
		}
	}
   
    map<string, float> evaluate_soil
    {
    	panel p <- (panel first_with(each.type="soil" and each.is_selected));
		map<string,float> res;
		switch(p.usage)
		{
			match "mena"  {write "mena"; res <- map(["C"::0.02108#gram/(#cm*#cm),"N"::0.00119#gram/(#cm*#cm),"P"::0.00062#gram/(#cm*#cm)]); } //21,08	1,19	0,62
			match "mavo"  {write "mavo"; res <- map(["C"::0.01708#gram/(#cm*#cm),"N"::0.00104#gram/(#cm*#cm),"P"::0.00059#gram/(#cm*#cm)]); } //17,08	1,04	0,59
			match "mainty"  {write "mainty"; res <- map(["C"::0.02157#gram/(#cm*#cm),"N"::0.00132#gram/(#cm*#cm),"P"::0.00077#gram/(#cm*#cm)]); }
			default   {write "mainty"; res <- map(["C"::0.02157#gram/(#cm*#cm),"N"::0.00132#gram/(#cm*#cm),"P"::0.00077#gram/(#cm*#cm)]); }
		}	
		return res;
    }
   
   action run_subsimulation
   {
		ask Charette
		{
			do die;
		}
		do update_history; 
		is_run <- true; 
	}
    
   action update_history {
		ask history where(each.grid_x = year)
		{
			soil_gui ssl <- soil_gui first_with(each.grid_x=0 and each.grid_y=1);
			//panel pp_soil <- panel first_with(each.grid_x = 0 and each.is_selected);
			self.image_button <- ssl.image_button;
			panel pp_culture <- panel first_with(each.grid_x = 2 and each.is_selected);
			self.subCulture <- pp_culture;
			world.current_landuse <- pp_culture.usage;
			list<panel> pp_amm <- panel where((each.grid_x = 4 or each.grid_x = 5)  and each.is_selected);
			self.subAmend <-  pp_amm;
			is_visible <- true;
		}
		year <- year + 1;
   }
   
   reflex update_cycle_sub_simulation when: sub_simulation_cycle != end_cycle
   {   	
    	ask (SoilModel.Simple collect each.simulation)
	    {
			do _step_;
	    }
	    sub_simulation_cycle <- sub_simulation_cycle +1;
   }
   
   reflex compute_result when: sub_simulation_cycle = end_cycle
   {
		do update_model;
  		do display_log;
  		do compute_charette;
  		do display_log;
		do write_history_in_file;
		
		do pause;
	}
	
   action update_model
   {
		is_run <- false;
		result_updated <- false;
   }   
   
   action display_log
   {
   		write "---------------------------------------";
  		write "Cl "+SoilModel.Simple[0].get_C_L();
  		write "Cr "+SoilModel.Simple[0].get_C_R();
  		write "N "+SoilModel.Simple[0].get_N();
  		write "P "+SoilModel.Simple[0].get_P();
  		write "iN "+SoilModel.Simple[0].get_DIM_N();
  		write "iP "+SoilModel.Simple[0].get_DIM_P();
   }
   
	action compute_charette
   	{
		nb_charette_total <- (SoilModel.Simple[0].get_production_without_charette() / 2) as int;
		int nb <- 0;
		create Charette number: nb_charette_total
		{
			location <- point(point(1#cm+2#cm*(nb mod 5),5#cm+2#cm*(nb div 5)));
			nb <- nb + 1;
		}
	}
	
	action write_history_in_file
	{
		save [current_soil, current_landuse,current_compost, SoilModel.Simple[0].get_production_total(), nb_charette_total, end_cycle, seed] 
		rewrite: false to: "../includes/history_file.csv" type: "csv";
	}

	init
	{
		create Valid_button{
			location <- point(5#cm,2.5#cm);
		}
		create Plant
		{
			location <- point(5#cm,2.5#cm);
			image_button <- "../includes/buttons/plant.png";
		}
		
		// sol
		ask panel where(each.grid_x=0 and each.grid_y=0)
		{
			type <- "soil";
			usage <- "mainty";
			image_button <- "../includes/buttons/mainty.png";
		}
		ask panel where(each.grid_x=0 and each.grid_y=3)
		{
			type <- "soil";
			usage <- "mena";
			image_button <- "../includes/buttons/mena.png";
		}
		ask panel where(each.grid_x=0 and each.grid_y=6)
		{
			type <- "soil";
			usage <- "mavo";
			image_button <- "../includes/buttons/mavo.png";
		}
		
		// plantation
		ask panel where(each.grid_x=2 and each.grid_y=0)
		{
			type <- "culture";
			usage <- "riz";
			image_button <- "../includes/buttons/riz.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=1)
		{
			type <- "culture";
			usage <- "patate";
			image_button <- "../includes/buttons/patate.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=2)
		{
			type <- "culture";
			usage <- "haricot";
			image_button <- "../includes/buttons/haricot.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=3)
		{
			type <- "culture";
			usage <- "ble";
			image_button <- "../includes/buttons/ble.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=4)
		{
			type <- "culture";
			usage <- "coton";
			image_button <- "../includes/buttons/coton.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=5)
		{
			type <- "culture";
			usage <- "soja";
			image_button <- "../includes/buttons/soja.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=6)
		{
			type <- "culture";
			usage <- "choux";
			image_button <- "../includes/buttons/choux.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=7)
		{
			type <- "culture";
			usage <- "brachiaria";
			image_button <- "../includes/buttons/brachiaria.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=8)
		{
			type <- "culture";
			usage <- "mais";
			image_button <- "../includes/buttons/mais.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=9)
		{
			type <- "culture";
			usage <- "manioc";
			image_button <- "../includes/buttons/manioc.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=10)
		{
			type <- "culture";
			usage <- "tomate";
			image_button <- "../includes/buttons/tomate.png";
		}
		
		// animal
		ask panel where(each.grid_x=4 and each.grid_y=0)
		{
			type <- "animal";
			usage <- "cow";
			image_button <- "../includes/buttons/bovin.png";
		}
		ask panel where(each.grid_x=4 and each.grid_y=2)
		{
			type <- "animal";
			usage <- "pig";
			image_button <- "../includes/buttons/porcin.png";
		}
		ask panel where(each.grid_x=4 and each.grid_y=4)
		{
			type <- "animal";
			usage <- "bird";
			image_button <- "../includes/buttons/volaille.png";
		}
		
		// amendement
		ask panel where(each.grid_x=6 and each.grid_y=1)
		{
			type <- "amandement";
			usage <- "NPK";
			image_button <- "../includes/buttons/NPK.png";
		}
		
		ask panel where(each.grid_x=6 and each.grid_y=3)
		{
			type <- "amandement";
			usage <- "compost";
			image_button <- "../includes/buttons/compost.png";
		}
		
		ask panel where(each.grid_x=6 and each.grid_y=5)
		{
			type <- "amandement";
			usage <- "debris";
			image_button <- "../includes/buttons/debris.png";
		}
			
		ask panel where(each.grid_x > 0 or each.image_button = nil)
		{
			is_visible <- false;
		}
	}
}



species Charette
{
	string image_button <- nil;
		
	init
	{
		shape <- circle(2#cm);
		image_button <- "../includes/buttons/charette.png";
	}
	
	aspect base
	{
		if(image_button != nil) {
			draw image_file(image_button) size:2#cm;
		}
	}
	
}

species Plant
{
	string image_button <- nil;
	float percent <- 0.0;
	
	reflex modify_h when:world.is_run
	{
		percent <- (1 - (((end_cycle - sub_simulation_cycle))/nb_cycle)) ;
	}
	aspect base
	{
		if(image_button != nil and world.is_run) {
			draw image_file(image_button) at:point(5#cm,5#cm) size:percent*8#cm;
		}
	}
}

species Valid_button
{
	string type <-nil;
	string usage <-nil;
	string image_button <- nil;
		
	init
	{
		shape <- circle(2#cm);
		type <-"start";
		usage <- "start";
		image_button <- "../includes/buttons/start.png";
	}
	aspect base
	{
		if(image_button != nil and !world.is_run) {
			draw image_file(image_button) size:2#cm;
		}
	}
}

grid soil_gui width:1 height:3
{
	string image_button <- nil;
	aspect base
	{
	 	if(image_button != nil)
		{
			draw image_file(image_button) size:self.shape.width ;
		}
	}
}

grid panel width:10 height:12
{
	string type <-nil;
	string usage <-nil;
	string image_button <- nil;
	bool is_visible <- true;
	bool is_selected <- false;
	
	aspect base
	{
		if(is_visible)
		{
			if(image_button != nil) {
				draw image_file(image_button) size:is_selected?self.shape.width*1.5:self.shape.width ;
			}
		}
	}
}

grid history width:5 height:1 schedules:[] {
	string image_button <- nil;
	bool is_visible <- false;
	panel subCulture <- nil;
	list<panel> subAmend<-[];
	
	aspect base
	{
		if(is_visible and image_button != nil)
		{
			draw image_file(image_button) size:self.shape.width ;
			draw image_file(subCulture.image_button) at:point(self.location.x,self.location.y-self.shape.width/4 ) size:self.shape.width/2 ;
			
			int i <- 1;
			loop p over:subAmend
			{
				draw image_file(p.image_button) at:point(self.location.x-self.shape.width/2 + self.shape.width/4*i,self.location.y+self.shape.width/4 ) size:self.shape.width/4 ;
				i <- i +1;
			}
		}
	}
}

grid history_file width:history_file_matrix.rows height:2 {
	
	string get_link_to_image(string name_image)
	{
		return "../includes/buttons/"+name_image+".png";
	}
	
	string get_production_for_row(int row)
	{
		return history_file_matrix[3,row];
	}
	
	action draw_history(int row)
	{
		draw image_file(get_link_to_image(history_file_matrix[0,row])) size:self.shape.width at: point((row*self.shape.width),self.shape.height);
		draw image_file(get_link_to_image(history_file_matrix[1,row])) size:self.shape.width/2 at: point((row*self.shape.width),self.shape.height);
		draw image_file(get_link_to_image(history_file_matrix[2,row])) size:self.shape.width/3 at: point((row*self.shape.width-self.shape.width/6),self.shape.height);
		draw ""+get_production_for_row(row) size:5 color:#black at:point((row*self.shape.width),self.shape.height*1.2);		
	}
	
	aspect base
	{
		loop row from: 1 to: history_file_matrix.rows - 1
		{
			do draw_history(row);	
		}
	}
}

experiment run
{
	output
	{
		display Choix type: java2D 
		{
			species panel aspect:base;
			event "mouse_down" action: select_button;
		}
		display Sol type: java2D 
		{
			species soil_gui aspect:base;
		}
		display History_Local type: java2D 
		{
			species history aspect:base;
		}
		display History_File type: java2D
		{
			species history_file aspect:base;
		}
		display Demarrer{
			species Valid_button aspect:base;
			species Charette aspect:base;
			species Plant aspect:base;
			event "mouse_down" action: valid_button;
		}
	}
}