/***
***/


model participativecamisole

import "camisole_adapter.gaml" as SoilModel

/* Insert your model definition here */
global
{
	
	int end_cycle <- -1;
	int nb_cycle <- 500;
	int sub_simulation_cycle <- 0;
	
	int year <- 0;
	string current_soil <-nil;
	string current_landuse<-nil;
	bool result_updated <- false;
	geometry shape <- square(10#cm);
	bool is_run <- false;
	float plot_area <- 10#m*10#m;
	geometry area_result <- polygon([point(1#cm,5#cm),point(9#cm,5#cm),point(9#cm,9#cm),point(1#cm,9#cm),point(1#cm,5#cm) ]);
	
	action valid_button
	{
		point loc <- #user_location; // contains the location of the mouse in the world 
      	list<Valid_button> selected_agents <- Valid_button overlapping loc; // contains agents clicked by the event 
      	if (!empty(selected_agents) and !empty(SoilModel.Simple) and !is_run and world.could_start_simulation())
      	{
      		ask world 
	       	{
	       		do run_subsimulation;
	       	}
      	}
      
	}
	action extract_charette(int nb, string landUse)
	{
		float size <- size_of_transportation(landUse);
		map<string,float> landuse_spec <-evaluate_landuse(landUse);
		float useN <- landuse_spec["N"]*size*nb/plot_area*SoilModel.Simple[0].get_area();
		float useP <- landuse_spec["P"]*size*nb/plot_area*SoilModel.Simple[0].get_area();
		ask SoilModel.Simple[0]
		{
			do extract_N_P(useN,useP);
		}
		
	}
	int count_charrette(float N, float P, string landUse)
	{
		
		map<string,float> landuse_spec <-evaluate_landuse(landUse);
		float sampleArea <- SoilModel.Simple[0].get_area();
		float SumN <- N/sampleArea*plot_area;
		float SumP <- P/sampleArea*plot_area;
		float useN <- landuse_spec["N"];
		float useP <- landuse_spec["P"];
		write "used N "+ SumN +" "+useN+" " +N;
		write "used P "+ SumP +" "+useP+" "+P;
		write "used by plant : (N) " + (SumN /useN)+ " ; (P) "+ (SumP /useP);

		float max_prod <- max_production(landUse);
		
		float resN <- min([max_prod/useN,(SumN /useN)]);
		float resP <- min([max_prod/useP,(SumP /useP)]);
		write "restricted used by plant : (N) " + resN+ " ; (P) "+ resP;
		
		int nb_char <- min([int(resN/size_of_transportation(landUse)),int(resP/size_of_transportation(landUse))]);
		

		return nb_char;
	}
	
	action select_button
	{ 
      point loc <- #user_location; // contains the location of the mouse in the world 
      list<panel> selected_agents <- panel overlapping loc; // contains agents clicked by the event 
      if (empty(selected_agents where(each.image_button=nil or each.is_visible = false)))
      {
      	  ask panel where(each.is_squared)
	      {
	      	is_squared <- false;
	      }
	      ask selected_agents
	       {
	       	is_squared <- true;
	       	if(self.grid_x = 6 and self.grid_y = 2)
	       	{
	       		is_squared <- false;
	       		ask world 
	       		{
	       			do run_subsimulation;
	       		}
				write "start simulation";
	       	}
	       	
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
	       	
	       	if(self.grid_x = 2)
	       	{
	       		ask panel where(each.is_selected and each.grid_x >=2)
	       		{
	       			is_selected <- false;
	       			
	       		}
	       		self.is_selected <- true;
	       		current_landuse <- self.usage;

	       	}
	       	if(self.grid_x >= 4)
	       	{
	       		if(!world.could_select_new_admenment() and !self.is_selected)
	       		{
	       			ask panel where(each.grid_x >=4 or each.grid_x >=5 )
		       		{
		       			is_selected <- false;
		       		}
	       		}
	       		else {
	       			self.is_selected <- !self.is_selected;
	       		}
	       	}
	       
	       }
	       
	  }
   } 
   
	 bool could_select_new_admenment
	 {
	 	int cc <- panel count(each.is_selected and (each.grid_x =4 or each.grid_x =5));
	 	return cc<3;	
	 }
	 
	 
	bool could_start_simulation
	 {
	 	return current_soil != nil and current_landuse != nil;	
	 }
   action create_subsimulation
   {
   	write "create sub simulation";
   	 create SoilModel.Simple with: [soil_characteristics:: evaluate_soil()] ;

   }
   
   action run_subsimulation
   {
   	  ask Charette
   	  {
   	  	do die;
   	  }
   	  if(sub_simulation_cycle>=end_cycle)
   	  {
   	  	do update_history;
 		do update_simulator;
 // pour les tests....
	  	end_cycle <- sub_simulation_cycle + nb_cycle;
   
   	  } 
   	   is_run <- true;
   	 
    }
    
    
   action update_simulator
   {
   		list<panel> pp_amm <- panel where((each.grid_x = 4 or each.grid_x = 5)  and each.is_selected);
   		float cl <- 0.0;
   		float cr <- 0.0;
   		float nN <- 0.0;
   		float pP <- 0.0;
   		
   		loop p over:pp_amm
   		{
   			map<string,float> data <-evaluate_amendement(p);
   			cl <- cl + data["CL"];
   			cr <- cr + data["CR"];
   			nN <- nN + data["N"];
   			pP <- pP + data["P"];
   		}
   		
   		   			write "Before nY ----------------------------";
	  		write "Cl "+cl+","+  cr+","+nN+","+pP+")";
	  		
	  		write "Cl "+first(SoilModel.Simple).get_C_L();
	  		write "Cr "+SoilModel.Simple[0].get_C_R();
	  		write "N "+nN +" "+SoilModel.Simple[0].get_N();
	  		write "P "+pP +" "+SoilModel.Simple[0].get_P();
	   		write "iN "+nN +" "+SoilModel.Simple[0].get_DIM_N();
	  		write "iP "+pP +" "+SoilModel.Simple[0].get_DIM_P();
	   		ask first(SoilModel.Simple) { //SoilModel.Simple[0]{
			do init_new_year(cl,  cr, nN, pP);
	   		}
   			  		write "After Ny----------------------------";
	  		write "Cl "+SoilModel.Simple[0].get_C_L();
	  		write "Cr "+SoilModel.Simple[0].get_C_R();
	  		write "N "+SoilModel.Simple[0].get_N();
	  		write "P "+SoilModel.Simple[0].get_P();
	  		write "iN "+nN +" "+SoilModel.Simple[0].get_DIM_N();
	  		write "iP "+pP +" "+SoilModel.Simple[0].get_DIM_P();
	   		
   		
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
   reflex play_simulation when:sub_simulation_cycle < end_cycle
   {
   		 int i <- 0;
   	   	 loop  while: i <  10
   		 {
	   	 	ask (SoilModel.Simple collect each.simulation)
		    {
				do _step_;
		    }
		    i <- i +1;
		    sub_simulation_cycle <- sub_simulation_cycle +1;
		    result_updated <- true;
		    	 	
   	 }
   	    	write "during step ----------------------------";
	  		write "Cl "+SoilModel.Simple[0].get_C_L();
	  		write "Cr "+SoilModel.Simple[0].get_C_R();
	  		write "N "+SoilModel.Simple[0].get_N();
	  		write "P "+SoilModel.Simple[0].get_P();
	  		write "iN "+SoilModel.Simple[0].get_DIM_N();
	  		write "iP "+SoilModel.Simple[0].get_DIM_P();
	   		write "end step ----------------------------";
	  		
   }
   
   
   reflex display_result when:sub_simulation_cycle = end_cycle and result_updated
   {
		is_run <- false;
		result_updated <- false;
		write "After simulation";
	  		write "Cl "+SoilModel.Simple[0].get_C_L();
	  		write "Cr "+SoilModel.Simple[0].get_C_R();
	  		write "N "+SoilModel.Simple[0].get_N();
	  		write "P "+SoilModel.Simple[0].get_P();
	  		write "iN "+SoilModel.Simple[0].get_DIM_N();
	  		write "iP "+SoilModel.Simple[0].get_DIM_P();
  		int nb_char <- count_charrette(SoilModel.Simple[0].get_DIM_N(),SoilModel.Simple[0].get_DIM_P(),current_landuse);
   		do extract_charette(nb_char,current_landuse);
   		int nb <- 0;
   		create Charette number:nb_char
   		{
   			location <- point(point(1#cm+2#cm*(nb mod 5),5#cm+2#cm*(nb div 5)));
   			nb <- nb + 1;
   	//		location <- any_location_in(area_result);
   		}
   			write "After calculus";
	  		write "Cl "+SoilModel.Simple[0].get_C_L();
	  		write "Cr "+SoilModel.Simple[0].get_C_R();
	  		write "N "+SoilModel.Simple[0].get_N();
	  		write "P "+SoilModel.Simple[0].get_P();
	  		write "iN "+SoilModel.Simple[0].get_DIM_N();
	  		write "iP "+SoilModel.Simple[0].get_DIM_P();
  
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
	do create_subsimulation;
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
 	map<string, float> evaluate_landuse
 	{
 		panel p <- (panel first_with(each.type="soil" and each.is_selected));
		return evaluate_landuse( p.usage);
 	}
   
    map<string, float> evaluate_landuse(string p)
    {
    	map<string,float> res;
    	switch(p)
		{
			match "rice"  {res <- map(["N"::12.5#gram,"P"::1.5#gram]); }
			match "patatoes"  {res <- map(["N"::4.5#gram,"P"::1.75#gram]); } //21,08	1,19	0,62
			match "beans"  {res <- map(["N"::37.5#gram,"P"::4.5#gram]); } //17,08	1,04	0,59
		}	
		return res;
    }
    float size_of_transportation(string p)
    {
    	float res;
    	switch(p)
		{
			match "rice"  {res <- 2.0; }
			match "patatoes"  {res <- 2.0; } //21,08	1,19	0,62
			match "beans"  {res <- 2.0; } //17,08	1,04	0,59
		}	
		return res;
    }

    float max_production(string p)
    {
    	float res;
    	switch(p)
		{
			match "rice"  {res <- 6000.0/(100#m*100#m); }
			match "patatoes"  {res <- 15000.0/(100#m*100#m); } //21,08	1,19	0,62
			match "beans"  {res <- 800.0/(100#m*100#m); } //17,08	1,04	0,59
		}	
		return res;
    }


    map<string, float> evaluate_amendement(panel p)
    {
    	map<string,float> res;
		switch(p.usage)
		{
			match "cow"  {res <- map(["CL"::13.95*0.9102,"CR"::13.95*0.0898,"N"::0.74,"P"::0.68]); }
			match "pig"  {res <- map(["CL"::21.41*0.8413,"CR"::21.41*0.1597,"N"::1.87,"P"::0.30]);  } //21,08	1,19	0,62
			match "bird"  {res <- map(["CL"::29.8*0.9218,"CR"::29.8*0.0782,"N"::2.11,"P"::1.55]); } //17,08	1,04	0,59
			match "NPK"  {res <- map(["CL"::0.34*0.9803,"CR"::0.34*0.0197,"N"::0.04,"P"::8.29]); }
			match "compost"  {res <- map(["CL"::29.4*0.7806,"CR"::29.4*0.2194,"N"::1.96,"P"::0.19]); } //21,08	1,19	0,62
			match "debris"  {res <- map(["CL"::46.4*0.1783,"CR"::46.4*0.8217,"N"::15.5,"P"::0.12]); } //17,08	1,04	0,59
		}	
		return res;
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
		ask panel where(each.grid_x=0 and each.grid_y=0)
		{
			type <-"soil";
			usage <-"mainty";
			image_button <- "../includes/buttons/tany_mainty.png";
		}
		ask panel where(each.grid_x=0 and each.grid_y=2)
		{
			type <-"soil";
			usage <-"mena";
			image_button <- "../includes/buttons/tany_mena.png";
		}
		ask panel where(each.grid_x=0 and each.grid_y=4)
		{
			type <-"soil";
			usage <-"mavo";
			image_button <- "../includes/buttons/tany_mavo.png";
		}
		
		
		ask panel where(each.grid_x=2 and each.grid_y=0)
		{
			type <-"culture";
			usage <-"rice";
			image_button <- "../includes/buttons/vary.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=2)
		{
			type <-"culture";
			usage <-"patatoes";
			image_button <- "../includes/buttons/patate.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=4)
		{
			type <-"culture";
			usage <- "beans";
			image_button <- "../includes/buttons/tomate.png";
		}
		
		
		ask panel where(each.grid_x=4 and each.grid_y=0)
		{
			type <-"amandement";
			usage <- "cow";
			image_button <- "../includes/buttons/bovin.png";
		}
		ask panel where(each.grid_x=4 and each.grid_y=2)
		{
			type <-"amandement";
			usage <- "pig";
			image_button <- "../includes/buttons/porcin.png";
		}
		ask panel where(each.grid_x=4 and each.grid_y=4)
		{
			type <-"amandement";
			usage <- "bird";
			image_button <- "../includes/buttons/volaille.png";
		}
		
		ask panel where(each.grid_x=5 and each.grid_y=1)
		{
			type <-"amandement";
			usage <- "NPK";
			image_button <- "../includes/buttons/NPK.png";
		}
		
		ask panel where(each.grid_x=5 and each.grid_y=3)
		{
			type <-"amandement";
			usage <- "compost";
			image_button <- "../includes/buttons/compost.png";
		}
		
		ask panel where(each.grid_x=5 and each.grid_y=5)
		{
			type <-"amandement";
			usage <- "debris";
			image_button <- "../includes/buttons/debris.png";
		}
			
		ask panel where(each.grid_x>0 or  each.image_button = nil)
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

grid panel width:7 height:7
{
	string type <-nil;
	string usage <-nil;
	string image_button <- nil;
	bool is_visible <- true;
	bool is_selected <- false;
	bool is_squared <- false;
	aspect base
	{
		if(is_visible)
		{
			if(is_squared) {
				draw shape border:#red;
			}
			if(image_button != nil) {
				draw image_file(image_button) size:is_selected?self.shape.width*1.5:self.shape.width ;
			}
		}
	}
}

grid history width:5 height:1 schedules:[] {
	string image_button <- nil;
	bool is_visible <- false;
	panel subCulture<-nil;
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

experiment run
{
	output
	{
		display Sol type: java2D 
		{
			species soil_gui aspect:base;
		}
		display Choix type: java2D 
		{
			species panel aspect:base;
			event "mouse_down" action: select_button;
		}
		display history type: java2D 
		{
			species history aspect:base;
		}
		display Demarrer{
			species Valid_button aspect:base;
			species Charette aspect:base;
			species Plant aspect:base;
			event "mouse_down" action: valid_button;
			
		}
	}
}