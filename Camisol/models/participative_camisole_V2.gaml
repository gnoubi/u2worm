/***
***/

model participativecamisole

/* Insert your model definition here */
global
{
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
	       		ask panel where(each.is_selected and self.grid_x >=2)
	       		{
	       			is_selected <- false;
	       		}
	       		self.is_selected <- true;
	       		ask world 
	       		{
	       			//do select_soil(myself);
	       		}
	       	}
	       	if(self.grid_x >= 4)
	       	{
	       		self.is_selected <- !self.is_selected;
	       		ask world 
	       		{
	       			//do select_soil(myself);
	       		}
	       	}
	       
	       }
	       
	  }
   } 
   
   action select_soil(panel agt)
   {
   	agt.is_selected <- true;
   	ask soil where(each.grid_x=0 and each.grid_y=1) {
   		image_button <- agt.image_button;
   	}
   			
   	ask panel where(each.image_button != nil){
		is_visible <- !is_visible;
	}
   }
   
    action start_simulation
   	{
   		
   	}	
   		
    map<string, float> evaluate_soil
    {
    	panel p <- (panel first_with(each.type="soil" and each.is_selected));
		map<string,float> res;
		switch(p.usage)
		{
			match "mainty"  {res <- map(["C"::0.02157#gram,"N"::0.00132#gram,"P"::0.00077#gram]); }
			match "mena"  {res <- map(["C"::0.02108#gram,"N"::0.00119#gram,"P"::0.00062#gram]); } //21,08	1,19	0,62
			match "mavo"  {res <- map(["C"::0.01708#gram,"N"::0.00104#gram,"P"::0.00059#gram]); } //17,08	1,04	0,59
		}	
		return res;
    }

    map<string, float> evaluate_landuse
    {
    	panel p <- (panel first_with(each.type="soil" and each.is_selected));
		map<string,float> res;
		switch(p.usage)
		{
			match "rice"  {res <- map(["N"::12.5#gram/1#kilogram,"P"::1.5#gram/#kilogram]); }
			match "patatoes"  {res <- map(["N"::4.5#gram/1#kilogram,"P"::1.75#gram]); } //21,08	1,19	0,62
			match "beans"  {res <- map(["N"::37.5#gram/1#kilogram,"P"::4.5#gram/1#kilogram]); } //17,08	1,04	0,59
		}	
		return res;
    }

    map<string, float> evaluate_amendement
    {
    	panel p <- (panel first_with(each.type="soil" and each.is_selected));
		map<string,float> res;
		switch(p.usage)
		{
			match "cow"  {res <- map(["CL"::13.95*0.9102,"CR"::13.95*0.898,"N"::0.74,"P"::0.68]); }
			match "pig"  {res <- map(["CL"::21.41*0.8413,"CR"::21.41*0.1597,"N"::1.87,"P"::0.30]);  } //21,08	1,19	0,62
			match "bird"  {res <- map(["CL"::29.8*0.9218,"CR"::29.8*0.0782,"N"::2.11,"P"::1.55]); } //17,08	1,04	0,59
			match "NPK"  {res <- map(["CL"::0.34*0.9803,"CR"::0.34*1.97,"N"::0.04,"P"::8.29]); }
			match "compost"  {res <- map(["CL"::29.4*0.7806,"CR"::29.4*0.2194,"N"::1.96,"P"::0.19]); } //21,08	1,19	0,62
			match "debris"  {res <- map(["CL"::46.4*0.1783,"CR"::46.4*0.8217,"N"::15.5,"P"::0.12]); } //17,08	1,04	0,59
		}	
		return res;
    }

	init
	{
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


grid soil width:1 height:3
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

experiment run
{
	output
	{
		display Sol type: java2D 
		{
			species soil aspect:base;
		}
		display Control type: java2D 
		{
			species panel aspect:base;
			event "mouse_down" action: select_button;
		}
	}
}