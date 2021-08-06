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
   	ask soil where(each.grid_x=0 and each.grid_y=1)
   	{
   		image_button <- agt.image_button;
   	}
   			
   	ask panel where(each.image_button != nil)
	{
		is_visible <- !is_visible;
	}
   }
   
	init
	{
		ask panel where(each.grid_x=0 and each.grid_y=0)
		{
			image_button <- "../includes/buttons/tany_mainty.png";
		}
		ask panel where(each.grid_x=0 and each.grid_y=2)
		{
			image_button <- "../includes/buttons/tany_mena.png";
		}
		ask panel where(each.grid_x=0 and each.grid_y=4)
		{
			image_button <- "../includes/buttons/tany_mavo.png";
		}
		
		
		ask panel where(each.grid_x=2 and each.grid_y=0)
		{
			image_button <- "../includes/buttons/vary.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=2)
		{
			image_button <- "../includes/buttons/patate.png";
		}
		ask panel where(each.grid_x=2 and each.grid_y=4)
		{
			image_button <- "../includes/buttons/tomate.png";
		}
		
		
		ask panel where(each.grid_x=4 and each.grid_y=0)
		{
			image_button <- "../includes/buttons/bovin.png";
		}
		ask panel where(each.grid_x=4 and each.grid_y=2)
		{
			image_button <- "../includes/buttons/porcin.png";
		}
		ask panel where(each.grid_x=4 and each.grid_y=4)
		{
			image_button <- "../includes/buttons/volaille.png";
		}
		
		ask panel where(each.grid_x=5 and each.grid_y=1)
		{
			image_button <- "../includes/buttons/NPK.png";
		}
		
		ask panel where(each.grid_x=5 and each.grid_y=3)
		{
			image_button <- "../includes/buttons/compost.png";
		}
		
		ask panel where(each.grid_x=5 and each.grid_y=5)
		{
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