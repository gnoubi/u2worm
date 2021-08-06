/***
* Name: camisol
* Author: nicolas
* Description: 
***/

model camisol

/* Insert your model definition here */
global
{
	string ORGANIC <- "organic";
	string MINERAL <- "mineral";
	string PORE <- "pore";
	
	geometry shape <- square(1#cm);
	
	//Charactéristiques des décomposeurs :
	float temps_division_opportuniste <- 1#h;
	float temps_division_decomposeur <- 2#h;
	
	float dividing_time_copiotrophe_R <- 1#h;
	float dividing_time_copiotrophe_K <- 12#h;
	float dividing_time_oligotrophe <- 168#h;
	
	float growth_decomposeur <- 0.5/#mn;
	
	float CO2_produit; 
	
	float C_Speed_opportuniste <- 0.00001#gram/#mn;
	float C_Speed_decomposeur <- 0.00001#gram/#mn;
	
	
	// 1 et 30 nematode / gram de sol
	
	// poid sec 3*10-13 gram dont 50% de carbone
	// 20 min par génération 
	// 2*1,5 10_13 gram dont 1,5 en CO2 en 20min
	//int macro_pop_unit <- 1000;
	float poids_sec_bacterie <-  0.0000000000003#gram;
	//float taux_respiration <- 0.5;
	
	float taux_excression_P <- 0.7;
	float taux_excression_N <- 0.15;


	
	list<particle> pores <- nil;
	list<particle> organic_particles <- nil;
	
	float C_labile ->{(sum(particle where( each.my_type = PORE) collect(each.C_labile)))};
	float C_recalcitrant ->{(sum(particle where( each.my_type = PORE) collect(each.C_recalcitrant)))};
	
	float pop_init_Cop_R;
	float pop_init_Cop_K;
	float pop_init_Oligo;
	float pop_copio_R ->{sum (copiotrophe_R collect(each.C))};
	float pop_copio_K -> {sum (copiotrophe_K collect(each.C))};
	float pop_oligo -> {sum (oligotrophe_K collect(each.C))};
	int count_pores -> {particle count ( each.my_type = PORE) };
	float pop_init <- 0.05* 1.5#gram/(#mm*#mm)*world.shape.area;
	float K_init <- 10*pop_init;
	
		
		
	reflex 	apport when:every(20#day)
	{
		ask organic_particles{
		C_labile <- 0.75#gram/(#mm*#mm)*self.shape.area;
		C_recalcitrant <- 0.75#gram/(#mm*#mm)*self.shape.area;
		N <- 0.04#gram/(#mm*#mm)*self.shape.area; 
		P <- 0.04#gram/(#mm*#mm)*self.shape.area;  
		
		}
	}	
	init
	{
		step <- 1#hour;
	//	create soil number:1;
		pores <- particle where(each.my_type = PORE);
		pop_init <- 0.05* 1.5#gram/(#mm*#mm)*world.shape.area;
		K_init <- 1.2*pop_init;
			
		organic_particles<- particle where(each.my_type = ORGANIC);
		ask particle{
			my_neighbors_organics <- neighbors where(each.my_type = ORGANIC);
			K_C <- 0.0;
		}
		
		int nb_pores <- length(pores);
		
		ask pores //particle where(each.my_type = PORE)
		{
			K_C <- K_init / nb_pores;
			write "K_C "+K_C;
			particle pp <- particle(self);
			create Dam number:1{
				associated_particle <- pp;
				associated_particle.dam <- self;
			}
/**
 * voir fichier de paramétrisation excel
 * 		Espece	C	N	P
 *	Copiotrophe R	1,42857E-05	2,85714E-06	1,42857E-06
 *	Copiotrophe  K	3,33333E-05	6,66667E-06	3,33333E-06
 *	Oligotrohphe	0,00002	0,000004	0,000002
 * 
 */			
			
			create copiotrophe_R number:1 {
				porous_cell <- pp;
				location <- any_location_in(pp.shape);
				add self to:pp.populations;
				//N/min	P/min	C/min	C/N	C/P
				//0,000001	0,0000005	0,00001	10	20	
			}
			create copiotrophe_K number:1 {
				porous_cell <- pp;
				location <- any_location_in(pp.shape);
				add self to:pp.populations;
			}
			create oligotrophe_K number:1 {
				porous_cell <- pp;
				location <- any_location_in(pp.shape);
				add self to:pp.populations;
			}  
		} 
		pop_init_Cop_R <- sum (copiotrophe_R collect(each.C));
		pop_init_Cop_K <- sum (copiotrophe_K collect(each.C));
		pop_init_Oligo <- sum (oligotrophe_K collect(each.C));
		
		//int nb_nema <- round(20/(#mm*#mm)*world.shape.area);
		//write nb_nema;
		create nematode number:200
		{
			do move;
		} 
	 	
	}
	
	reflex scheduleSimulation 
	{
		CO2_produit <- 0;
		ask Dam{
			do decompe;
		}
		ask pores{
			do microbe_eat;
		}
	}
	
}



species soil  schedules:[]//skills:[camisole]
{
	init
	{
		//location <- point(10#cm,10#cm,10#cm);
	}
}



// Dam solution de matière accessible
species Dam //solution inorganique du sol
{	//solution organique du sol
	//					N	P	C
	list<float> dom <- [0.0,0.0,0.0];
	//solution inorganique du sol
	//					N	P
	list<float> dim <- [0.0,0.0];
	particle associated_particle;
	float C_N ->{(dom at 2)/((dom at 0) +(dim at 0))};
	float C_P ->{(dom at 2)/((dom at 1) +(dim at 1))};
	
	float available_C ->{dom at 2};
	float available_N ->{((dom at 0) +(dim at 0))};
	float available_P ->{((dom at 1) +(dim at 1))};
	
	float enzyme_Cl <- 0.0;
	float enzyme_Cr <- 0.0;
	float enzyme_P <- 0.0;
	float enzyme_N <- 0.0;
	
	action inject_enzim(float e_Cl,float e_Cr,float e_P,float e_N)
	{
		enzyme_Cl <- enzyme_Cl + e_Cl;
		enzyme_Cr <- enzyme_Cr + e_Cr;
		enzyme_P <- enzyme_P + e_P;
		enzyme_N <- enzyme_N + e_N;
	}
		
	action decompe
	{
		list<particle> organics <- shuffle(associated_particle.my_neighbors_organics + self.associated_particle);
		float add_C <- 0.0;
		float add_P <- 0.0;
		float add_N <- 0.0;
		float taille <- length(organics)+0.0;
		ask organics
		{
			float dec_Cl <- min([C_labile,myself.enzyme_Cl/taille]);
			C_labile  <- C_labile - dec_Cl;
			float dec_Cr <- min([C_recalcitrant,myself.enzyme_Cr/taille]);
			C_recalcitrant  <- C_recalcitrant - dec_Cr;
			add_C <- add_C + dec_Cl+ dec_Cr;
			
			float dec_P <- min([P,myself.enzyme_P/taille]);
			P <- P - dec_P;
			add_P <- add_P + dec_P;
			
			float dec_N <- min([N,myself.enzyme_N/taille]);
			N <- N - dec_N;
			add_N <- add_N + dec_N;
		}
		dom <- [dom[0]+add_N,dom[1]+add_P,dom[2]+add_C];
		enzyme_Cl <- 0.0;
		enzyme_Cr <- 0.0;
		enzyme_P <- 0.0;
		enzyme_N <- 0.0;
	}
	
}

grid particle width:30 height:30 
{
	string my_type <- one_of([ORGANIC,MINERAL,PORE]);
	//string my_type <- grid_x = 1 and grid_y = 1 ? PORE:ORGANIC; // one_of([ORGANIC,MINERAL,PORE]);
	
	list<particle> my_neighbors <-neighbors;
	list<particle> my_neighbors_organics <- nil; //ps where(each.my_type = "organic")
	list<particle> my_neighbors_local <- neighbors;
	
	float C_labile;
	float C_recalcitrant;
	float N;
	float P;
	
	
	Dam dam;
	
	float total_bacteria_C -> {sum(populations collect(each.C))};
	float K_C;
	
	list<microbes_P> populations<- [];
	
	float pop_init ;
	
	

	init
	{
		//qte carbone labile et recalcitrant par gram volume de sol
		N <- gauss(12,3)#gram;
		P <- gauss(3,0.3)#gram;
		switch(my_type)
		{
			match MINERAL { 
					C_labile <- 0.0;
					C_recalcitrant <- 0.0; 
					N <- 0.0004; 
					P <- 0.0004;  
			}
			match ORGANIC { 
					C_labile <- 0.2#gram/(#mm*#mm)*self.shape.area;
					C_recalcitrant <- 0.002#gram/(#mm*#mm)*self.shape.area;
					N <- 0.04#gram/(#mm*#mm)*self.shape.area; 
					P <- 0.04#gram/(#mm*#mm)*self.shape.area;  
					
			}
			
			default { 
				C_labile <- 0.0#gram;
				C_recalcitrant <- 0.0#gram;  
				N <- 0.3; 
				P <- 0.3; 
				create dam returns:tmp;
				dam <- first(tmp);
			}
		}
	
	}
		
	/*
	 *  [Croissance, Ratio Carbone/respiration, Qte de matiere ingérée,
	 *  4,5 10-13g masse d'une bactérie (avec eau) (2micrometre) (Masse volumique 0.9 Kg/l)  et 10-14g de carbone 
	 *  K masse Carbone biomasse
	 */
	
	
	action microbe_eat{
		float total_C_wanted <- sum(populations collect(each.C_wanted));
		float total_N_wanted <- sum(populations collect(each.N_wanted));
		float total_P_wanted <- sum(populations collect(each.P_wanted));
		
		float C_Soil <- min([dam.available_C,total_C_wanted]);
		float N_Soil <- min([dam.available_N, total_N_wanted]);
		float P_Soil <- min([dam.available_P,total_P_wanted]);
		ask populations {
			float C_rate  <- total_C_wanted>0 ? C_wanted / total_C_wanted:0;
			float N_rate  <- total_N_wanted>0 ?N_wanted / total_N_wanted:0;
			float P_rate  <- total_P_wanted>0 ?P_wanted / total_P_wanted:0;
			
			float C_consum <- C_Soil * C_rate;
			float N_consum <- N_Soil * N_rate;
			float P_consum <- P_Soil * P_rate;
			
			income_C <- income_C + C_consum;
			income_N <- income_N + N_consum;
			income_P <- income_P + P_consum;

			myself.dam.dom[2]<- myself.dam.dom[2] - C_consum;
			myself.dam.dom[1]<- myself.dam.dom[1] - P_consum;
			myself.dam.dom[0]<- myself.dam.dom[0] - N_consum;
		}
	}
	
	rgb selectColor
	{
		switch(my_type)
		{
			match "pore" { return #yellow;}
			match "mineral" { return #red;}
			match "organic" { return #green;}
			default { return #blue;}
		}
	}
	aspect base
	{
		if(my_type != "porous")
		{
		//	draw cube(size) at:location color:selectColor();
		}
	}
	
	aspect default
	{
		rgb mcolor <- #blue;
		switch(my_type)
		{
			match MINERAL {mcolor <- #black;}
			match ORGANIC {mcolor <- #green;}
			match PORE {mcolor <- #white;}
		}
		draw shape color:mcolor;
	}
	
}


species microbes_P //skills:[soil_process]
{

	float threshold_DOC <- 0.5;
	
	// quantity of C N P in the colony
	float C ;
	float N ;
	float P ;
	
	float taux_respiration <- 0.5;
	
	// carbone fraichement assimilé //A modifier selon les paramètres
	float assimilated_C <- 0; //0.000001#gram; //0.000001#gram;
	float assimilated_N <- 0; //0.000001#gram;
	float assimilated_P <- 0; //0.000001#gram;
	
	float income_C <- 0#gram; //0.000001#gram;
	float income_N <- 0#gram;
	float income_P <- 0#gram;
	
	
	float dividing_time <- 0.0;

	float C_assimilation_speed <- 0.0 update: C * ( step / dividing_time)/(1-taux_respiration);

		
	float C_decomposing_speed;
	float N_decomposing_speed;
	float P_decomposing_speed;
	
	//taux labile récalcitrant
	float L_R_rate;
	float C_N;
	float C_P;
	
	float CO2_producted <- 0.0;
	
	particle porous_cell;
	//float decomposing_speed <- 0.00001#gram/#mn;
	float C_wanted -> {C_assimilation_speed };
	float N_wanted -> { (C+assimilated_C+C_wanted) / C_N - (assimilated_N + N)};
	float P_wanted -> { (C+assimilated_C+C_wanted) / C_P - (assimilated_P + P)};
	float DOC_assimilation_friction;
	float death_rate <- 0.0;
	
	action respirate
	{
		// 4.09+0.37*argile+limonFin <-- carbonne à la saturation...
		//write "C   "+ income_C+ " ";
		float production_co2 <- income_C * taux_respiration;
		income_C <- income_C  - production_co2; 
		CO2_producted <- production_co2;
	}
	
	action growth {
		float new_individual <- C * ( step/dividing_time) * (1- self.porous_cell.total_bacteria_C / self.porous_cell.K_C);
		
		new_individual <- min([new_individual , float(assimilated_C ) ]);
		float assi_C_N <- assimilated_N * C_N;  //(assimilated_N=0)?0:(assimilated_C/assimilated_N);
		float assi_C_P <- assimilated_P * C_P; //(assimilated_P=0)?0:(assimilated_C/assimilated_P);
		//qte N suffisante ? 
		new_individual <- min([new_individual, (assi_C_N) ]);
		//write "new individual 3 " + assimilated_C; 
		
		new_individual <- min([new_individual, (assi_C_P) ]);
		
		//write "new individual 4 " + assimilated_C; 
		float transfert_C <- new_individual;
		float transfert_N <- transfert_C / C_N;
		float transfert_P <- transfert_C / C_P;
		
		assimilated_C <- assimilated_C - transfert_C;
		assimilated_N <- assimilated_N - transfert_N;
		assimilated_P <- assimilated_P - transfert_P;
		C <- transfert_C + C;
		N <- transfert_N + N;
		P <- transfert_P + P;
		//write "ajout indi" + species(self).name + " "+transfert_C +" " +C;
		
		
	}
	
	action death
	{
		/*porous_cell.C_labile <- C * death_rate + porous_cell.C_labile;
		porous_cell.C_labile <- assimilated_C * death_rate + porous_cell.C_labile;
		C <-C - C * death_rate ;
		assimilated_C <-assimilated_C - assimilated_C * death_rate ;
		*///un taux de mortalité par espèce....
	}
	
	float decompose
	{
		//write species(self).name + " "+C_wanted;
		ask porous_cell.dam
		{
			do inject_enzim(myself.L_R_rate*myself.C_wanted,(1-myself.L_R_rate)*myself.C_wanted,myself.P_wanted,myself.N_wanted);
		}
		return 0.0;
	}
	
	reflex life
	{
		CO2_producted <- 0;
		
		bool sleeping <-C_wanted=0?true: (income_C/C_wanted) >=threshold_DOC;
		
		
		if(sleeping)
		{
			do respirate;
			do growth;
			do decompose;
			assimilated_C <- assimilated_C + income_C;
			assimilated_N <- assimilated_N + income_N;
			assimilated_P <- assimilated_P + income_P;
		}
		else
		{
			ask porous_cell
			{
				dam.dom[2]<- dam.dom[2] + myself.income_C;
				dam.dom[1]<- dam.dom[1] + myself.income_P;
				dam.dom[0]<- dam.dom[0] + myself.income_N;
			}
		}
			income_C<-0.0;
			income_N<-0.0;
			income_P<-0.0;

		
		//		1 - respiration 
//		2 - basale
//		3 - enzime

		do death;
		
	}

	
	aspect base
	{
		draw circle(1#cm) color:#yellow;
	}
}

species copiotrophe_R parent:microbes_P
{
	init{
		dividing_time <- dividing_time_copiotrophe_R;
		//C_assimilation_speed <- 1*3.33333E-05#gram/#mn; //1.42857E-05#gram/#mn;
		
		L_R_rate <- 1.0;
		
		C_decomposing_speed <- 1.42857E-05;
		N_decomposing_speed <- 2.85714E-06;
		P_decomposing_speed <- 1.42857E-06;
		C_N <- 5.0;
		C_P <- 10.0;
		
		C <- pop_init/count_pores/3; //*0.1; //0.000001#gram;
		N <- C/ C_N;
		P <- C/ C_P;
		
		threshold_DOC <- 0.5;
		taux_respiration <- 0.4;
		death_rate <- 0.0001;
	}	
	
		
	aspect base
	{
		draw circle(0.5#cm) color:#blue;
	}
}
species copiotrophe_K parent:microbes_P
{
	init{
		dividing_time <- dividing_time_copiotrophe_K;
	//	C_assimilation_speed<- 1.42857E-05#gram/#mn; //3.33333E-05#gram/#mn;
		
		L_R_rate <- 0.5;
		
		C_decomposing_speed <- 3.33333E-05;
		N_decomposing_speed <- 6.66667E-06;
		P_decomposing_speed <- 3.33333E-06;
		C_N <- 5.0;
		C_P <- 10.0;
		C <- pop_init/count_pores/3; //*0.2;//0.000001#gram;
		N <- C/ C_N;
		P <- C/ C_P;
		
		threshold_DOC <- 0.3;
		taux_respiration <- 0.6;
		death_rate <- 0.00001;
		
		
	}	
	aspect base
	{
		draw circle(0.5#cm) color:#red;
	}
}

species oligotrophe_K parent:microbes_P
{
	init{
		dividing_time <- dividing_time_oligotrophe;
	//	C_assimilation_speed<- 0.1*1.42857E-05#gram/#mn;// 0.00002#gram/#mn;
		
		L_R_rate <- 0.8;
		
		C_decomposing_speed <- 0.00002;
		N_decomposing_speed <- 0.000004;
		P_decomposing_speed <- 0.000002;
		C_N <- 5.0;
		C_P <- 10.0;
		
		C <- pop_init/count_pores/3;//*0.7;//0.000001#gram;
		N <- C/ C_N;
		P <- C/ C_P;
		
		threshold_DOC <-0.0; // 0.0;
		taux_respiration <- 0.7;
		death_rate <- 0.0;
		

	}
	aspect base
	{
		draw circle(0.5#cm) color:#pink;
	}
}


species nematode
{
	float threshold_DOC <- 0.5;
	
	float C <- 0.0;
	float N <- 0.0;
	float P <- 0.0;
		
	particle my_pore <- nil;
	
	bool is_sleeping <- false;
	float predation_rate <- 100*0.00001#gram/#day; // 0.5E-9#gram/#mn; //un nematode mange entre 10K et 100k bacterie par jour -> moins de 10k il dort
	float wanted_C <- predation_rate * step;
	float available_C <- 0;
	
	reflex do_action
	{
		available_C <- sum(my_pore.populations collect(each.C+ each.assimilated_C));
		
		do sleepm;
		if(true or !is_sleeping)
		{
			do eat;
			do move;
		}
	}
	
	action sleepm 
	{
		is_sleeping <- available_C/wanted_C > 0.5;
		
	}
	

	action eat 
	{
		float available_N <- sum(my_pore.populations collect(each.N+ each.assimilated_N));
		float available_P <- sum(my_pore.populations collect(each.P+ each.assimilated_P));
		float catch_C <- min([available_C,wanted_C ]);
		ask my_pore.populations
		{
			float C_to_catch <- min([C,catch_C * C/2/myself.available_C]);
			float assi_C_to_catch <- min([assimilated_C, catch_C * assimilated_C/2/myself.available_C]);
			
		//	write C_to_catch;
			
			float N_to_catch <- min([N,N * C/myself.available_C]);
			float P_to_catch <- min([P,P * C/myself.available_C]);
			
			
			assimilated_C <-assimilated_C- assi_C_to_catch;
			C <-C - C_to_catch ;
			N <-P - N_to_catch ;
			P <-P - P_to_catch ;

			myself.my_pore.C_labile <-myself.my_pore.C_labile + C_to_catch + assi_C_to_catch;
			myself.my_pore.dam.dim[0] <-myself.my_pore.dam.dim[0]+ N_to_catch ;
			myself.my_pore.dam.dim[1] <-myself.my_pore.dam.dim[1]+ P_to_catch ;	
		}
		
	}
	
	action move 
	{
		particle my_new_pore <- one_of(pores);
		location <-  my_new_pore.location;
		my_pore <- my_new_pore;
	}
}

experiment ex type:gui
{
	//Qualité des apports et azote et phosphore
	
	output
	{
		display map  background:#black
		{
			species particle ;
		/* 	species copiotrophe_R aspect:base;
			species copiotrophe_K aspect:base;
			species oligotrophe_K aspect:base;*/
		}
		
		display "Population" type: java2D 
		{
			chart "Population" type: series 
			{
				data "Copiotrophe R" value: pop_copio_R - pop_init_Cop_R color: #blue ;// style: spline;
				data "Copiotrophe K" value: pop_copio_K - pop_init_Cop_K color: #red;// style: spline;
				data "oligotrophe" value: pop_oligo - pop_init_Oligo color: #green ;//style: spline;
			}

		}
		display "Carboon in soil" type: java2D 
		{
			chart "Carboon in soil" type: series
			{
				data "C Labile" value: sum(particle collect(each.C_labile)) color: #blue style: spline;
				data "C Recalcitrant" value: sum(particle collect(each.C_recalcitrant)) color: #orange style: spline;
				data "C in DOM" value: sum(particle where (each.my_type = PORE) collect(each.dam.dom[2])) color: #red style: spline;
			}

		}
	 display "CO2" type: java2D 
		{
			chart "C02" type: series
			{
				data "CO2 copiotrophe_R" value: sum(copiotrophe_R collect(each.CO2_producted)) color: #blue style: spline;
				data "CO2 copiotrophe_K" value: sum(copiotrophe_K collect(each.CO2_producted)) color: #red style: spline;
				data "CO2 oligotophe" value: sum(oligotrophe_K collect(each.CO2_producted)) color: #green style: spline;
			}

		}
	 	 
			
	}
}