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
	
	geometry shape <- square(20#cm);
	float seuilOM <- 100;
	float seuilN <- 100;
	float seuilP <- 100;
	
	//Charactéristiques des décomposeurs :
	float temps_division_opportuniste <- 1#h;
	float temps_division_decomposeur <- 2#h;
	
	float dividing_time_copiotrophe_R <- 1#h;
	float dividing_time_copiotrophe_K <- 12#h;
	float dividing_time_oligotrophe <- 24#h;
	
	float growth_decomposeur <- 0.5/#mn;
	
	float CO2_produit; 
	
	float C_Speed_opportuniste <- 0.00001#gram/#mn;
	float C_Speed_decomposeur <- 0.00001#gram/#mn;
	
	
	// 1 et 30 nematode / gram de sol
	
	// poid sec 3*10-13 gram dont 50% de carbone
	// 20 min par génération 
	// 2*1,5 10_13 gram dont 1,5 en CO2 en 20min
	
	float poids_sec_bacterie <- 0.0000000000003;
	float taux_respiration <- 0.5;
	
	float taux_excression_P <- 0.7;
	float taux_excression_N <- 0.15;
	
	init
	{
		step <- 1#hour;
	//	create soil number:1;
	
		ask particle where(each.my_type = PORE)
		{
			particle pp <- self;
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
			create oligotrophe number:1 {
				porous_cell <- pp;
				location <- any_location_in(pp.shape);
				add self to:pp.populations;
			}
		} 	
	}
}



species soil //skills:[camisole]
{
	init
	{
		location <- point(10#cm,10#cm,10#cm);
	/* 	do granulometry organic_matter:0.5#gram/#kg  min:2#mm ;													//MO1	//
		do granulometry mineral_matter:164.9#gram/#kg organic_matter:0.04#gram/#kg  min:0.2#mm max:2#mm;     	//MO2   //décomposeur mineur
		do granulometry mineral_matter:92.5#gram/#kg organic_matter:2.03#gram/#kg  min:0.05#mm max:0.2#mm;		//MO3
		do granulometry mineral_matter:86.5#gram/#kg 	min:0.02#mm max:0.05#mm;								//MO3
		do granulometry mineral_matter:232.1#gram/#kg   min:0.002#mm max:0.02#mm;								//MO3
		do granulometry mineral_matter:423.9#gram/#kg   max:0.002#mm;  											//MO3
		do granulometry organic_matter: 28.13 #gram/#kg   max:0.05#mm;											//MO3
		
		do initialize_soil size:0.2#m dividing_factor:10 bulk_density:1.3 number_of_try:2000 default_species:particle;
		
		do associate particle:"pore" from_canvas:"C1" at_scale:2 with_process:Microbes;
		
		
		do apply_processes;
		* 
		* 
		* 
		*/
	}
}



// Dam solution de matière accessible
species Dam schedules:[]//solution inorganique du sol
{	//solution organique du sol
	//					N	P	C
	list<float> dom <- [0.0,0.0,0.0];
	//solution inorganique du sol
	//					N	P
	list<float> dim <- [0.0,0.0];
	particle associated_particle;
	float C_N ->{(dom at 2)/((dom at 0) +(dim at 0))};
	float C_P ->{(dom at 2)/((dom at 1) +(dim at 1))};
	
	action decompe(float enzyme_Cl,float enzyme_Cr,float enzyme_P,float enzyme_N)
	{
		list<particle> ps <- associated_particle.my_neighbors;
		list<particle> organics <- shuffle(ps where(each.my_type = "organic"));
		
		float add_C <- 0.0;
		float add_P <- 0.0;
		float add_N <- 0.0;
		
		ask organics
		{
			float dec_Cl <- min([C_labile,enzyme_Cl]);
			C_labile  <- C_labile - dec_Cl;
			float dec_Cr <- min([C_recalcitrant,enzyme_Cr]);
			C_recalcitrant  <- C_recalcitrant - dec_Cr;
			add_C <- add_C + dec_Cl+ dec_Cr;
			
			float dec_P <- min([P,enzyme_P]);
			P <- P - dec_P;
			add_P <- add_P + dec_P;
			
			float dec_N <- min([N,enzyme_N]);
			N <- N - dec_N;
			add_N <- add_N + dec_N;
		}
		dom <- [dom[0]+add_N,dom[1]+add_P,dom[2]+add_C];
	}
	
}

grid particle width:100 height:100 schedules:[]
//species particle skills:[apsf_particle] schedules:[]
{
	string my_type <- one_of([ORGANIC,MINERAL,PORE]);
	list<particle> my_neighbors ->{neighbors};
	list<particle> my_neighbors_local ->{neighbors};
	
	float C_labile;
	float C_recalcitrant;
	float N;
	float P;
	
	Dam dam;
	
	float total_population -> {sum(populations collect(each.population))};
	
	list<microbes_P> populations<- [];
	
	
	
	init
	{
		//qte carbone labile et recalcitrant par gram volume de sol
		
		
		N <- gauss(12,3)#gram;
		P <- gauss(3,0.3)#gram;
		switch(my_type)
		{
			match "mineral" { 
					C_labile <- 0.0;
					C_recalcitrant <- 0.0; 
			}
			default { 
				C_labile <- 0.0;
				C_recalcitrant <- 0.0;  
				N <- 0.0; 
				P <- 0.0; 
			}
		}
		
		
	}
	

	
	/*
	 *  [Croissance, Ratio Carbone/respiration, Qte de matiere ingérée,
	 *  4,5 10-13g masse d'une bactérie (avec eau) (2micrometre) (Masse volumique 0.9 Kg/l)  et 10-14g de carbone 
	 *  K masse Carbone biomasse
	 */
	reflex mineur
	{
		//décompose la matiere organique plus évoluée c'est à dire en dessous de 50 micron
		
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
	float K <- 250000/(#mm*#mm);
	float population;
	float threshold_DOC <- 0.5;
	
	// quantity of C N P in the colony
	float C;
	float N;
	float P;
	
	// carbone fraichement assimilé //A modifier selon les paramètres
	float assimilated_C <- 0.000001#gram;
	float assimilated_N <- 0.000001#gram;
	float assimilated_P <- 0.000001#gram;
	

	
	float dividing_time <- 0.0;

	float C_assimilation_speed;
	float C_decomposition_efficiency;
	
		
	float C_decomposing_speed;
	float N_decomposing_speed;
	float P_decomposing_speed;
	
	//taux labile récalcitrant
	float L_R_rate;
	float C_N;
	float C_P;
	
	particle porous_cell;
	//float decomposing_speed <- 0.00001#gram/#mn;
	float C_wanted -> {C_assimilation_speed * step * population};
	float N_wanted -> { C_wanted / C_N};
	float P_wanted -> { C_wanted / C_P};
	
	
	float DOC_assimilation_friction;
	
	
	action respirate
	{
		float production_co2 <- assimilated_C * taux_respiration;
		assimilated_C <- assimilated_C  - production_co2; 
		CO2_produit <- production_co2 + CO2_produit;
	}
	
	action growth {
		float new_individual <- population* (1 - (self.porous_cell.total_population) / K)  * (step/dividing_time);
		//qte carbone suffisante ?
		new_individual <- min([new_individual,assimilated_C / poids_sec_bacterie ]);
		//qte N suffisante ? 
		new_individual <- min([new_individual, (C_N/assimilated_N)/poids_sec_bacterie ]);
		//qte P suffisante ? 
		new_individual <- min([new_individual,(C_P/assimilated_P)/poids_sec_bacterie ]);
		
		float transfert_C <- assimilated_C - new_individual*poids_sec_bacterie;
		float transfert_N <- transfert_C / C_N;
		float transfert_P <- transfert_C / C_P;
		
		population <- population + new_individual;
		assimilated_C <- assimilated_C - transfert_C;
		assimilated_N <- assimilated_N - transfert_N;
		assimilated_P <- assimilated_P - transfert_P;
		C <- transfert_C + C;
		N <- transfert_N + N;
		P <- transfert_P + P;
	}
	
	action death
	{
		//un taux de mortalité par espèce....
	}
	
	action decompose
	{
		//assimilated_C * 
		
		float C_to_get <- C_decomposition_efficiency *assimilated_C;
	}
	
	reflex life
	{
		do respirate;
		do growth;
		
//		1 - respiration 
//		2 - basale
//		3 - enzime

		do death;
		
	}
	reflex decompose
	{
		ask porous_cell.dam{
			//do decompose()
		}
	}
	
	aspect base
	{
		draw circle(1#cm) color:#blue;
	}
}

species copiotrophe_R parent:microbes_P
{
	init{
		dividing_time <- dividing_time_copiotrophe_R;
		C_assimilation_speed<- 1.42857E-05#gram/#mn;
		
		L_R_rate <- 1.0;
		
		C_decomposing_speed <- 1.42857E-05;
		N_decomposing_speed <- 2.85714E-06;
		P_decomposing_speed <- 1.42857E-06;
		C_N <- 5.0;
		C_P <- 10.0;
	}	
	
		
	aspect base
	{
		draw circle(0.05#cm) color:#blue;
	}
}
species copiotrophe_K parent:microbes_P
{
	init{
		dividing_time <- dividing_time_copiotrophe_K;
		C_assimilation_speed<- 3.33333E-05#gram/#mn;
		
		L_R_rate <- 0.5;
		
		C_decomposing_speed <- 3.33333E-05;
		N_decomposing_speed <- 6.66667E-06;
		P_decomposing_speed <- 3.33333E-06;
		C_N <- 5.0;
		C_P <- 10.0;
	}	
	aspect base
	{
		draw circle(0.05#cm) color:#red;
	}
}
species oligotrophe parent:microbes_P
{
	init{
		dividing_time <- dividing_time_oligotrophe;
		C_assimilation_speed<- 0.00002#gram/#mn;
		
		L_R_rate <- 1.0;
		
		C_decomposing_speed <- 0.00002;
		N_decomposing_speed <- 0.000004;
		P_decomposing_speed <- 0.000002;
		C_N <- 5.0;
		C_P <- 10.0;
	}
	aspect base
	{
		draw circle(0.05#cm) color:#yellow;
	}
}


species Microbes //skills:[soil_process]
{
	float K <- 250000/(#mm*#mm);
	float pop_opportuniste; //proche des opportunistes
	float pop_decomposeur; // proche des mineurs
	float C_opportuniste;
	float C_decomposeur ; 
	float N_decomposeur;
	float N_opportuniste;
	float P_opportuniste;
	float P_decomposeur;
	
	float number_microbe -> {pop_opportuniste + pop_decomposeur };
//
//	reflex manger
//	{
//		list<particle> particles <- my_neighbors_local;
//		particles <- particles where(each != nil and each.N > 0 and each.P > 0);
//		
//		float C_labile_neighbors <- sum(particles collect(each.C_labile));
//		float C_stable_neighbors <- sum(particles collect(each.C_recalcitrant));
//		
//		float N_neighbors <- sum(particles collect(each.N));
//		float P_neighbors <- sum(particles collect(each.P));
//		
//		float C_wanted_decomposeur <- C_Speed_decomposeur * step * pop_decomposeur;
//		float N_wanted_decomposeur <- C_wanted_decomposeur / C_N;
//		float P_wanted_decomposeur <- C_wanted_decomposeur / C_P;
//		
//		float C_wanted_opportuniste <- C_Speed_opportuniste * step * pop_opportuniste;
//		float N_wanted_opportuniste <- C_wanted_opportuniste / C_N;
//		float P_wanted_opportuniste <- C_wanted_opportuniste / C_P;	
//		
//		float min1 <- min([ (C_labile_neighbors - C_wanted_opportuniste), 
//							(C_stable_neighbors - C_wanted_decomposeur),
//							(N_neighbors - (N_wanted_decomposeur + N_wanted_opportuniste) ),
//							(P_neighbors - (P_wanted_decomposeur + P_wanted_opportuniste) )] );
//		
//		if(min1 < 0)
//		{
//			if((N_neighbors - (N_wanted_decomposeur + N_wanted_opportuniste) ) = min1)
//			{
//				N_wanted_decomposeur <- N_neighbors * N_wanted_decomposeur * (N_wanted_decomposeur + N_wanted_opportuniste);
//				C_wanted_decomposeur <- N_wanted_decomposeur * C_N;
//				P_wanted_decomposeur <- C_wanted_decomposeur / C_P;
//				
//				N_wanted_opportuniste <- N_neighbors * N_wanted_opportuniste * (N_wanted_decomposeur + N_wanted_opportuniste);
//				C_wanted_opportuniste <- N_wanted_opportuniste * C_N;
//				P_wanted_opportuniste <- C_wanted_opportuniste / C_P;		
//			}
//			if((P_neighbors - (P_wanted_decomposeur + P_wanted_opportuniste) ) = min1)
//			{
//				P_wanted_decomposeur <- P_neighbors * P_wanted_decomposeur * (P_wanted_decomposeur + P_wanted_opportuniste);
//				C_wanted_decomposeur <- P_wanted_decomposeur * C_P;
//				N_wanted_decomposeur <- C_wanted_decomposeur / C_N;
//				
//				P_wanted_opportuniste <- P_neighbors * P_wanted_opportuniste * (P_wanted_decomposeur + P_wanted_opportuniste);
//				C_wanted_opportuniste <- P_wanted_opportuniste * C_P;
//				N_wanted_opportuniste <- C_wanted_opportuniste / C_N;
//			}
//
//			if(C_stable_neighbors - C_wanted_decomposeur = min1  )
//			{
//				C_wanted_decomposeur <- C_stable_neighbors;
//				N_wanted_decomposeur <- C_wanted_decomposeur / C_N;
//				P_wanted_decomposeur <- C_wanted_decomposeur / C_P;
//			}
//			if((C_labile_neighbors - C_wanted_opportuniste) = min1)
//			{
//				C_wanted_opportuniste <- C_labile_neighbors;
//				N_wanted_opportuniste <- C_wanted_opportuniste / C_N;
//				P_wanted_opportuniste <- C_wanted_opportuniste / C_P;	
//			}
//		}
//		
//		ask(particles)
//		{
//			N <-  N  - (N_wanted_decomposeur + N_wanted_opportuniste)* (N /N_neighbors);
//	 		P <-  P  - (P_wanted_decomposeur + P_wanted_opportuniste)* (P /P_neighbors);
//			C_labile <- C_labile - C_wanted_opportuniste * (C_labile / C_labile_neighbors);
//			C_recalcitrant <- C_recalcitrant - C_wanted_decomposeur * (C_recalcitrant / C_stable_neighbors);	
//		}
//		
//		C_opportuniste <- C_opportuniste + C_wanted_opportuniste;
//		C_decomposeur <- C_decomposeur + C_wanted_decomposeur;
//	
//		N_opportuniste <- N_opportuniste + N_wanted_opportuniste;
//		N_decomposeur <- N_decomposeur + N_wanted_decomposeur;
//		
//		P_opportuniste <- P_opportuniste + P_wanted_opportuniste;
//		P_decomposeur <- P_decomposeur + P_wanted_decomposeur;		
//	}
	
	reflex respirer
	{
		float l_production_co2 <- C_opportuniste * taux_respiration;
		C_opportuniste <- C_opportuniste  - l_production_co2; 

		float s_production_co2 <- C_decomposeur * taux_respiration;
		C_opportuniste <- C_opportuniste  - s_production_co2; 
		
		CO2_produit <- l_production_co2 + s_production_co2 + CO2_produit;
	}
	
	reflex reproduire
	{
		pop_opportuniste <- pop_opportuniste* (1 - number_microbe / K)  * (step/temps_division_opportuniste) + pop_opportuniste;
		pop_decomposeur <- pop_opportuniste* (1 - number_microbe / K)  * (step/temps_division_decomposeur) + pop_decomposeur;
		
		write "population O:"+ pop_opportuniste	+" D:"+ pop_decomposeur;
	}
	

	aspect base
	{
		//draw cube(size) at:location color:#blue;
	}
	
	init{
		pop_opportuniste <- 1;
		pop_decomposeur <- 1;

	}
	
	reflex test
	{
	//	organic_matter <- organic_matter - 0.01*organic_matter;
	//	my_neighbors where(each.my_type = "organic");
		//write
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
			species copiotrophe_R aspect:base;
			species copiotrophe_K aspect:base;
			species oligotrophe aspect:base;
		}
		
		display "Population" type: java2D
		{
			chart "Population" type: series
			{
				data "decomposeur" value: sum(Microbes collect(each.pop_decomposeur)) color: # orange style: spline;
				data "opportuniste" value: sum(Microbes collect(each.pop_opportuniste)) color: # orange style: spline;
			}

		}
		
		
				
	}
}