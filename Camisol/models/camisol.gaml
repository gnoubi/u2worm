/***
* Name: camisol
* Author: nicolas
* Description: 
***/

model camisol

/* Insert your model definition here */
global
{
	geometry shape <- square(20#cm);
	float seuilOM <- 100;
	float seuilN <- 100;
	float seuilP <- 100;
	
	//Charactéristiques des décomposeurs :
	float temps_division_opportuniste <- 2#h;
	float temps_division_decomposeur <- 1#h;
	
	float growth_decomposeur <- 0.5/#mn;
	float C_N <- 10.0;
	float C_P <- 20.0;
	
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
		create soil number:1;
	}
}



species soil skills:[camisole]
{
	init
	{
		location <- point(10#cm,10#cm,10#cm);
		do granulometry organic_matter:0.5#gram/#kg  min:2#mm ;													//MO1	//
		do granulometry mineral_matter:164.9#gram/#kg organic_matter:0.04#gram/#kg  min:0.2#mm max:2#mm;     	//MO2   //décomposeur mineur
		do granulometry mineral_matter:92.5#gram/#kg organic_matter:2.03#gram/#kg  min:0.05#mm max:0.2#mm;		//MO3
		do granulometry mineral_matter:86.5#gram/#kg 	min:0.02#mm max:0.05#mm;								//MO3
		do granulometry mineral_matter:232.1#gram/#kg   min:0.002#mm max:0.02#mm;								//MO3
		do granulometry mineral_matter:423.9#gram/#kg   max:0.002#mm;  											//MO3
		do granulometry organic_matter: 28.13 #gram/#kg   max:0.05#mm;											//MO3
		
		do initialize_soil size:0.2#m dividing_factor:10 bulk_density:1.3 number_of_try:2000 default_species:particle;
		
		do associate particle:"pore" from_canvas:"C1" at_scale:2 with_process:Bacteria;
		//do associate particle:"pore" from_canvas:"C2" at_scale:3 with_process:Microbesxxxx;
		
		
		do apply_processes;
	}
}

species particle skills:[apsf_particle] schedules:[]
{
	float N;
	float P;
	float C_labile;
	float C_stable;
	
	init
	{
		N <- gauss(12,3)#gram;
		P <- gauss(3,0.3)#gram;
		switch(my_type)
		{
			match "mineral" { 
					C_labile <- 0.0;
					C_stable <- 0.0; 
			}
			default { 
				C_labile <- 0.0;
				C_stable <- 0.0;  
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
			draw cube(size) at:location color:selectColor();
		}
	}
	
}

species Bacteria skills:[soil_process]
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

	reflex manger
	{
		write "followed particle "+ followed_particle;
		write "Local proccesses "+ local_processes;
		
	//	write "coucou "+ particle(followed_particle_int);
	//	particle truc <-  particle(follo);
		
		//write "coucou "+ truc;
		
		list<particle> particles <- my_neighbors_local;
		particles <- particles where(each != nil and each.N > 0 and each.P > 0);
		
		float C_labile_neighbors <- sum(particles collect(each.C_labile));
		float C_stable_neighbors <- sum(particles collect(each.C_stable));
		
		float N_neighbors <- sum(particles collect(each.N));
		float P_neighbors <- sum(particles collect(each.P));
		
		float C_wanted_decomposeur <- C_Speed_decomposeur * step * pop_decomposeur;
		float N_wanted_decomposeur <- C_wanted_decomposeur / C_N;
		float P_wanted_decomposeur <- C_wanted_decomposeur / C_P;
		
		float C_wanted_opportuniste <- C_Speed_opportuniste * step * pop_opportuniste;
		float N_wanted_opportuniste <- C_wanted_opportuniste / C_N;
		float P_wanted_opportuniste <- C_wanted_opportuniste / C_P;	
		
		float min1 <- min([ (C_labile_neighbors - C_wanted_opportuniste), 
							(C_stable_neighbors - C_wanted_decomposeur),
							(N_neighbors - (N_wanted_decomposeur + N_wanted_opportuniste) ),
							(P_neighbors - (P_wanted_decomposeur + P_wanted_opportuniste) )] );
		
		if(min1 < 0)
		{
			if((N_neighbors - (N_wanted_decomposeur + N_wanted_opportuniste) ) = min1)
			{
				N_wanted_decomposeur <- N_neighbors * N_wanted_decomposeur * (N_wanted_decomposeur + N_wanted_opportuniste);
				C_wanted_decomposeur <- N_wanted_decomposeur * C_N;
				P_wanted_decomposeur <- C_wanted_decomposeur / C_P;
				
				N_wanted_opportuniste <- N_neighbors * N_wanted_opportuniste * (N_wanted_decomposeur + N_wanted_opportuniste);
				C_wanted_opportuniste <- N_wanted_opportuniste * C_N;
				P_wanted_opportuniste <- C_wanted_opportuniste / C_P;		
			}
			if((P_neighbors - (P_wanted_decomposeur + P_wanted_opportuniste) ) = min1)
			{
				P_wanted_decomposeur <- P_neighbors * P_wanted_decomposeur * (P_wanted_decomposeur + P_wanted_opportuniste);
				C_wanted_decomposeur <- P_wanted_decomposeur * C_P;
				N_wanted_decomposeur <- C_wanted_decomposeur / C_N;
				
				P_wanted_opportuniste <- P_neighbors * P_wanted_opportuniste * (P_wanted_decomposeur + P_wanted_opportuniste);
				C_wanted_opportuniste <- P_wanted_opportuniste * C_P;
				N_wanted_opportuniste <- C_wanted_opportuniste / C_N;
			}

			if(C_stable_neighbors - C_wanted_decomposeur = min1  )
			{
				C_wanted_decomposeur <- C_stable_neighbors;
				N_wanted_decomposeur <- C_wanted_decomposeur / C_N;
				P_wanted_decomposeur <- C_wanted_decomposeur / C_P;
			}
			if((C_labile_neighbors - C_wanted_opportuniste) = min1)
			{
				C_wanted_opportuniste <- C_labile_neighbors;
				N_wanted_opportuniste <- C_wanted_opportuniste / C_N;
				P_wanted_opportuniste <- C_wanted_opportuniste / C_P;	
			}
		}
		
		ask(particles)
		{
			N <-  N  - (N_wanted_decomposeur + N_wanted_opportuniste)* (N /N_neighbors);
	 		P <-  P  - (P_wanted_decomposeur + P_wanted_opportuniste)* (P /P_neighbors);
			C_labile <- C_labile - C_wanted_opportuniste * (C_labile / C_labile_neighbors);
			C_stable <- C_stable - C_wanted_decomposeur * (C_stable / C_stable_neighbors);	
		}
		
		C_opportuniste <- C_opportuniste + C_wanted_opportuniste;
		C_decomposeur <- C_decomposeur + C_wanted_decomposeur;
	
		N_opportuniste <- N_opportuniste + N_wanted_opportuniste;
		N_decomposeur <- N_decomposeur + N_wanted_decomposeur;
		
		P_opportuniste <- P_opportuniste + P_wanted_opportuniste;
		P_decomposeur <- P_decomposeur + P_wanted_decomposeur;		
	}
	
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


species nematode skills:[soil_process]{
	//taux de reproduction
	particle particule <- particle(self.followed_particle);
	particle particule_default <- particle(self.followed_particle);
	float quatit_ingere;
	
	float N;
	float C;
	float P;
	float CN;

	float rejet(float qte,float taux_rejet){
		//10 à 20% de ce qu"il on ingérer
		//70%P et 15%N
		return qte*taux_rejet/100;
	}
		
	reflex predation{
		//entre 1 et 30 par gramme de sol
		//10.000/jours si il se réplique 2fois
		//7 microorganisme/minutes
		//vérifie si il reste encore des micro-organisme dans le pore
		bool is_sans_decomposeur <- false;
		float qte;
		
		
		int i <- particle(self.followed_particle).i;
		int j <- particle(self.followed_particle).j;
		int k <- particle(self.followed_particle).k;
		
		particule <- one_of(particle where(i=each.i and j=each.j and k=each.k));
		
		list<particle> neighbourhood <-(particle(self.followed_particle).my_neighbors) where(each!=nil);
		
		if(particule.decomposeur != 0 or particule.opportuniste!=0){
			//10.000 microbes/jours donc 417microbes/heure
			
			qte <- particule.decomposeur - ((417*taux_pred_decomposeur/100) + 417*taux_pred_opportuniste/100 )*step;
			
			if(qte<0) {qte<-0.0;}
			particule.decomposeur <- qte;
			particule.P <- rejet(qte,70.0);
			particule.N <- rejet(qte,15.0);
		}else{
			do deplacement;
		}
}
	
	action deplacement{
		list<particle> neighbourhood<-(self.particule.my_neighbors); //à tester
		neighbourhood <- neighbourhood where (each !=nil);
		//list<particle> neighbourhoodPorous <- neighbourhood where(each.my_type = "pore" and each!=nil);
		
		//write "juste voisin ccc s" + neighbourhood;
		//write "Pores voisins :"+neighbourhoodPorous;
		
		particle p_cible;
		particle p;
		
		float i_max_decomp<-0.0;
		float i_max_opport<-0.0;
		
		//regarde si il y a des microbes autour
		loop p over:neighbourhood {
			//write p.my_type;
			if(p.my_type="pore" and (p.decomposeur=0 or p.opportuniste=0)){
				if(p.decomposeur>i_max_decomp){
					i_max_decomp<-p.decomposeur;
					p_cible <-p;
				}
				if( p!=nil and p.opportuniste>i_max_opport){
					i_max_opport<-p.opportuniste;
					p_cible <-p;
				}
				
			}
			
		}
		
		
		
		 //se déplace là ou il y a le plus de microbes
		 if(p_cible!=nil){
		 	self.particule <- p_cible;
			//assigne les nouvelles coordonnées
			self.i<-p_cible.i;
			self.j<-p_cible.j;
			self.k<-p_cible.k;
			
			ask particule_default {
				color <- #yellow;
			}
			particule.color <- #purple; 
			
			//write [particule_default.i,particule_default.j,particule_default.k] + " parcelle cible : " + [p_cible.i, p_cible.j, p_cible.k];
		 }
		
		
		
	}
	
	aspect base
	{
		draw cube(size) at:location color:#purple;
	}
}



experiment ex type:gui
{
	//Qualité des apports et azote et phosphore
	
	output
	{
		display map type:opengl background:#black
		{
		//	species microbes aspect:base;
			species particle aspect:base;
		}
		
		display "Population" type: java2D
		{
			chart "Population" type: series
			{
				data "decomposeur" value: sum(Bacteria collect(each.pop_decomposeur)) color: # orange style: spline;
				data "opportuniste" value: sum(Bacteria collect(each.pop_opportuniste)) color: # orange style: spline;
			}

		}
		
		
				
	}
}