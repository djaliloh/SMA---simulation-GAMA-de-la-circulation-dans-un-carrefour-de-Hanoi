/**
 *  tuto
 *  Author: O.HAMZA Abdoul-Djalil
 *  Description: 
 */

model model1

/* Insert your model definition here */


global {
	file roads_shapefile <- file("../includes/routes.shp");
	file pnoeud_shapefile <- file("../includes/points.shp");
	geometry shape <- envelope(roads_shapefile);
	float step <- 1 #second;
	float temps_creer_voiture<-100.0 parameter:"temps_creer_voiture";
	graph the_graph;
	int nb_voitures_par_creer <- 1 parameter:"nb_voitures_par_creer";
	
	init{
		create Pnoeud from:pnoeud_shapefile with:[depart::string(read("type"))="depart", arrivee::string(read("type"))="arrivee"];
		
		list<point> nodes <- Pnoeud collect each.location;
		
		create road from: roads_shapefile with:[maxspeed::float(read('SPEEDLIMIT'))
			, oneway::string(read('OPPOSITENU')), lanes::int(read('NUMBEROFTR'))];
		map general_speed_map <- road as_map (each::(each.shape.perimeter / 10));
		ask road {
			lanes <- max([1,lanes]);
			maxspeed <-  max([30,maxspeed]) #km/#h;
			point fp <- first(shape.points);
			point lp <- last(shape.points);
			if not (fp in nodes) {create Pnoeud with:[location::fp]; nodes<<fp;}
			if not (lp in nodes) {create Pnoeud with:[location::lp]; nodes<<lp;}
			switch oneway {
				match "0" {
					create road {
						lanes <- myself.lanes;
						shape <- polyline(reverse(myself.shape.points));
						maxspeed <- myself.maxspeed;
						linked_road <- myself;
						myself.linked_road <- self;
					}
				}
				match "1" {
					shape <- polyline(reverse(shape.points));
				}		
			}
		}
		the_graph <-(as_driving_graph(road, Pnoeud));
		
	}
	float c<-temps_creer_voiture;
	reflex creervoiture{
		if(c>=temps_creer_voiture){
		create voitures number: nb_voitures_par_creer{ 
			max_speed <- 160 #km/#h;
			vehicle_length <- 5.0 ;
			right_side_driving <- true;
			proba_lane_change_up <- 0.1 + (rnd(500) / 500);
			proba_lane_change_down <- 0.5+ (rnd(500) / 500);
			location <- one_of(Pnoeud where(each.depart)).location;
			security_distance_coeff <- 5/9 * 3.6 * (1.5 - rnd(1000) / 1000);  
			proba_respect_priorities <- 1.0 - rnd(200/1000);
			proba_respect_stops <- [1.0];
			proba_block_node <- 0.0;
			proba_use_linked_road <- 0.0;
			max_acceleration <- 5/3.6;
			speed_coeff <- 1.2 - (rnd(400) / 1000);
			c<-0.0;
		}
		}else{
			c<-c+1;
		}
		}	
	
}

species voitures skills: [advanced_driving] { 
	rgb color <- rnd_color(255) ;
	Pnoeud target;
	reflex time_to_go when: final_target = nil {
		target <- one_of(Pnoeud where (each.arrivee));
		current_path <- compute_path(graph: the_graph, target: target );
		if (current_path = nil) {
			final_target <- nil;
		}
	}
	reflex move when: current_path != nil and final_target != nil {
		write name + location;
		do drive;
		write name + location;
		if (location = final_target) {
			final_target <- nil;
		}
	
	}
	
	aspect default { 
		draw triangle(10) color: color border: #black rotate:heading + 90;
	} 
}

species road skills: [skill_road]{
	string oneway;
	aspect base {
		draw shape color: #black;
	}
}


species Pnoeud skills: [skill_road_node]{
	bool depart<-false;
	bool arrivee<-false;
}

experiment model1 type: gui {
	output {
		display model1 {
			species road aspect: base refresh: false;
			species voitures;
		}
	}
}