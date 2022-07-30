/**
 *  tuto
 *  Author: O.HAMZA Abdoul-Djalil
 *  Description: 
 */

model model2

/* Insert your model definition here */


global {
	file roads_shapefile <- file("../includes/routes.shp");
	file pnoeud_shapefile <- file("../includes/points.shp");
	geometry shape <- envelope(roads_shapefile);
	float step <- 1 #second;
	float temps_creer_voiture<-100.0 parameter:"temps_creer_voiture";
	float temps_feu <-40.0;
	graph the_graph;
	int nb_voitures_par_creer <- 2 parameter:"nb_voitures_par_creer";
	
	init{
		create Pnoeud from: pnoeud_shapefile with:[isAfire::string(read("type"))="traffic_light", depart::string(read("type"))="depart", arrivee::string(read("type"))="arrivee"]{
		
		is_traffic_light<-true;
		}
		ask Pnoeud where each.is_traffic_light {
			stop << flip(0.5) ? roads_in : [] ;
		}
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
			Pnoeud l <-one_of(Pnoeud where (each.depart));
			list<road> s<-l.roads_out;
			road si <- s[0];
			
				point p <-si.target_node.location;
				write "p => "+p.location;	
		create voitures number: nb_voitures_par_creer{ 
			max_speed <- 160 #km/#h;
			vehicle_length <- 5.0 ;
			speed<-30.0;
			right_side_driving <- true;
			proba_lane_change_up <- 0.1 + (rnd(500) / 500);
			proba_lane_change_down <- 0.5+ (rnd(500) / 500);
			location <- one_of(Pnoeud where (each.depart)).location;
			security_distance_coeff <- 5/9 * 3.6 * (1.5 - rnd(1000) / 1000);  
			proba_respect_priorities <- 1.0;
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
	bool isAfire<-false;
	float temp_speed;
	rgb color; 
	list<list> stop<-[];
	list<road> ways1;
	list<road> ways2;
	bool is_traffic_light <- false;
	float time_to_change <- temps_feu;
	int counter <- rnd (time_to_change) ;
	string changeLight<-"red";
	bool is_green;
	init{if(is_traffic_light) {
			do compute_crossing;
			stop<< [];
			if (flip(0.5)) {
				do to_green;
			} else {
				do to_red;
			}	
		}
		}
		
		action compute_crossing{
		if  (length(roads_in) >= 2) {
			road rd0 <- road(roads_in[0]);
			list<point> pts <- rd0.shape.points;						
			float ref_angle <-  float( last(pts) direction_to rd0.location);
			loop rd over: roads_in {
				list<point> pts2 <- road(rd).shape.points;						
				float angle_dest <-  float( last(pts2) direction_to rd.location);
				float ang <- abs(angle_dest - ref_angle);
				if (ang > 45 and ang < 135) or  (ang > 225 and ang < 315) {
					ways2<< road(rd);
				}
			}
		}
		loop rd over: roads_in {
			if not(rd in ways2) {
				ways1 << road(rd);
			}
		}
	}
	action to_green {
		stop[0] <- ways2 ;
		color <- rgb("green");
		is_green <- true;
	}
	
	action to_red {
		stop[0] <- ways1;
		color <- rgb("red");
		is_green <- false;
	}
	reflex dynamic_node when: is_traffic_light {
		counter <- counter + 1;
		if (counter >= time_to_change) { 
			counter <- 0;
			if is_green {do to_red;}
			else {do to_green;}
		} 
	}

	aspect base {
		if (isAfire) {	
			draw circle(5) color: color;
		}
	}
	
	aspect base3D {
		if (isAfire) {	
			draw box(1,1,10) color:rgb("black");
			draw sphere(5) at: {location.x,location.y,12} color: color;
		}
	}
}

experiment model2 type: gui {
	output {
		display model2 {
			species road aspect: base refresh: false;
			species Pnoeud aspect: base;
			species voitures;
		}
	}
}