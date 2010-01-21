open Graphics
open Thread

class polygon=
object
	val mutable vert=[||]
	val mutable col=green
	method erase c=set_color c;fill_poly vert
	method draw=set_color col;fill_poly vert
	method setVert v=vert<-v
	method setColor c=col<-c
end

class background=
object (self)
	val mutable lifes=5
	val mutable points=0
	val mutable ammo=0
	val mutable level=1
	val mutable leftColor=white
	val mutable rightColor=red
	val mutable borderColor=black
	method getLeftColor=leftColor
	method drawLeft=set_color leftColor;fill_rect 0 0 480 (size_y())
	method drawRight=set_color rightColor;fill_rect 480 0 (size_x()) (size_y())
	method drawBorders=
		set_color borderColor;set_line_width 2;
		moveto 525 330;draw_string "Lifes left";
		draw_rect 490 280 130 50;
		moveto 540 250;draw_string "Level";
		draw_rect 490 200 130 50;
		moveto 515 170;draw_string "Points gained";
		draw_rect 490 120 130 50;
		moveto 525 90;draw_string "Ammunition";
		draw_rect 490 40 130 50;
		moveto 525 500;draw_string "Debugging info"
	method drawDInfo g=set_color red;fill_rect 525 470 100 20;moveto 525 480;set_color black;draw_string (string_of_int(g))
	method eraseLifes=set_color rightColor;fill_rect 540 290 40 20
	method erasePoints=set_color rightColor;fill_rect 500 130 80 20
	method eraseAmmo=set_color rightColor;fill_rect 500 50 80 20
	method eraseLevel=set_color rightColor;fill_rect 500 210 80 20
	method drawLifes=set_color black;moveto 550 300;draw_string (string_of_int(lifes))
	method drawPoints=set_color black;moveto 500 140;draw_string (string_of_int(points))
	method drawAmmo=set_color black;moveto 500 60;draw_string (string_of_int(ammo))
	method drawLevel=set_color black;moveto 500 220;draw_string (string_of_int(level))
	method draw=self#drawLeft;self#drawRight;self#drawBorders;self#drawLifes;self#drawPoints;self#drawLevel;self#drawAmmo
	method updateLifes f=lifes<-f lifes 1;self#eraseLifes;self#drawLifes
	method updatePoints p=points<-points+p;self#erasePoints;self#drawPoints
	method updateLevel=level<-level+1;self#eraseLevel;self#drawLevel
end

class plate=
object (self)
	val mutable width=60
	val mutable xPosition=180
	val mutable color=green
	method draw=set_color color;fill_rect xPosition 0 width 10
	method erase c=set_color c;fill_rect xPosition 0 width 10
	method reset=self#erase white;width<-60;xPosition<-180;color<-green;self#draw;synchronize()
	method mouseMove x=
		self#erase white;
		if x<60 then xPosition<-0
		else if x>529-width then xPosition<-479-width
		else xPosition<-(x-60);
		self#draw;synchronize()
	method keyboardMove x=
		self#erase white;
		let xPos=xPosition+x in
		if xPos<0 then xPosition<-0
		else if xPos>479-width then xPosition<-479-width
		else xPosition<-xPos;
		self#draw;synchronize()
	method resize w=width<-w;self#draw;synchronize()
	method getPosition=xPosition
	method getWidth=width
end

class ball=
object (self)
	val mutable xPosition=210
	val mutable yPosition=15
	val mutable radius=5
	val mutable color=blue
	val mutable state=false
	method draw=set_color color;fill_circle xPosition yPosition radius
	method erase c=set_color c;fill_circle xPosition yPosition radius
	method reset=self#erase white;xPosition<-210;yPosition<-15;radius<-5;self#draw;synchronize()
	method move (x,y)=self#erase white;xPosition<-(xPosition+x);yPosition<-(yPosition+y);self#draw;synchronize()
	method onPlateMouseMove x=
		self#erase white;
		let xPos=xPosition in
			xPosition<-x;
			if self#xCollided then xPosition<-xPos;
			self#draw;synchronize()
	method onPlateKeyboardMove x=
		self#erase white;
		let xPos=xPosition in
			xPosition<-(xPosition+x);
			if self#xCollided then xPosition<-xPos;
			self#draw;synchronize()
	method onPlate x w=if xPosition>=x&&xPosition-1<=(x+w)&&yPosition-radius=10 then true else false
	method xCollided=if (xPosition-radius)<=0||(xPosition+radius)>=480 then true else false
	method yCollided=if (yPosition+radius)>=size_y() then true else false
	method isDownBelow=if yPosition<0 then true else false
	method changeState x=state<-x
	method isMoving=state
	method getXPosition=xPosition
	method getYPosition=yPosition
	method getRadius=radius
end

class powerups=
object (self)
	inherit polygon as drawer
	val mutable model=0
	val mutable position=0
	val mutable color=white
	method distAwards=
		match model with
		|0->failwith "lol"
		|1->failwith "lol2"
		|2->failwith "lol3"
		|_->failwith "Unrecognized power-up model (sth's wrong)"
	method gained=false
	method missed (x,y)=if y<0 then true else false
	method pdraw_aux vert=
		let rec pdraw_aaux gained missed vert=
			match gained,missed with
			|false,false->
				delay 0.005;
				drawer#erase white;
				drawer#setColor color;
				drawer#setVert vert;
				drawer#draw;
				synchronize();
				pdraw_aaux (self#gained) (self#missed vert.(1)) (Array.map (fun (a,b)->(a,b-1)) vert)
			|true,false->self#distAwards
			|false,true->synchronize()
			|true,true->failwith "This should never happen..."
		in pdraw_aaux false false vert
	method pdraw=
	(
		Random.self_init();
		let model=Random.int 2 in
		match model with
		|0->drawer#setVert [|(position,340);(position,360);(position+10,360);(position+10,348);(position+15,348);(position+15,340)|];color<-red
		|1->drawer#setVert [|(position,340);(position,360);(position+10,360);(position+10,354);(position+6,354);(position+6,352);(position+9,352);(position+9,348);(position+6,348);(position+6,346);(position+10,346);(position+10,340)|];color<-green
		|2->drawer#setVert [|(position,340);(position+3,360);(position+7,360);(position+10,340);(position+7,340);(position+6,345);(position+4,345);(position+3,340)|];color<-black
		|_->failwith "Unrecognized power-up model type (sth's wrong)"
	);
	create self#pdraw_aux vert;synchronize()
	method setPosition p=position<-p
end

class board (background:background) (plate:plate) (ball:ball)=
object (self)
	inherit polygon as drawer
	val mutable collection=[]
	val mutable colors=[]
	val mutable counter=0;
	method lottery i=Random.int i
	method drawColors=
		match (self#lottery 14) with
		|0->blue
		|1->red
		|2->red
		|3->magenta
		|_->green
	method clearCollection=collection<-[]
	method createCollection=
		Random.self_init();
		let sy=size_y() in
		let vert=[|(0,sy);(0,sy-20);(40,sy-20);(40,sy)|] in
			let rec create_aux i x color col=
				match i with
				|12->collection<-col;colors<-color
				|_->let rec create_aaux j e y t color col=
						if j=e then create_aux (i+1) (x+40) color col else
						let c=self#drawColors in
						if t!=1 then create_aaux (j+1) e (y+20) (self#lottery 3) (c::color)	(if c!=magenta then (counter<-counter+1);(Array.map (fun (a,b)->(a+x,b-y)) vert)::col)
						else create_aaux (j+1) e (y+20) (self#lottery 3) color col
					in create_aaux 0 14 0 (self#lottery 3) color col
			in create_aux 0 0 [] []
	method getCol=collection
	method drawCollection=
		let rec draw_aux col color=
			match col,color with
			|h1::t1,h2::t2->drawer#setColor h2;drawer#setVert h1;drawer#draw;draw_aux t1 t2
			|_,_->synchronize()
		in draw_aux collection colors
	method xCollided=
		let x=ball#getXPosition and y=ball#getYPosition and r=ball#getRadius in
			let rec collided_aux col=
				match col with
				|hd::tl->let a=Array.get hd 0 and b=Array.get hd 2 in
					if (((x+r)=fst a||(x-r)=fst b)&&y<=snd a&&y>=snd b) then (self#removeFromCollection hd;true) else collided_aux tl
				|_->false
			in collided_aux collection
	method yCollided=
		let x=ball#getXPosition and y=ball#getYPosition and r=ball#getRadius in
			let rec collided_aux col=
				match col with
				|hd::tl->let a=Array.get hd 0 and b=Array.get hd 2 in
					if (((y-r)=snd a||(y+r)=snd b)&&x>=fst a&&x<=fst b) then (self#removeFromCollection hd;true) else collided_aux tl
				|_->false
			in collided_aux collection
	method removeFromCollection chosen=
	(
		let rec remove_aux col coltmp color colortmp=
			match col,color with
			|h1::t1,h2::t2->
			if h1=chosen then
			(
				if h2=green then
				(
					collection<-(List.rev_append coltmp t1);
					colors<-(List.rev_append colortmp t2);
					self#redrawOne chosen white;
					counter<-counter-1;
					background#updatePoints 10
				)
				else if h2=red then
				(
					colors<-(List.rev_append (green::colortmp) t2);
					self#redrawOne chosen green;
					background#updatePoints 10
				)
				else if h2=blue then
				(
					collection<-(List.rev_append coltmp t1);
					colors<-(List.rev_append colortmp t2);
					self#redrawOne chosen white;
					counter<-counter-1;
					background#updatePoints 5;
					let powerup=new powerups in
						powerup#setPosition (fst h1.(0));
						powerup#pdraw
				)
			)
			else remove_aux t1 (h1::coltmp) t2 (h2::colortmp)
			|_,_->failwith "Sth's wrong (index out of bounds)" (** To sie raczej nie powinno wydarzyc *)
		in remove_aux collection [] colors []
	);
	if counter=0 then (background#updateLevel;plate#reset;ball#reset;self#clearCollection;self#createCollection;self#drawCollection)
	else background#drawDInfo counter
	method redrawOne chosen c=drawer#setColor c;drawer#setVert chosen;drawer#draw
end

let background=new background
let plate=new plate
let ball=new ball
let board=new board background plate ball
let ballCoords (x,y)=
	let rx=ref 0 and ry=ref 0 in
		if ball#xCollided||board#xCollided then rx:=-x else rx:=x;
		if ball#yCollided||board#yCollided||((ball#onPlate plate#getPosition plate#getWidth)&&ball#isMoving) then ry:=-y else ry:=y;
		(!rx,!ry)
let rollTheBall()=
	let rec delay_aux (x,y)=
		match ball#isDownBelow with
		|true->background#updateLifes (-);plate#reset;ball#reset;ball#changeState false
		|false->delay 0.004;ball#move (x,y);ball#changeState true;delay_aux (ballCoords (x,y))
	in delay_aux (ballCoords(1,1))
(*let roll()=create rollTheBall()
let rec restartBall f arg=try f arg with e->restartBall f arg*)
let keyboardPlateNavigation()=
	let rec delay_aux i m=
		match i.key,m with
		|'\027',_->close_graph()
		|'z',_->if ball#onPlate plate#getPosition plate#getWidth then ball#onPlateKeyboardMove (-10);
			plate#keyboardMove (-10);
			delay_aux (wait_next_event [Key_pressed]) (ball#isMoving)
		|'c',_->if ball#onPlate plate#getPosition plate#getWidth then ball#onPlateKeyboardMove 10;
			plate#keyboardMove 10;
			delay_aux (wait_next_event [Key_pressed]) (ball#isMoving)
		|' ',false->create rollTheBall();delay_aux (wait_next_event [Key_pressed]) (ball#isMoving)
		|_,_->delay_aux (wait_next_event [Key_pressed]) (ball#isMoving)
	in delay_aux (wait_next_event [Key_pressed]) (ball#isMoving)
let mousePlateNavigation()=
	let rec delay_aux i=
		match i.key,i.button with
		|'\027',_->close_graph()
		|_,true->create rollTheBall();delay_aux (wait_next_event [Mouse_motion;Key_pressed;Button_down])
		|_,_->plate#mouseMove i.mouse_x;
			if ball#onPlate plate#getPosition plate#getWidth then ball#onPlateMouseMove i.mouse_x;
			delay_aux (wait_next_event [Mouse_motion;Key_pressed;Button_down])
	in delay_aux (wait_next_event [Mouse_motion;Key_pressed;Button_down])
let main=
	open_graph " 640x640";
	set_window_title "Ocamloid 0.0.0.0.4";
	background#draw;
	plate#draw;
	ball#draw;
	board#createCollection;
	board#drawCollection;
	auto_synchronize false;
	keyboardPlateNavigation()
