open Graphics
open Thread
open Printf

class background=
object (self)
	val mutable lifes=5
	val mutable points=0
	val mutable ammo=500
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
		draw_rect 490 40 130 50
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
	method addAmmo=ammo<-ammo+10;self#eraseAmmo;self#drawAmmo
	method removeAmmo=ammo<-ammo-1;self#eraseAmmo;self#drawAmmo
	method getAmmo=ammo
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
	method resize f=width<-f width 20;self#draw;synchronize()
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
	method erase=set_color white;fill_circle xPosition yPosition radius
	method reset=self#erase;xPosition<-210;yPosition<-15;radius<-5;self#draw;synchronize()
	method move (x,y)=self#erase;xPosition<-(xPosition+x);yPosition<-(yPosition+y);self#draw;synchronize()
	method onPlateMouseMove x=
		self#erase;
		let xPos=xPosition in
			xPosition<-(x-30);
			if self#xCollided then xPosition<-xPos;
			self#draw;synchronize()
	method onPlateKeyboardMove x=
		self#erase;
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

class powerups (p:int) (background:background) (plate:plate)=
object (self)
	val position=p
	val mutable model=0
	val mutable color=white
	val mutable vert=[||]
	method erase=set_color white;fill_poly vert
	method draw=set_color color;fill_poly vert
	method distAwards=
		self#erase;
		match model with
		|0->background#updateLifes (+)
		|1->create (fun ()->plate#resize (+);delay 10.;plate#resize (-)) ();synchronize()
		|2->background#addAmmo
		|_->failwith "Unrecognized power-up model (sth's wrong)"
	method gained y=
		let x=plate#getPosition in
			if y<10&&position+10>x&&position<x+plate#getWidth then true else false
	method missed y=if y<0 then true else false
	method pdraw_aux()=
		let rec pdraw_aaux gained missed=
			match gained,missed with
			|false,false->
				let rec delayer()=try delay 0.005 with e->delayer() in
				delayer();
				self#erase;
				vert<-Array.map (fun (a,b)->(a,b-1)) vert;
				self#draw;
				synchronize();
				pdraw_aaux (self#gained (snd vert.(0))) (self#missed (snd vert.(1)))
			|true,false->self#distAwards
			|false,true->synchronize()
			|true,true->failwith "This should never happen..."
		in pdraw_aaux false false
	method pdraw=
	(
		Random.self_init();
		model<-Random.int 2;
		match model with
		|0->vert<-[|(position,340);(position,360);(position+10,360);(position+10,348);(position+15,348);(position+15,340)|];color<-red
		|1->vert<-[|(position,340);(position,360);(position+10,360);(position+10,354);(position+6,354);(position+6,352);(position+9,352);(position+9,348);(position+6,348);(position+6,346);(position+10,346);(position+10,340)|];color<-green
		|2->vert<-[|(position,340);(position+3,360);(position+7,360);(position+10,340);(position+7,340);(position+6,345);(position+4,345);(position+3,340)|];color<-black
		|_->failwith "Unrecognized power-up model type (sth's wrong)"
	);
	create self#pdraw_aux();synchronize()
end

class board (background:background) (plate:plate) (ball:ball)=
object (self)
	val mutable vert=[||]
	val mutable maincolor=white
	val mutable collection=[]
	val mutable colors=[]
	val mutable counter=0;
	val mutable bulletFlying=false
	method erase c=set_color c;fill_poly vert
	method draw=set_color maincolor;fill_poly vert
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
			|h1::t1,h2::t2->maincolor<-h2;vert<-h1;self#draw;draw_aux t1 t2
			|_,_->synchronize()
		in draw_aux collection colors
	method xCollided=
		let x=ball#getXPosition and y=ball#getYPosition and r=ball#getRadius in
			let rec collided_aux col=
				match col with
				|hd::tl->let a=Array.get hd 0 and b=Array.get hd 2 in
					if ((x+r)>=fst a&&(x-r)<=fst b&&y<=snd a&&y>=snd b) then (self#removeFromCollection hd;true) else collided_aux tl
				|_->false
			in collided_aux collection
	method yCollided=
		let x=ball#getXPosition and y=ball#getYPosition and r=ball#getRadius in
			let rec collided_aux col=
				match col with
				|hd::tl->let a=Array.get hd 0 and b=Array.get hd 2 in
					if ((y-r)<=snd a&&(y+r)>=snd b&&x>=fst a&&x<=fst b) then (self#removeFromCollection hd;true) else collided_aux tl
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
						let powerup=new powerups (fst h1.(0)) background plate in
							powerup#pdraw
					)
					else if h2=magenta then
						self#redrawOne chosen magenta
				)
				else remove_aux t1 (h1::coltmp) t2 (h2::colortmp)
			|_,_->failwith "Sth's wrong (index out of bounds)" (** To sie nie powinno wydarzyc *)
		in remove_aux collection [] colors []
	);
	if counter=0 then (background#updateLevel;plate#reset;ball#reset;self#clearCollection;self#createCollection;self#drawCollection)
	method redrawOne chosen c=maincolor<-c;vert<-chosen;self#draw
	method getCollection=collection
	method setBulletFlying s=bulletFlying<-s
	method getBulletFlying=bulletFlying
end

class rifle (p:int) (board:board)=
object (self)
	val mutable vert=[||]
	val mutable position=0
	method erase=set_color white;fill_poly vert
	method draw=set_color black;fill_poly vert
	method setPosition x=position<-x
	method hit (x,y)=
		if y>=360 then
		(
			let rec hit_aux col=		
				match col with
				|hd::tl->let a=hd.(0) and b=hd.(2) in
					if (x>=fst a&&(x+3)<=fst b&&y>=snd b) then (board#removeFromCollection hd;board#setBulletFlying false;true) else hit_aux tl
				|_->false
			in hit_aux board#getCollection
		)
		else false
	method missed y=if y>size_y() then true else false
	method flightOfTheBullet()=
		let rec flight_aux hit missed=
			match hit,missed with
			|false,false->
				let rec delayer()=try delay 0.003 with e->delayer() in
				delayer();
				self#erase;
				vert<-Array.map (fun (a,b)->(a,b+1)) vert;
				self#draw;
				synchronize();
				flight_aux (self#hit (vert.(1))) (self#missed (snd vert.(1)))
			|_,_->self#erase;synchronize()
		in flight_aux false false
	method shot=
		position<-p;
		vert<-[|(position,10);(position,16);(position+3,16);(position+1,10)|];
		create self#flightOfTheBullet();
		synchronize()
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
let rec delayer()=try delay 0.004 with e->delayer()
let rollTheBall()=
	let rec delay_aux (x,y)=
		match ball#isDownBelow with
		|true->background#updateLifes (-);plate#reset;ball#reset;ball#changeState false
		|false->ball#move (x,y);ball#changeState true;delayer();delay_aux (ballCoords (x,y))
	in delay_aux (ballCoords(1,1))
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
	let rec delay_aux m i=
		match i.key,i.button,m with
		|'\027',_,_->close_graph()
		|_,true,false->create rollTheBall();delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
		|_,false,false->
			plate#mouseMove i.mouse_x;
			ball#onPlateMouseMove i.mouse_x;
			delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
		|_,true,true->
		(
			if (background#getAmmo!=0&&board#getBulletFlying=false) then
			(
				background#removeAmmo;
				board#setBulletFlying true;
				let lrifle=new rifle plate#getPosition board in lrifle#shot;
				let rrifle=new rifle (plate#getPosition+plate#getWidth) board in rrifle#shot
			)
		);
		delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
		|_,_,true->plate#mouseMove i.mouse_x;delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
	in delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
let main=
	open_graph " 640x640";
	set_window_title "OCamloid 0.0.0.0.9";
	background#draw;
	plate#draw;
	ball#draw;
	board#createCollection;
	board#drawCollection;
	auto_synchronize false;
	mousePlateNavigation()
