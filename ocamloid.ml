open Graphics
open Thread

(** Funkcja odpowiedzialna za blokowanie obiektow w trybie pauzy *)
let locker=ref false

class instructions=
object (self)
	val color=rgb 150 150 150
	method drawBackground=set_color color;fill_rect 0 360 (size_x()) (size_y())
	method drawText=
		set_color black;
		moveto 20 611;draw_string "Controls:";
		moveto 20 592;draw_string "Mouse button - release the ball or shot the gun.";
		moveto 20 573;draw_string "Mouse movement - control the plate.";
		moveto 0 569;lineto (size_x()) 569;
		moveto 20 554;draw_string "Blocks types:";
		set_color green;fill_rect 20 526 40 20;set_color black;moveto 70 530;draw_string "- normal block, disappears when hit (+10 points).";
		set_color red;fill_rect 20 496 40 20;set_color black;moveto 70 500;draw_string "- 2xhit block, turns to green when hit (+10 points).";
		set_color magenta;fill_rect 20 466 40 20;set_color black;moveto 70 470;draw_string "- Permanent block, impossible to destroy (no points).";
		set_color blue;fill_rect 20 436 40 20;set_color black;moveto 70 440;draw_string "- Bonus block, drops bonus when hit (+5 points).";
		moveto 0 432;lineto (size_x()) 432;
		moveto 20 418;draw_string "Bonuses:";		
		set_color red;fill_poly [|(20,390);(20,410);(30,410);(30,398);(35,398);(35,390)|];set_color black;moveto 45 395;draw_string "- Lifes +1.";
		set_color green;fill_poly [|(130,390);(130,410);(140,410);(140,404);(136,404);(136,402);(139,402);(139,398);(136,398);(136,396);(140,396);(140,390)|];set_color black;moveto 150 395;draw_string "- Enlarge plate.";
		set_color black;fill_poly [|(260,390);(263,410);(267,410);(270,390);(267,390);(266,395);(264,395);(263,390)|];moveto 280 395;draw_string "- Ammo +1.";
		moveto 0 386;lineto (size_x()) 386;
		moveto 200 368;draw_string "Press mouse button to close this message."
end

class logo=
object (self)
	val mutable v=[||]
	method color=
		Random.self_init();
		let i=Random.int 4 in
			match i with
			|0->blue
			|1->red
			|2->magenta
			|_->green
	method draw c v=set_color c;fill_poly v
	method drawVertic j=for i=1 to j do self#draw self#color v;v<-Array.map (fun (a,b)->(a,b+10)) v done
	method drawHorizon j=for i=1 to j do self#draw self#color v;v<-Array.map (fun (a,b)->(a+20,b)) v done
	(** Rysowanie loga i numeru wersji *)
	method drawLogo=
		(** Logo *)
		v<-[|(50,540);(50,550);(70,550);(70,540)|];self#drawVertic 6;
		v<-[|(60,530);(60,540);(80,540);(80,530)|];self#drawHorizon 2;
		v<-[|(90,540);(90,550);(110,550);(110,540)|];self#drawVertic 6;
		v<-[|(60,600);(60,610);(80,610);(80,600)|];self#drawHorizon 2;
		v<-[|(120,540);(120,550);(140,550);(140,540)|];self#drawVertic 6;
		v<-[|(130,530);(130,540);(150,540);(150,530)|];self#drawHorizon 2;
		v<-[|(130,600);(130,610);(150,610);(150,600)|];self#drawHorizon 2;
		v<-[|(160,540);(160,550);(180,550);(180,540)|];self#drawVertic 1;
		v<-[|(160,590);(160,600);(180,600);(180,590)|];self#drawVertic 1;
		v<-[|(190,540);(190,550);(210,550);(210,540)|];self#drawVertic 3;
		v<-[|(200,530);(200,540);(220,540);(220,530)|];self#drawHorizon 3;
		v<-[|(200,570);(200,580);(220,580);(220,570)|];self#drawHorizon 2;
		v<-[|(230,540);(230,550);(250,550);(250,540)|];self#drawVertic 3;
		v<-[|(270,530);(270,540);(290,540);(290,530)|];self#drawVertic 4;
		v<-[|(280,570);(280,580);(300,580);(300,570)|];self#drawHorizon 3;
		v<-[|(300,530);(300,540);(320,540);(320,530)|];self#drawVertic 4;
		v<-[|(330,530);(330,540);(350,540);(350,530)|];self#drawVertic 4;
		v<-[|(360,540);(360,550);(380,550);(380,540)|];self#drawVertic 4;
		v<-[|(370,530);(370,540);(390,540);(390,530)|];self#drawHorizon 2;
		v<-[|(420,540);(420,550);(440,550);(440,540)|];self#drawVertic 3;
		v<-[|(430,530);(430,540);(450,540);(450,530)|];self#drawHorizon 2;
		v<-[|(430,570);(430,580);(450,580);(450,570)|];self#drawHorizon 2;
		v<-[|(460,540);(460,550);(480,550);(480,540)|];self#drawVertic 3;
		v<-[|(490,530);(490,540);(510,540);(510,530)|];self#drawVertic 5;
		v<-[|(520,540);(520,550);(540,550);(540,540)|];self#drawVertic 3;
		v<-[|(530,530);(530,540);(550,540);(550,530)|];self#drawHorizon 3;
		v<-[|(530,570);(530,580);(550,580);(550,570)|];self#drawHorizon 2;
		v<-[|(560,540);(560,550);(580,550);(580,540)|];self#drawVertic 7;
		(** Numer wersji *)
		v<-[|(160,440);(160,450);(180,450);(180,440)|];self#drawHorizon 1;
		v<-[|(150,450);(150,460);(170,460);(170,450)|];self#drawHorizon 2;
		v<-[|(140,460);(140,470);(160,470);(160,460)|];self#drawHorizon 1;
		v<-[|(180,460);(180,470);(200,470);(200,460)|];self#drawHorizon 1;
		v<-[|(130,470);(130,480);(150,480);(150,470)|];self#drawHorizon 1;
		v<-[|(190,470);(190,480);(210,480);(210,470)|];self#drawHorizon 1;
		v<-[|(120,480);(120,490);(140,490);(140,480)|];self#drawHorizon 1;
		v<-[|(200,480);(200,490);(220,490);(220,480)|];self#drawHorizon 1;
		v<-[|(230,440);(230,450);(250,450);(250,440)|];self#drawVertic 2;
		v<-[|(260,480);(260,490);(280,490);(280,480)|];self#drawHorizon 1;
		v<-[|(270,490);(270,500);(290,500);(290,490)|];self#drawHorizon 1;
		v<-[|(280,500);(280,510);(300,510);(300,500)|];self#drawHorizon 1;
		v<-[|(290,440);(290,450);(310,450);(310,440)|];self#drawVertic 8;
		v<-[|(320,440);(320,450);(340,450);(340,440)|];self#drawVertic 2;
		v<-[|(350,450);(350,460);(370,460);(370,450)|];self#drawVertic 6;
		v<-[|(360,440);(360,450);(380,450);(380,440)|];self#drawHorizon 2;
		v<-[|(360,510);(360,520);(380,520);(380,510)|];self#drawHorizon 2;
		v<-[|(390,450);(390,460);(410,460);(410,450)|];self#drawVertic 6
end

class menu=
object (self)
	inherit logo as logo
	inherit instructions as instructions
	val mutable pause=false
	val mutable opt=(-1)
	val mutable gameOver=false
	val normalColor=rgb 150 150 150
	val highlightColor=rgb 200 200 200
	method drawLeft=set_color normalColor;fill_rect 480 0 (size_x()) 360
	method drawTop=set_color normalColor;fill_rect 0 360 (size_x()) (size_y())
	method drawButtons=
		set_color black;
		if pause then (moveto 537 339;draw_string "Resume");
		moveto 515 310;draw_string "Start New Game";
		moveto 522 281;draw_string "Instructions";
		moveto 545 252;draw_string "Exit"
	method getButton x y=
		if (x>=480&&x<=(size_y())) then
		(
			if (y>=242&&y<=266) then 0
			else if (y>=271&&y<=300) then 1
			else if (y>=300&&y<=329) then 2
			else if (y>=329&&y<=358&&pause) then 3
			else (-1)
		)
		else (-1)
	method highlight x y=
		let i=self#getButton x y in
		(
			match i with
			|0->self#drawLeft;set_color highlightColor;fill_rect 480 244 160 28;self#drawButtons
			|1->self#drawLeft;set_color highlightColor;fill_rect 480 273 160 28;self#drawButtons
			|2->self#drawLeft;set_color highlightColor;fill_rect 480 302 160 28;self#drawButtons
			|3->self#drawLeft;set_color highlightColor;fill_rect 480 331 160 28;self#drawButtons
			|_->if (opt!=(-1)) then self#drawLeft;self#drawButtons
		);
		opt<-i;
		synchronize()
	method drawGameOver=if gameOver then (set_color black;moveto 260 356;draw_string "Game Over!")
	method drawMenu=self#drawLeft;self#drawTop;self#drawButtons;logo#drawLogo;self#drawGameOver
	method drawPoints p=moveto 320 368;draw_string ("Points gained: "^string_of_int(p))
	method drawInstructions=instructions#drawBackground;instructions#drawText;synchronize()
	method redrawLogo=self#drawTop;logo#drawLogo
	method getOpt=opt
	method setPause f=pause<-f
	method setGameOver f=gameOver<-f
end

class background=
object (self)
	val mutable lifes=5
	val mutable points=0
	val mutable ammo=0
	val mutable level=1
	val leftColor=white
	val rightColor=red
	val borderColor=black
	method drawLeft=set_color leftColor;fill_rect 0 0 480 (size_y())
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
	method drawRight=set_color rightColor;fill_rect 480 0 (size_x()) (size_y());self#drawBorders;self#drawLifes;self#drawPoints;self#drawLevel;self#drawAmmo
	method draw=self#drawLeft;self#drawRight
	method updateLifes f=lifes<-f lifes 1;self#eraseLifes;self#drawLifes
	method getLifes=lifes
	method getPoints=points
	method updatePoints p=points<-points+p;self#erasePoints;self#drawPoints
	method updateLevel=level<-level+1;self#eraseLevel;self#drawLevel
	method addAmmo=ammo<-ammo+10;self#eraseAmmo;self#drawAmmo
	method removeAmmo=ammo<-ammo-1;self#eraseAmmo;self#drawAmmo
	method ammoState=if ammo!=0 then true else false
	method resetGame=lifes<-5;ammo<-0;points<-0;level<-1;self#drawRight
end

class plate=
object (self)
	val mutable width=60
	val mutable xPosition=180
	val mutable color=green
	method draw=set_color color;fill_rect xPosition 0 width 10
	method erase=set_color white;fill_rect xPosition 0 width 10
	method reset=self#erase;width<-60;xPosition<-180;color<-green;self#draw;synchronize()
	(** Przesuniecie paletki na pozycje x. *)
	method move x=
		self#erase; (** Usuwamy paletke ze starej pozycji. *)
		if x<60 then xPosition<-0 (** Jesli pozycja wypada poza lewa krawedzia ekranu, to ustawiamy paletke na pozycji x=0 *)
		else if x>529-width then xPosition<-479-width (** J.w. z prawej strony. *)
		else xPosition<-(x-60); (** Ustawiamy paletke na podanej pozycji *)
		self#draw;synchronize() (** Rysujemy paletke w nowym miejscu i synchronizujemy obraz *)
	(** Zmiana rozmiaru paletki (if dla zabezpieczenia przed "ucieciem" kawalka badz "wyjechaniem" poza obszar). *)
	method resize f=
		if (f width 20)>=60 then
		(
			let nwidth=f width 20 in
				if (xPosition+nwidth>480) then xPosition<-480-nwidth;
				self#erase;width<-nwidth;self#draw;synchronize()
		)
	method collision cx=
		let lleft=xPosition+(width/6) 
		and left=xPosition+(width/3) 
		and middle=xPosition+(width/2) 
		and right=int_of_float(float_of_int(xPosition)+.(float_of_int(width))/.1.5)
		and rright=int_of_float(float_of_int(xPosition)+.(float_of_int(width))/.1.2) in
			if cx=middle then 0
			else if cx<=lleft then (-3)
			else if cx>lleft&&cx<=left then (-2)
			else if cx>left&&cx<middle then (-1)
			else if cx>middle&&cx<=right then 1
			else if cx>right&&cx<=rright then 2
			else 3
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
	method onPlateMove x=
		self#erase;
		let xPos=xPosition in
			xPosition<-(x-30);
			if self#xCollided xPosition then xPosition<-xPos;
			self#draw;synchronize()
	method onPlate x w=if xPosition>=x&&xPosition-1<=(x+w)&&yPosition-radius=10 then true else false
	method xCollided x=if (xPosition+x-radius)<=0||(xPosition+radius)>=480 then true else false
	method yCollided y=if (xPosition+y+radius)>=size_y() then true else false
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
				while !locker do delay 0.001 done;
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
	val mutable lBulletFlying=false
	val mutable rBulletFlying=false
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
	method collided (xs,ys)=
		let x=ball#getXPosition and y=ball#getYPosition and r=ball#getRadius in
			if (y+r)>=360 then
			(
				let rec collided_aux col=
					match col with
					|hd::tl->let a=hd.(0) and b=hd.(2) and result=ref 0 in
						if ((x+xs+r)>=fst a&&(x+xs-r)<=fst b)&&((y+ys+r)>=snd b&&(y+ys-r)<=snd a) then
						(
							if (x+r<fst a&&x+xs+r>=fst a)||(x-r>fst b&&x+xs-r<=fst b) then result:=1
							else if (y+r<snd b&&y+ys+r>=snd b)||(y-r>snd a&&y+ys-r<=snd a) then result:=2
							else result:=2;
							self#removeFromCollection hd;
							!result
						)
						else collided_aux tl
					|_->0
				in collided_aux collection
			)
			else 0
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
	method setLBulletFlying s=lBulletFlying<-s
	method getLBulletFlying=lBulletFlying
	method setRBulletFlying s=rBulletFlying<-s
	method getRBulletFlying=rBulletFlying
end

class rifle (p:int) (board:board)=
object (self)
	val mutable vert=[||]
	val mutable position=0
	val mutable hits=false
	method erase=set_color white;fill_poly vert
	method draw=set_color black;fill_poly vert
	method setPosition x=position<-x
	method hit (x,y)=
		if y>=360 then
		(
			let rec hit_aux col=		
				match col with
				|hd::tl->let a=hd.(0) and b=hd.(2) in
					if (x>=fst a&&x<=fst b&&y>=snd b) then (board#removeFromCollection hd;true) else hit_aux tl
				|_->false
			in hit_aux board#getCollection
		)
		else false
	method missed y=if y>size_y() then true else false
	method flightOfTheBullet s=
		let rec flight_aux hit missed=
			match hit,missed with
			|false,false->
				let rec delayer()=try delay 0.003 with e->delayer() in
				while !locker do delay 0.001 done;
				delayer();
				self#erase;
				vert<-Array.map (fun (a,b)->(a,b+1)) vert;
				self#draw;
				synchronize();
				flight_aux (self#hit (vert.(1))) (self#missed (snd vert.(1)))
			|_,_->self#erase;if s=0 then board#setLBulletFlying false else board#setRBulletFlying false;synchronize()
		in flight_aux false false
	method shot s=
		position<-p;
		vert<-[|(position,10);(position,16);(position+3,16);(position+1,10)|];
		create self#flightOfTheBullet s;
		synchronize()
end

(** Nowa instancja tla (rowniez info dot. punktacji, itp.). *)
let background=new background
(** Nowa instancja paletki *)
let plate=new plate
(** Nowa instancja pilki *)
let ball=new ball
(** Nowa instancja planszy, przekazujemy do niej utworzone wczesniej tlo, paletke i pilke *)
let board=new board background plate ball
(** Nowa instancja menu *)
let menu=new menu
(** Funkcja ustalajaca nowe koordynaty pilki. *)
let ballCoords (x,y,d)=
	let rx=ref 0 and ry=ref 0 and rd=ref d in
		if ((ball#onPlate plate#getPosition plate#getWidth)&&ball#isMoving) then
			let q=plate#collision ball#getXPosition in
			rx:=q;ry:=-y;
			if q=(-3)||q=3 then rd:=0.010
			else if q=(-2)||q=2 then rd:=0.006
			else rd:=0.004
		else 
		(
			if ball#yCollided x then ry:=-y else ry:=y;
			if ball#xCollided y then rx:=-x else rx:=x;
			let collision=board#collided (x,y) in
				match collision with
				|1->rx:=-x
				|2->ry:=-y
				|_->()
		);
		(!rx,!ry,!rd)
let rec delayer f=try delay f with e->delayer f
let rollTheBall()=
	let rec delay_aux (x,y,d)=
		match ball#isDownBelow with
		|true->
			background#updateLifes (-);
			plate#reset;
			if background#getLifes=0 then (menu#setPause false;menu#setGameOver true;menu#drawMenu;menu#drawPoints background#getPoints);
			ball#reset;
			ball#changeState false
		|false->
			while !locker do delay 0.001 done;
			ball#move (x,y);
			if ball#getXPosition+ball#getRadius+x>=478 then (background#drawRight;synchronize());
			ball#changeState true;
			delayer d;
			delay_aux (ballCoords (x,y,d))
	in delay_aux (ballCoords(1,1,0.004))
let plateNavigation()=
	let rec delay_aux m i=
		if background#getLifes!=0 then
		(
			match i.key,i.button,m with
			|'\027',_,_->locker:=true;menu#drawMenu;synchronize()
			|_,true,false->create rollTheBall();delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
			|_,false,false->
				plate#move i.mouse_x;
				ball#onPlateMove i.mouse_x;
				delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
			|_,true,true->
			(
				if (background#ammoState&&board#getLBulletFlying=false&&board#getRBulletFlying=false) then
				(
					background#removeAmmo;
					board#setLBulletFlying true;
					board#setRBulletFlying true;
					let lrifle=new rifle plate#getPosition board in lrifle#shot 0;
					let rrifle=new rifle (plate#getPosition+plate#getWidth-3) board in rrifle#shot 1
				)
			);
			delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
			|_,_,true->plate#move i.mouse_x;delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
		)
	in delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
let instructionsNavigation()=
	let rec navi_aux i=
		match i.button with
		|true->menu#redrawLogo;synchronize()
		|false->navi_aux (wait_next_event [Button_down])
	in navi_aux (wait_next_event [Button_down])
let menuNavigation()=
	let rec navi_aux i=
		match i.button with
		|true->
			let opt=menu#getOpt in
			(
				match opt with
				|0->synchronize()
				|1->menu#drawInstructions;instructionsNavigation();navi_aux (wait_next_event [Mouse_motion;Button_down])
				|2->
					background#resetGame;
					menu#setGameOver false;
					locker:=false;
					ball#reset;
					ball#changeState false;
					menu#setPause true;
					background#draw;
					board#createCollection;
					board#drawCollection;
					plateNavigation();
					navi_aux (wait_next_event [Mouse_motion;Button_down])
				|3->locker:=false;background#draw;board#drawCollection;plateNavigation();navi_aux (wait_next_event [Mouse_motion;Button_down])
				|_->navi_aux (wait_next_event [Mouse_motion;Button_down])
			)
		|false->menu#highlight i.mouse_x i.mouse_y;navi_aux (wait_next_event [Mouse_motion;Button_down])
	in navi_aux (wait_next_event [Mouse_motion;Button_down])
let main=
	open_graph " 640x640";
	set_window_title "OCamloid 0.9";
	plate#draw;
	ball#draw;
	menu#drawMenu;
	auto_synchronize false;
	menuNavigation()
