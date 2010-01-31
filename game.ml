open Graphics
open Thread

let locker=ref false (** Funkcja odpowiedzialna za blokowanie obiektow w trybie pauzy *)

class background= (** Klasa sluzaca do rysowania tla i obslugi panelu bocznego gry. *)
object (self)
	val mutable lifes=5 (** Liczba zyc. *)
	val mutable points=0 (** Liczba punktow. *)
	val mutable ammo=0 (** Ilosc amunicji. *)
	val mutable level=1 (** Poziom. *)
	val leftColor=white (** Kolor lewej czesci planszy. *)
	val rightColor=red (** Kolor prawej czesci planszy (panelu bocznego). *)
	val borderColor=black (* Kolor ramek. *)
	method drawLeft=set_color leftColor;fill_rect 0 0 480 (size_y()) (** Rysowanie lewej czesci planszy. *)
	method drawBorders= (** Rysowanie ramek panelu bocznego. *)
		set_color borderColor;set_line_width 2; (** Ustawianie koloru i grubosci ramek. *)
		moveto 525 330;draw_string "Lifes left"; (** Rysowanie tekstu. *)
		draw_rect 490 280 130 50; (** Rysowanie ramki. *)
		moveto 540 250;draw_string "Level";
		draw_rect 490 200 130 50;
		moveto 515 170;draw_string "Points gained";
		draw_rect 490 120 130 50;
		moveto 525 90;draw_string "Ammunition";
		draw_rect 490 40 130 50
	method eraseLifes=set_color rightColor;fill_rect 540 290 40 20 (** Czyszczenie ilosci zyc z ekranu (potrzebne do aktualizacji). *)
	method erasePoints=set_color rightColor;fill_rect 500 130 80 20 (** Czyszczenie ilosci punktow z ekranu (j.w.). *)
	method eraseAmmo=set_color rightColor;fill_rect 500 50 80 20 (** Czyszczenie ilosci amunicji z ekranu (j.w.). *)
	method eraseLevel=set_color rightColor;fill_rect 500 210 80 20 (** Czyszczenie poziomu z ekranu (j.w.) *)
	method drawLifes=set_color black;moveto 550 300;draw_string (string_of_int(lifes)) (** Rysowanie ilosci zyc. *)
	method drawPoints=set_color black;moveto 500 140;draw_string (string_of_int(points)) (** Rysowanie ilosci punktow. *)
	method drawAmmo=set_color black;moveto 500 60;draw_string (string_of_int(ammo)) (** Rysowanie ilosci amunicji. *)
	method drawLevel=set_color black;moveto 500 220;draw_string (string_of_int(level)) (** Rysowanie poziomu. *)
	method drawRight=set_color rightColor;fill_rect 480 0 (size_x()) (size_y());self#drawBorders;self#drawLifes;self#drawPoints;self#drawLevel;self#drawAmmo (** Rysowanie prawej czesci planszy (panelu). *)
	method draw=self#drawLeft;self#drawRight (** Rysowanie calosci planszy. *)
	method updateLifes f=lifes<-f lifes 1;self#eraseLifes;self#drawLifes (** Aktualizacja ilosci zyc. *)
	method getLifes=lifes (** Pobieranie ilosci zyc. *)
	method getPoints=points (** Pobieranie ilosci punktow. *)
	method updatePoints p=points<-points+p;self#erasePoints;self#drawPoints (** Aktualizacja ilosci punktow. *)
	method updateLevel=level<-level+1;self#eraseLevel;self#drawLevel (** Aktualizacja poziomu. *)
	method addAmmo=ammo<-ammo+10;self#eraseAmmo;self#drawAmmo (** Dodawanie amunicji. *)
	method removeAmmo=ammo<-ammo-1;self#eraseAmmo;self#drawAmmo (** Odejmowanie amunicji. *)
	method ammoState=if ammo!=0 then true else false (** Pobieranie stanu amunicji (jest/nie ma). *)
	method resetGame=lifes<-5;ammo<-0;points<-0;level<-1;self#drawRight (** Reset planszy do ustawien wyjsciowych. *)
end

class plate= (** Klasa sluzaca do rysowania i obslugi paletki. *)
object (self)
	val mutable width=60 (** Szerokosc. *)
	val mutable xPosition=180 (** Pozycja. *)
	val mutable color=green (** Kolor. *)
	method draw=set_color color;fill_rect xPosition 0 width 10 (** Rysowanie. *)
	method erase=set_color white;fill_rect xPosition 0 width 10 (** Czyszczenie. *)
	method reset=self#erase;width<-60;xPosition<-180;color<-green;self#draw;synchronize() (** Reset do ustawien wyjsciowych. *)
	method move x= (** Przesuniecie paletki na pozycje x. *)
		self#erase; (** Usuwanie paletki ze starej pozycji. *)
		if x<60 then xPosition<-0 (** Jesli pozycja wypada poza lewa krawedzia ekranu, to ustawianie paletki na pozycji x=0 *)
		else if x>529-width then xPosition<-479-width (** J.w. z prawej strony. *)
		else xPosition<-(x-60); (** Ustawianie paletki na podanej pozycji. *)
		self#draw;synchronize() (** Rysowanie paletki w nowym miejscu i synchronizacja obrazu. *)
	method resize f= (** Zmiana rozmiaru paletki (if dla zabezpieczenia przed "ucieciem" kawalka badz "wyjechaniem" poza obszar). *)
		if (f width 20)>=60 then
		(
			let nwidth=f width 20 in
				if (xPosition+nwidth>480) then xPosition<-480-nwidth;
				self#erase;width<-nwidth;self#draw;synchronize()
		)
	method collision cx= (** Wykrywanie kolizji z pilka. *)
		let lleft=xPosition+(width/6) (** Obszar mocno na lewo. *)
		and left=xPosition+(width/3) (** Obszar srednio na lewo. *)
		and middle=xPosition+(width/2) (** Srodek. *)
		and right=int_of_float(float_of_int(xPosition)+.(float_of_int(width))/.1.5) (** Obszar srednio na prawo. *)
		and rright=int_of_float(float_of_int(xPosition)+.(float_of_int(width))/.1.2) in (** Obszar mocno na prawo. *)
			if cx=middle then 0 (** Jesli srodek. *)
			else if cx<=lleft then (-3) (** Jesli pomiedzy brzegiem a mocno na lewo. *)
			else if cx>lleft&&cx<=left then (-2) (** Jesli pomiedzy mocno a srednio na lewo. *)
			else if cx>left&&cx<middle then (-1) (** Jesli pomiedzy srodkiem a srednio na prawo. *)
			else if cx>middle&&cx<=right then 1 (** Jesli pomiedzy srednio a mocno na prawo. *)
			else if cx>right&&cx<=rright then 2 (** Jesli pomiedzy mocno na prawo a brzegiem. *)
			else 3 (** W przeciwnym wypadku (kompatybilnosc typow.) *)
	method getPosition=xPosition (** Pobieranie pozycji. *)
	method getWidth=width (** Pobieranie szerokosci. *)
end

class ball= (** Klasa sluzaca do rysowania i obslugi pilki. *)
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
			if self#xCollided (xPosition-xPos) then xPosition<-xPos;
			self#draw;synchronize()
	method onPlate x w=if xPosition>=x&&xPosition-1<=(x+w)&&yPosition-radius=10 then true else false
	method xCollided x=if (xPosition+x-radius)<=0||(xPosition+x+radius)>=480 then true else false
	method yCollided y=if (yPosition+y+radius)>=size_y() then true else false
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
		model<-Random.int 3;
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
