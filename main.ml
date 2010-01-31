(** Ladowanie modulow menu i gry. *)
open Menu
open Game
(** Ladowanie modulow grafiki i watkow. *)
open Graphics
open Thread

let background=new background (** Nowa instancja tla (rowniez info dot. punktacji, itp.). *)
let plate=new plate (** Nowa instancja paletki *)
let ball=new ball (** Nowa instancja pilki *)
let board=new board background plate ball (** Nowa instancja planszy, przekazujemy do niej utworzone wczesniej tlo, paletke i pilke *)
let menu=new menu (** Nowa instancja menu *)
let ballCoords (x,y,d)= (** Funkcja ustalajaca nowe koordynaty pilki. *)
	let rx=ref 0 and ry=ref 0 and rd=ref d in
		if ((ball#onPlate plate#getPosition plate#getWidth)&&ball#isMoving) then
			let q=plate#collision ball#getXPosition in
			rx:=q;ry:=-y;
			if q=(-3)||q=3 then rd:=0.010
			else if q=(-2)||q=2 then rd:=0.006
			else rd:=0.004
		else 
		(
			if ball#yCollided y then ry:=-y else ry:=y;
			if ball#xCollided x then rx:=-x else rx:=x;
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
