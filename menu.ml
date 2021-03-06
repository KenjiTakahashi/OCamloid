open Graphics
open Io
open Tcp

class instructions= (** Klasa sluzaca do wyswietlania instrukcji do gry. *)
object (self)
    method drawText= (** Rysowanie wlasciwego tekstu instrukcji. *)
        set_color black; (** Ustawienie koloru. *)
        moveto 20 611;draw_string "Controls:"; (** Przejscie do punktu i wypisanie tekstu. *)
        moveto 20 592;draw_string "Mouse button - release the ball or shot the gun.";
        moveto 20 573;draw_string "Mouse movement - control the plate.";
        moveto 0 569;lineto (size_x()) 569; (** Rysowanie poziomej linii dzielacej segmenty. *)
        moveto 20 554;draw_string "Blocks types:"; (** Przejscie do punktu i wypisanie tekstu. *)
        moveto 70 530;draw_string "- normal block, disappears when hit (+10 points).";
        moveto 70 500;draw_string "- 2xhit block, turns to green when hit (+10 points).";
        moveto 70 470;draw_string "- Permanent block, impossible to destroy (no points).";
        moveto 70 440;draw_string "- Bonus block, drops bonus when hit (+5 points).";
        set_color green;fill_rect 20 526 40 20; (** Zmiana koloru i rysowanie bloku. *)
        set_color red;fill_rect 20 496 40 20;
        set_color magenta;fill_rect 20 466 40 20;
        set_color blue;fill_rect 20 436 40 20;
        set_color black;moveto 0 432;lineto (size_x()) 432; (** Ustawienie koloru i rysowanie poziomej linii dzielacej segmenty. *)
        moveto 20 418;draw_string "Bonuses:"; (** Przejscie do punktu i wypisanie tekstu. *)
        moveto 45 395;draw_string "- Lifes +1.";
        moveto 150 395;draw_string "- Enlarge plate.";
        moveto 280 395;draw_string "- Ammo +1.";
        set_color red;fill_poly [|(20,390);(20,410);(30,410);(30,398);(35,398);(35,390)|]; (** Rysowanie znakow power-upow. *)
        set_color green;fill_poly [|(130,390);(130,410);(140,410);(140,404);(136,404);(136,402);(139,402);(139,398);(136,398);(136,396);(140,396);(140,390)|];
        set_color black;fill_poly [|(260,390);(263,410);(267,410);(270,390);(267,390);(266,395);(264,395);(263,390)|];
        moveto 0 386;lineto (size_x()) 386;
        moveto 200 368;draw_string "Press mouse button to close this message." (** Przejscie do punktu i wypisanie tekstu. *)
end

class logo= (** Klasa sluzaca do wyswietlania loga. *)
object (self)
    val mutable v=[||] (** Tablica wierzcholkow. *)
    method color= (** Losowanie koloru. *)
        Random.self_init();
        let i=Random.int 4 in
            match i with
            |0->blue
            |1->red
            |2->magenta
            |_->green
    method draw c v=set_color c;fill_poly v (** Rysowanie wielokata o danym kolorze c i wspolrzednych v. *)
    method drawVertic j=for i=1 to j do self#draw self#color v;v<-Array.map (fun (a,b)->(a,b+10)) v done (** Rysowanie j wielokatow z przesunieciem w pionie. *)
    method drawHorizon j=for i=1 to j do self#draw self#color v;v<-Array.map (fun (a,b)->(a+20,b)) v done (** Rysowanie j wielokatow z przesunieciem w poziomie *)
    method drawLogo= (** Rysowanie loga i numeru wersji *)
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

class scoreboard=
object (self)
    inherit tcp as remote
    inherit io as local
    val arrow_up=[|(310,620);(320,630);(330,620)|]
    val arrow_down=[|(310,380);(320,370);(330,380)|]
    val highlightColor=rgb 200 200 200
    val normalColor=rgb 150 150 150
    val mutable scores=[]
    val mutable index=0
    val mutable scoreboard_opt=(-1)
    val mutable points_opt=(-1)
    val mutable auth_opt=(-1)
    method private drawTop= (** Rysowanie tla pod top. *)
        set_color normalColor;
        fill_rect 0 360 (size_x()) (size_y())
    method get_scores=
        if (remote#sock_connect "127.0.0.1" 6666)
        then
        (
            scores<-remote#inet_get_scores;
            remote#sock_close;
            local#setScoreboard scores;
            local#setScores
        )
        else scores<-local#getScores
    method private draw_arrows=
        fill_poly arrow_up;
        fill_poly arrow_down;
        moveto 0 620;
        lineto (size_x()) 620;
        moveto 0 380;
        lineto (size_x()) 380;
    method draw_scoreboard=
        set_color black;
        self#draw_arrows;
        let rec draw_aux s i h=
            match s with
            |(name,score)::tl->
                        if i>=index&&i<(index+12) then
                        (
                            moveto 20 h;
                            draw_string name;
                            moveto 220 h;
                            draw_string (string_of_int score);
                            draw_aux tl (i+1) (h-19)
                        )
                        else if i<(index+11) then draw_aux tl (i+1) h else synchronize()
            |_->synchronize()
        in draw_aux scores 0 601
    method scoreboard_down=
        if (index+12)<List.length scores then
        (
            self#drawTop;
            index<-index+1;
            self#draw_scoreboard
        )
    method scoreboard_up=
        if index>0 then
        (
            self#drawTop;
            index<-index-1;
            self#draw_scoreboard
        )
    method scoreboard_get_option x y=
        if (x>=0&&x<(size_x())&&y<(size_y())&&y>=359) then
        (
            if (y>=359&&y<=380) then 0
            else if (y<=(size_y())&&y>=620) then 1
            else 2
        )
        else 2
    method scoreboard_highlight x y=
        let i=self#scoreboard_get_option x y in
        (
            match i with
            |0->
                    set_color highlightColor;
                    fill_rect 0 360 (size_x()) 20;
                    set_color black;
                    self#draw_arrows
            |1->
                    set_color highlightColor;
                    fill_rect 0 620 (size_x()) 20;
                    set_color black;
                    self#draw_arrows
            |_->
                    set_color normalColor;
                    fill_rect 0 360 (size_x()) 20;
                    fill_rect 0 620 (size_x()) 20;
                    set_color black;
                    self#draw_arrows
        );
        scoreboard_opt<-i;
        synchronize()
    method get_scoreboard_opt=scoreboard_opt
    method drawPoints p= (** Rysowanie ilosci zdobytych punktow i miejsca na wpisanie nicku (po zakonczeniu gry). *)
        set_color normalColor;
        fill_rect 180 260 244 99;
        moveto 190 344;
        set_color black;
        draw_string ("You've gained "^string_of_int(p)^" points");
        self#draw_points_options
   method private draw_points_options=
        set_color normalColor;
        fill_rect 180 260 244 44;
        set_color black;
        moveto 190 324;
        draw_string "Enter your name:";
        moveto 180 302;
        lineto 424 302;
        moveto 265 286;
        draw_string "Store locally.";
        moveto 180 280;
        lineto 424 280;
        moveto 265 264;
        draw_string "Send to server.";
        synchronize()
    method points_get_option x y=
        if (x>=180&&x<=424)
        then
        (
            if (y>280&&y<=302) then 0
            else if (y>=260&&y<=280) then 1
            else (-1)
        )
        else (-1)
    method points_highlight x y=
        let i=self#points_get_option x y in
        (
            match i with
            |0->
                    self#draw_points_options;
                    set_color highlightColor;
                    fill_rect 180 282 244 19;
                    set_color black;
                    moveto 265 286;
                    draw_string "Store locally."
            |1->
                    self#draw_points_options;
                    set_color highlightColor;
                    fill_rect 180 260 244 19;
                    set_color black;
                    moveto 265 264;
                    draw_string "Send to server."
            |_->self#draw_points_options
        );
        points_opt<-i;
        synchronize()
    method draw_name name=
        set_color normalColor;
        fill_rect 288 324 136 12;
        set_color black;
        moveto 288 324;
        draw_string name;
        fill_rect (290+(String.length name*6)) 324 5 12;
        synchronize()
    method draw_errors err=
        set_color normalColor;
        fill_rect 190 308 234 10;
        set_color black;
        moveto 190 308; 
        draw_string err;
        synchronize()
    method draw_auth name=
        set_color normalColor;
        fill_rect 180 260 244 99;
        moveto 190 344;
        set_color black;
        draw_string ("Password for "^name);
        self#draw_auth_options
    method draw_auth_options=
        set_color normalColor;
        fill_rect 180 260 244 44;
        set_color black;
        moveto 190 324;
        draw_string "Enter the password:";
        moveto 180 302;
        lineto 424 302;
        moveto 265 286;
        draw_string "Authenticate.";
        moveto 180 280;
        lineto 424 280;
        moveto 265 264;
        draw_string "Create account.";
        synchronize()
    method auth_get_option x y=
        if (x>=180&&x<=424)
        then
        (
            if (y>280&&y<=302) then 0
            else if (y>=260&&y<=280) then 1
            else (-1)
        )
        else (-1)
    method auth_highlight x y=
        let i=self#auth_get_option x y in
        (
            match i with
            |0->
                    self#draw_auth_options;
                    set_color highlightColor;
                    fill_rect 180 282 244 19;
                    set_color black;
                    moveto 265 286;
                    draw_string "Authenticate."
            |1->
                    self#draw_auth_options;
                    set_color highlightColor;
                    fill_rect 180 260 244 19;
                    set_color black;
                    moveto 265 264;
                    draw_string "Create account."
            |_->
                    self#draw_auth_options
        );
        auth_opt<-i;
        synchronize()
    method draw_password l=
        set_color normalColor;
        fill_rect 306 324 114 12;
        set_color black;
        moveto 306 324;
        draw_string (String.make l '*');
        fill_rect (308+l*6) 324 5 12;
        synchronize()
end

class menu= (** Klasa sluzaca do wyswietlania i obslugi menu. *)
object (self)
    inherit logo as logo (** Dziedziczenie loga. *)
    inherit instructions as instructions (** Dziedziczenie instrukcji. *)
    inherit scoreboard as scoreboard (** Dziedziczenie tablicy wynikow. *)
    val mutable pause=false (** Stan pauzy. *)
    val mutable opt=(-1) (** Aktualnie wybrana opcja. *)
    val mutable gameOver=false (** Stan konca gry. *)
    method drawRight=set_color normalColor;fill_rect 480 0 (size_x()) 360 (** Rysowanie prawej czesci pod menu. *)
    method drawButtons= (** Rysowanie przyciskow menu. *)
        set_color black;
        if pause then (moveto 537 339;draw_string "Resume"); (** Jesli gra jest w stanie pauzy, rysujemy rowniez przycisk wnowienia. *)
        moveto 515 310;draw_string "Start New Game";
        moveto 528 281;draw_string "Scoreboard";
        moveto 522 252;draw_string "Instructions";
        moveto 545 223;draw_string "Exit"
    method private getButton x y= (** Ustalanie, ktora opcja zostala wskazana. *)
        if (x>=480&&x<(size_x())&&y<=358&&y>=213) then (** Jesli wskazanie nastapilo w obszarze menu. *)
        (
            if (y>=213&&y<=237) then 0 (** Obszar piatej opcji. (Exit) *)
            else if (y>=242&&y<=271) then 1 (** Obszar czwartej opcji. (Instructions) *)
            else if (y>=271&&y<=300) then 2 (** Obszar trzeciej opcji. (Scoreboard) *)
            else if (y>=300&&y<=329) then 3 (** Obszar drugiej opcji. (Start New Game) *)
            else if (y>=329&&y<=358&&pause) then 4 (** Obszar pierwszej opcji (Resume). Tylko jesli gra jest w stanie pauzy. *)
            else (-1) (** W przeciwnym wypadku (dla kompatybilnosci typow). *)
        )
        else (-1) (** Nie nastapilo wskazanie w obszarze menu. *)
    method highlight x y= (** Podswietlanie opcji w menu. *)
        let i=self#getButton x y in (** Pobieranie aktualnie wskazanej opcji. *)
        (
            match i with (** Matchowanie po opcjach menu. *)
            |0->self#drawRight;set_color highlightColor;fill_rect 480 215 160 28;self#drawButtons (** Podswietlenie piatej opcji. (Exit) *)
            |1->self#drawRight;set_color highlightColor;fill_rect 480 244 160 28;self#drawButtons (** Podswietlenie czwartej opcji. (Instructions) *)
            |2->self#drawRight;set_color highlightColor;fill_rect 480 273 160 28;self#drawButtons (** Podswietlenie trzeciej opcji. (Scoreboard) *)
            |3->self#drawRight;set_color highlightColor;fill_rect 480 302 160 28;self#drawButtons (** Podswietlenie drugiej opcji. (Start New Game) *)
            |4->self#drawRight;set_color highlightColor;fill_rect 480 331 160 28;self#drawButtons (** Podswietlenie pierwszej opcji. (Resume) *)
            |_->self#drawRight;self#drawButtons (** Powrot do stanu poczatkowego (bez podswietlenia). *)
        );
        opt<-i; (** Przypisanie aktualnie podswietlonej opcji do (potencjalnie) wybranej. *)
        synchronize() (** Synchronizacja obrazu. *)
    method drawGameOver=if gameOver then (set_color black;moveto 260 368;draw_string "Game Over!") (** Rysowanie napisu o koncu gry. *)
    method drawMenu=self#drawRight;self#drawTop;self#drawButtons;logo#drawLogo;self#drawGameOver (** Rysowanie menu. *)
    method redrawPointsField=
        set_color white;
        fill_rect 180 260 244 99
    method drawInstructions=self#drawTop;instructions#drawText;synchronize() (** Rysowanie instrukcji. *)
    method drawScoreboard=
        self#drawTop;
        scoreboard#draw_scoreboard;
        synchronize()
    method redrawLogo=self#drawTop;logo#drawLogo (** Przerysowywanie loga (potrzebne po zamknieciu instrukcji). *)
    method getOpt=opt (** Pobieranie aktualnie wybranej opcji. *)
    method setPause f=pause<-f (** Ustawianie stanu pauzy. *)
    method setGameOver f=gameOver<-f (** Ustawianie stanu konca gry. *)
end
