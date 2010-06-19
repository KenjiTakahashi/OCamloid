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
    method addAmmo=ammo<-ammo+5;self#eraseAmmo;self#drawAmmo (** Dodawanie amunicji. *)
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
    val mutable xPosition=210 (** Pozycja w poziomie. *)
    val mutable yPosition=15 (** Pozycja w pionie. *)
    val mutable radius=5 (** Promien. *)
    val mutable color=blue (** Kolor. *)
    val mutable state=false (** Stan (w ruchu/spoczynku). *)
    method draw=set_color color;fill_circle xPosition yPosition radius (** Rysowanie. *)
    method erase=set_color white;fill_circle xPosition yPosition radius (** Czyszczenie (do animacji). *)
    method reset=self#erase;xPosition<-210;yPosition<-15;radius<-5;self#draw;synchronize() (** Reset do pozycji poczatkowej. *)
    method move (x,y)=self#erase;xPosition<-(xPosition+x);yPosition<-(yPosition+y);self#draw;synchronize() (** Animacja. *)
    method onPlateMove x= (** Przemieszczenie na paletce. *)
        self#erase; (** Czyszczenie. *)
        xPosition<-(x-30); (** Ustawienie nowej pozycji (-30 zeby kursor byl zawsze na koncu paletki. *)
        if xPosition>=450 then xPosition<-450 else if xPosition<=30 then xPosition<-30; (** Sprawdzanie, czy pilka nie znajdzie sie poza obszarem. *)
        self#draw;synchronize() (** Rysowanie i odswiezenie ekranu. *)
    method onPlate x w=if xPosition>=x&&xPosition-1<=(x+w)&&yPosition-radius=10 then true else false (** Sprawdzanie, czy pilka znajduje sie na paletce. *)
    method xCollided x=if (xPosition+x-radius)<=0||(xPosition+x+radius)>=480 then true else false (** Sprawdzanie kolizji ze scianami bocznymi. *)
    method yCollided y=if (yPosition+y+radius)>=size_y() then true else false (** Sprawdzanie kolizji z gora obszaru. *)
    method isDownBelow=if yPosition<0 then true else false (** Sprawdzanie, czy pilka spadala pod plansze. *)
    method changeState x=state<-x (** Zmiana stanu. *)
    method isMoving=state (** Pobranie stanu. *)
    method getXPosition=xPosition (** Pobranie pozycji w poziomie. *)
    method getYPosition=yPosition (** Pobranie pozycji w pionie. *)
    method getRadius=radius (** Pobranie promienia. *)
end

class powerups (p:int) (background:background) (plate:plate)= (** Klasa sluzaca do rysowania i obslugi power-upow. *)
object (self)
    val position=p (** Pozycja. *)
    val mutable model=0 (** Typ (0-dodanie zycia,1-powiekszenie paletki,2-dodanie amunicji). *)
    val mutable color=white (** Kolor. *)
    val mutable vert=[||] (** Tablica wierzcholkow. *)
    method erase=set_color white;fill_poly vert (** Czyszczenie (do animacji). *)
    method draw=set_color color;fill_poly vert (** Rysowanie wielokata. *)
    method distAwards= (** Rozdawanie bonusow. *)
        self#erase; (** Czyszczenie. *)
        match model with (** Match po typie. *)
        |0->background#updateLifes (+) (** Dodanie zycia. *)
        |1->create (fun ()->plate#resize (+);delay 10.;plate#resize (-)) ();synchronize() (** Powiekszenie paletki na 10 sekund. *)
        |2->background#addAmmo (** Dodanie amunicji. *)
        |_->failwith "Unrecognized power-up model (sth's wrong)" (** Kompatybilnosc (w normalnych warunkach nie powinno sie wydarzyc). *)
    method gained y= (** Sprawdzanie, czy power-up zostal zebrany. *)
        let x=plate#getPosition in (** Pobieranie pozycji paletki. *)
            if y<10&&position+10>x&&position<x+plate#getWidth then true else false (** Sprawdzenie, czy power-up jest w obszarze paletki. *)
    method missed y=if y<0 then true else false (** Sprawdzanie, czy power-up zostal stracony (tj. znalazl sie pod plansza). *)
    method pdraw_aux()= (** Animacja. *)
        let rec pdraw_aaux gained missed=
            match gained,missed with (** Match po straceniu badz zebraniu power-upa. *)
            |false,false-> (** Jesli nie stracono, ani nie zebrano - to rysujemy dalej. *)
                let rec delayer()=try delay 0.005 with e->delayer() in (** Funkcja wylapujaca bledy w opoznieniu i w razie potrzeby wywolujaca ponownie. *)
                while !locker do delay 0.001 done; (** Jesli pauza, to oczekuje na wznowienie. *)
                delayer(); (** Wywolanie funkcji opozniajacej. *)
                self#erase; (** czyszczenie. *)
                vert<-Array.map (fun (a,b)->(a,b-1)) vert; (** Ustawianie nowych wspolrzednych. *)
                self#draw; (** Rysowanie. *)
                synchronize();
                pdraw_aaux (self#gained (snd vert.(0))) (self#missed (snd vert.(1))) (** Wywolanie rekurencyjne z nowymi parametrami. *)
            |true,false->self#distAwards (** Jesli zebrano, to rozdzielamy nagrody. *)
            |false,true->synchronize() (** Jesli stracono, konczymy wywolanie. *)
            |true,true->failwith "This should never happen..." (** Kompatybilnosc (nie powinno sie wydarzyc). *)
        in pdraw_aaux false false
    method pdraw= (** Wywolanie. *)
    (
        Random.self_init(); (** Inicjalizacja randomizera. *)
        model<-Random.int 3; (** Przypisanie losowego typu. *)
        match model with (** Match po typie. *)
        |0->vert<-[|(position,340);(position,360);(position+10,360);(position+10,348);(position+15,348);(position+15,340)|];color<-red (** Rusowanie czerwonego L (Dodanie zycia). *)
        |1->vert<-[|(position,340);(position,360);(position+10,360);(position+10,354);(position+6,354);(position+6,352);(position+9,352);(position+9,348);(position+6,348);(position+6,346);(position+10,346);(position+10,340)|];color<-green (** Rysowanie zielonego E (powiekszenie paletki). *)
        |2->vert<-[|(position,340);(position+3,360);(position+7,360);(position+10,340);(position+7,340);(position+6,345);(position+4,345);(position+3,340)|];color<-black (** Rysowanie czarnego A (dodanie amunicji). *)
        |_->failwith "Unrecognized power-up model type (sth's wrong)"
    );
    create self#pdraw_aux();synchronize() (** Utworzenie watku i odswiezenie ekranu. *)
end

class board (background:background) (plate:plate) (ball:ball)= (** Klasa sluzaca do rysowania i obslugi planszy. *)
object (self)
    val mutable vert=[||] (** Tablica wierzcholkow. *)
    val mutable maincolor=white (** Kolor. *)
    val mutable collection=[] (** Lista wszystkich blokow. *)
    val mutable colors=[] (** Lista kolorow wszystkich blokow. *)
    val mutable counter=0; (** Liczba blokow. *)
    val mutable lBulletFlying=false (** Lot lewego pocisku. *)
    val mutable rBulletFlying=false (** Lot prawego pocisku. *)
    method erase c=set_color c;fill_poly vert (** Czyszczenie. *)
    method draw=set_color maincolor;fill_poly vert (** Rysowanie. *)
    method lottery i=Random.int i (** Losowanie liczby z przedzialu od 0 do i. *)
    method drawColors= (** Losowanie koloru bloku. *)
        match (self#lottery 14) with (** Match po wylosowanym numerze. *)
        |0->blue
        |1->red
        |2->red
        |3->magenta
        |_->green
    method clearCollection=collection<-[] (** Czyszczenie kolekcji. *)
    method createCollection= (** Tworzenie kolekcji. *)
        Random.self_init(); (** Inicjalizacja randomizera. *)
        let sy=size_y() in (** Pobranie wysokosci planszy. *)
        let vert=[|(0,sy);(0,sy-20);(40,sy-20);(40,sy)|] in (** Stworzenie tablicy wierzcholkow w lewym gornym rogu. *)
            let rec create_aux i x color col=
                match i with (** Match po kolumnie planszy. *)
                |12->collection<-col;colors<-color (** Jesli doszlismy do 12 kolumny, to zapisujemy utworzone kolekcje. *)
                |_->let rec create_aaux j e y t color col= (** Tworzenie blokow w obrebie danej kolumny. (j-wiersz, e-max. liczba blokow w kolumnie, y-wspolrzedna pionowa, t-(jesli 1 nie tworzymy bloku w danym miejscu), color-lista kolorow, col-lista blokow) *)
                        if j=e then create_aux (i+1) (x+40) color col else (** Po przejsciu 14 pozycji, przechodzimy do nastepnej kolumny. *)
                        let c=self#drawColors in (** Losowanie koloru. *)
                        if t!=1 then create_aaux (j+1) e (y+20) (self#lottery 3) (c::color) (if c!=magenta then (counter<-counter+1);(Array.map (fun (a,b)->(a+x,b-y)) vert)::col) (** Tworzenie bloku i dodawanie go do tablicy, jesli kolor bloku to magenta, nie liczymy go do liczby blokow (aby wiedziec, kiedy skonczyc plansze).  *)
                        else create_aaux (j+1) e (y+20) (self#lottery 3) color col (** Puste wywolanie z nowymi danymi (jesli t=1, czyli nie tworzymy bloku). *)
                    in create_aaux 0 14 0 (self#lottery 3) color col
            in create_aux 0 0 [] []
    method drawCollection= (** Rysowanie kolekcji. *)
        let rec draw_aux col color=
            match col,color with (** Match po liscie blokow i odpowiadajacej liscie kolorow. *)
            |h1::t1,h2::t2->maincolor<-h2;vert<-h1;self#draw;draw_aux t1 t2 (** Rysowanie kolejnych elementow kolekcji. *)
            |_,_->synchronize() (** Odswiezenie ekranu. *)
        in draw_aux collection colors
    method collided (xs,ys)= (** Sprawdzanie kolizcji pilki z blokami (xs,ys)-szybkosc pilki odp. w poziomie i w pionie. *)
        let x=ball#getXPosition and y=ball#getYPosition and r=ball#getRadius in (** Pobieranie pozycji i promienia pilki. *)
            if (y+r)>=360 then (** Jesli pilka jest w obszarze planszy. *)
            (
                let rec collided_aux col=
                    match col with (** Match po kolekcji blokow. *)
                    |hd::tl->let a=hd.(0) and b=hd.(2) and result=ref 0 in (** Pobieranie wspolrzednych bloku. *)
                        if ((x+xs+r)>=fst a&&(x+xs-r)<=fst b)&&((y+ys+r)>=snd b&&(y+ys-r)<=snd a) then (** Sprawdzenie, czy pilka jest w obrebie bloku. *)
                        (
                            if (x+r<fst a&&x+xs+r>=fst a)||(x-r>fst b&&x+xs-r<=fst b) then result:=1 (** Zderzenie w pionie. *)
                            else if (y+r<snd b&&y+ys+r>=snd b)||(y-r>snd a&&y+ys-r<=snd a) then result:=2 (** Zderzenie w poziomie. *)
                            else result:=2; (** Rowniez zderzenie w poziomie. *)
                            self#removeFromCollection hd; (** Usuwanie bloku z kolekcji. *)
                            !result (** Zwracanie wyniku. *)
                        )
                        else collided_aux tl (** Wywolanie rekurencyjne. *)
                    |_->0 (** Brak kolizji. *)
                in collided_aux collection
            )
            else 0 (** Brak kolizji. *)
    method removeFromCollection chosen= (** Usuwanie elementu z kolekcji. *)
    (
        let rec remove_aux col coltmp color colortmp=
            match col,color with (** Match po kolekcji blokow i kolorow. *)
            |h1::t1,h2::t2->
                if h1=chosen then (** Jesli aktualny blok jest przeznaczony do usuniecia. *)
                (
                    if h2=green then (** Jesli jest zielony (normalny). *)
                    (
                        collection<-(List.rev_append coltmp t1); (** Przypisanie do kolekcji wczesniejszych i pozniejszych blokow. *)
                        colors<-(List.rev_append colortmp t2); (** J.w. dla kolorow. *)
                        self#redrawOne chosen white; (** Usuniecie bloku z planszy. *)
                        counter<-counter-1; (** Zmniejszenie liczby blokow. *)
                        background#updatePoints 10 (** Dodanie punktow. *)
                    )
                    else if h2=red then (** Jesli czerwony (podwojny). *)
                    (
                        colors<-(List.rev_append (green::colortmp) t2); (** Zlozenie kolekcji z powrotem. *)
                        self#redrawOne chosen green; (** Zmiana koloru bloku na zielony. *)
                        background#updatePoints 10 (** Dodanie punktow. *)
                    )
                    else if h2=blue then (** Jesli niebieski (power-up). *)
                    (
                        collection<-(List.rev_append coltmp t1); (** zlozenie wczesniejszych i pozniejszych blokow. *)
                        colors<-(List.rev_append colortmp t2); (** J.w. dla kolorow. *)
                        self#redrawOne chosen white; (** Usuniecie bloku z planszy. *)
                        counter<-counter-1; (** Zmniejszenie liczby blokow. *)
                        background#updatePoints 5; (** Dodanie punktow. *)
                        let powerup=new powerups (fst h1.(0)) background plate in (** Utworzenie power-upa. *)
                            powerup#pdraw
                    )
                )
                else remove_aux t1 (h1::coltmp) t2 (h2::colortmp) (** Wywolanie rekurencyjne, jesli h1 nie jest wybranym do usuniecia blokiem. *)
            |_,_->failwith "Sth's wrong (index out of bounds)" (** To sie nie powinno wydarzyc *)
        in remove_aux collection [] colors []
    );
    if counter=0 then (background#updateLevel;plate#reset;ball#reset;self#clearCollection;self#createCollection;self#drawCollection) (** Jesli nie ma juz blokow, rysujemy nowa plansze i aktualizujemy poziom. *)
    method redrawOne chosen c=maincolor<-c;vert<-chosen;self#draw (** Przerysowanie bloku. *)
    method getCollection=collection (** Pobieranie kolekcji. *)
    method setLBulletFlying s=lBulletFlying<-s (** Ustawianie lotu lewego pocisku. *)
    method getLBulletFlying=lBulletFlying (** Pobieranie lotu lewego pocisku. *)
    method setRBulletFlying s=rBulletFlying<-s (** Ustawianie lotu prawego pocisku. *)
    method getRBulletFlying=rBulletFlying (** Pobieranie lotu prawego pocisku. *)
end

class rifle (p:int) (board:board)= (** Klasa sluzaca do rysowania i obslugi pociskow. *)
object (self)
    val mutable vert=[||] (** Tablica wierzcholkow. *)
    val mutable position=0 (** Pozycja. *)
    val mutable hits=false (** Stan (trafienie/pudlo). *)
    method erase=set_color white;fill_poly vert (** Czyszczenie (do animacji). *)
    method draw=set_color black;fill_poly vert (** Rysowanie. *)
    method setPosition x=position<-x (** Ustawianie pozycji. *)
    method hit (x,y)= (** Sprawdzenie, czy pocisk trafil w blok. *)
        if y>=360 then (** Jesli jest w obszarze planszy. *)
        (
            let rec hit_aux col=
                match col with (** Match po kolekcji. *)
                |hd::tl->let a=hd.(0) and b=hd.(2) in (** Pobieranie wspolrzednych bloku. *)
                    if (x>=fst a&&x<=fst b&&y>=snd b) then (board#removeFromCollection hd;true) else hit_aux tl (** Jesli pocisk jest w obrebie bloku-true, w przeciwnym wypadku-wywolanie rekurencyjne. *)
                |_->false (** Pocisk nie trafil w zaden blok. *)
            in hit_aux board#getCollection
        )
        else false (** Pocisk nie trafil w zaden blok. *)
    method missed y=if y>size_y() then true else false (** Sprawdzanie, czy pocisk jest juz poza plansza. *)
    method flightOfTheBullet s= (** Animacja. *)
        let rec flight_aux hit missed=
            match hit,missed with (** Match po trafieniu, badz wyleceniu poza plansze. *)
            |false,false-> (** Jesli nic nie nastapilo-pocisk leci dalej. *)
                let rec delayer()=try delay 0.003 with e->delayer() in (** Funkcja wylapujaca bledy w opoznieniu i w razie potrzeby wywolujaca je ponownie. *)
                while !locker do delay 0.001 done; (** Jesli pauza, to oczekuje na wznowienie. *)
                delayer(); (** Wywolanie opoznienia. *)
                self#erase; (** Czyszczenie. *)
                vert<-Array.map (fun (a,b)->(a,b+1)) vert; (** Ustawianie nowych wspolrzednych. *)
                self#draw; (** Rysowanie. *)
                synchronize();
                flight_aux (self#hit (vert.(1))) (self#missed (snd vert.(1))) (** Wywolanie rekurencyjne z nowymi danymi. *)
            |_,_->self#erase;if s=0 then board#setLBulletFlying false else board#setRBulletFlying false;synchronize(); (** Jesli nastapilo trafienie lub wylecenie poza plansze, sprawdzamy ktory to byl pocisk (l/r) i ustawiamy jego lot na false (tzn. przelecial) *)
        in flight_aux false false
    method shot s= (** Wywolanie. *)
        position<-p; (** Ustawianie pozycji w poziomie. *)
        vert<-[|(position,10);(position,16);(position+3,16);(position+1,10)|]; (** Ustawianie wspolrzednych. *)
        create self#flightOfTheBullet s;synchronize() (** Tworzenie watku i odswiezenie ekranu. *)
end
