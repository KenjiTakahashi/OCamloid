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
    let rx=ref 0 and ry=ref 0 and rd=ref d in (** Nowe wartosci koordynatow (x,y) oraz opoznienia (d). *)
        if ((ball#onPlate plate#getPosition plate#getWidth)&&ball#isMoving) then (** Jesli pilka odbila sie od paletki *)
            let q=plate#collision ball#getXPosition in (** Pobieranie obszaru odbicia. *)
            rx:=q;ry:=-y; (** Ustawianie nowych wspolrzednych. *)
            if q=(-3)||q=3 then rd:=0.010 (** ustawianie odpowiedniego opoznienia. *)
            else if q=(-2)||q=2 then rd:=0.006
            else rd:=0.004
        else (** Pilka odbila sie od sciany/bloku. *)
        (
            if ball#yCollided y then ry:=-y else ry:=y; (** Jesli od gornej sciany, zmiana wspolrzednej y. *)
            if ball#xCollided x then rx:=-x else rx:=x; (** Jesli od prawej lub lewej sciany, zmiana wspolrzednej x. *)
            let collision=board#collided (x,y) in (** Jesli od bloku planszy. *)
                match collision with (** Match po typie odbicia. *)
                |1->rx:=-x (** Jesli od prawej lub lewej sciany bloku, zmiana wspolrzednej x. *)
                |2->ry:=-y (** Jesli od gornej lub dolnej sciany, zmiana wspolrzednej y. *)
                |_->() (** Kompatybilnosc (nie zachodzi). *)
        );
        (!rx,!ry,!rd) (** Zwracanie nowych koordynatow i opoznienia. *)
let rec delayer f=try delay f with e->delayer f (** Funkcja wylapujaca bledy w opoznieniu i w razie potrzeby ponawiajaca wywolanie. *)
let rollTheBall()= (** Funkcja przemieszczajaca pilke. *)
    let rec delay_aux (x,y,d)=
        match ball#isDownBelow with (** Match po pozycji pilki (na planszy/pod plansza). *)
        |true-> (** Jesli pilka pod plansza. *)
            background#updateLifes (-); (** Strata zycia. *)
            plate#reset; (** Reset paletki. *)
            if background#getLifes=0 then (menu#setPause false;menu#setGameOver true;menu#drawMenu;menu#drawPoints background#getPoints); (** Jesli 0 zyc, wyjscie do menu i wypisanie punktow. *)
            ball#reset; (** Reset pilki. *)
            ball#changeState false (** Pilka nie porusza sie. *)
        |false-> (** Jesli pilka na planszy *)
            while !locker do delay 0.001 done; (* Jesli pauza, to oczekuje na wznowienie. *)
            ball#move (x,y); (** Ruch pilki. *)
            ball#changeState true; (** Zmiana stanu na aktywny (tj. pilka jest w ruchu). *)
            delayer d; (** Wywolanie funkcji opozniajacej. *)
            delay_aux (ballCoords (x,y,d)) (** Zwracanie nowych koordynatow i ponowne wywolanie funkcji przemieszczenia. *)
    in delay_aux (ballCoords(1,1,0.004))
let plateNavigation()= (** Obsluga paletki. *)
    let rec delay_aux m i=
        if background#getLifes!=0 then (** Jesli sa jeszcze zycia. *)
        (
            match i.key,i.button,m with (** Match po wcisnietym klawiszu, przycisku myszy oraz stanie pilki. *)
            |'\027',_,_->locker:=true;menu#drawMenu;synchronize() (** Jesli wcisnieto Escape - pauza *)
            |_,true,false->create rollTheBall();delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down]) (** Jesli wcisnieto przycisk myszy i pilka nie byla w ruchu - wprawienie pilki w ruch. *)
            |_,false,false-> (** Jesli nie wcisnieto przycisku i pilka nie jest w ruchu - przemieszczenie. *)
                plate#move i.mouse_x; (** Przemieszczenie paletki. *)
                ball#onPlateMove i.mouse_x; (** Przemieszczenie pilki. *)
                delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
            |_,true,true-> (** Jesli wcisnieto przycisk i pilka jest w ruchu - strzal. *)
            (
                if (background#ammoState&&board#getLBulletFlying=false&&board#getRBulletFlying=false) then (** Jesli jest amunicja i nie wystrzelono juz pociskow. *)
                (
                    background#removeAmmo; (** Odjecie pocisku. *)
                    board#setLBulletFlying true; (** Ustawianie lotu lewego pocisku. *)
                    board#setRBulletFlying true; (** Ustawianie lotu prawego pocisku. *)
                    let lrifle=new rifle plate#getPosition board in lrifle#shot 0; (** Wystrzelenie lewego pocisku. *)
                    let rrifle=new rifle (plate#getPosition+plate#getWidth-3) board in rrifle#shot 1 (** Wystrzelenie prawego pocisku. *)
                )
            );
            delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
            |_,_,true->plate#move i.mouse_x;delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down]) (** Jesli nie wcisnieto zadnego przycisku i pilka jest w ruchu - przemieszczenie paletki. *)
        )
    in delay_aux (ball#isMoving) (wait_next_event [Mouse_motion;Key_pressed;Button_down])
let instructionsNavigation()= (** Instrukcja. *)
    let rec navi_aux i=
        match i.button with (** Match po wcisnietym przycisku myszy. *)
        |true->menu#redrawLogo;synchronize() (** Jesli wcisnieto przycisk - wyjscie do menu. *)
        |false->navi_aux (wait_next_event [Button_down]) (** W przeciwnym wypadku - nic. *)
    in navi_aux (wait_next_event [Button_down])
let scoreboardNavigation()= (** Tablica wynikow. *)
    let rec navi_aux i=
        match i.button with
        |true->
                let opt=menu#get_scoreboard_opt in
                (
                    match opt with
                    |0->
                            menu#scoreboard_down;
                            navi_aux (wait_next_event [Mouse_motion;Button_down])
                    |1->
                            menu#scoreboard_up;
                            navi_aux (wait_next_event [Mouse_motion;Button_down])
                    |2->
                            menu#redrawLogo;
                            synchronize()
                    |_->navi_aux (wait_next_event [Mouse_motion;Button_down])
                )
        |false->
                menu#scoreboard_highlight i.mouse_x i.mouse_y;
                navi_aux (wait_next_event [Mouse_motion;Button_down])
    in navi_aux (wait_next_event [Mouse_motion;Button_down])
let menuNavigation()= (** Nawigacja po menu. *)
    let rec navi_aux i=
        match i.button with (** Match po wcisnietym przycisku myszy. *)
        |true-> (** Jesli wcisinieto przycisk - wykonanie akcji. *)
            let opt=menu#getOpt in (** Pobieranie aktualnie zaznaczonej opcji. *)
            (
                match opt with (** Match po opcji. *)
                |0->synchronize() (** Wyjscie. *)
                |1-> (** Rysowanie instrukcji. *)
                        menu#drawInstructions;
                        instructionsNavigation();
                        navi_aux (wait_next_event [Mouse_motion;Button_down])
                |2-> (** Rysowanie tablicy wynikow. *)
                        menu#drawScoreboard;
                        scoreboardNavigation();
                        navi_aux (wait_next_event [Mouse_motion;Button_down])
                |3-> (** Uruchamianie nowej gry. *)
                        background#resetGame; (** Reset calej planszy. *)
                        menu#setGameOver false; (** Poczatek gry. *)
                        locker:=false; (** Pauza wylaczona. *)
                        ball#reset; (** Reset pilki. *)
                        ball#changeState false; (** Pilka nie jest w ruchu. *)
                        menu#setPause true;
                        background#draw; (** Rysowanie tla i panelu bocznego. *)
                        board#createCollection; (** Tworzenie planszy. *)
                        board#drawCollection; (** Rysowanie planszy. *)
                        plateNavigation(); (** Uruchamianie sterowania. *)
                        navi_aux (wait_next_event [Mouse_motion;Button_down])
                |4-> (** Wlaczenie pauzy. *)
                        locker:=false;
                        background#draw;board#drawCollection;
                        plateNavigation();
                        navi_aux (wait_next_event [Mouse_motion;Button_down])
                |_->navi_aux (wait_next_event [Mouse_motion;Button_down]) (** Wybor poza obszarem menu - czekanie na nastepny. *)
            )
        |false->menu#highlight i.mouse_x i.mouse_y;navi_aux (wait_next_event [Mouse_motion;Button_down]) (** W przeciwnym wypadku - podswietlenie opcji w menu. *)
    in navi_aux (wait_next_event [Mouse_motion;Button_down])
let main= (** Uruchamianie programu. *)
    open_graph " 640x640"; (** Otwarcie okna z grafika. *)
    set_window_title "OCamloid 1.0"; (** Ustawienie tytulu okna. *)
    plate#draw; (** Rysowanie paletki. *)
    ball#draw; (** Rysowanie pilki. *)
    menu#get_scores;
    menu#drawMenu; (** Rysowanie menu. *)
    auto_synchronize false; (** Wylaczenie automatycznej synchronizacji (aby zapobiec miganiu animacji). *)
    menuNavigation() (** Uruchamianie nawigacji po menu. *)
