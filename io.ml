open List
open Printf

class io=
object (self)
    val file="scores.ocamloid"
    val mutable scoreboard=[]
    method private parse lines=
        let rec parse_aux lines results=
            match lines with
            |h1::t1->
                    let name::scores=Str.split (Str.regexp ",") h1 in
                    let rec parse_aux_aux scores results=
                        match scores with
                        |h2::t2->parse_aux_aux t2 ((name,int_of_string(h2))::results)
                        |_->parse_aux t1 results
                    in parse_aux_aux scores results
            |_->scoreboard<-stable_sort (fun (_,a) (_,b)->if a<b then 1 else if a>b then -1 else 0) results
        in parse_aux lines []
    method private read=
        let lines=ref [] and channel=open_in file in
        try
            while true; do
                lines:=input_line channel::!lines
            done
        with End_of_file->
            close_in channel;
            self#parse !lines
    method private write=
        let channel=open_out file in
        let rec write_aux scoreboard previous_name=
            match scoreboard with
            |(name,score)::tl->
                    (
                        if name!=previous_name then
                        (
                            (if (compare previous_name "")!=0 then fprintf channel "\n");
                            fprintf channel "%s" name
                        )
                    );
                    fprintf channel ",%i" score;
                    write_aux tl name
            |_->
                    fprintf channel "\n";
                    close_out channel
        in write_aux scoreboard ""
    method getScores=
        self#read;
        scoreboard
    method setScores=
        self#write
    method addScore name score=
        scoreboard<-(name,score)::scoreboard
    method removeScore name score=
        let rec remove_aux scores results=
            match scores with
            |hd::tl->if hd=(name,score) then remove_aux tl results else remove_aux tl (hd::results)
            |_->scoreboard<-results
        in remove_aux scoreboard []
end
